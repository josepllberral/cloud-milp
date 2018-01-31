import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.String;

import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

public class SchedulerBestFit extends quickSchedule
{
	private int minusTime = 0;
	private int waitTime = 90000;

	public static void main (String [] args)
	{
		SchedulerBestFit qs = new SchedulerBestFit(args);
		qs.controlLoop();
	}

	/* ---------------------------------------------------------- */
	/*  Constructor and Models                                    */
	/* ---------------------------------------------------------- */

	private Map <String,Double> wattpriceMap = new HashMap<String,Double>();	// This is <hostInfo.name,Value>

	public SchedulerBestFit (String [] args)
	{
		super(args);

		jobprice = jobprice * (stepTime/1000) / 3600;
		wattprice = wattprice * (stepTime/1000) / 3600;

		/* Load Information about latencies and wattprices */
		try
		{
				FileInputStream fstream = new FileInputStream("prices.data");
				DataInputStream in = new DataInputStream(fstream);
				BufferedReader br = new BufferedReader(new InputStreamReader(in));
				String strLine = "";
				while ((strLine = br.readLine()) != null)
				{
					if (strLine.compareTo("") == 0) continue;
					String[] aux = strLine.split("\\s+");
					double daux = (new Double(aux[1])).doubleValue() * (stepTime/1000) / 3600;
					wattpriceMap.put(aux[0],new Double(daux));
				}
				in.close();
		}
		catch (Exception e) { System.err.println("scheduler-WS> Error at opening price data"); e.printStackTrace(); System.exit(-1); }
	}

	/* ---------------------------------------------------------- */
	/*  Configuration of the Full System                          */
	/* ---------------------------------------------------------- */

	private double jobprice = 0.17;
	private double wattprice = 15e-05;		// WattHour
	private double jOriginalRT = 0.1;		// RT per al càlcul SLA total
	private double jOriginalRTT = 0.1;		// RT de Transpñort aqui només
	private double jBetaRT = 9; 			// Beta := Alpha - 1
	private double jBetaRTT = 9; 			// Beta := Alpha - 1 -> Aqui només per al RT de Transport
	private double velocity = 10 * (1024 * 1024 * 1024 / 8);	// 10 Gb/s
	private double imagevol = 110 * (1024 * 1024);				// 110 MB
	private double reboottime = 60;								// 60 seconds shut-down + boot up

	/* ---------------------------------------------------------- */
	/*  Scheduler                                                 */
	/* ---------------------------------------------------------- */

	private boolean beginning = true;
	private int iteration = 0;
	private int lastmigs = 0;

	protected void schedule ()
	{

		/* ------------------------------------------------------ */
		/*  RT and Cost Accounting                                */
		/* ------------------------------------------------------ */

		Map <Integer,Double> jobLastRT = new HashMap<Integer,Double>();	// This is <vmInfo.id, Value>
		DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");

		if (beginning)
		{
			beginning = false;
			System.out.println("scheduler-BestFit> Iteration: " + iteration + " " + dateFormat.format(new Date()));
			printErr.println("scheduler-BestFit> Iteration: " + iteration + " " + dateFormat.format(new Date()));
			return;
		}
		else
		{
			iteration = iteration + 1;

			/* Get RTs and Costs from the last Iteration */
			printLog.println("Iteration: " + iteration + " " + dateFormat.format(new Date()));
			System.out.println("scheduler-BestFit> Iteration: " + iteration + " " + dateFormat.format(new Date()));
			printErr.println("scheduler-BestFit> Iteration: " + iteration + " " + dateFormat.format(new Date()));
			
			jobLastRT.clear();

			double slabenefit = 0.0;
			double avgsla = 0.0;
			int counter = 0;
			for (Map.Entry<Integer,vmInfo> entry : vms.entrySet())
			{
				vmInfo vmaux = entry.getValue();
				double sr1 = vmaux.avg_served;													// Avg Served per TU (15 sec)
				double rt1 = (vmaux.avg_tpr/1000);												// Avg Response Time [includes rtt1], Now in Seconds!
				double rtt1 = (vmaux.avg_remote > 0)?((Double.valueOf(latency)/1000) * (vmaux.avg_remote/((vmaux.avg_reqs > 0)?vmaux.avg_reqs:1)) + (vmaux.avg_bpr * vmaux.avg_remote) / velocity):0;	// RT_Transport - Supposing all goes to the same location or in serie.
				if (sr1 == 0)
				{
					rt1 = 0.0; 
					rtt1 = 0.0;
					counter = counter + 1;
					slabenefit = slabenefit + 1 * jobprice;
				}
				else
				{
					double sla_aux = (1 - ((rt1 - jOriginalRT) / (jBetaRT * jOriginalRT)));
			 		if (sla_aux < 0) sla_aux = 0.0;
					if (sla_aux > 1) sla_aux = 1.0;
					slabenefit = slabenefit + sla_aux * jobprice;
					avgsla = avgsla + sla_aux;
				}
				System.out.println("scheduler-BestFit> A VM " + vmaux.id + " in " + vmaux.state + " status found at " + vmaux.hostname + " -> (per 15s): [ RTPR: " + rt1 + " (Local: " + (rt1-rtt1) + " Remote:" + rtt1 + "); Req: " + vmaux.avg_reqs + " (Local: " + vmaux.avg_localr + " Remote: " + vmaux.avg_remote + "); Queue: " + vmaux.avg_queue + "; Served: " + vmaux.avg_served + "; BPR: " + vmaux.avg_bpr + "]");
				jobLastRT.put(new Integer(vmaux.id).intValue(),rt1);

				/* TEST */
				System.out.println("ARFF> " + (vmaux.avg_reqs + vmaux.avg_queue) + " " + vmaux.avg_reqs + " " + vmaux.avg_queue + " " + vmaux.avg_bpr + " " + vmaux.cpupm + " " + vmaux.mempm + " " + vmaux.usedcpu + " " + (vmaux.usedmemory/1024) + " "+ 0 + " " + 0 + " " + 0 + " " + 0 + " " + vmaux.avg_tppr + " " + 0 + " " + vmaux.avg_sla + " " + 0 + " " + vmaux.avg_slaint + " " + 0 + " " + ((vmaux.avg_reqs > 0)?(vmaux.avg_remote/vmaux.avg_reqs):0));
				/* TEST END */
			}
			avgsla = (Double.valueOf(vms.size() - counter) > 0)?(avgsla / Double.valueOf(vms.size() - counter)):1.0;

			double usedcpus = 0.0;
			double usedhosts = 0.0;
			double energycons = 0.0;
			double energycost = 0.0;
			for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet())
			{
				hostInfo hostaux = entry.getValue();

				double auxcpu = Math.ceil(hostaux.used_cpu_real/100.0);
				if (hostaux.running_vms > 0) 
				{
					usedcpus = usedcpus + auxcpu;
					energycons = energycons + powercons(auxcpu);
					usedhosts = usedhosts + 1;

					double wattprice_aux = (wattpriceMap.get(hostaux.name)).doubleValue();
					energycost = energycost + powercons(auxcpu) * wattprice_aux;
				}
			}
			double benefit = slabenefit - energycost;// - lastmigs * ((((imagevol * 8) / velocity) + (latency/1000) + reboottime) * (jobprice/(stepTime/1000)));
			printLog.println("Iteration: " + iteration + " " + benefit + " " + slabenefit + " " + energycons + " " + energycost + " " + avgsla + " " + usedcpus + " " + usedhosts);
			System.out.println("Summary Prev: " + iteration + " " + benefit + " " + slabenefit + " " + energycons + " " + energycost + " " + avgsla + " " + usedcpus + " " + usedhosts);
		}

		/* ------------------------------------------------------ */
		/*  Scheduling Decision                                   */
		/* ------------------------------------------------------ */

		/* Order VMs by CPU Occupation: InsertSort -> Replace if Host Size becomes >>>50 */
		int [] order = new int[vms.size()];
		for (int i=0; i < vms.size(); i++) order[i] = i;
		Object [] candidateVMS = (vms.keySet()).toArray();

		for (int i=0; i < vms.size(); i++)
		{
			for (int j=i; j > 0; j--)
			{
				vmInfo auxvj1 = vms.get((Integer)candidateVMS[order[j-1]]);
				vmInfo auxvj2 = vms.get((Integer)candidateVMS[order[j]]);
				if (auxvj1.usedcpu < auxvj2.usedcpu)
				{
					int swap = order[j-1];
					order[j-1] = order[j];
					order[j] = swap;
				}
			}
		}
		
		/* Put Host Occupation to Zero*/
		for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet())
		{
			entry.getValue().used_cpu_real = 0;
			entry.getValue().used_mem_real = 0;
		}

		/* TEMPATIVE NEW SCHEDULING MATRIX */
		Map <vmInfo,hostInfo> binmatrix = new HashMap<vmInfo,hostInfo>();
		for (int i=0; i < order.length; i++) binmatrix.put(vms.get((Integer)candidateVMS[order[i]]),null);

		/* BEST BENEFIT GIVEN A HOST */
		Map <String,Double> hostBenefitAux = new HashMap<String,Double>();
		for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet()) hostBenefitAux.put(entry.getValue().name,0.0);

		/* CPU, MEM, IN, OUT Requirements */
		Map <vmInfo,Double[]> requirements = new HashMap<vmInfo,Double[]>();
		for (int i=0; i < order.length; i++)
		{
			vmInfo auxjob = vms.get((Integer)candidateVMS[order[i]]);

			double auxJobCPU = auxjob.usedcpu;
			double auxJobMEM = ((auxjob.usedmemory/1024)>1024)?1024:(auxjob.usedmemory/1024);
			double auxJobIN  = (auxjob.nettx/1024);
			double auxJobOUT = (auxjob.netrx/1024);
			if (auxJobCPU < 0.1) auxJobCPU = auxjob.usedcpu;

			if ((auxjob.lcm_state).compareTo("RUNNING") != 0) { auxJobCPU = 1; auxJobMEM = 128; auxJobIN = 1; auxJobOUT = 1; }
			System.out.println("Trying " + i + " (" + auxjob.id + ") [" +  auxjob.avg_reqs + " " + auxjob.avg_bpr + " " + auxjob.avg_tpr0 + "] " + auxJobCPU + " (" + auxjob.usedcpu + ") " + auxJobMEM + " (" + auxjob.memory + ")");

			requirements.put(auxjob,new Double[]{auxjob.avg_reqs,auxjob.avg_bpr,auxjob.avg_tpr0,auxJobCPU,auxJobMEM,auxJobIN,auxJobOUT});
		}

		/* FOR EACH JOB: FIND BEST FIT */
		for (int i=0; i < order.length; i++)
		{
			/* Job (VM) Number 'i' */
			vmInfo auxjob = vms.get((Integer)candidateVMS[order[i]]);

			hostInfo bestHostFound = null;
			double maxBenefitFound = -9E15;

			/* Compile Source table */
			if (debugInfo) System.out.println("Compilation for VM: " + auxjob.id);
			int totalLoad = 0;
			List<HashMap<String,Integer>> jsources = auxjob.sources;
			HashMap<String,Integer> sourceComp = new HashMap<String,Integer>();
			for (HashMap<String,Integer> element : jsources)
			{
				for (Map.Entry<String,Integer> RTTentry : element.entrySet())
				{
					int rsource = RTTentry.getValue().intValue();
					String lsource = RTTentry.getKey();
					if (debugInfo) System.out.println("VM:" + auxjob.id + " Source:" + lsource + " Reqs:" + rsource);
					if (sourceComp.get(lsource) == null) sourceComp.put(lsource,rsource);
					else
					{
						int aux = (sourceComp.get(lsource)).intValue();
						sourceComp.put(lsource,(aux+rsource));
					}
					totalLoad += rsource;
				}
			}
			if (debugInfo) for (Map.Entry<String,Integer> element : sourceComp.entrySet())	{ System.out.println("VM: " + auxjob.id + " " + element.getKey() + " " + element.getValue() + " R:" + auxjob.avg_remote + " T:" + auxjob.avg_reqs); }

			/* Current VM information */
			double jreq_aux = requirements.get(auxjob)[0];
			double jbyt_aux = requirements.get(auxjob)[1];
			double jcpu_aux = requirements.get(auxjob)[3] * 1.2; // Overhead sobre CPUPM -> 1.2
			double jmem_aux = requirements.get(auxjob)[4];

			if (debugInfo) System.out.println("Working with: " + auxjob.id);

			/* FOR EACH HOST: TRY */
			for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet())
			{
				/* Host Candidate */
				hostInfo auxhost = entry.getValue();
				if (bestHostFound == null) bestHostFound = auxhost;

				/* Register in binmatrix auxiliar */
				Map <vmInfo,hostInfo> binaux = new HashMap<vmInfo,hostInfo>();
				for (Map.Entry<vmInfo,hostInfo> auxentry : binmatrix.entrySet()) binaux.put(auxentry.getKey(),auxentry.getValue());
				binaux.put(auxjob,auxhost);

				/* What host has Occupied + new temptative load */
				double auxOccCPU = 0;
				double auxOccMEM = 0;
				for (Map.Entry<vmInfo,hostInfo> auxentry : binaux.entrySet())
				{
					if (auxhost == auxentry.getValue())
					{
						auxOccCPU = auxOccCPU + requirements.get(auxentry.getKey())[3] * 1.2;
						auxOccMEM = auxOccMEM + requirements.get(auxentry.getKey())[4];
					}
				}

				/* Check Viability and Individual Benefit */
				double benefitTemp = 0.0;	// Alerta! - No és el benefici real, sino l'increment de benefici

				/*if (debugInfo)*/ System.out.println("Attempting VM " + auxjob.id + " in " + auxhost.hostname + " (CPU: " + auxhost.max_cpu + " vs " + auxOccCPU + " [" + jcpu_aux + "]) (MEM: " + (auxhost.max_mem/1024) + " vs " + auxOccMEM + " [" + jmem_aux + "])");	
				if (auxOccCPU <= auxhost.max_cpu && auxOccMEM <= (auxhost.max_mem/1024))
				{
					/* Calculate Transport Response Time and SLA */
					String isp_aux = mapISP.get(auxhost.hostname);
					double loadsource_aux = (sourceComp.get(isp_aux) != null)?sourceComp.get(isp_aux):0.0;
					double remoteLoadFact = (totalLoad > 0)?((totalLoad - loadsource_aux)/totalLoad):0.0;
					double jrttf_aux = (((new Double(latency)).doubleValue()/1000) * remoteLoadFact) + (jbyt_aux * (totalLoad - loadsource_aux)) / velocity;
					/*if (debugInfo)*/ System.out.println("              " + auxjob.id + " in " + auxhost.hostname + " (ISP: " + isp_aux + "; load from it: " + loadsource_aux + " vs ext load: " + (totalLoad-loadsource_aux) + ")");

					double sla_aux = (1 - ((jrttf_aux - jOriginalRTT) / (jBetaRTT * jOriginalRTT)));
					if (sla_aux < 0) sla_aux = 0.0;
					if (sla_aux > 1) sla_aux = 1.0;

					/* Calculate Increment of Power */
					double power_aux = powercons(auxOccCPU/100.0) - powercons((auxOccCPU-jcpu_aux)/100.0);
					double wattprice_aux = (wattpriceMap.get(auxjob.hostname)).doubleValue();

					/* Calculate Cost of Migration */
					double migr_aux = 0.0;
					int latency_aux = (latencyMap.get(auxjob.hostname+"-"+auxhost.name)).intValue();
					if (auxjob.hostname.compareTo(auxhost.name) != 0) migr_aux = ((((imagevol * 8) / velocity) + ((new Double(latency_aux)).doubleValue()/1000)) * (jobprice/(stepTime/1000)));

					/* Check If Current Best Fit */
					benefitTemp = benefitTemp + sla_aux * jobprice - power_aux * wattprice_aux - migr_aux; // BenefitTemp = Incr. JobPrice - Incr. Power - Migr. Penalty
					if (benefitTemp > maxBenefitFound)
					{
						maxBenefitFound = benefitTemp;
						bestHostFound = auxhost;
					}
					/*if (debugInfo)*/ System.out.println("              " + auxjob.id + " in " + auxhost.hostname + " (RTF: " + jrttf_aux + " SLA: " + sla_aux + " PWR: " + power_aux + " MIG:" + migr_aux + " BNF:" + benefitTemp + ")");
				}
				else
				{
					if (bestHostFound == auxhost) bestHostFound = null;
				}
				
			}
			if (bestHostFound != null)
			{
				binmatrix.put(auxjob,bestHostFound);
				hostBenefitAux.put(bestHostFound.name,maxBenefitFound);

				/* Final Resolution */
				bestHostFound.used_cpu_real += jcpu_aux;
				bestHostFound.used_mem_real += jmem_aux;
		
				System.out.println("For VM " + auxjob.id + ": selected " + bestHostFound.id + " (" + bestHostFound.hostname + ") " + bestHostFound.used_cpu_real + " " + bestHostFound.used_mem_real);
			}
			else
			{
				hostBenefitAux.put(bestHostFound.name,maxBenefitFound);
				System.out.println("WARNING> " + auxjob.id + " Not allocated... Should Be STOPPED");	// FIXME - Send stopvm command
			}
		}

		/* ------------------------------------------------------ */
		/*  Movement Realization                                  */
		/* ------------------------------------------------------ */

		lastmigs = 0;
		for (Map.Entry<vmInfo,hostInfo> entry : binmatrix.entrySet())
		{
			vmInfo auxvm = entry.getKey();
			hostInfo auxhost = entry.getValue();
			if (debugInfo) System.out.println("scheduler-BestFit> A VM " + auxvm.id + " in " + auxvm.state + " status found...");
			if (debugInfo) System.out.println("scheduler-BestFit> A VM " + auxvm.id + " to be placed in " + auxhost.hostname + " status found...");

			boolean condicio = (auxvm.hostname == null)?true:(auxvm.hostname.compareTo(auxhost.hostname) != 0);
			if (condicio)
			{
				if ((auxvm.state).compareTo("FAILED") == 0)
				{
					operateVM(new int[]{auxvm.id},RESUBMITVM);
					System.out.println("scheduler-BestFit> The VM " + auxvm.id + " is resubmited");
					try { Thread.currentThread().sleep(waitTime); minusTime = minusTime + waitTime; }
					catch (Exception e) { System.err.println("scheduler-BestFit> Error on Running Sleep (REALIZEMOVS)\n" + e.getMessage()); }
				}

				if ((auxvm.state).compareTo("PENDING") == 0)
				{
					operateVM(new int[]{auxvm.id, auxhost.id},DEPLOYVM);
					/*if (debugInfo)*/ System.out.println("scheduler-BestFit> The VM " + auxvm.id + " is deployed in " + auxhost.id);
				}
				else if ((auxvm.state).compareTo("ACTIVE") == 0)
				{
					if (isVBox)
					{
						operateVM(new int[]{auxvm.id, auxhost.id},MIGRATEVM);
					}
					else operateVM(new int[]{auxvm.id, auxhost.id},LIVEMIGRATEVM);
					/*if (debugInfo)*/ System.out.println("scheduler-BestFit> The VM " + auxvm.id + " is migrated from " + auxvm.hostname + " to " + auxhost.hostname);
					lastmigs = lastmigs + 1;
				}
				/* Sleep until deployed */
				try { Thread.currentThread().sleep(waitTime); minusTime = minusTime + waitTime; }
				catch (Exception e) { System.err.println("scheduler-BestFit> Error on Running Sleep OPERATIONVM\n" + e.getMessage()); }
			}
			else
			{
				/*if (debugInfo)*/ System.out.println("scheduler-BestFit> The VM " + auxvm.id + " stays at " + auxvm.hostname + " (" + auxhost.hostname + ")");			
			}
		}
	}

	/* ---------------------------------------------------------- */
	/*  Models of Power and Auxiliar Functions                    */
	/* ---------------------------------------------------------- */

	//private double powercons (double usedcpu)
	//{
	//	if (usedcpu <= 0) return 0;
	//	else if (usedcpu > 0 && usedcpu <= 1) return 267.8;
	//	else if (usedcpu > 1 && usedcpu <= 2) return 285.5;
	//	else if (usedcpu > 2 && usedcpu <= 3) return 302.5;
	//	else if (usedcpu > 3 && usedcpu <= 4) return 317.9;
	//	else return 317.9;
	//}

	private double powercons (double usedcpu)
	{
		if (usedcpu <= 0) return 0;
		else if (usedcpu > 0 && usedcpu <= 1) return 29.1;
		else if (usedcpu > 1 && usedcpu <= 2) return 30.4;
		else if (usedcpu > 2 && usedcpu <= 3) return 31.3;
		else if (usedcpu > 3 && usedcpu <= 4) return 31.8;
		else return 31.8;
	}

}
