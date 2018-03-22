import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.String;

import java.util.Date;
import java.text.DateFormat;
import java.text.SimpleDateFormat;

import weka.core.Attribute;
import weka.core.DenseInstance;
import weka.classifiers.Evaluation;
//import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import weka.classifiers.trees.M5P;
import weka.classifiers.functions.LinearRegression;
import weka.classifiers.lazy.IBk;

public class SchedulerWeka extends quickSchedule
{
	private int minusTime = 0;
	private int waitTime = 90000;

	public static void main (String [] args)
	{
		SchedulerWeka qs = new SchedulerWeka(args);
		qs.controlLoop();
	}

	/* ---------------------------------------------------------- */
	/*  Constructor, Models and Structures                        */
	/* ---------------------------------------------------------- */

	private M5P cpuRegTree;
	private M5P inputRegTree;
	private M5P outputRegTree;
	private M5P resptimeRegTree;
//	private M5P slaRegTree;
	private IBk slaKNN;
	private M5P slaiRegTree;
	private M5P cpupmRegTree;
	private M5P queueRegTree;
//	private LinearRegression queueLinReg;
	private LinearRegression memoryLinReg;

	private Map <String,Double> wattpriceMap = new HashMap<String,Double>();	// This is <hostInfo.name,Value>

	public SchedulerWeka (String [] args)
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

		/* Loading Machine Learning Models */
		this.cpuRegTree = new M5P();
		this.inputRegTree = new M5P();
		this.outputRegTree = new M5P();
		this.resptimeRegTree = new M5P();
		this.slaKNN = new IBk();
		this.slaiRegTree = new M5P();
		this.memoryLinReg = new LinearRegression();

		try { loadModels(); }
		catch (Exception e) { System.err.println("scheduler-WS> Error at opening ML models"); e.printStackTrace(); System.exit(-1); }
	}

	/* ---------------------------------------------------------- */
	/*  Configuration of the Full System                          */
	/* ---------------------------------------------------------- */

	private double jobprice = 0.17;
	private double wattprice = 15e-05;							// WattsHour
	private double jOriginalRT = 0.1;							// RT per al càlcul SLA total
	private double jOriginalRTT = 0.01;							// RT de Transpñort aqui només
	private double jBetaRT = 9; 								// Beta := Alpha - 1
	//private double jBetaRTT = 9; 								// Beta := Alpha - 1 -> Aqui només per al RT de Transport
	private double velocity = 10 * (1024 * 1024 * 1024 / 8);	// 10 Gb/s
	private double imagevol = 110 * (1024 * 1024);				// 110 MB
	private double reboottime = 60;								// 60 seconds shut-down + boot up

	/* ---------------------------------------------------------- */
	/*  Scheduler                                                 */
	/* ---------------------------------------------------------- */

	private boolean beginning = true;
	private int iteration = 0;
	private int lastmigs = 0;

	private Map <Integer,Long> jobLastMem = new HashMap<Integer,Long>();		// This is <vmInfo.id, Value>
	private Map <Integer,Double> jobLastQueue = new HashMap<Integer,Double>();	// This is <vmInfo.id, Value>

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
			System.out.println("scheduler-WS> Iteration: " + iteration + " " + dateFormat.format(new Date()));
			return;
		}
		else
		{
			iteration = iteration + 1;

			/* Get RTs and Costs from the last Iteration */
			printLog.println("Iteration: " + iteration + " " + dateFormat.format(new Date()));
			System.out.println("scheduler-WS> Iteration: " + iteration + " " + dateFormat.format(new Date()));
			printErr.println("scheduler-WS> Iteration: " + iteration + " " + dateFormat.format(new Date()));
			
			jobLastRT.clear();

			double slabenefit = 0.0;
			double avgsla = 0.0;
			int counter = 0;
			for (Map.Entry<Integer,vmInfo> entry : vms.entrySet())
			{
				vmInfo vmaux = entry.getValue();
				double sr1 = vmaux.avg_served;													// Avg Served per TU (15 sec)
				double rt1 = (vmaux.avg_tpr/1000);												// Avg Response Time [includes rtt1], Now in Seconds!
				double rtt1 = (vmaux.avg_remote > 0)?((Double.valueOf(latency)/1000) * ((vmaux.avg_reqs > 0)?(vmaux.avg_remote/vmaux.avg_reqs):0) + (vmaux.avg_bpr * vmaux.avg_remote) / velocity):0;	// RT_Transport - Supposing all goes to the same location or in serie.
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
				System.out.println("scheduler-WS> VM " + vmaux.id + " " + vmaux.hostname + " (" + vmaux.state + ") -> (per 15s): [ RTPR: " + rt1 + " (Process: " + (vmaux.avg_tppr/1000) + " Remote:" + rtt1 + "); Req: " + vmaux.avg_reqs + " (Local: " + vmaux.avg_localr + " Remote: " + vmaux.avg_remote + "); Queue: " + vmaux.avg_queue + "; Served: " + vmaux.avg_served + "; BPR: " + vmaux.avg_bpr + "]");
				jobLastRT.put(new Integer(vmaux.id).intValue(),rt1);

				/* TEST */
				long prevMem = (jobLastMem.containsKey(new Integer(vmaux.id)))?jobLastMem.get(new Integer(vmaux.id)):280;
				double auxAvgTPR = Math.min(vmaux.avg_tpr0,vmaux.avg_tppr);	// FIXME - Min entre avg temps en buit i avg temps proces (evitar que un temps en buit saturat passi)...
				double prevQueue = (jobLastQueue.containsKey(new Integer(vmaux.id)))?jobLastQueue.get(new Integer(vmaux.id)):0.0;

				double predcpu = predictCPU((vmaux.avg_reqs+vmaux.avg_queue),vmaux.avg_bpr,auxAvgTPR);					// (0 - 100*n)
				double predmem = predictMEM((vmaux.avg_reqs+vmaux.avg_queue),vmaux.avg_bpr,auxAvgTPR,prevMem,predcpu);	// (MBs)
				double predin  = predictIN((vmaux.avg_reqs+vmaux.avg_queue),vmaux.avg_bpr,auxAvgTPR,predcpu,predmem);
				double predout = predictOUT((vmaux.avg_reqs+vmaux.avg_queue),vmaux.avg_bpr,auxAvgTPR,predcpu,predmem);
				if (predin <= 0) predin = 0;
				if (predout <= 0) predout = 0;

				double jqueued_aux = (vmaux.avg_reqs - vmaux.avg_served);
				if (jqueued_aux < 0) jqueued_aux = 0.0;

				double jrtf_aux = predictRT((vmaux.avg_reqs+vmaux.avg_queue),vmaux.avg_reqs,vmaux.avg_queue,vmaux.avg_bpr,vmaux.cpupm,vmaux.mempm,vmaux.usedcpu,(vmaux.usedmemory/1024),predcpu,predmem,predin,predout,jqueued_aux,prevQueue,vmaux.id,-1);
				if (jrtf_aux < 0) jrtf_aux = 0.0;

				//double jrtf_aux2 = predictRT((vmaux.avg_reqs+0),vmaux.avg_reqs,0,vmaux.avg_bpr,vmaux.cpupm,vmaux.mempm,vmaux.usedcpu,(vmaux.usedmemory/1024),predcpu,predmem,predin,predout,jqueued_aux,prevQueue,vmaux.id,-1);
				//if (jrtf_aux2 < 0) jrtf_aux2 = 0.0;

				double jqueue_aux = predictQUEUE(0,vmaux.avg_reqs,vmaux.avg_tpr0,vmaux.avg_bpr,vmaux.cpupm,vmaux.mempm,vmaux.usedcpu,(vmaux.usedmemory/1024),predcpu,predmem,predin,predout,jqueued_aux,prevQueue,vmaux.id,-1);
				if (jqueue_aux < 0) jqueue_aux = 0.0;

				//double jrtf_aux3 = predictRT((vmaux.avg_reqs+jqueue_aux),vmaux.avg_reqs,jqueue_aux,vmaux.avg_bpr,vmaux.cpupm,vmaux.mempm,vmaux.usedcpu,(vmaux.usedmemory/1024),predcpu,predmem,predin,predout,jqueued_aux,prevQueue,vmaux.id,-1);
				//if (jrtf_aux3 < 0) jrtf_aux3 = 0.0;

				//double jrtf_aux4 = predictRT((vmaux.avg_reqs+prevQueue),vmaux.avg_reqs,prevQueue,vmaux.avg_bpr,vmaux.cpupm,vmaux.mempm,vmaux.usedcpu,(vmaux.usedmemory/1024),predcpu,predmem,predin,predout,jqueued_aux,prevQueue,vmaux.id,-1);
				//if (jrtf_aux4 < 0) jrtf_aux4 = 0.0;

				double foreig_aux = ((vmaux.avg_reqs > 0)?(vmaux.avg_remote/vmaux.avg_reqs):0);

				double jsla_aux = predictSLA(vmaux.avg_reqs,vmaux.avg_bpr,vmaux.cpupm,vmaux.mempm,predcpu,predmem,predin,predout,foreig_aux,vmaux.avg_tpr0,jqueued_aux,prevQueue,vmaux.id,-1);
				if (jsla_aux < 0) jsla_aux = 0.0; if (jsla_aux > 1) jsla_aux = 1.0;

				//double jslai_aux = predictSLAI((vmaux.avg_reqs+vmaux.avg_queue),vmaux.avg_bpr,vmaux.cpupm,vmaux.mempm,vmaux.usedcpu,(vmaux.usedmemory/1024),predcpu,predmem,predin,predout,jrtf_aux,foreig_aux,vmaux.id,-1);
				//if (jslai_aux < 0) jslai_aux = 0.0; if (jslai_aux > 1) jslai_aux = 1.0;

				System.out.println("ARFF> " + (vmaux.avg_reqs + vmaux.avg_queue) + " " + vmaux.avg_reqs + " " + vmaux.avg_queue + " " + vmaux.avg_bpr + " " + vmaux.cpupm + " " + vmaux.mempm + " " + vmaux.usedcpu + " " + (vmaux.usedmemory/1024) + " "+ predcpu + " " + predmem + " " + predin + " " + predout + " " + vmaux.avg_tppr + " " + jrtf_aux + " " + vmaux.avg_sla + " " + jsla_aux + " " + vmaux.avg_slaint + " " + foreig_aux + " " + vmaux.ioinpm + " " + vmaux.iooutpm + " " + vmaux.avg_tpr0 + " " +  jqueue_aux + " " + jqueued_aux + " " + prevQueue );
				/* TEST END */

			}
			avgsla = (Double.valueOf(vms.size() - counter) > 0)?(avgsla / Double.valueOf(vms.size() - counter)):1.0;

			/* TEST - ONLY FOR SAME MACHINE */
			double auxOccCPU = 0;
			int auxNumVMs = 0;
			double auxMeanCPU = 0;
			for (Map.Entry<Integer,vmInfo> entry : vms.entrySet())
			{
				vmInfo vmaux = entry.getValue();
				double auxAvgTPR = Math.min(vmaux.avg_tpr0,vmaux.avg_tppr);	// FIXME - Min entre avg temps en buit i avg temps proces (evitar que un temps en buit saturat passi)...

				auxOccCPU = auxOccCPU + predictCPU((vmaux.avg_reqs+vmaux.avg_queue),vmaux.avg_bpr,auxAvgTPR);
				auxNumVMs += 1 ;
				auxMeanCPU += vmaux.cpupm;
			}
			System.out.println("PMPred> " + auxOccCPU + " " + auxNumVMs + " " + (auxMeanCPU/auxNumVMs) + " " + predictCPUPM(auxOccCPU,auxNumVMs));
			/* TEST END */

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
			double benefit = slabenefit - energycost; // - lastmigs * ((((imagevol * 8) / velocity) + ((new Double(latency)).doubleValue()/1000) + reboottime) * (jobprice/((new Double(stepTime)).doubleValue()/1000)));
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

		/* CPU, MEM, IN, OUT PREDICTIONS */
		Map <vmInfo,Double[]> predictions = new HashMap<vmInfo,Double[]>();
		for (int i=0; i < order.length; i++)
		{
			vmInfo auxjob = vms.get((Integer)candidateVMS[order[i]]);

			long prevMem = (jobLastMem.containsKey(new Integer(auxjob.id)))?jobLastMem.get(new Integer(auxjob.id)):280;
			double auxAvgTPR = Math.min(auxjob.avg_tpr0,auxjob.avg_tppr);	// FIXME - Min entre avg temps en buit i avg temps proces (evitar que un temps en buit saturat passi)...
			double prevQueue = (jobLastQueue.containsKey(new Integer(auxjob.id)))?jobLastQueue.get(new Integer(auxjob.id)):0.0;
	
			double auxJobCPU = predictCPU((auxjob.avg_reqs+auxjob.avg_queue),auxjob.avg_bpr,auxAvgTPR);					// (0 - 100*n)
			double auxJobMEM = predictMEM((auxjob.avg_reqs+auxjob.avg_queue),auxjob.avg_bpr,auxAvgTPR,prevMem,auxJobCPU);				// (MBs)
			double auxJobIN  = predictIN((auxjob.avg_reqs+auxjob.avg_queue),auxjob.avg_bpr,auxAvgTPR,auxJobCPU,auxJobMEM);
			double auxJobOUT = predictOUT((auxjob.avg_reqs+auxjob.avg_queue),auxjob.avg_bpr,auxAvgTPR,auxJobCPU,auxJobMEM);
			if (auxJobCPU <= 0) auxJobCPU = auxjob.usedcpu; if (auxJobCPU > 400) auxJobCPU = 400;
			if (auxJobMEM <= 0) auxJobMEM = (auxjob.usedmemory/1024); if (auxJobMEM > 1600) auxJobMEM = 1600;
			if (auxJobIN <= 0) auxJobIN = 0;
			if (auxJobOUT <= 0) auxJobOUT = 0;

			if ((auxjob.lcm_state).compareTo("RUNNING") != 0) { auxJobCPU = 1; auxJobMEM = 512; auxJobIN = 1; auxJobOUT = 1; }
			System.out.println("schedule-WS> Prediction " + i + " (" + auxjob.id + ") [" +  auxjob.avg_reqs + " " + auxjob.avg_queue + " " + auxjob.avg_served + " " + auxjob.avg_bpr + " " + auxjob.avg_tpr0 + " " + auxjob.avg_tppr + " " + auxjob.avg_tpr + "] " + auxJobCPU + " (" + auxjob.usedcpu + ") " + auxJobMEM + " (" + (auxjob.usedmemory/1024) + ") " + auxJobIN + " (" + auxjob.ioinpm + ") " + auxJobOUT + " (" + auxjob.iooutpm + ") [" + auxjob.cpupm + " " + auxjob.mempm + " " + auxjob.ioinpm + " " + auxjob.iooutpm + "]");

			//jobLastMem.put(new Integer(auxjob.id),new Long(auxjob.usedmemory/1024));
			//jobLastQueue.put(new Integer(auxjob.id),new Double(auxjob.avg_queue));

			predictions.put(auxjob,new Double[]{auxjob.avg_reqs,auxjob.avg_bpr,auxjob.avg_tpr0,auxJobCPU,auxJobMEM,auxJobIN,auxJobOUT});
		}

		/* BEST JRTTF GIVEN A HOST */
		Map <Integer,Double> JRTTFAux = new HashMap<Integer,Double>();
		for (Map.Entry<Integer,vmInfo> entry : vms.entrySet()) JRTTFAux.put(new Integer(entry.getValue().id),-1.0);

		/* FOR EACH JOB: FIND BEST FIT */
		for (int i=0; i < order.length; i++)
		{
			/* Job (VM) Number 'i' */
			vmInfo auxjob = vms.get((Integer)candidateVMS[order[i]]);

			hostInfo bestHostFound = null;
			double maxBenefitFound = -9E15;
			double currentBenefitAux = -9E15;
			double [] bestPrediction = new double[]{-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0, -1.0}; /* CPU MEM IN OUT JRTF REQS BYTES CPUPM MEMPM RCPU RMEM JRTTF JSLA*/

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
			//double jreq_aux = predictions.get(auxjob)[0];
			double jbyt_tmp = predictions.get(auxjob)[1];
			double jcpu_aux = predictions.get(auxjob)[3];
			double jmem_aux = predictions.get(auxjob)[4];

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
				int auxNumVMs = 0;
				for (Map.Entry<vmInfo,hostInfo> auxentry : binaux.entrySet())
				{
					if (auxhost == auxentry.getValue())
					{
						auxOccCPU = auxOccCPU + predictions.get(auxentry.getKey())[3];
						auxOccMEM = auxOccMEM + predictions.get(auxentry.getKey())[4];
						auxNumVMs += 1 ;
					}
				}

				/* Share of resources the job adquires */
				double ratioOccupCPU = auxOccCPU/auxhost.max_cpu;
				double ratioOccupMEM = auxOccMEM/(auxhost.max_mem/1024);
				double estOccCPU = predictCPUPM(auxOccCPU,auxNumVMs);

				/* Calculate Transport Response Time and SLA */
				String isp_aux = mapISP.get(auxhost.hostname);
				double loadsource_aux = (sourceComp.get(isp_aux) != null)?sourceComp.get(isp_aux):0.0;
				double remoteLoadFact = (totalLoad > 0)?((totalLoad - loadsource_aux)/totalLoad):0;
				double jrttf_temp = (((new Double(latency)).doubleValue()/1000) * remoteLoadFact) + (jbyt_tmp * (totalLoad - loadsource_aux)) / velocity;
				/*if (debugInfo)*/ System.out.println("Attempting VM " + auxjob.id + " in " + auxhost.hostname + " (ISP: " + isp_aux + "; load from it: " + loadsource_aux + " vs ext load: " + (totalLoad-loadsource_aux) + ") -> RTTF: " + jrttf_temp);

				/* Check Viability and Individual Benefit */
				double benefitTemp = 0.0;					// Alerta! - No és el benefici real, sino l'increment de benefici
				double [] tempPrediction = new double[13];	// reqs queue_aux bpr cpuvm memvm qcpuvm invm outvm

				/* CHECK ALL JOBS IN HOST [only when sharing CPU completely] */
				for (Map.Entry<vmInfo,hostInfo> auxentry : binaux.entrySet())
				{
					/* FOR EACH JOB IN HOST -> CHECK/PREDICT SLA ADDING THE JOB */
					if (auxhost == auxentry.getValue())
					{
						// Calculate Production Response Time
						double jreq_aux = predictions.get(auxentry.getKey())[0];
						double jbyt_aux = predictions.get(auxentry.getKey())[1];

						double jobcpu_aux = predictions.get(auxentry.getKey())[3];
						double jobmem_aux = predictions.get(auxentry.getKey())[4];
						double jobin_aux = predictions.get(auxentry.getKey())[5];
						double jobout_aux = predictions.get(auxentry.getKey())[6];
						if (jobcpu_aux == 0.0) jobcpu_aux = auxentry.getKey().usedcpu;

						double rcpu_aux = (ratioOccupCPU > 1)?(jobcpu_aux / ratioOccupCPU):jobcpu_aux;//(auxentry.getValue().max_cpu) * ratioOccupCPU;
						double rmem_aux = (ratioOccupMEM > 1)?(jobmem_aux / ratioOccupMEM):jobmem_aux;//(auxentry.getValue().max_mem/1024) * ratioOccupMEM;

						double jqueued_aux = (auxentry.getKey().avg_reqs - auxentry.getKey().avg_served);
						if (jqueued_aux < 0) jqueued_aux = 0.0;

						double immQueue = auxentry.getKey().avg_queue;
						double prevQueue = (jobLastQueue.containsKey(new Integer(auxentry.getKey().id)))?jobLastQueue.get(new Integer(auxentry.getKey().id)):0.0;

						double jrtf_aux = 0.001 * predictRT((jreq_aux+immQueue),jreq_aux,immQueue,jbyt_aux,estOccCPU,auxOccMEM*1.5,rcpu_aux,rmem_aux,jobcpu_aux,jobmem_aux,jobin_aux,jobout_aux,jqueued_aux,prevQueue,auxentry.getKey().id,auxentry.getValue().id);
						if (jrtf_aux < 0) jrtf_aux = 0.0;

						// Retrieve Transport Response Time
						double jrttf_aux = 0.0;
						if (auxentry.getKey() == auxjob) jrttf_aux = jrttf_temp;
						else jrttf_aux = JRTTFAux.get(new Integer(auxentry.getKey().id));
				
						// Calculate SLA
						double sla_aux = (1 - (((jrtf_aux + jrttf_aux) - jOriginalRT) / (jBetaRT * jOriginalRT)));
						if (sla_aux < 0) sla_aux = 0.0; if (sla_aux > 1) sla_aux = 1.0;

						// Predict SLA
						double foreig_aux = ((auxentry.getKey().avg_reqs > 0)?(auxentry.getKey().avg_remote/auxentry.getKey().avg_reqs):0);
						double jsla_aux = predictSLA(jreq_aux,jbyt_aux,estOccCPU,auxOccMEM*1.5,jobcpu_aux,jobmem_aux,jobin_aux,jobout_aux,foreig_aux,auxentry.getKey().avg_tpr0,jqueued_aux,prevQueue,auxentry.getKey().id,auxentry.getValue().id);
						if (jsla_aux < 0) jsla_aux = 0.0; if (jsla_aux > 1) jsla_aux = 1.0;

						// Si Reqs = 0 -> Es cobra igual!
						if (jreq_aux == 0) { jsla_aux = 1.0; sla_aux = 1.0; }

						// Decidir aqui si guiar-se per CalculateSLA o PredictedSLA
						benefitTemp = benefitTemp + jsla_aux * jobprice;
						if (auxjob == auxentry.getKey()) 
						{
							if (debugInfo) System.out.println("              " + auxjob.id + " in " + auxhost.hostname + " (CPU: " + auxhost.max_cpu + " vs " + auxOccCPU + " [" + jcpu_aux + "])");	
							if (debugInfo) System.out.println("              " + auxjob.id + " in " + auxhost.hostname + " (MEM: " + (auxhost.max_mem/1024) + " vs " + auxOccMEM + " [" + jmem_aux + "])");	
							/*if (debugInfo)*/ System.out.println("              " + auxjob.id + " in " + auxhost.hostname + " (PredRT: " + jrtf_aux + " - " + jrttf_aux + " [ " + " " + sla_aux + " " + jsla_aux + " ] (" + jobcpu_aux + " " + jobmem_aux + " " + jobin_aux + " " + jobout_aux + ")" );

							tempPrediction = new double[]{jobcpu_aux,jobmem_aux,jobin_aux,jobout_aux,jrtf_aux,jreq_aux,jbyt_aux,auxOccCPU,auxOccMEM,rcpu_aux,rmem_aux,jrttf_aux,jsla_aux};
						}
						System.out.println("    OSTIAPUTA " + auxentry.getKey().id + " in " + auxhost.hostname + " ITS SLA NOW: " + jsla_aux + " CUMULATED BENEF-TEMP: " + benefitTemp);
					}
				}

				/* Calculate Cost of Migration */
				double migr_aux = 0.0;
				int latency_aux = (latencyMap.get(auxjob.hostname+"-"+auxhost.name)).intValue();
				if (auxjob.hostname.compareTo(auxhost.name) != 0) migr_aux = ((((imagevol * 8) / velocity) + ((new Double(latency_aux)).doubleValue()/1000)) * (jobprice/((new Double(stepTime)).doubleValue()/1000)));

				/* Calculate Increment of Power */
				double estOccCPUMinus = predictCPUPM((auxOccCPU-jcpu_aux),(auxNumVMs-1));
				double power_aux = powercons(estOccCPU/100.0) - powercons(estOccCPUMinus/100.0);
				double wattprice_aux = (wattpriceMap.get(auxjob.hostname)).doubleValue();

				/* Calculate Increment of Benefit */
				double benefit_aux = benefitTemp - hostBenefitAux.get(auxhost.name).doubleValue();
				
				if (false) /* FIXME - LA QoS ARA NO ENTRA EN JOC */
				{
					/* Calculate Transport Response Time and SLA [only NoML] */
					double sla_aux = (1 - ((jrttf_temp - jOriginalRTT) / (jBetaRT * jOriginalRTT)));
					if (sla_aux < 0) sla_aux = 0.0;
					if (sla_aux > 1) sla_aux = 1.0;
					benefit_aux = sla_aux * jobprice;
				}
				//wattprice_aux = 0.0; /* FIXME - EL POWER ARA NO ENTRA EN JOC */
				//migr_aux = 0.0; /* FIXME - LA LATENCIA ARA NO ENTRA EN JOC */

				/* Check If Current Best Fit */
				double benefitGain = benefit_aux - power_aux * wattprice_aux - migr_aux;
				System.out.println("              " + auxjob.id + " evaluation: " + auxhost.name + " Benefit Incr: " + benefit_aux + " Power Incr: " + (power_aux * wattprice_aux) + " Migs Incr: " + migr_aux + " = " + (benefit_aux - power_aux * wattprice_aux - migr_aux) + " " + " [" + hostBenefitAux.get(auxhost.name) + "] SLA: " + tempPrediction[12]);
				/*if (debugInfo)*/ System.out.println("              " + auxjob.id + " in " + bestHostFound.name + " -> " + maxBenefitFound + " ; in " + auxhost.name + " -> " + benefitGain);

				/* ADD NEW JOB IF FITS */
				if (benefitGain > maxBenefitFound)
				//if (benefitTemp > maxBenefitFound)
				{
					maxBenefitFound = benefit_aux - power_aux * wattprice_aux - migr_aux;
					bestHostFound = auxhost;
					currentBenefitAux = benefitTemp; //benefit_aux;
					for (int ind = 0; ind < 13; ind++) bestPrediction[ind] = tempPrediction[ind];
					JRTTFAux.put(new Integer(auxjob.id),jrttf_temp);
				}
			}
			if (bestHostFound != null)
			{
				binmatrix.put(auxjob,bestHostFound);
				hostBenefitAux.put(bestHostFound.name,currentBenefitAux);

				/* Final Resolution */
				bestHostFound.used_cpu_real += predictions.get(auxjob)[3];
				bestHostFound.used_mem_real += predictions.get(auxjob)[4];
		
				//System.out.println("...selected: " + bestHostFound.id + " " + bestHostFound.used_cpu_real + " " + bestHostFound.used_mem_real);
				System.out.println("scheduler-WS> Decision " + auxjob.id + ": " + (bestPrediction[4] + bestPrediction[11]) + " (" + bestPrediction[4] + " - " + bestPrediction[11] + " : " + bestPrediction[12] + " ) [" + bestPrediction[0] + " " + bestPrediction[1] + " " + bestPrediction[2] + " " + bestPrediction[3] + "]" );
				//System.out.println("ARFF> " + jobLastRT.get(auxjob) + "," + bestPrediction[5] + "," + bestPrediction[6] + "," + bestPrediction[0] + "," + bestPrediction[1] + "," + bestPrediction[2] + "," + bestPrediction[3] + "," + bestPrediction[7] + "," + bestPrediction[8] + "," + bestPrediction[9] + "," + bestPrediction[10] + "," + bestPrediction[11] + "," + + bestPrediction[12]);
			}
			else
			{
				System.out.println("WARNING> " + auxjob.id + " Not allocated... Should Be STOPPED");	// FIXME - Send stopvm command
			}

		}

		/* Update Olds... */
		for (int i=0; i < order.length; i++)
		{
			vmInfo auxjob = vms.get((Integer)candidateVMS[order[i]]);
			jobLastMem.put(new Integer(auxjob.id),new Long(auxjob.usedmemory/1024));
			jobLastQueue.put(new Integer(auxjob.id),new Double(auxjob.avg_queue));
		}

		/* ------------------------------------------------------ */
		/*  Movement Realization                                  */
		/* ------------------------------------------------------ */

		lastmigs = 0;
		for (Map.Entry<vmInfo,hostInfo> entry : binmatrix.entrySet())
		{
			vmInfo auxvm = entry.getKey();
			hostInfo auxhost = entry.getValue();
			if (debugInfo) System.out.println("scheduler-WS> A VM " + auxvm.id + " in " + auxvm.state + " status found...");
			if (debugInfo) System.out.println("scheduler-WS> A VM " + auxvm.id + " to be placed in " + auxhost.hostname + " status found...");

			boolean condicio = (auxvm.hostname == null)?true:(auxvm.hostname.compareTo(auxhost.hostname)!=0);
			if (condicio)
			{
				if ((auxvm.state).compareTo("FAILED") == 0)
				{
					operateVM(new int[]{auxvm.id},RESUBMITVM);
					System.out.println("scheduler-WS> The VM " + auxvm.id + " is resubmited");
					try { Thread.currentThread().sleep(waitTime); minusTime = minusTime + waitTime; }
					catch (Exception e) { System.err.println("scheduler-WS> Error on Running Sleep (REALIZEMOVS)\n" + e.getMessage()); }
				}

				if ((auxvm.state).compareTo("PENDING") == 0)
				{
					operateVM(new int[]{auxvm.id, auxhost.id},DEPLOYVM);
					/*if (debugInfo)*/ System.out.println("scheduler-WS> The VM " + auxvm.id + " is deployed in " + auxhost.id);
				}
				else if ((auxvm.state).compareTo("ACTIVE") == 0)
				{
					if (isVBox) operateVM(new int[]{auxvm.id, auxhost.id},MIGRATEVM);
					else operateVM(new int[]{auxvm.id, auxhost.id},LIVEMIGRATEVM);
					/*if (debugInfo)*/ System.out.println("scheduler-WS> The VM " + auxvm.id + " is migrated from " + auxvm.hostname + " to " + auxhost.hostname);
					lastmigs = lastmigs + 1;
				}
				/* Sleep until deployed */
				try { Thread.currentThread().sleep(waitTime); minusTime = minusTime + waitTime; }
				catch (Exception e) { System.err.println("scheduler-WS> Error on Running Sleep OPERATIONVM\n" + e.getMessage()); }
			}
			else
			{
				/*if (debugInfo)*/ System.out.println("scheduler-WS> The VM " + auxvm.id + " stays at " + auxvm.hostname + " (" + auxhost.hostname + ")");			
			}
		}

		printLog.println("Iteration END: " + iteration + " " + dateFormat.format(new Date()));
		System.out.println("scheduler-WS> Iteration END: " + iteration + " " + dateFormat.format(new Date()));
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
		// PUE average = 1.50
		if (usedcpu <= 0) return 0;
		else if (usedcpu > 0 && usedcpu <= 1) return 29.1 * 1.5;
		else if (usedcpu > 1 && usedcpu <= 2) return 30.4 * 1.5;
		else if (usedcpu > 2 && usedcpu <= 3) return 31.3 * 1.5;
		else if (usedcpu > 3 && usedcpu <= 4) return 31.8 * 1.5;
		else return 31.8 * 1.5;
	}

	/* ---------------------------------------------------------- */
	/*  Load and Cnfigure Machine Learning Models and Predictors  */
	/* ---------------------------------------------------------- */

	private void loadModels() throws IOException, FileNotFoundException, ClassNotFoundException
	{
		ObjectInputStream objectInputStream = new ObjectInputStream(new FileInputStream("models/cpu_lr3.model"));
		this.cpuRegTree = (M5P) objectInputStream.readObject();
		objectInputStream.close();

		objectInputStream = new ObjectInputStream(new FileInputStream("models/mem_lr3.model"));
		this.memoryLinReg = (LinearRegression) objectInputStream.readObject();
		objectInputStream.close();

		objectInputStream = new ObjectInputStream(new FileInputStream("models/in_lr3.model"));
		this.inputRegTree = (M5P) objectInputStream.readObject();
		objectInputStream.close();

		objectInputStream = new ObjectInputStream(new FileInputStream("models/out_lr3.model"));
		this.outputRegTree = (M5P) objectInputStream.readObject();
		objectInputStream.close();

		objectInputStream = new ObjectInputStream(new FileInputStream("models/rt_lr6.model"));
		this.resptimeRegTree = (M5P) objectInputStream.readObject();
		objectInputStream.close();

		objectInputStream = new ObjectInputStream(new FileInputStream("models/sla_knn6.model"));
		this.slaKNN = (IBk) objectInputStream.readObject();
		objectInputStream.close();

		objectInputStream = new ObjectInputStream(new FileInputStream("models/rt_lr5-3.model"));
		this.slaiRegTree = (M5P) objectInputStream.readObject();
		objectInputStream.close();

		objectInputStream = new ObjectInputStream(new FileInputStream("models/cpupm_lr1-1.model"));
		this.cpupmRegTree = (M5P) objectInputStream.readObject();
		objectInputStream.close();

		objectInputStream = new ObjectInputStream(new FileInputStream("models/queue_lr6.model"));
		this.queueRegTree = (M5P) objectInputStream.readObject();
//		this.queueLinReg = (LinearRegression) objectInputStream.readObject();
		objectInputStream.close();

		System.out.println("scheduler-WS> Learned models loaded");
	}

	private double predictCPU (double reqs, double bpr, double tpr)
	{
		ArrayList<Attribute> atts = new ArrayList<Attribute>();	// Abans era "FastVector" en comptes de "ArrayList<Attribute>"
		atts.add(new Attribute("BPR"));
  		atts.add(new Attribute("CPUVM"));							// Abans era "addElement" en comptes de "add"
		atts.add(new Attribute("TPPR"));
		atts.add(new Attribute("TREQS"));
		Instances cpuData = new Instances("CPU_M5P", atts, 0);
		cpuData.setClassIndex(1);

		Instance auxinst = new DenseInstance(4);
		auxinst.setDataset(cpuData);
		auxinst.setValue(cpuData.attribute("BPR"), bpr);
		auxinst.setValue(cpuData.attribute("CPUVM"), 0);
		auxinst.setValue(cpuData.attribute("TPPR"), tpr);
		auxinst.setValue(cpuData.attribute("TREQS"), reqs);
		cpuData.add(auxinst);

		double clsLabel = 0;
		try { clsLabel = this.cpuRegTree.classifyInstance(auxinst); }
		catch (Exception e) { System.err.println("Error PredictCPU:" + e); e.printStackTrace(); }

		//System.out.println("CPU-PRED> TREQS: " + reqs + " BPR: " + bpr + " TPPR: " +  tpr + " => CPUVM: " + clsLabel);

		return clsLabel;
	}

	private double predictMEM (double reqs, double bpr, double tpr, double mem_aux, double cpuvm)
	{
		ArrayList<Attribute> memAtts = new ArrayList<Attribute>();
		memAtts.add(new Attribute("BPR"));
		memAtts.add(new Attribute("CPUVM"));
		memAtts.add(new Attribute("MEMVM"));
		memAtts.add(new Attribute("TPPR"));
		memAtts.add(new Attribute("MEMAUX"));
		memAtts.add(new Attribute("TREQS"));

		Instances memData = new Instances("MEM_LINREG", memAtts, 0);
		memData.setClassIndex(2);

    	Instance auxinst = new DenseInstance(6);	
        auxinst.setDataset(memData);
    	auxinst.setValue(memData.attribute("BPR"), bpr);
    	auxinst.setValue(memData.attribute("CPUVM"), cpuvm);
    	auxinst.setValue(memData.attribute("MEMVM"), 0);
    	auxinst.setValue(memData.attribute("TPPR"), tpr);
    	auxinst.setValue(memData.attribute("MEMAUX"), mem_aux);
    	auxinst.setValue(memData.attribute("TREQS"), reqs);
    	memData.add(auxinst);

		double clsLabel = 0;
        try { clsLabel = this.memoryLinReg.classifyInstance(auxinst); }
        catch (Exception e) { System.err.println("Error PredictMEM:" + e); e.printStackTrace(); }

		//System.out.println("MEM-PRED> TREQS: " + reqs + " BPR: " + bpr + " TPPR: " +  tpr + " CPUVM: " + cpuvm + " MEMAUX: " + mem_aux + " => MEMVM: " + clsLabel);

		return clsLabel;
	}
/*
	private double predictIN (double reqs, double bpr, double tpr, double cpuvm, double memvm)
	{
		ArrayList<Attribute> inAtts = new ArrayList<Attribute>();
		inAtts.add(new Attribute("BPR"));
		inAtts.add(new Attribute("TPR"));
		inAtts.add(new Attribute("RMEMVM")); // En realitat és MEMVM
  		inAtts.add(new Attribute("NKBIN"));
		inAtts.add(new Attribute("REQS"));
		inAtts.add(new Attribute("CPUPM"));  // En realitat és CPUVM

		Instances inData = new Instances("IN_M5P", inAtts, 0);
		inData.setClassIndex(3);

		Instance auxinst = new DenseInstance(6);
		auxinst.setDataset(inData);

		auxinst.setValue(inData.attribute("BPR"), bpr);
		auxinst.setValue(inData.attribute("TPR"), tpr);
		auxinst.setValue(inData.attribute("RMEMVM"), memvm);
		auxinst.setValue(inData.attribute("NKBIN"), 0);
		auxinst.setValue(inData.attribute("REQS"), reqs);
		auxinst.setValue(inData.attribute("CPUPM"), cpuvm);
		inData.add(auxinst);

		double clsLabel = 0;
		try { clsLabel = this.inputRegTree.classifyInstance(auxinst); }
		catch (Exception e) { System.err.println("Error PredictIO-IN:" + e); e.printStackTrace(); }

		return clsLabel;
	}
*/
	private double predictIN (double reqs, double bpr, double tpr, double cpuvm, double memvm)
	{
		ArrayList<Attribute> inAtts = new ArrayList<Attribute>();
		inAtts.add(new Attribute("TREQS"));
		inAtts.add(new Attribute("BPR"));
		inAtts.add(new Attribute("PCPUVM"));
		inAtts.add(new Attribute("PMEMVM"));
		inAtts.add(new Attribute("TPPR"));
  		inAtts.add(new Attribute("INVM"));

		Instances inData = new Instances("IN_M5P", inAtts, 0);
		inData.setClassIndex(5);

		Instance auxinst = new DenseInstance(6);
		auxinst.setDataset(inData);

		auxinst.setValue(inData.attribute("TREQS"), reqs);
		auxinst.setValue(inData.attribute("BPR"), bpr);
		auxinst.setValue(inData.attribute("PCPUVM"), cpuvm);
		auxinst.setValue(inData.attribute("PMEMVM"), memvm);
		auxinst.setValue(inData.attribute("TPPR"), tpr);
		auxinst.setValue(inData.attribute("INVM"), 0);

		inData.add(auxinst);

		double clsLabel = 0;
		try { clsLabel = this.inputRegTree.classifyInstance(auxinst); }
		catch (Exception e) { System.err.println("Error PredictIO-IN:" + e); e.printStackTrace(); }

		return clsLabel;
	}
/*
	private double predictOUT (double reqs, double bpr, double tpr, double cpuvm, double memvm)
	{
		ArrayList<Attribute> atts = new ArrayList<Attribute>();
		atts.add(new Attribute("BPR"));
		atts.add(new Attribute("TPR"));
		atts.add(new Attribute("RMEMVM")); // En realitat és MEMVM
  		atts.add(new Attribute("NKBOUT"));
		atts.add(new Attribute("REQS"));
		atts.add(new Attribute("CPUPM"));  // En realitat és CPUVM

		Instances outData = new Instances("OUT_M5P", atts, 0);
		outData.setClassIndex(3);

		Instance auxinst = new DenseInstance(6);
		auxinst.setDataset(outData);
		auxinst.setValue(outData.attribute("BPR"), bpr);
		auxinst.setValue(outData.attribute("TPR"), tpr);
		auxinst.setValue(outData.attribute("RMEMVM"), memvm);
		auxinst.setValue(outData.attribute("NKBOUT"), 0);
		auxinst.setValue(outData.attribute("REQS"), reqs);
		auxinst.setValue(outData.attribute("CPUPM"), cpuvm);
		outData.add(auxinst);

		double clsLabel = 0;
		try { clsLabel = this.outputRegTree.classifyInstance(auxinst); }
		catch (Exception e) { System.err.println("Error PredictIO-OUT:" + e); e.printStackTrace(); }

		return clsLabel;
	}
*/
	private double predictOUT (double reqs, double bpr, double tpr, double cpuvm, double memvm)
	{
		ArrayList<Attribute> atts = new ArrayList<Attribute>();
		atts.add(new Attribute("TREQS"));
		atts.add(new Attribute("BPR"));
		atts.add(new Attribute("PCPUVM"));
		atts.add(new Attribute("PMEMVM"));
		atts.add(new Attribute("TPPR"));
  		atts.add(new Attribute("OUTVM"));

		Instances outData = new Instances("OUT_M5P", atts, 0);
		outData.setClassIndex(5);

		Instance auxinst = new DenseInstance(6);
		auxinst.setDataset(outData);
		auxinst.setValue(outData.attribute("TREQS"), reqs);
		auxinst.setValue(outData.attribute("BPR"), bpr);
		auxinst.setValue(outData.attribute("PCPUVM"), cpuvm);
		auxinst.setValue(outData.attribute("PMEMVM"), memvm);
		auxinst.setValue(outData.attribute("TPPR"), tpr);
		auxinst.setValue(outData.attribute("OUTVM"), 0);
		outData.add(auxinst);

		double clsLabel = 0;
		try { clsLabel = this.outputRegTree.classifyInstance(auxinst); }
		catch (Exception e) { System.err.println("Error PredictIO-OUT:" + e); e.printStackTrace(); }

		return clsLabel;
	}

	private double predictRT (double treqs, double reqs, double queue, double bpr, double cpupm, double mempm, double rcpuvm, double rmemvm, double pcpuvm, double pmemvm, double invm, double outvm, double dqueue, double oqueue, int idvm, int idhst)
	{
		ArrayList<Attribute> rtAtts = new ArrayList<Attribute>();

		rtAtts.add(new Attribute("REQS"));
		rtAtts.add(new Attribute("BPR"));
		rtAtts.add(new Attribute("CPUPM"));
		rtAtts.add(new Attribute("MEMPM"));
		rtAtts.add(new Attribute("PCPUVM"));
		rtAtts.add(new Attribute("PMEMVM"));
		rtAtts.add(new Attribute("PINVM"));
		rtAtts.add(new Attribute("POUTVM"));
		rtAtts.add(new Attribute("TPPR"));
		rtAtts.add(new Attribute("DQUEUE"));
		rtAtts.add(new Attribute("OQUEUE"));

		Instances rtData = new Instances("RT_M5P", rtAtts, 0);
		rtData.setClassIndex(8);

		Instance auxinst = new DenseInstance(11);
		auxinst.setDataset(rtData);

		auxinst.setValue(rtData.attribute("REQS"), reqs);
		auxinst.setValue(rtData.attribute("BPR"), bpr);
		auxinst.setValue(rtData.attribute("CPUPM"), cpupm);
		auxinst.setValue(rtData.attribute("MEMPM"), mempm);
		auxinst.setValue(rtData.attribute("PCPUVM"), pcpuvm);
		auxinst.setValue(rtData.attribute("PMEMVM"), pmemvm);
		auxinst.setValue(rtData.attribute("PINVM"), invm);
		auxinst.setValue(rtData.attribute("POUTVM"), outvm);
		auxinst.setValue(rtData.attribute("TPPR"), 0);
		auxinst.setValue(rtData.attribute("DQUEUE"), dqueue);
		auxinst.setValue(rtData.attribute("OQUEUE"), oqueue);

		rtData.add(auxinst);

		double clsLabel = 0;
        try { clsLabel = this.resptimeRegTree.classifyInstance(rtData.instance(0)); }
        catch (Exception e) { System.err.println("Error RT:" + e); }

		//System.out.println("RT-PRED> IDs: " + idhst + " " + idvm + " REQS: " + reqs + " QUEUE_AUX: " + queue_aux + " BPR: " + bpr + " PCPUVM: " + pcpuvm + " CPUVM: " + rcpuvm + " PMEMVM: " + pmemvm + " MEMVM: " + rmemvm + " INVM: " + invm + " OUTVM: " + outvm + " => TPPR: " + clsLabel);

		return clsLabel;
	}
/*
	private double predictSLA (double treqs, double bpr, double cpupm, double mempm, double rcpuvm, double rmemvm, double pcpuvm, double pmemvm, double invm, double outvm, double ptppr, double foreig, int idvm, int idhst)
	{
		ArrayList<Attribute> slaAtts = new ArrayList<Attribute>();

		slaAtts.add(new Attribute("TREQS"));
		slaAtts.add(new Attribute("BPR"));
		slaAtts.add(new Attribute("CPUPM"));
		slaAtts.add(new Attribute("MEMPM"));
		slaAtts.add(new Attribute("RCPUVM"));
		slaAtts.add(new Attribute("RMEMVM"));
		slaAtts.add(new Attribute("PCPUVM"));
		slaAtts.add(new Attribute("PMEMVM"));
		slaAtts.add(new Attribute("PINVM"));
		slaAtts.add(new Attribute("POUTVM"));
		slaAtts.add(new Attribute("PTPPR"));
		slaAtts.add(new Attribute("SLA"));
		slaAtts.add(new Attribute("FOREIG"));

		Instances slaData = new Instances("SLA_M5P", slaAtts, 0);
		slaData.setClassIndex(11);

		Instance auxinst = new DenseInstance(13);
		auxinst.setDataset(slaData);

		auxinst.setValue(slaData.attribute("TREQS"), treqs);
		auxinst.setValue(slaData.attribute("BPR"), bpr);
		auxinst.setValue(slaData.attribute("CPUPM"), cpupm);
		auxinst.setValue(slaData.attribute("MEMPM"), mempm);
		auxinst.setValue(slaData.attribute("RCPUVM"), rcpuvm);
		auxinst.setValue(slaData.attribute("RMEMVM"), rmemvm);
		auxinst.setValue(slaData.attribute("PCPUVM"), pcpuvm);
		auxinst.setValue(slaData.attribute("PMEMVM"), pmemvm);
		auxinst.setValue(slaData.attribute("PINVM"), invm);
		auxinst.setValue(slaData.attribute("POUTVM"), outvm);
		auxinst.setValue(slaData.attribute("PTPPR"), ptppr);
		auxinst.setValue(slaData.attribute("SLA"), 0);
		auxinst.setValue(slaData.attribute("FOREIG"), foreig);

		slaData.add(auxinst);

		double clsLabel = 0;
        try { clsLabel = this.slaRegTree.classifyInstance(slaData.instance(0)); }
        catch (Exception e) { System.err.println("Error SLA:" + e); }

		//System.out.println("SLA-PRED> IDs: " + idhst + " " + idvm + " TREQS: " + treqs + " BPR: " + bpr + " PCPUVM: " + pcpuvm + " CPUVM: " + rcpuvm + " PMEMVM: " + pmemvm + " MEMVM: " + rmemvm + " INVM: " + invm + " OUTVM: " + outvm + " => TPPR: " + clsLabel);

		return clsLabel;
	}
*/
	private double predictSLA (double reqs, double bpr, double cpupm, double mempm, double pcpuvm, double pmemvm, double invm, double outvm, double foreig, double tppr0, double dqueue, double oqueue, int idvm, int idhst)
	{
		ArrayList<Attribute> slaAtts = new ArrayList<Attribute>();

		slaAtts.add(new Attribute("REQS"));
		slaAtts.add(new Attribute("BPR"));
		slaAtts.add(new Attribute("CPUPM"));
		slaAtts.add(new Attribute("MEMPM"));
		slaAtts.add(new Attribute("PCPUVM"));
		slaAtts.add(new Attribute("PMEMVM"));
		slaAtts.add(new Attribute("PINVM"));
		slaAtts.add(new Attribute("POUTVM"));
		slaAtts.add(new Attribute("SLA"));
		slaAtts.add(new Attribute("FOREIG"));
		slaAtts.add(new Attribute("TPPR0"));
		slaAtts.add(new Attribute("DQUEUE"));
		slaAtts.add(new Attribute("OQUEUE"));

		Instances slaData = new Instances("SLA_KNN", slaAtts, 0);
		slaData.setClassIndex(8);

		Instance auxinst = new DenseInstance(13);
		auxinst.setDataset(slaData);

		auxinst.setValue(slaData.attribute("REQS"), reqs);
		auxinst.setValue(slaData.attribute("BPR"), bpr);
		auxinst.setValue(slaData.attribute("CPUPM"), cpupm);
		auxinst.setValue(slaData.attribute("MEMPM"), mempm);
		auxinst.setValue(slaData.attribute("PCPUVM"), pcpuvm);
		auxinst.setValue(slaData.attribute("PMEMVM"), pmemvm);
		auxinst.setValue(slaData.attribute("PINVM"), invm);
		auxinst.setValue(slaData.attribute("POUTVM"), outvm);
		auxinst.setValue(slaData.attribute("SLA"), 0);
		auxinst.setValue(slaData.attribute("FOREIG"), foreig);
		auxinst.setValue(slaData.attribute("TPPR0"), tppr0);
		auxinst.setValue(slaData.attribute("DQUEUE"), dqueue);
		auxinst.setValue(slaData.attribute("OQUEUE"), oqueue);

		slaData.add(auxinst);

		double clsLabel = 0;
        try { clsLabel = this.slaKNN.classifyInstance(slaData.instance(0)); }
        catch (Exception e) { System.err.println("Error SLA:" + e); }

		//System.out.println("SLA-PRED> IDs: " + idhst + " " + idvm + " TREQS: " + treqs + " BPR: " + bpr + " PCPUVM: " + pcpuvm + " CPUVM: " + rcpuvm + " PMEMVM: " + pmemvm + " MEMVM: " + rmemvm + " INVM: " + invm + " OUTVM: " + outvm + " => TPPR: " + clsLabel);

		return clsLabel;
	}
/*
	private double predictSLAI (double treqs, double bpr, double cpupm, double mempm, double rcpuvm, double rmemvm, double pcpuvm, double pmemvm, double invm, double outvm, int idvm, int idhst)
	{
		ArrayList<Attribute> slaiAtts = new ArrayList<Attribute>();

		slaiAtts.add(new Attribute("TREQS"));
		slaiAtts.add(new Attribute("BPR"));
		slaiAtts.add(new Attribute("CPUPM"));
		slaiAtts.add(new Attribute("MEMPM"));
		slaiAtts.add(new Attribute("RCPUVM"));
		slaiAtts.add(new Attribute("RMEMVM"));
		slaiAtts.add(new Attribute("PCPUVM"));
		slaiAtts.add(new Attribute("PMEMVM"));
		slaiAtts.add(new Attribute("PINVM"));
		slaiAtts.add(new Attribute("POUTVM"));
		slaiAtts.add(new Attribute("SLAI"));

		Instances slaiData = new Instances("SLA_M5P", slaiAtts, 0);
		slaiData.setClassIndex(10);

		Instance auxinst = new DenseInstance(11);
		auxinst.setDataset(slaiData);

		auxinst.setValue(slaiData.attribute("TREQS"), treqs);
		auxinst.setValue(slaiData.attribute("BPR"), bpr);
		auxinst.setValue(slaiData.attribute("CPUPM"), cpupm);
		auxinst.setValue(slaiData.attribute("MEMPM"), mempm);
		auxinst.setValue(slaiData.attribute("RCPUVM"), rcpuvm);
		auxinst.setValue(slaiData.attribute("RMEMVM"), rmemvm);
		auxinst.setValue(slaiData.attribute("PCPUVM"), pcpuvm);
		auxinst.setValue(slaiData.attribute("PMEMVM"), pmemvm);
		auxinst.setValue(slaiData.attribute("PINVM"), invm);
		auxinst.setValue(slaiData.attribute("POUTVM"), outvm);
		auxinst.setValue(slaiData.attribute("SLAI"), 0);

		slaiData.add(auxinst);

		double clsLabel = 0;
        try { clsLabel = this.slaiRegTree.classifyInstance(slaiData.instance(0)); }
        catch (Exception e) { System.err.println("Error :" + e); }

		//System.out.println("SLA-PRED> IDs: " + idhst + " " + idvm + " TREQS: " + treqs + " BPR: " + bpr + " PCPUVM: " + pcpuvm + " CPUVM: " + rcpuvm + " PMEMVM: " + pmemvm + " MEMVM: " + rmemvm + " INVM: " + invm + " OUTVM: " + outvm + " => TPPR: " + clsLabel);

		return clsLabel;
	}
*/

	private double predictSLAI (double treqs, double bpr, double cpupm, double mempm, double rcpuvm, double rmemvm, double pcpuvm, double pmemvm, double invm, double outvm, double ptppr, double foreig, int idvm, int idhst)
	{
		ArrayList<Attribute> slaiAtts = new ArrayList<Attribute>();

		slaiAtts.add(new Attribute("TREQS"));
		slaiAtts.add(new Attribute("BPR"));
		slaiAtts.add(new Attribute("CPUPM"));
		slaiAtts.add(new Attribute("MEMPM"));
		slaiAtts.add(new Attribute("RCPUVM"));
		slaiAtts.add(new Attribute("RMEMVM"));
		slaiAtts.add(new Attribute("PCPUVM"));
		slaiAtts.add(new Attribute("PMEMVM"));
		slaiAtts.add(new Attribute("PINVM"));
		slaiAtts.add(new Attribute("POUTVM"));
		slaiAtts.add(new Attribute("PTPPR"));
		slaiAtts.add(new Attribute("SLAI"));
		slaiAtts.add(new Attribute("FOREIG"));

		Instances slaiData = new Instances("SLAI_M5P", slaiAtts, 0);
		slaiData.setClassIndex(11);

		Instance auxinst = new DenseInstance(13);
		auxinst.setDataset(slaiData);

		auxinst.setValue(slaiData.attribute("TREQS"), treqs);
		auxinst.setValue(slaiData.attribute("BPR"), bpr);
		auxinst.setValue(slaiData.attribute("CPUPM"), cpupm);
		auxinst.setValue(slaiData.attribute("MEMPM"), mempm);
		auxinst.setValue(slaiData.attribute("RCPUVM"), rcpuvm);
		auxinst.setValue(slaiData.attribute("RMEMVM"), rmemvm);
		auxinst.setValue(slaiData.attribute("PCPUVM"), pcpuvm);
		auxinst.setValue(slaiData.attribute("PMEMVM"), pmemvm);
		auxinst.setValue(slaiData.attribute("PINVM"), invm);
		auxinst.setValue(slaiData.attribute("POUTVM"), outvm);
		auxinst.setValue(slaiData.attribute("PTPPR"), ptppr);
		auxinst.setValue(slaiData.attribute("SLAI"), 0);
		auxinst.setValue(slaiData.attribute("FOREIG"), foreig);

		slaiData.add(auxinst);

		double clsLabel = 0;
        try { clsLabel = this.slaiRegTree.classifyInstance(slaiData.instance(0)); }
        catch (Exception e) { System.err.println("Error SLAI:" + e); }

		//System.out.println("SLA-PRED> IDs: " + idhst + " " + idvm + " TREQS: " + treqs + " BPR: " + bpr + " PCPUVM: " + pcpuvm + " CPUVM: " + rcpuvm + " PMEMVM: " + pmemvm + " MEMVM: " + rmemvm + " INVM: " + invm + " OUTVM: " + outvm + " => TPPR: " + clsLabel);

		return clsLabel;
	}

	private double predictCPUPM (double sumCPUVM, int numVMs)
	{
		/*
		if (numVMs <= 0) return 0.0;
		if (numVMs == 1) return (19.0 + sumCPUVM * 1.94);
		if (numVMs == 2) return (80.0 + sumCPUVM * 1.20);
		if (numVMs == 3) return (98.0 + sumCPUVM * 0.78);
		if (numVMs >= 4) return (275.0 + sumCPUVM * 0.50);
		return -1.0;
		// generic: (sumCPUVM * 0.57) + (numVMs * 97.29) - 129.67;
		*/

		ArrayList<Attribute> atts = new ArrayList<Attribute>();
		atts.add(new Attribute("V1"));	// CPUPM
		atts.add(new Attribute("V2"));	// SumRCPUVM
		atts.add(new Attribute("V3"));	// NumVMs

		Instances PMData = new Instances("CPUPM_M5P", atts, 0);
		PMData.setClassIndex(0);

		Instance auxinst = new DenseInstance(3);
		auxinst.setDataset(PMData);
		auxinst.setValue(PMData.attribute("V1"), 0);
		auxinst.setValue(PMData.attribute("V2"), sumCPUVM);
		auxinst.setValue(PMData.attribute("V3"), numVMs);
		PMData.add(auxinst);

		double clsLabel = 0;
		try { clsLabel = this.cpupmRegTree.classifyInstance(auxinst); }
		catch (Exception e) { System.err.println("Error PredictCPUPM:" + e); e.printStackTrace(); }

		return clsLabel;
	}

	private double predictQUEUE (double treqs, double reqs, double tpr0, double bpr, double cpupm, double mempm, double rcpuvm, double rmemvm, double pcpuvm, double pmemvm, double invm, double outvm, double dqueue, double oqueue, int idvm, int idhst)
	{
		ArrayList<Attribute> rtAtts = new ArrayList<Attribute>();

		rtAtts.add(new Attribute("REQS"));
		rtAtts.add(new Attribute("QUEUE"));
		rtAtts.add(new Attribute("BPR"));
		rtAtts.add(new Attribute("CPUPM"));
		rtAtts.add(new Attribute("MEMPM"));		
		rtAtts.add(new Attribute("PCPUVM"));
		rtAtts.add(new Attribute("PMEMVM"));
		rtAtts.add(new Attribute("PINVM"));
		rtAtts.add(new Attribute("POUTVM"));
		rtAtts.add(new Attribute("TPPR0"));
		rtAtts.add(new Attribute("DQUEUE"));
		rtAtts.add(new Attribute("OQUEUE"));

		Instances rtData = new Instances("QUEUE_M5P", rtAtts, 0);
		rtData.setClassIndex(1);

		Instance auxinst = new DenseInstance(12);
		auxinst.setDataset(rtData);

		auxinst.setValue(rtData.attribute("REQS"), reqs);
		auxinst.setValue(rtData.attribute("QUEUE"), 0);
		auxinst.setValue(rtData.attribute("BPR"), bpr);
		auxinst.setValue(rtData.attribute("CPUPM"), cpupm);
		auxinst.setValue(rtData.attribute("MEMPM"), mempm);
		auxinst.setValue(rtData.attribute("PCPUVM"), pcpuvm);
		auxinst.setValue(rtData.attribute("PMEMVM"), pmemvm);
		auxinst.setValue(rtData.attribute("PINVM"), invm);
		auxinst.setValue(rtData.attribute("POUTVM"), outvm);
		auxinst.setValue(rtData.attribute("TPPR0"), tpr0);
		auxinst.setValue(rtData.attribute("DQUEUE"), dqueue);
		auxinst.setValue(rtData.attribute("OQUEUE"), oqueue);
		rtData.add(auxinst);

		double clsLabel = 0;
        try { clsLabel = this.queueRegTree.classifyInstance(rtData.instance(0)); }
        catch (Exception e) { System.err.println("Error PQ:" + e); }

		//System.out.println("RT-PRED> IDs: " + idhst + " " + idvm + " REQS: " + reqs + " QUEUE_AUX: " + queue_aux + " BPR: " + bpr + " PCPUVM: " + pcpuvm + " CPUVM: " + rcpuvm + " PMEMVM: " + pmemvm + " MEMVM: " + rmemvm + " INVM: " + invm + " OUTVM: " + outvm + " => TPPR: " + clsLabel);

		return clsLabel;
	}
}
