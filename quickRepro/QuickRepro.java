import java.io.*;
import java.net.*;
import java.nio.*;
import java.util.*;
import java.math.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.TimeZone;
import java.util.Random;

class QuickRepro
{
	public static void main(String args[])
	{
		String a = (args.length > 0)?args[0]:"localhost";
		int t = (args.length > 1)?(new Integer(args[1])).intValue():0;
		boolean c = false;
		boolean p = false;
		boolean e = false;
		boolean f = false;
		int fixedvar = 9999;
		int tstep = 60;
		for (int i = 2; i < args.length; i++)
		{
			if (args[i].compareTo("coop")==0)
			{
				c = true;
				System.out.println("* MODE COOP");
			}
			else if (args[i].compareTo("pre")==0)
			{
				p = true;
				System.out.println("* MODE PREEMPTIVE");
			}
			else if (args[i].compareTo("elastic")==0)
			{
				e = true;
				System.out.println("* MODE ELASTIC");
			}
			else if (args[i].compareTo("fixed")==0)
			{
				f = true;
				fixedvar = (new Integer(args[++i])).intValue();
				System.out.println("* MODE FIXED POLICY");
			}
			else if (args[i].compareTo("step")==0)
			{
				tstep = (new Integer(args[++i])).intValue();
				System.out.println("* TIME STEP " + tstep );
			}
		}

		QuickRepro qr = new QuickRepro(a,t,c,p,e,f,fixedvar,tstep);
		qr.run();
	}

	private Map <Integer,Map> 	ListOfVMs;			// VM-S
	private Map <Integer,Map> 	ListOfVMc;			// VM-C
	private Map <Integer,Map> 	vmDistributionS;	// VM-S
 	private Map <Integer,Map> 	vmDistributionC;	// VM-C
 	private Map <String,Double> vmAvgDown;
	private Map <String,String> DNSMap;
	private	List <String> 		hostList;
 	
	private int numDCs = 0;
	private int numIter = 0;
	private long currentTS = 0;
	private long nextSchedTS = 0;

	private int numVMs;
	private int numVMc;
	private BigInteger volumeVMs;
	private BigInteger volumeData;
	private BigInteger volumeDataDG;
	private int maxTimeRound;
	private	int timeStep;

	private String address = "localhost";
	private int port = 5555;
	private int portlistener = 5550;
	private int typeExps = 0;
	private boolean coop = false;
	private boolean preemptive = false;
	private boolean elastic = false;
	private boolean fixedPolicy = false;
	private int fixedvar = 9999;
	private int idsurrogate = 0;
	private int countNegotiations = 0;

	private Map<String,Integer> vectorMap;	// FIXME - New Policy of Moving VM-S: Order of DCs implicitly and totally hardcoded to the problem... 
	private double[][] evacPolicy = {{0.00,0.00,0.00,0.00,0.02,0.13,0.22,0.25,0.20,0.13,0.05,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,0.00,0.00},
									 {1.00,1.00,1.00,1.00,0.00,0.00,0.00,0.00,0.00,0.00,0.02,0.13,0.22,0.25,0.20,0.13,0.05,1.00,1.00,1.00,1.00,1.00,1.00,1.00},
									 {1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,0.00,0.00,0.00,0.00,0.00,0.00,0.02,0.13,0.22,0.25,0.20,0.13,0.05,1.00},
									 {0.22,0.25,0.20,0.13,0.05,1.00,1.00,1.00,1.00,1.00,1.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.02,0.13}}; // FIXME - Must match number of DCs

	private Object lock = new Object();

	public QuickRepro (String addr, int typeExps, boolean coop, boolean pre, boolean elas, boolean fixed, int fixvar, int tstep)
	{
		ListOfVMs = new HashMap<Integer,Map>();
		ListOfVMc = new HashMap<Integer,Map>();
		DNSMap = new HashMap<String,String>();
		vmDistributionS = new HashMap<Integer,Map>();
		vmDistributionC = new HashMap<Integer,Map>();
		vmAvgDown = new HashMap<String,Double>();
		hostList = new ArrayList<String>();
		vectorMap = new HashMap<String,Integer>();

		// Info VMs - Configuration
		numVMs = 35000;
		numVMc = 300000;
		if (typeExps == 0) numVMc = 0;				// Only VM-S
		if (typeExps == 1) numVMs = 0;				// Only VM-C
		
		volumeVMs = BigInteger.valueOf(5);			// BITS per VM - Default 5GB/VM
		volumeVMs = volumeVMs.multiply(BigInteger.valueOf(1000*1000));
		volumeVMs = volumeVMs.multiply(BigInteger.valueOf(1000*8));
		maxTimeRound = 3600;

		volumeData = BigInteger.valueOf(450);			// BITS per Generated VM Data - Default 450MB/VM
		volumeData = volumeData.multiply(BigInteger.valueOf(1000*1000));
		volumeData = volumeData.multiply(BigInteger.valueOf(8));

		volumeDataDG = BigInteger.valueOf(5);			// BITS per Generated VM Data Degraded - Default 5GB/VM
		volumeDataDG = volumeDataDG.multiply(BigInteger.valueOf(1000*1000));
		volumeDataDG = volumeDataDG.multiply(BigInteger.valueOf(1000*8));

		// Simulator Address
		this.address = addr; // "idealist.pc.ac.upc.edu";
		this.typeExps = typeExps;
		this.coop = coop;
		this.preemptive = pre;
		this.elastic = elas;
		this.fixedPolicy = fixed;
		this.fixedvar = fixvar;
		this.timeStep = tstep;
	}
	
	private String[] TimeTableS = new String[1024];
	private String[] TimeTableC = new String[1024];
	private int maxItersS = 0;
	private int maxItersC = 0;

	private void readTimeTable ()
	{
		try
		{
			FileInputStream fstream = new FileInputStream("qr-scheduler.log");
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));

			String buffer = "0 0 0";
			int auxNumIter = 0;
			String strLine;
			while ((strLine = br.readLine()) != null)
			{
				if (strLine.contains("Iteration:"))
				{
					// FIXME - At this time VM-S and VM-C share file...
					// Timetable VM-S
					TimeTableS[auxNumIter] = buffer;
					maxItersS = maxItersS + 1;
					
					// TimeTable VM-C
					TimeTableC[auxNumIter] = buffer;
					maxItersC = maxItersS + 1;
				
					// Get new Iteration TS
					String[] aux = strLine.split("\\s+");
					//DateFormat dfm = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
					//dfm.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
					//long auxCurrentTS = dfm.parse(aux[3] + " " + aux[4]).getTime() / 1000;
					auxNumIter = (new Integer(aux[2])).intValue();
					long auxCurrentTS = (auxNumIter - 1) * maxTimeRound;
					long auxNextSchedTS = currentTS + maxTimeRound;
					
					buffer = auxCurrentTS + " " + auxNextSchedTS + " " + auxNumIter;
				}		
				if (strLine.contains("load from it:"))
				{
					// Get Load from Places
					String[] aux = strLine.split("\\s+");					
					buffer = buffer + " " + aux[2] + " " + aux[4] + " " + aux[10];
				}
				else if (strLine.contains("Found ISP for"))
				{
					String[] aux = strLine.split("\\s+");
					System.out.println("Location: " + aux[4] + " with IP " + aux[7]);

					DNSMap.put(aux[4],aux[7]);
					hostList.add(aux[4]);
					vectorMap.put(aux[4],numDCs);

					numDCs++;
				}
				else if (strLine.contains("Built proxy for"))
				{
					String[] aux = strLine.split("\\s+");
					ListOfVMs.put(new Integer(aux[4]),new HashMap<String,Integer>());
					ListOfVMc.put(new Integer(aux[4]),new HashMap<String,Integer>());

					// Inicialization of Load VM Distribution (Singularity)
					Map <String,Integer> vmInDCS = new HashMap<String,Integer>();
					for (String dckey : DNSMap.keySet()) vmInDCS.put(dckey,0);
					vmInDCS.put("atom02",numVMs * 1 / 4);
					vmInDCS.put("atom03",numVMs * 3 / 4);
					vmDistributionS.put(new Integer(aux[4]),vmInDCS);

					// Inicialization of Load VM Distribution (Clonic VMs)
					Map <String,Integer> vmInDCC = new HashMap<String,Integer>();
					for (String dckey : DNSMap.keySet()) vmInDCC.put(dckey,0);
					vmInDCC.put("atom00",numVMc/4);
					vmInDCC.put("atom01",numVMc/4);
					vmInDCC.put("atom02",numVMc/4);
					vmInDCC.put("atom03",numVMc/4);
					vmDistributionC.put(new Integer(aux[4]),vmInDCC);
				}
				else if (strLine.contains("BPR:"))
				{
					String[] aux = strLine.split("\\s+");
					vmAvgDown.put(auxNumIter+"-"+aux[3]+"-VM",new Double(aux[30].substring(0, aux[30].length()-1)));
					vmAvgDown.put(auxNumIter+"-"+aux[3]+"-DB",new Double(aux[30].substring(0, aux[30].length()-1)));
				}
			}
			in.close();
		}
		catch (Exception e) { System.err.println("Error: " + e.getMessage()); e.printStackTrace(); }		
	}

	public void run()
	{	
		readTimeTable ();
		boolean warmup = false;	// Only if Minimal Connections

		for (int j = 0; j < maxItersS; j++)
		{
			String bufferS = TimeTableS[j];
			String bufferC = TimeTableC[j];
			
			// Schedule Previous TimeTable
			String[] auxS = bufferS.split("\\s+");
			String[] auxC = bufferC.split("\\s+");
			currentTS = (new Long(auxS[0])).longValue();
			nextSchedTS = currentTS + maxTimeRound;
			numIter = (new Integer(auxS[2])).intValue();
			
			System.out.println("Execute Iteration: " + numIter + " at " + currentTS + " (" + nextSchedTS + ")");

			// Prepare Next Iteration
			ListOfVMs.clear();
			ListOfVMc.clear();
			
			String runReport = "Info Iter> ";
			for (int i = 3; i < auxS.length; i=i+3)
			{
				Map <String,Integer> LoadLocation;
				// Load for VM-S
				if (ListOfVMs.containsKey(new Integer(auxS[i])))	LoadLocation = ListOfVMs.get(new Integer(auxS[i]));
				else LoadLocation = new HashMap<String,Integer>();

				LoadLocation.put(auxS[i+1],(new Integer((auxS[i+2].split("\\."))[0])));
				ListOfVMs.put(new Integer(auxS[i]),LoadLocation);
				
				// Load for VM-C
				if (ListOfVMc.containsKey(new Integer(auxC[i])))	LoadLocation = ListOfVMc.get(new Integer(auxC[i]));
				else LoadLocation = new HashMap<String,Integer>();

				LoadLocation.put(auxC[i+1],(new Integer((auxC[i+2].split("\\."))[0])));
				ListOfVMc.put(new Integer(auxC[i]),LoadLocation);
				
				double avgbprS = vmAvgDown.get(numIter+"-"+auxS[i]+"-VM").doubleValue();
				double avgbprC = vmAvgDown.get(numIter+"-"+auxC[i]+"-DB").doubleValue();
				System.out.println("Prepare Next Iter: for VM " + auxS[i] + " place " + auxS[i+1] + " [" + auxS[i+2] + "] " + (new Double((new Double(auxS[i+2])).doubleValue() * (avgbprS+avgbprC))).intValue() + " bytes per VM ");

				Map <String,Integer> vmInDC = vmDistributionS.get(new Integer(auxS[i]));
				double vmshold = ((vmInDC.containsKey(auxS[i+1]))?vmInDC.get(auxS[i+1]).doubleValue():0.0);

				runReport = runReport + " VM-kind: " + auxS[i] + " DC: " + auxS[i+1] + " LoadVMs: " + ((new Integer(auxS[i+2])).intValue() * 22000 / 700) + " VMs: " + vmshold;
			}
			System.out.println(runReport);			
			
			if (warmup)
			{
				// Open Minimal Connections
				SortedSet<String> keys = new TreeSet<String>(DNSMap.keySet());
				for (String key : keys)
				{
					for (String key2 : keys)
					{
						if (key2.compareTo(key) == 0) continue;
						System.out.println("Minimal Conn: " + key + "-" + key2);

						int bwd = (coop)?10:100; // FIXME - Open to 100 Gbps to have space... must adjust to common traffic
						boolean done = negotiateBWD("VM",key,key2,bwd,true);
						done = negotiateBWD("DB",key,key2,bwd,true);
					}
				}
				warmup = false;
			}
			else schedule();
		}
	
		// Closing Time
		for (String key : connections.keySet())
		{
			int id = connections.get(key);
			if (!fixedPolicy) { boolean baux = closeConn (id); }
		}
		if (!fixedPolicy) { String reply = udpSender ("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"999\"></Request>"); }
	}

	private double powercons (double usedcpu)
	{
		if (usedcpu <= 0) return 0;
		else if (usedcpu > 0 && usedcpu <= 1) return 267.8;
		else if (usedcpu > 1 && usedcpu <= 2) return 285.5;
		else if (usedcpu > 2 && usedcpu <= 3) return 302.5;
		else if (usedcpu > 3 && usedcpu <= 4) return 317.9;
		else return 317.9;
	}

	private Map <String,Integer> preemptedVMs = new HashMap<String,Integer>();

	private void schedule()
	{
		System.out.println("Iteration: " + numIter);

		// --------------------------------------------------------------------
		// Statistics (Part 1)
		// --------------------------------------------------------------------

		if (numIter == 0) return;

		countNegotiations = 0;

		double energy = 0.0;
		String energyLog = "";
		for (String key: hostList)
		{
			// Get VMs in DC
			double auxVMs = 0.0;
			double auxVMc = 0.0;
			for (Integer vmkey : ListOfVMs.keySet())
			{
				Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);
				auxVMs = auxVMs + ((vmInDC.containsKey(key))?vmInDC.get(key).doubleValue():0.0);
			}
			for (Integer vmkey : ListOfVMc.keySet())
			{
				Map <String,Integer> vmInDC = vmDistributionC.get(vmkey);
				auxVMc = auxVMc + ((vmInDC.containsKey(key))?vmInDC.get(key).doubleValue():0.0);
			}
			double auxVMt = auxVMs + auxVMc;

			// Counting 8 VMs per Xeon 4 core (2VMs per CPU, 1VM per Thread -> 8 VMs per Màquina)
			int machines = (int)Math.floor(auxVMt / 8);
			int remaining = (int)auxVMt - machines * 8;
			energy = energy + ((machines * powercons(4)) + powercons(remaining/2))/1000;
			energyLog = energyLog + " " + key + ": " + ((machines * powercons(4)) + powercons(remaining/2))/1000;
		}

		// --------------------------------------------------------------------
		// Singular VMs (VM Migration)
		// --------------------------------------------------------------------
		Map <String,Integer> aggregatedVMTotal = new HashMap<String,Integer>();
		Map <String,Integer> movementNumVM = new HashMap<String,Integer>();

		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);

			// For each DC do crazy stuff and things
			for (String key : keys)
			{
				int currentIndex = vectorMap.get(key).intValue();
				int nextIndex = (currentIndex+1)%numDCs;

				String currentHost = key;
				String nextHost = "";
				for (String key2 : keys)
				{
					int iaux = vectorMap.get(key2).intValue();
					if (iaux == nextIndex) nextHost = key2;
				}
			
				double vectorValue = evacPolicy[currentIndex][(numIter-1)%24];
				int vmsindc = vmInDC.get(key).intValue();

				Double daux = (((double)numVMs) * vectorValue);
				int move = Math.min(vmsindc,daux.intValue());

				if (preemptive) move = Math.max(0,(move - preemptedVMs.get(vmkey+"-"+key).intValue()));

				if (currentHost.compareTo(nextHost) != 0 && nextHost.compareTo("") != 0)
				{
					String origin = currentHost;
					String destination = nextHost;

					// Accumulate VMs into paths
					int auxaccum = 0;
					if (aggregatedVMTotal.containsKey(origin+"-"+destination)) auxaccum = (aggregatedVMTotal.get(origin+"-"+destination)).intValue();
					aggregatedVMTotal.put(origin+"-"+destination,auxaccum + move);
					movementNumVM.put(vmkey+"-"+origin+"-"+destination,move);
				}
			}
		}

/*
		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);

			// Print Values of Load
			for (String key : keys)
			{ 
				int value = LoadLocation.get(key).intValue();
				System.out.println("DEBG VM-S> VMs: " + vmkey + " Load from DC: " + key + " -> " + value);
			}
			
			// Find Best Host
			String bestHost = "";
			int bestValue = 0;
			for (String key : keys)
			{
				int value = LoadLocation.get(key).intValue();
				if (value > bestValue)
				{
					bestHost = key;
					bestValue = value;
				}
			}
			
			// For each DC move the VMs of kind 'vmkey' to the BestHost
			for (String key : keys)
			{
				String currentHost = key;
				int numVMInDC = vmInDC.get(key).intValue();
				System.out.println("DEBG VM-S> VM: " + vmkey + " -> Must move " + numVMInDC +  " VMs from " + currentHost + " to " + bestHost + " (max load here: " + bestValue + ")");	

				if (currentHost.compareTo(bestHost) != 0 && bestHost.compareTo("") != 0)
				{
					String origin = currentHost;
					String destination = bestHost;
					int move = numVMInDC;

					// Accumulate VMs into paths
					int auxaccum = 0;
					if (aggregatedVMTotal.containsKey(currentHost+"-"+bestHost)) auxaccum = (aggregatedVMTotal.get(currentHost+"-"+bestHost)).intValue();
					aggregatedVMTotal.put(currentHost+"-"+bestHost,auxaccum + move);
					movementNumVM.put(vmkey+"-"+currentHost+"-"+bestHost,move);
				}
			}
		}
*/
		// --------------------------------------------------------------------
		// Clonic VMs (DB sync)
		// --------------------------------------------------------------------
		Map <String,Integer> aggregatedDBTotal = new HashMap<String,Integer>();
		Map <String,Integer> movementNumDB = new HashMap<String,Integer>();
		for (Integer vmkey : ListOfVMc.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMc.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistributionC.get(vmkey);
		
			// Data generated per place -> Distribute to all other DCs
			Map <String,Integer> aggregatedTraffic = new HashMap<String,Integer>();
			for (String key : keys)
			{
				// VMs in location (to get generated data in location)
				int load = vmInDC.get(key).intValue();

				// For each other DC, send data
				for (String key2 : keys)
				{
					if (key2.compareTo(key) == 0) continue;
					if (!aggregatedTraffic.containsKey(key+"-"+key2)) aggregatedTraffic.put(key+"-"+key2, new Integer(0));
					int addition = (aggregatedTraffic.get(key+"-"+key2)).intValue() + load;
					aggregatedTraffic.put(key+"-"+key2,new Integer(addition));
				}
			}
			
			// Aggregate Traffic key -> key2
			for (String key : keys)
			{ 
				for (String key2 : keys)
				{
					if (key2.compareTo(key) == 0) continue;
					if (!aggregatedDBTotal.containsKey(key+"-"+key2)) aggregatedDBTotal.put(key+"-"+key2,new Integer(0));
				
					int valueS = aggregatedTraffic.get(key+"-"+key2);
					int valueT = aggregatedDBTotal.get(key+"-"+key2);
					aggregatedDBTotal.put(key+"-"+key2,valueS + valueT);
				}
			}

			// Move VMs after prepare Data Dispatching
			// Distribute VMs proportionally
			int sumLoad = 0;
			for (String key : keys)
			{ 
				sumLoad += LoadLocation.get(key).intValue();
			}
			int checkLoad = 0;
			Map <String,Integer> newBalance = new HashMap<String,Integer>();
			for (String key : keys)
			{ 
				double percent = LoadLocation.get(key).doubleValue() / (new Double(sumLoad)).doubleValue();
				int value = (int)Math.round(numVMc * percent);
				newBalance.put(key,new Integer(value));
				checkLoad += value;
			}
			// Correct Roundings
			if (checkLoad < numVMs)	for (String key : keys)	if (newBalance.get(key).intValue() > 0) { newBalance.put(key,newBalance.get(key).intValue()+(numVMs-checkLoad)); break; }

			// Schedule VMs
			for (String key : keys)
			{
				int v11 = vmInDC.get(key).intValue();
				int v12 = newBalance.get(key).intValue();

				if (v11 - v12 != 0)
				{
					for (String key2 : keys)
					{
						int v21 = vmInDC.get(key2).intValue();
						int v22 = newBalance.get(key2).intValue();
						if (key.compareTo(key2) != 0 && v21 - v22 != 0)
						{
							int diff1 = v11 - v12;
							int diff2 = v21 - v22;
							int move = Math.min(Math.abs(diff1),Math.abs(diff2));
							String origin = "";
							String destination = "";

							if (diff1 > 0 && diff2 < 0)
							{
								origin = key;
								destination = key2;
							}
							else if (diff1 < 0 && diff2 > 0)
							{
								origin = key2;
								destination = key;
							}
							if ((diff1 > 0 && diff2 < 0) || (diff1 < 0 && diff2 > 0))
							{
								vmInDC.put(origin,vmInDC.get(origin).intValue()-move);
								vmInDC.put(destination,vmInDC.get(destination).intValue()+move);
								v11 = vmInDC.get(key).intValue();
								v21 = vmInDC.get(key2).intValue();
							}
						}
					}
				}
			}

			for (String key : keys)
			{
				// VMs in location (to get generated data in location)
				int load = vmInDC.get(key).intValue();
			}

		}

		// From aggregated Traffic VM-C, get what to move
		for (String key : aggregatedDBTotal.keySet())
		{
			int move = aggregatedDBTotal.get(key).intValue();
			movementNumDB.put(key,move);
		}
		
		// --------------------------------------------------------------------
		// Policies to be applied
		// --------------------------------------------------------------------

		if (!coop) policyMS (aggregatedVMTotal,movementNumVM,aggregatedDBTotal,movementNumDB);
		else policyCC (aggregatedVMTotal,movementNumVM,aggregatedDBTotal,movementNumDB);

		// --------------------------------------------------------------------
		// Statistics (Print)
		// --------------------------------------------------------------------

		System.out.println("Info Schd> Iteration: " + numIter + " Finished: " + currentTS + " Negotiations: " + countNegotiations);
		System.out.println("Info Ener> " + energy + " kWh " + energyLog + " (all in kWh)");

		// --------------------------------------------------------------------
		// Preemptive Scheduling 
		// --------------------------------------------------------------------

		if ((nextSchedTS - currentTS > 15*60) && preemptive)
		{
			currentTS = nextSchedTS - 15*60;
			advanceTime(currentTS);
			preempSchedule ();
		}
	}
	
	private void preempSchedule ()
	{
		System.out.println("Preemptive Sched: " + numIter + " at " + currentTS + " (" + nextSchedTS + ")");
		
		// Get Next Sched in TimeTable
		String bufferS = TimeTableS[numIter+1];
		String[] auxS = bufferS.split("\\s+");

		// Prepare Next Iteration for VM-S
		ListOfVMs.clear();
		for (int i = 3; i < auxS.length; i=i+3)
		{
			Map <String,Integer> LoadLocation;
			// Load for VM-S
			if (ListOfVMs.containsKey(new Integer(auxS[i]))) LoadLocation = ListOfVMs.get(new Integer(auxS[i]));
			else LoadLocation = new HashMap<String,Integer>();

			// Expected Load for next TS (with a random error of +-10%)
			double newExpLoad = (new Integer((auxS[i+2].split("\\."))[0])).doubleValue();
			Random randomGenerator = new Random();
			newExpLoad = newExpLoad * (1 + (((randomGenerator.nextDouble()*20) - 10) / 100));

			LoadLocation.put(auxS[i+1],(new Double(newExpLoad)).intValue());
			ListOfVMs.put(new Integer(auxS[i]),LoadLocation);
		}	
		
		// Schedule for VM-S
		Map <String,Integer> beforePreemptive = new HashMap<String,Integer>();
		Map <String,Integer> aggregatedVMTotal = new HashMap<String,Integer>();
		Map <String,Integer> movementNumVM = new HashMap<String,Integer>();

		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);

			// For each DC do crazy stuff and things
			for (String key : keys)
			{
				int currentIndex = vectorMap.get(key).intValue();
				int nextIndex = (currentIndex+1)%numDCs;

				String currentHost = key;
				String nextHost = "";
				for (String key2 : keys)
				{
					int iaux = vectorMap.get(key2).intValue();
					if (iaux == nextIndex) nextHost = key2;
				}
			
				double vectorValue = evacPolicy[currentIndex][(numIter-1)%24];
				int vmsindc = vmInDC.get(key).intValue();

				Double daux = (((double)numVMs) * vectorValue);
				int move = Math.min(vmsindc,daux.intValue());

				if (currentHost.compareTo(nextHost) != 0 && nextHost.compareTo("") != 0)
				{
					String origin = currentHost;
					String destination = nextHost;

					// Accumulate VMs into paths
					int auxaccum = 0;
					if (aggregatedVMTotal.containsKey(origin+"-"+destination)) auxaccum = (aggregatedVMTotal.get(origin+"-"+destination)).intValue();
					aggregatedVMTotal.put(origin+"-"+destination,auxaccum + move);
					movementNumVM.put(vmkey+"-"+origin+"-"+destination,move);
				}

				beforePreemptive.put(vmkey+"-"+key,vmsindc);
			}
		}

/*
		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);

			// Print Values of Load
			for (String key : keys)
			{ 
				int value = LoadLocation.get(key).intValue();
				System.out.println("Pre> VMs: " + vmkey + " Load from DC: " + key + " -> " + value);
			}
			
			// Find Best Host
			String bestHost = "";
			int bestValue = 0;
			for (String key : keys)
			{
				int value = LoadLocation.get(key).intValue();
				if (value > bestValue)
				{
					bestHost = key;
					bestValue = value;
				}
			}
			
			// For each DC move the VMs of kind 'vmkey' to the BestHost
			for (String key : keys)
			{
				String currentHost = key;
				int numVMInDC = vmInDC.get(key).intValue();
				System.out.println("VM: " + vmkey + " -> Must move " + numVMInDC +  " VMs from " + currentHost + " to " + bestHost + " (max load here: " + bestValue + ")");	

				if (currentHost.compareTo(bestHost) != 0 && bestHost.compareTo("") != 0)
				{
					String origin = currentHost;
					String destination = bestHost;
					int move = numVMInDC;

					// Accumulate VMs into paths
					int auxaccum = 0;
					if (aggregatedVMTotal.containsKey(currentHost+"-"+bestHost)) auxaccum = (aggregatedVMTotal.get(currentHost+"-"+bestHost)).intValue();
					aggregatedVMTotal.put(currentHost+"-"+bestHost,auxaccum + move);
					movementNumVM.put(vmkey+"-"+currentHost+"-"+bestHost,move);
				}
			}
		}
*/
		
		// Execute Negotiation and Transference
		int auxmtr = maxTimeRound;
		maxTimeRound = 15 * 60;
		policyMS (aggregatedVMTotal,movementNumVM,new HashMap<String,Integer>(),new HashMap<String,Integer>());
		maxTimeRound = auxmtr;

		// Check Preempted VMs and take into account for next scheduling
		preemptedVMs.clear();
		for (String key : beforePreemptive.keySet())
		{
			String[] aux = key.split("-");
			Integer vmkey = new Integer(aux[0]);
			String host = aux[1];

			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);

			int vmsindc = vmInDC.get(host).intValue();
			int prevvmsindc	= beforePreemptive.get(key);
			int diff = (prevvmsindc-vmsindc>0)?(prevvmsindc-vmsindc):0;

			preemptedVMs.put(key,diff);
		}
	}

	/**************************************************************************/
	/* TADS                                                                   */
	/**************************************************************************/

	private Map <String,Integer> connections = new HashMap<String,Integer>();
	private Map <String,Double> availableBWD = new HashMap<String,Double>();
	
	private int retrieveCONID (String keyname, String origin, String destination)
	{
		if (!connections.containsKey(keyname+"-"+origin+"-"+destination)) return -1;
		return connections.get(keyname+"-"+origin+"-"+destination);
	}

	private double retrieveBWD (String keyname, String origin, String destination)
	{
		if (!availableBWD.containsKey(keyname+"-"+origin+"-"+destination)) return -1.0;
		return availableBWD.get(keyname+"-"+origin+"-"+destination);
	}
	
	private void provideBWD (String keyname, String origin, String destination, double value)
	{
		availableBWD.put(keyname+"-"+origin+"-"+destination,value);
	}
	
	private Set<String> setOfBWD ()
	{
		return availableBWD.keySet();
	}
	
	private Map <String,Integer> timedConns = new HashMap<String,Integer>();	// <KN-Origin-Dest,id>
	private Map <Integer,Double> timedIDBWD = new HashMap<Integer,Double>();	// <id,bwd>

	private int retrieveTCID (String keyname, String origin, String destination)
	{
		if (!timedConns.containsKey(keyname+"-"+origin+"-"+destination)) return -1;
		return timedConns.get(keyname+"-"+origin+"-"+destination);
	}
	
	private double retrieveTCBWD (int id)
	{
		if (!timedIDBWD.containsKey(new Integer(id))) return -1;
		return timedIDBWD.get(new Integer(id));
	}
	
	private double retrieveTCBWD (String keyname, String origin, String destination)
	{
		int id = retrieveTCID(keyname,origin,destination);
		if (id < 0) return -1.0;
		return retrieveTCBWD(id);
	}

	private void provideTCID (String keyname, String origin, String destination,int id)
	{
		timedConns.put(keyname+"-"+origin+"-"+destination,new Integer(id));
	}

	private void provideTCBWD (int id, double bwd)
	{
		timedIDBWD.put(new Integer(id),new Double(bwd));
	}

	private void provideTCBWD (String keyname, String origin, String destination, double bwd)
	{
		int id = retrieveTCID(keyname,origin,destination);
		if (id < 0) return;
		provideTCBWD(id,bwd);
	}

	private void removeTCID (String keyname, String origin, String destination)
	{
		timedConns.remove(keyname+"-"+origin+"-"+destination);
	}
	
	private void removeTCBWD (int id)
	{
		timedIDBWD.remove(new Integer(id));
	}

	private void removeTCBWD (String keyname, String origin, String destination)
	{
		int id = retrieveTCID(keyname,origin,destination);
		if (id < 0) return;
		removeTCBWD(id);
	}

	private Set<String> setOfTCID ()
	{
		return timedConns.keySet();
	}

	/**************************************************************************/
	/* Polítiques                                                             */
	/**************************************************************************/

	private Map <String,Integer> degradedDBs = new HashMap<String,Integer>();

	private void pmsNegotiateVM(String orig, String dest, String bwdkey, Map <String,Integer> aggregatedVMTotal, int newTimeVM)
	{
		// Get BWD per path
		int move = aggregatedVMTotal.get(bwdkey);
		int bwd = calculateBWD("VM",move,orig,dest,volumeVMs,newTimeVM);				

		// Negotiate VM Sending
		if (move > 0)
		{
			if (fixedPolicy)
			{
				bwd = fixedvar;
				connections.put("VM-"+orig+"-"+dest,idsurrogate++);
				provideBWD("VM",orig,dest,bwd);
				countNegotiations++;
			}
			else { boolean done = negotiateBWD("VM",orig,dest,bwd,true); }
		}
	}

	private void pmsNegotiateDB(String orig, String dest, String bwdkey, Map <String,Integer> aggregatedDBTotal, Map <String,Integer> degradedDBs, int newTimeDB)
	{
		// Calculate estimated BWD
		int move = aggregatedDBTotal.get(orig+"-"+dest);
		int degr = degradedDBs.get(orig+"-"+dest);
		int bwd = calculateBWD("DB",move,degr,orig,dest,volumeData,volumeDataDG,newTimeDB);

		// Negotiate Data Sending
		if (move > 0 || degr > 0)
		{
			if (fixedPolicy) 
			{
				bwd = fixedvar;
				connections.put("DB-"+orig+"-"+dest,idsurrogate++);
				provideBWD("DB",orig,dest,bwd);
				countNegotiations++;
			}
			else { boolean done = negotiateBWD("DB",orig,dest,bwd,true); }
		}
	}

	private void policyMS (Map <String,Integer> aggregatedVMTotal, Map <String,Integer> movementNumVM, Map <String,Integer> aggregatedDBTotal, Map <String,Integer> movementNumDB)
	{
		// --------------------------------------------------------------------
		// BWD Negotiation per Path
		// --------------------------------------------------------------------

		int newTimeVM = (maxTimeRound * 1 / 2); //(maxTimeRound * 3 / 4);
		int newTimeDB = (maxTimeRound * 1 / 2);

		// Perform BWD negotiations for VM-S
		for (String bwdkey : aggregatedVMTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			pmsNegotiateVM(aux[0],aux[1],bwdkey,aggregatedVMTotal,newTimeVM);
		}

		// Perform BWD negotiations for VM-C
		for(String bwdkey : aggregatedDBTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			if (!degradedDBs.containsKey(aux[0]+"-"+aux[1])) degradedDBs.put(aux[0]+"-"+aux[1],new Integer(0));
			pmsNegotiateDB(aux[0],aux[1],bwdkey,aggregatedDBTotal,degradedDBs,newTimeDB);
		}

		// --------------------------------------------------------------------
		// Looping of Stuff and Shit
		// --------------------------------------------------------------------

		Map <String,Double> directVMs = new HashMap<String,Double>();	// <Origin-Dest,vms>
		Map <String,Double> directVMc = new HashMap<String,Double>();	// <Origin-Dest,dbs>
		Map <String,Double> directVMd = new HashMap<String,Double>();	// <Origin-Dest,dbs degr>
		for (String key : aggregatedVMTotal.keySet()) {	directVMs.put(key,0.0); }
		for (String key : aggregatedDBTotal.keySet()) { directVMc.put(key,0.0); }
		for (String key : aggregatedDBTotal.keySet()) { directVMd.put(key,0.0); }

		Map <String,Integer> directDone = new HashMap<String,Integer>();	// <Key-Origin-Dest,0-1>
		for (String key : aggregatedVMTotal.keySet()) {	directDone.put("VM-"+key,0); }
		for (String key : aggregatedDBTotal.keySet()) { directDone.put("DB-"+key,0); }

		// STATISTICS: Print Connections
		for (String key: aggregatedVMTotal.keySet())
		{
				String[] aux = key.split("-");
				int id = retrieveCONID("VM",aux[0],aux[1]);
				if (id == -1) continue;
				double auxbwd =	retrieveBWD("VM",aux[0],aux[1]);
				System.out.println("CTRL VM-S> Transmission start at " + currentTS + " " + currentTS + " for " + aux[0] + " -> " + aux[1] + " BWD: " + auxbwd + " ID: " + id);
		}
		for (String key: aggregatedDBTotal.keySet())
		{
				String[] aux = key.split("-");
				int id = retrieveCONID("DB",aux[0],aux[1]);
				if (id == -1) continue;
				double auxbwd =	retrieveBWD("DB",aux[0],aux[1]);
				System.out.println("CTRL VM-C> Transmission start at " + currentTS + " " + currentTS + " for " + aux[0] + " -> " + aux[1] + " BWD: " + auxbwd + " ID: " + id);
		}

		long startTS = currentTS;

		// Loop for Time Steps
		while (currentTS < nextSchedTS)
		{
			// How much VM-S have been transmited in this time
			for (String key: aggregatedVMTotal.keySet())
			{
				String[] aux = key.split("-");

				if (directDone.get("VM-"+aux[0]+"-"+aux[1]).intValue() == 2) continue;

				int id = retrieveCONID("VM",aux[0],aux[1]);
				if (id == -1) continue;

				double vmtomove = aggregatedVMTotal.get(key).doubleValue();
				double auxbwd = retrieveBWD("VM",aux[0],aux[1]);
				if (auxbwd != -1.0 && vmtomove > 0)
				{
					double gbpstx = auxbwd * timeStep; // Step Time 60 seconds
					BigDecimal vmaux = new BigDecimal(volumeVMs);
					vmaux = vmaux.divide(BigDecimal.valueOf(1000 * 1000));
					vmaux = vmaux.divide(BigDecimal.valueOf(1000));
					double vmsmoved = gbpstx / vmaux.doubleValue();

					double prevmoved = directVMs.get(key);
					double alreadymoved = (prevmoved+vmsmoved < vmtomove)?(prevmoved+vmsmoved):vmtomove;
					vmsmoved = (prevmoved+vmsmoved > vmtomove)?vmtomove-alreadymoved:vmsmoved;
					directVMs.put(key,alreadymoved);
					System.out.println("Info VM-S> Transmission Info at " + currentTS + " for " + aux[0] + " -> " + aux[1] + " Moved_VMs: " + vmsmoved + " [" + alreadymoved + " / "+ vmtomove + "] BWD: " + auxbwd + " ID: " + id);
				}

				if (directVMs.get(key).doubleValue() >= aggregatedVMTotal.get(key).doubleValue())
				{
					// Finished VM-S. Reduce connection
					if (fixedPolicy) { connections.remove("VM-"+aux[0]+"-"+aux[1]); }
					else { boolean baux = closeConn (id); }
					if (vmtomove > 0 ) System.out.println("CTRL VM-S> Transmission Finished at " + currentTS + " " + startTS + " for " + aux[0] + " -> " + aux[1] + " ID: " + id);
					directDone.put("VM-"+aux[0]+"-"+aux[1],new Integer(2));
				}
				else
				{
					// Dynamic Elastic Policy: Demand more BWD with base new bwd reqs
					if (elastic && currentTS%600 == 0 && currentTS%3600 != 0 && !fixedPolicy)
					{
						// Recalculate BWD per path
						int move = aggregatedVMTotal.get(key);

						long remainingTime = 0;
						if ((nextSchedTS - currentTS) > (maxTimeRound - newTimeVM)) { remainingTime = (nextSchedTS - (maxTimeRound - newTimeVM)) - currentTS; }
						else { remainingTime = nextSchedTS - currentTS; }

						int newbwd = calculateBWD("VM",move,aux[0],aux[1],volumeVMs,(new Long(remainingTime)).intValue());				
						double currentbwd = retrieveBWD("VM",aux[0],aux[1]);

						System.out.println("Nego VM-S> " + currentTS + " Move: " + move + " Degr: - " + " NextTS: " + nextSchedTS + " MaxTimeRound: " + maxTimeRound + " TimeVM: " + newTimeVM  + " RemainingTime: " + remainingTime + " CurrentBWD: " + currentbwd + " NewBWD_Required " + newbwd);

						// Negotiate VM Sendin
						if (move > 0 && ((double)newbwd > ((double)currentbwd) * 1.10)) { boolean done = negotiateBWD("VM",aux[0],aux[1],newbwd,true); }
					}
				}
			}

			// How much VM-C have been transmited in this time
			for (String key: aggregatedDBTotal.keySet())
			{
				String[] aux = key.split("-");

				if (directDone.get("DB-"+aux[0]+"-"+aux[1]).intValue() > 0) continue;

				int id = retrieveCONID("DB",aux[0],aux[1]);
				if (id == -1) continue;

				double dbtomove = aggregatedDBTotal.get(key).doubleValue();
				double auxbwd = retrieveBWD("DB",aux[0],aux[1]);
				if (auxbwd != -1.0 && dbtomove > 0)
				{
					double gbpstx = auxbwd * timeStep; // Step Time 60 seconds
					BigDecimal dbaux = new BigDecimal(volumeData);
					dbaux = dbaux.divide(BigDecimal.valueOf(1000 * 1000));
					dbaux = dbaux.divide(BigDecimal.valueOf(1000));
					double bdsmoved = gbpstx / dbaux.doubleValue();

					double prevmoved = directVMc.get(key);
					double alreadymoved = (prevmoved+bdsmoved < dbtomove)?(prevmoved+bdsmoved):dbtomove;
					bdsmoved = (prevmoved+bdsmoved > dbtomove)?dbtomove-alreadymoved:bdsmoved;
					directVMc.put(key,alreadymoved);
					System.out.println("Info VM-C> Transmission Info at " + currentTS + " for " + aux[0] + " -> " + aux[1] + " Moved_DBs: " + bdsmoved + " [" + alreadymoved + " / " + dbtomove + "] BWD: " + auxbwd + " ID: " + id);
				}

				if (directVMc.get(key).doubleValue() >= aggregatedDBTotal.get(key).doubleValue())
				{
					// Finish VM-C. Go to next Step: Degraded VM-C
					if (dbtomove > 0 ) System.out.println("CTRL VM-C> Transmission Phased at " + currentTS + " " + startTS + " for " + aux[0] + " -> " + aux[1] + " ID: " + id);
					directDone.put("DB-"+aux[0]+"-"+aux[1],new Integer(1));
				}
				else
				{
					// Dynamic Elastic Policy: Demand more BWD with base bwd but max as possible
					if (elastic && currentTS%600 == 0 && currentTS%3600 != 0 && !fixedPolicy)
					{
						// Recalculate BWD per path
						int move = aggregatedDBTotal.get(key);
						int degr = degradedDBs.get(key);

						long remainingTime = 0;
						if ((nextSchedTS - currentTS) > (maxTimeRound - newTimeDB)) { remainingTime = (nextSchedTS - (maxTimeRound - newTimeDB)) - currentTS; }
						else { remainingTime = nextSchedTS - currentTS; }

						int newbwd = calculateBWD("DB",move,degr,aux[0],aux[1],volumeData,volumeDataDG,(new Long(remainingTime)).intValue());
						double currentbwd = retrieveBWD("DB",aux[0],aux[1]);

						System.out.println("Nego VM-C> " + currentTS + " Move: " + move + " Degr: " + degr + " NextTS: " + nextSchedTS + " MaxTimeRound: " + maxTimeRound + " TimeDB: " + newTimeDB  + " RemainingTime: " + remainingTime + " CurrentBWD: " + currentbwd + " NewBWD_Required " + newbwd);

						// Negotiate DB Sending
						if ((move > 0 || degr > 0) && ((double)newbwd > ((double)currentbwd) * 1.10)) { boolean done = negotiateBWD("DB",aux[0],aux[1],newbwd,true); }
					}
				}
			}

			// How much VM-C Degraded have been transmited in this time
			for (String key: aggregatedDBTotal.keySet())
			{
				String[] aux = key.split("-");

				if (directDone.get("DB-"+aux[0]+"-"+aux[1]).intValue() != 1) continue;

				int id = retrieveCONID("DB",aux[0],aux[1]);
				if (id == -1) continue;

				int dbtomove = aggregatedDBTotal.get(key);
				double degtomove = degradedDBs.get(key).doubleValue();
				double auxbwd = retrieveBWD("DB",aux[0],aux[1]);
				if (auxbwd != -1.0 && degtomove > 0)
				{
					double gbpstx = auxbwd * timeStep; // Step Time 60 seconds
					BigDecimal dbaux = new BigDecimal(volumeDataDG);
					dbaux = dbaux.divide(BigDecimal.valueOf(1000 * 1000));
					dbaux = dbaux.divide(BigDecimal.valueOf(1000));
					double bdsmoved = gbpstx / dbaux.doubleValue();

					double prevmoved = directVMd.get(key);
					double alreadymoved = (prevmoved+bdsmoved < degtomove)?(prevmoved+bdsmoved):degtomove;
					bdsmoved = (prevmoved+bdsmoved > degtomove)?degtomove-alreadymoved:bdsmoved;
					directVMd.put(key,alreadymoved);
					System.out.println("Info VM-C> Transmission Info at " + currentTS + " for " + aux[0] + " -> " + aux[1] + " Moved_Deg_DBs: " + bdsmoved + " [" + alreadymoved + " / " + degtomove + "] BWD: " + auxbwd + " ID: " + id);
				}

				if (directVMd.get(key).doubleValue() >= degradedDBs.get(key).doubleValue())
				{
					if (fixedPolicy) { connections.remove("DB-"+aux[0]+"-"+aux[1]); }
					else { boolean baux = closeConn (id); }
					if (dbtomove > 0 || degtomove > 0 ) System.out.println("CTRL VM-C> Transmission Finished at " + currentTS + " " + startTS + " for " + aux[0] + " -> " + aux[1] + " ID: " + id);
					directDone.put("DB-"+aux[0]+"-"+aux[1],new Integer(2));
				}
				else
				{
					// Dynamic Elastic Policy: Demand more BWD with base bwd but max as possible
					if (elastic && currentTS%600 == 0 && currentTS%3600 != 0 && !fixedPolicy)
					{
						// Recalculate BWD per path
						int move = 0;
						int degr = degradedDBs.get(key);

						long remainingTime = 0;
						if ((nextSchedTS - currentTS) > (maxTimeRound - newTimeDB)) { remainingTime = (nextSchedTS - (maxTimeRound - newTimeDB)) - currentTS; }
						else { remainingTime = nextSchedTS - currentTS; }

						int newbwd = calculateBWD("DB",move,degr,aux[0],aux[1],volumeData,volumeDataDG,(new Long(remainingTime)).intValue());
						double currentbwd = retrieveBWD("DB",aux[0],aux[1]);

						System.out.println("Nego VM-C> " + currentTS + " Move: " + move + " Degr: " + degr + " NextTS: " + nextSchedTS + " MaxTimeRound: " + maxTimeRound + " TimeDB: " + newTimeDB  + " RemainingTime: " + remainingTime + " CurrentBWD: " + currentbwd + " NewBWD_Required " + newbwd);

						// Negotiate DB Sending
						if (degr > 0 && ((double)newbwd > ((double)currentbwd) * 1.10)) { boolean done = negotiateBWD("DB",aux[0],aux[1],newbwd,true); }
					}
				}
			}

			currentTS = currentTS + timeStep; // Step Time 60 seconds
			if (!fixedPolicy) advanceTime(currentTS);
		}

		// Terminate unfinished connections
		for (String key: directDone.keySet())
		{
			int val = directDone.get(key).intValue();
			if (val != 2)
			{
				String[] aux = key.split("-");
				int id = retrieveCONID(aux[0],aux[1],aux[2]);
				if (id != -1)
				{
					if (fixedPolicy) { connections.remove(aux[0]+"-"+aux[1]+"-"+aux[2]); }
					else { boolean baux = closeConn (id); }
					System.out.println("CTRL VM-" + ((aux[0].compareTo("VM")==0)?"S":"C") + "> Transmission Finished at " + currentTS + " " + (((currentTS-1)/3600)*3600) + " for " + aux[1] + " -> " + aux[2] + " ID: " + id);
				}
			}
		}
		connections.clear();
		availableBWD.clear();

		// --------------------------------------------------------------------
		// Solving Movements for Transmissions
		// --------------------------------------------------------------------

		// How much VM-S have been migrated sucessfully
		for (String key: movementNumVM.keySet())
		{
			String[] aux = key.split("-");
			int move = movementNumVM.get(key);
			int vmkey = new Integer(aux[0]);
			String orig = aux[1];
			String dest = aux[2];

			Map <String,Integer> vmInDC = vmDistributionS.get(new Integer(vmkey));

			int toMigPath = aggregatedVMTotal.get(orig+"-"+dest);
			double migrPath = directVMs.get(orig+"-"+dest).doubleValue();
			double ratio = migrPath / (new Double(toMigPath)).doubleValue();

			int moved = move;
			if (ratio < 1.0) moved = (new Double(move*ratio)).intValue(); // Some VMs are behind
			vmInDC.put(orig,vmInDC.get(orig).intValue()-moved);
			vmInDC.put(dest,vmInDC.get(dest).intValue()+moved);
			System.out.println("REPO VM-S> " + currentTS + " VM-kind: " + vmkey + " Path: " + orig + "-" + dest + " VMs_To_Move: " + move + " VMs_Moved: " + moved);			
		}

		// How much VM-C data have been moved sucessfully
		for (String key: movementNumDB.keySet())
		{
			String[] aux = key.split("-");
			int move = movementNumDB.get(key);
			String orig = aux[0];
			String dest = aux[1];

			int toMigPath = aggregatedDBTotal.get(orig+"-"+dest);
			double migrPath = directVMc.get(orig+"-"+dest).doubleValue();
			int toMigPathD = degradedDBs.get(orig+"-"+dest);
			double migrPathD = directVMd.get(orig+"-"+dest).doubleValue();

			double ratio = migrPath / (new Double(toMigPath)).doubleValue();
			double ratio2 = migrPathD / (new Double(toMigPathD)).doubleValue();

			int moved = move;
			int prevdeg = degradedDBs.get(aux[0]+"-"+aux[1]).intValue();
			int degmoved = directVMd.get(aux[0]+"-"+aux[1]).intValue();
			int newdegs = 0;
			if (ratio < 1.0 || ratio2 < 1.0)
			{
				moved = (new Double(move*ratio)).intValue();
				newdegs = prevdeg - degmoved + (move - moved);
			}
			degradedDBs.put(aux[0]+"-"+aux[1],new Integer(newdegs));
			System.out.println("REPO VM-C> " + currentTS + " VM-kind: --- Path: " + orig + "-" + dest + " Images_To_Update: " + move + " Images_Updated: " + moved + 
																										" Prev_Degraded: " + prevdeg + " Degraded_Updated " + degmoved + " Remaining: " + newdegs);				
		}
	}
	
	private void pccNegotiateVM(String orig, String dest, String bwdkey, Map <String,Integer> aggregatedVMTotal)
	{
		int newTimeVM = (maxTimeRound * 1 / 2);

		// Calculate estimated BWD
		int move = aggregatedVMTotal.get(bwdkey);
		BigInteger totalVol = BigInteger.valueOf(move);
		totalVol = totalVol.multiply(volumeData);	

		// Negotiate VM Sending
		if (move > 0) {	boolean done = negotiateTime("VM",orig,dest,totalVol,newTimeVM); }
	}

	private void pccNegotiateDB(String orig, String dest, String bwdkey, Map <String,Integer> aggregatedDBTotal, Map <String,Integer> degradedDBs)
	{
		int newTimeDB = (maxTimeRound * 1 / 2);

		// Calculate estimated BWD
		int move = aggregatedDBTotal.get(bwdkey);
		int degraded = degradedDBs.get(orig+"-"+dest);
		BigInteger totalVol = BigInteger.valueOf(move);
		totalVol = totalVol.multiply(volumeData);	
		BigInteger totalDeg = BigInteger.valueOf(degraded);
		totalDeg = totalDeg.multiply(volumeDataDG);
		totalVol = totalVol.add(totalDeg);

		// Negotiate Data Sending
		if (move > 0 || degraded > 0) { boolean done = negotiateTime("DB",orig,dest,totalVol,newTimeDB);	}
	}

	private void policyCC (Map <String,Integer> aggregatedVMTotal, Map <String,Integer> movementNumVM, Map <String,Integer> aggregatedDBTotal, Map <String,Integer> movementNumDB)
	{
		// Calculate BWD and negotiate each connection for VM-S
		for(String bwdkey : aggregatedVMTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			pccNegotiateVM(aux[0],aux[1],bwdkey,aggregatedVMTotal);
		}

		// Calculate BWD and negotiate each connection for VM-C
		for(String bwdkey : aggregatedDBTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			if (!degradedDBs.containsKey(aux[0]+"-"+aux[1])) degradedDBs.put(aux[0]+"-"+aux[1],new Integer(0));
			pccNegotiateDB(aux[0],aux[1],bwdkey,aggregatedDBTotal,degradedDBs);
		}

		// STATISTICS: Print Connections
		for (String key: aggregatedVMTotal.keySet())
		{
				String[] aux = key.split("-");
				int id = retrieveTCID("VM",aux[0],aux[1]);
				if (id == -1) continue;
				double auxbwd =	retrieveTCBWD("VM",aux[0],aux[1]);
				System.out.println("CTRL VM-S> Transmission start at " + currentTS + " " + currentTS + " for " + aux[0] + " -> " + aux[1] + " BWD: " + auxbwd + " ID: " + id);
		}
		for (String key: aggregatedDBTotal.keySet())
		{
				String[] aux = key.split("-");
				int id = retrieveTCID("DB",aux[0],aux[1]);
				if (id == -1) continue;
				double auxbwd =	retrieveTCBWD("DB",aux[0],aux[1]);
				System.out.println("CTRL VM-C> Transmission start at " + currentTS + " " + currentTS + " for " + aux[0] + " -> " + aux[1] + " BWD: " + auxbwd + " ID: " + id);
		}

		Map <String,Double> timedVMs = new HashMap<String,Double>();	// <Origin-Dest,vms>
		Map <String,Double> timedVMc = new HashMap<String,Double>();	// <Origin-Dest,dbs>
		Map <String,Double> timedVMd = new HashMap<String,Double>();	// <Origin-Dest,dbs deg>
		for (String key : aggregatedVMTotal.keySet()) {	timedVMs.put(key,0.0); }
		for (String key : aggregatedDBTotal.keySet()) { timedVMc.put(key,0.0); }
		for (String key : aggregatedDBTotal.keySet()) { timedVMd.put(key,0.0); }

		Map <String,Integer> timedDone = new HashMap<String,Integer>();	// <Key-Origin-Dest,0-1>
		for (String key : aggregatedVMTotal.keySet()) {	timedDone.put("VM-"+key,0); }
		for (String key : aggregatedDBTotal.keySet()) { timedDone.put("DB-"+key,0); }

		long startTS = currentTS;

		// Start listener for Notify
		NotifyListener ntfl = new NotifyListener();
  		ntfl.start();

		// Loop for Time Steps
		//int count = timedConns.size();
		while (currentTS < nextSchedTS)
		{
			// How much VM-S have been transmited in this time
			for (String key: aggregatedVMTotal.keySet())
			{
				String[] aux = key.split("-");

				if (timedDone.get("VM-"+aux[0]+"-"+aux[1]).intValue() == 2) continue;

				int id = retrieveTCID("VM",aux[0],aux[1]);
				if (id == -1) continue;

				double vmtomove = aggregatedVMTotal.get(key).doubleValue();
				double auxbwd = retrieveTCBWD("VM",aux[0],aux[1]);
				if (auxbwd != -1.0 && vmtomove > 0)
				{
					double gbpstx = auxbwd * timeStep; // Step Time 60 seconds
					BigDecimal vmaux = new BigDecimal(volumeVMs);
					vmaux = vmaux.divide(BigDecimal.valueOf(1000 * 1000));
					vmaux = vmaux.divide(BigDecimal.valueOf(1000));
					double vmsmoved = gbpstx / vmaux.doubleValue();

					double prevmoved = timedVMs.get(key);
					double alreadymoved = (prevmoved+vmsmoved < vmtomove)?(prevmoved+vmsmoved):vmtomove;
					vmsmoved = (prevmoved+vmsmoved > vmtomove)?vmtomove-alreadymoved:vmsmoved;
					timedVMs.put(key,alreadymoved);
					System.out.println("Info VM-S> Transmission Info at " + currentTS + " for " + aux[0] + " -> " + aux[1] + " Moved_VMs: " + vmsmoved + " [" + alreadymoved + " / " + vmtomove + "] BWD: " + auxbwd + " ID: " + id);
				}

				if (timedVMs.get(key).doubleValue() >= aggregatedVMTotal.get(key).doubleValue())
				{
					// Mata la connexió tipus 4
					boolean baux = closeConn(id);
					removeTCID("VM",aux[0],aux[1]);
					removeTCBWD(id);
					System.out.println("CTRL VM-S> Transmission Finished at " + currentTS + " " + startTS + " for " + aux[0] + " -> " + aux[1] + " ID: " + id);
					timedDone.put("VM-"+aux[0]+"-"+aux[1],new Integer(2));
				}

			}

			// How much VM-C have been transmited in this time
			for (String key: aggregatedDBTotal.keySet())
			{
				String[] aux = key.split("-");

				if (timedDone.get("DB-"+aux[0]+"-"+aux[1]).intValue() > 0) continue;

				int id = retrieveTCID("DB",aux[0],aux[1]);
				if (id == -1) continue;

				double dbtomove = aggregatedDBTotal.get(key).doubleValue();
				double auxbwd = retrieveTCBWD("DB",aux[0],aux[1]);
				if (auxbwd != -1.0 && dbtomove > 0)
				{
					double gbpstx = auxbwd * timeStep; // Step Time 60 seconds
					BigDecimal dbaux = new BigDecimal(volumeData);
					dbaux = dbaux.divide(BigDecimal.valueOf(1000 * 1000));
					dbaux = dbaux.divide(BigDecimal.valueOf(1000));
					double bdsmoved = gbpstx / dbaux.doubleValue();

					double prevmoved = timedVMc.get(key);
					double alreadymoved = (prevmoved+bdsmoved < dbtomove)?(prevmoved+bdsmoved):dbtomove;
					bdsmoved = (prevmoved+bdsmoved > dbtomove)?dbtomove-alreadymoved:bdsmoved;
					timedVMc.put(key,alreadymoved);
					System.out.println("Info VM-C> Transmission Info at " + currentTS + " for " + aux[0] + " -> " + aux[1] + " Moved_DBs: " + bdsmoved + " [" + alreadymoved + " / " + dbtomove + "] BWD: " + auxbwd + " ID: " + id);
				}

				if (timedVMc.get(key).doubleValue() >= aggregatedDBTotal.get(key).doubleValue())
				{
					// Indicate NON-DBdeg Finished
					System.out.println("CTRL VM-C> Transmission Phased at " + currentTS + " " + startTS + " for " + aux[0] + " -> " + aux[1] + " ID: " + id);
					timedDone.put("DB-"+aux[0]+"-"+aux[1],new Integer(1));
				}
			}

			// How much VM-C Degraded have been transmited in this time
			for (String key: aggregatedDBTotal.keySet())
			{
				String[] aux = key.split("-");

				if (timedDone.get("DB-"+aux[0]+"-"+aux[1]).intValue() != 1) continue;

				int id = retrieveTCID("DB",aux[0],aux[1]);
				if (id == -1) continue;

				double degtomove = degradedDBs.get(key).doubleValue();
				double auxbwd = retrieveBWD("DB",aux[0],aux[1]);
				if (auxbwd != -1.0 && degtomove > 0)
				{
					double gbpstx = auxbwd * timeStep; // Step Time 60 seconds
					BigDecimal dbaux = new BigDecimal(volumeDataDG);
					dbaux = dbaux.divide(BigDecimal.valueOf(1000 * 1000));
					dbaux = dbaux.divide(BigDecimal.valueOf(1000));
					double bdsmoved = gbpstx / dbaux.doubleValue();

					double prevmoved = timedVMd.get(key);
					double alreadymoved = (prevmoved+bdsmoved < degtomove)?(prevmoved+bdsmoved):degtomove;
					bdsmoved = (prevmoved+bdsmoved > degtomove)?degtomove-alreadymoved:bdsmoved;
					timedVMd.put(key,alreadymoved);
					System.out.println("Info VM-C> Transmission Info at " + currentTS + " for " + aux[0] + " -> " + aux[1] + " Moved_Deg_DBs: " + bdsmoved + " [" + alreadymoved + " / " + degtomove + "] BWD: " + auxbwd + " ID: " + id);
				}

				if (timedVMd.get(key).doubleValue() >= degradedDBs.get(key).doubleValue())
				{
					// Mata la connexió tipus 4
					boolean baux = closeConn(id);
					removeTCID("DB",aux[0],aux[1]);
					removeTCBWD(id);
					System.out.println("CTRL VM-C> Transmission Finished at " + currentTS + " " + startTS + " for " + aux[0] + " -> " + aux[1] + " ID: " + id);
					timedDone.put("DB-"+aux[0]+"-"+aux[1],new Integer(2));
				}
			}

			// Adavance to Next Step
			currentTS = currentTS + timeStep; // Step Time 60 seconds
			advanceTime(currentTS);
		}

		synchronized (lock) 
		{
			try
			{
				String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Notify type=\"0\"><timestamp>" + currentTS + "</timestamp></Notify>\0";
				msgToMe(request);
			}
			catch (Exception e) { System.err.println("EXCEPTION: " + e); e.printStackTrace(); }
		}

		// Terminate unfinished connections
		for (String key: timedDone.keySet())
		{
			int val = timedDone.get(key).intValue();
			if (val != 2)
			{
				String[] aux = key.split("-");

				int id = retrieveTCID(aux[0],aux[1],aux[2]);
				if (id != -1)
				{
					boolean baux = closeConn(id);
					removeTCID(aux[0],aux[1],aux[2]);
					removeTCBWD(id);
					System.out.println("CTRL VM-" + ((aux[0].compareTo("VM")==0)?"S":"C") + "> Transmission Terminated at " + currentTS + " for " + aux[1] + " -> " + aux[2] + " ID: " + id);
				}
			}
		}

		timedConns.clear();
		timedIDBWD.clear();

		// --------------------------------------------------------------------
		// Solving Movements for Transmissions
		// --------------------------------------------------------------------

		// How much VM-S have been migrated sucessfully
		for (String key: movementNumVM.keySet())
		{
			String[] aux = key.split("-");
			int move = movementNumVM.get(key);
			int vmkey = new Integer(aux[0]);
			String orig = aux[1];
			String dest = aux[2];

			Map <String,Integer> vmInDC = vmDistributionS.get(new Integer(vmkey));

			int toMigPath = aggregatedVMTotal.get(orig+"-"+dest);
			double migrPath = timedVMs.get(orig+"-"+dest).doubleValue();
			double ratio = migrPath / (new Double(toMigPath)).doubleValue();

			int moved = move;
			if (ratio < 1.0) moved = (new Double(move*ratio)).intValue(); // Some VMs are behind
			vmInDC.put(orig,vmInDC.get(orig).intValue()-moved);
			vmInDC.put(dest,vmInDC.get(dest).intValue()+moved);
			System.out.println("REPO VM-S> " + currentTS + " VM-kind: " + vmkey + " Path: " + orig + "-" + dest + " VMs_To_Move: " + move + " VMs_Moved: " + moved);			
		}

		// How much VM-C data have been moved sucessfully
		for (String key: movementNumDB.keySet())
		{
			String[] aux = key.split("-");
			int move = movementNumDB.get(key);
			String orig = aux[0];
			String dest = aux[1];

			int toMigPath = aggregatedDBTotal.get(orig+"-"+dest);
			double migrPath = timedVMc.get(orig+"-"+dest).doubleValue();
			int toMigPathD = degradedDBs.get(orig+"-"+dest);
			double migrPathD = timedVMd.get(orig+"-"+dest).doubleValue();

			double ratio = migrPath / (new Double(toMigPath)).doubleValue();
			double ratio2 = migrPathD / (new Double(toMigPathD)).doubleValue();

			int moved = move;
			int prevdeg = degradedDBs.get(aux[0]+"-"+aux[1]).intValue();
			int degmoved = timedVMd.get(aux[0]+"-"+aux[1]).intValue();
			int newdegs = 0;
			if (ratio < 1.0 || ratio2 < 1.0)
			{
				moved = (new Double(move*ratio)).intValue();
				newdegs = prevdeg - degmoved + (move - moved);
			}
			degradedDBs.put(aux[0]+"-"+aux[1],new Integer(newdegs));
			System.out.println("REPO VM-C> " + currentTS + " VM-kind: --- Path: " + orig + "-" + dest + " Images_To_Update: " + move + " Images_Updated: " + moved + " Prev_Degraded: " + prevdeg + " Degraded_Updated " + degmoved + " Remaining: " + newdegs);
		}
	}

	
	/**************************************************************************/
	/* Funcions Calcular BWD i Negociar BWD                                   */
	/**************************************************************************/

	private final Pattern msgAssBWD = Pattern.compile("<assignedbw>(.+?)</assignedbw>");
	private final Pattern msgMaxTime = Pattern.compile("<maxtime>(.+?)</maxtime>");
	private final Pattern msgMaxBWD = Pattern.compile("<maxbw>(.+?)</maxbw>");
	private final Pattern msgConnID = Pattern.compile("<id>(.+?)</id>");
	private final Pattern msgTimestamp = Pattern.compile("<timestamp>(.+?)</timestamp>");
	
	private int calculateBWD (String keyname, int move, String origin, String destination, BigInteger volume, int maxTime)
	{
/*		if (move < 1) return 1;

		if (keyname.compareTo("DB") != 0)
		{
			System.out.println("Migrate VMs " + keyname + " from " + origin + " to " + destination);
			System.out.println("* Num VMs: " + move + " ( " + volume + " bits per VM ) ");
		}
		else
		{
			System.out.println("Migrate DBs " + keyname + " from " + origin + " to " + destination);
			System.out.println("* Num DBs: " + move + " ( " + volume + " bits per DB ) ");
		}

		// Calculate MaxTime in total... and per VM
		BigInteger reqBWD = BigInteger.valueOf(move);
		reqBWD = reqBWD.multiply(volume);
		reqBWD = reqBWD.divide(BigInteger.valueOf(maxTime));

		BigInteger reqGbps = reqBWD.divide(BigInteger.valueOf(1000*1000));
		reqGbps = reqGbps.divide(BigInteger.valueOf(1000));

		BigDecimal VMps = new BigDecimal(reqBWD);
		VMps = VMps.divide(new BigDecimal(volumeVMs));

		return Math.max(reqGbps.intValue(),1) +1;
*/
		return calculateBWD (keyname,move,0,origin,destination,volume,null,maxTime);
	}

	private int calculateBWD (String keyname, int move, int degraded, String origin, String destination, BigInteger volume, BigInteger volumeDeg, int maxTime)
	{
		if (move < 1) return 1;

		if (keyname.compareTo("DB") != 0)
		{
			System.out.println("Migrate VMs " + keyname + " from " + origin + " to " + destination);
			System.out.println("* Num VMs: " + move + " ( " + volume + " bits per VM ) ");
		}
		else
		{
			System.out.println("Migrate DBs " + keyname + " from " + origin + " to " + destination);
			System.out.println("* Num DBs: " + move + " ( " + volume + " bits per DB ) " + degraded + " ( " + volumeDeg + " bits poer DegDB )");
		}

		// Calculate MaxTime in total... and per VM
		BigInteger reqBWD = BigInteger.valueOf(move);
		reqBWD = reqBWD.multiply(volume);
		if (volumeDeg != null)
		{
			BigInteger reqBWD2 = BigInteger.valueOf(degraded);
			reqBWD2 = reqBWD2.multiply(volumeDeg);
			reqBWD = reqBWD.add(reqBWD2);
		}
		reqBWD = reqBWD.divide(BigInteger.valueOf(maxTime));

		BigInteger reqGbps = reqBWD.divide(BigInteger.valueOf(1000*1000));
		reqGbps = reqGbps.divide(BigInteger.valueOf(1000));

		BigDecimal VMps = new BigDecimal(reqBWD);
		VMps = VMps.divide(new BigDecimal(volumeVMs));

		return Math.max(reqGbps.intValue(),1) + 1;
	}
	
	private boolean negotiateBWD (String keyname, String origin, String destination, double bwd, boolean fixed)
	{
		boolean retval = false;
		if (bwd < 10.0) bwd = 10.0;

		boolean exists = connections.containsKey(keyname+"-"+origin+"-"+destination);
		double testBWD = (fixed)?bwd:200;//Double.MAX_VALUE;
		if (exists)
		{
			if (bwd < retrieveBWD(keyname,origin,destination)) testBWD = bwd;	// We want to reduce, not negotiate above bwd.
			if (bwd == retrieveBWD(keyname,origin,destination))
			{
				//System.out.println("* No Negotiation Needed " + origin + " -> " + destination + " " + bwd + " Gbps"); 
				return true;
			}
		}
		System.out.println("* Negotiate connection " + origin + " -> " + destination + " " + bwd + " Gbps");

		boolean done = false;
		boolean keepGoing = true;
		while (keepGoing)
		{
			countNegotiations = countNegotiations + 1;
			System.out.println("* Current Negotiation " + origin + " -> " + destination + " " + testBWD + " Gbps");
			double resaux = 0.0;
			if (!exists) resaux = openConn(origin,destination,testBWD,keyname);
			else 
			{
				int id = connections.get(keyname+"-"+origin+"-"+destination);
				resaux = modifyConn(id,testBWD);
			}
			
			if (resaux > 0)
			{
				if (!exists) connections.put(keyname+"-"+origin+"-"+destination,((new Double(resaux)).intValue()-1)); // FIXME - IDs i coses. Fer estructura de resposta
				provideBWD(keyname,origin,destination,testBWD);
				keepGoing = false;
				done = true;
				retval = true;
				System.out.println("* Negotiation Success " + origin + " -> " + destination + " " + retrieveBWD(keyname,origin,destination) + " Gbps"); 
			}		
			else if (resaux <= 0)
			{
				testBWD = -1 * resaux;					// Proposed new BWD
				if (testBWD < 0.0) keepGoing = false;	// Game Over, we keep old bwd
				System.out.println("* Proposed new BWD " + origin + " -> " + destination + " " + testBWD + " Gbps");
			}
		}
		if (!done)
		{
			System.out.println("* Failed Negotiation " + origin + " -> " + destination + " " + bwd + " Gbps");
			if (!exists)
			{
				provideBWD(keyname,origin,destination,0.0);
			}
		}

		return retval;
	}

	private boolean negotiateTime (String keyname, String origin, String destination, BigInteger volume, long time)
	{
		boolean retval = false;
		boolean done = false;

		System.out.println("* Negotiate connection " + origin + " -> " + destination + " " + volume + " bits in " + time + " seconds");

		BigInteger newVolume = new BigInteger(volume.toString());

		boolean keepGoing = true;
		while (keepGoing)
		{
			countNegotiations = countNegotiations + 1;
			long resaux = timedConn(keyname,origin,destination,newVolume,time,keyname);
			if (resaux > 0)
			{
				done = true;
				retval = true;
				keepGoing = false;
				double bwd = retrieveTCBWD(keyname,origin,destination);
				int id = retrieveTCID(keyname,origin,destination);
				System.out.println("* Negotiation Success " + origin + " -> " + destination + " id: " + id + " bwd: " + bwd + " Gbps; Time: " + resaux + " seconds");
			}		
			else if (resaux <= 0)
			{
				long newTime = -1 * resaux;						// Proposed new Time
				
				// Change Volume of sendings to fit in time
				double ratio = (new Double(newTime)).doubleValue()/(new Double(time)).doubleValue();
				BigDecimal bda = new BigDecimal(newVolume);
				bda = bda.divide(BigDecimal.valueOf(ratio), 2, RoundingMode.HALF_UP);
				newVolume = bda.toBigInteger();
				
				BigInteger bda2 = newVolume.multiply(BigInteger.valueOf(10));
				if (bda2.compareTo(volume) < 0) keepGoing = false;	// Game Over, we keep old bwd FIXME - Stablish limit.
				//System.out.println("* Proposed new Time " + origin + " -> " + destination + " " + newTime + " seconds");
			}
		}
		if (!done)
		{
			System.out.println("* Failed Negotiation " + origin + " -> " + destination + " " + volume + " Gbits in " + time + " seconds");
		}
		return retval;
	}

	/**************************************************************************/
	/* Funcions De Comunicació amb VNM                                        */
	/**************************************************************************/
	
	// Return (ID+1) if OK, -1 * Proposed BWD if KO
	private double openConn(String origin, String destination, double bwd, String connClass)
	{
		long unixTime = currentTS;
		String ip1 = DNSMap.get(origin);
		String ip2 = DNSMap.get(destination);

		double retval = 0.0;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"1\"><sourceip>" + ip1 + "</sourceip><destinationip>" + ip2 + "</destinationip><requestedbw>" + bwd + "</requestedbw><timestamp>" + unixTime + "</timestamp><class>" + connClass + "</class></Request>";
		String reply = sendMessage (request);
			
		if (reply.contains("<status>OK</status>"))
		{
			Matcher matcher = msgConnID.matcher(reply);
			matcher.find();
			int id = (new Integer(matcher.group(1))).intValue() + 1; // FIXME - IDs i coses. Fer estructura de resposta
			retval = (new Double(id)).doubleValue();
		}
		else if (reply.contains("<status>KO</status>"))
		{
			Matcher matcher = msgMaxBWD.matcher(reply);
			matcher.find();
			retval = -1 * (new Double(matcher.group(1))).doubleValue();	// proposed new BWD
		}
		return retval;
	}

	// Return 1 if OK, -1 * Proposed BWD if KO
	private double modifyConn (int id, double bwd)
	{
		long unixTime = currentTS;

		double retval = 0.0;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"2\"><id>" + id + "</id><requestedbw>" + bwd + "</requestedbw><timestamp>" + unixTime + "</timestamp></Request>";
		String reply = sendMessage (request);

		if (reply.contains("<status>OK</status>"))
		{
			retval = 1;
		}
		else if (reply.contains("<status>KO</status>"))
		{
			Matcher matcher = msgMaxBWD.matcher(reply);
			matcher.find();
			retval = -1 * (new Double(matcher.group(1))).doubleValue();	// Proposed new BWD
		}
		return retval;
	}

	private boolean closeConn (int id)
	{
		long unixTime = currentTS;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"3\"><id>" + id + "</id><timestamp>" + unixTime + "</timestamp></Request>";

		boolean retval = false;
		String reply = sendMessage (request);
		if (reply.contains("<status>OK</status>")) retval = true;
		
		return retval;
	}

	// Return: 1 if OK;  required time * -1 if KO
	private long timedConn(String keyname, String origin, String destination, BigInteger volume, long time, String connClass)
	{
		long unixTime = currentTS;
		String ip1 = DNSMap.get(origin);
		String ip2 = DNSMap.get(destination);

		long retval = 1;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"4\"><sourceip>" + ip1 + "</sourceip><destinationip>" + ip2 + "</destinationip><requestedTransfer>"+volume+"</requestedTransfer><completion>"+time+"</completion><timestamp>" + unixTime + "</timestamp><class>" + connClass + "</class></Request>";
		String reply = sendMessage (request);
			
		if (reply.contains("<status>OK</status>"))
		{
			Matcher matcher = msgConnID.matcher(reply);
			matcher.find();
			int id = (new Integer(matcher.group(1))).intValue();

			matcher = msgAssBWD.matcher(reply);
			matcher.find();
			double bwd = (new Double(matcher.group(1))).doubleValue();

			provideTCID(keyname,origin,destination,id);
			provideTCBWD(id,bwd);

			matcher = msgMaxTime.matcher(reply);
			matcher.find();

			long givenTime = (new Long(matcher.group(1))).longValue();
			retval = givenTime;
		}
		else if (reply.contains("<status>KO</status>"))
		{

			//System.out.println("ERROR de TIMEDCONN: " + reply);

			Matcher matcher = msgMaxTime.matcher(reply);
			matcher.find();
			long newTime = (new Long(matcher.group(1))).longValue(); // Proposed new time
			retval = -1 * newTime;
		}
		return retval;
	}

	private void advanceTime (long tstamp)
	{
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"5\"><timestamp>" + tstamp + "</timestamp></Request>";
		String reply = sendMessage (request);

		//System.out.println("ADV> " + reply);
	}

	/**************************************************************************/
	/* Funcions Enviar Missatges UDP                                          */
	/**************************************************************************/

	private String sendMessage (String sentence)
	{
		String retval = "";
		try
		{
			DatagramSocket clientSocket = new DatagramSocket();
			InetAddress IPAddress = InetAddress.getByName(address);
			byte[] sendData = sentence.getBytes();

			//byte[] headerInt = ByteBuffer.allocate(4).putInt(sendData.length).array();
			byte[] headerInt = new byte[4];
			headerInt[3] = (byte)((sendData.length & 0x000000FF) >> 0*8);
			headerInt[2] = (byte)((sendData.length & 0x0000FF00) >> 1*8);
			headerInt[1] = (byte)((sendData.length & 0x00FF0000) >> 2*8);
			headerInt[0] = (byte)((sendData.length & 0xFF000000) >> 3*8);
			
			DatagramPacket sendInteger = new DatagramPacket(headerInt,headerInt.length,IPAddress,port);
			clientSocket.send(sendInteger);
			DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, IPAddress,port);
			clientSocket.send(sendPacket);
			
			//System.out.println("* Sending message: [" + sendData.length + "]" + sentence);

			byte[] receiveData = new byte[1024];
			DatagramPacket receivePacket = new DatagramPacket(receiveData,receiveData.length,IPAddress,port);
			clientSocket.receive(receivePacket);

			clientSocket.close();
			retval = new String(receivePacket.getData());
			
			//System.out.println("* Received message: " + retval);
		}
		catch (Exception e) { retval = "<status>KO</status>"; e.printStackTrace(); }
		
		return retval;
	}

	private String udpListener() throws InterruptedException, InterruptedIOException
	{
		String retval = "";
		DatagramSocket clientSocket = null;
		try
		{
			clientSocket = new DatagramSocket(portlistener);

			byte[] buffer = new byte[2048];
			DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
			clientSocket.receive(packet);
			retval = new String(buffer, 0, packet.getLength());

			//System.out.println("* Received message: " + retval);
		}
		catch (Exception e) { retval = "<status>KO</status>"; e.printStackTrace(); }
		finally
		{
			if (clientSocket != null) clientSocket.close();
		}	

		return retval;
	}

	private String udpSender (String sentence)
	{
		String retval = "";
		try
		{
			DatagramSocket clientSocket = new DatagramSocket();
			InetAddress IPAddress = InetAddress.getByName(address);
			byte[] sendData = sentence.getBytes();

			//byte[] headerInt = ByteBuffer.allocate(4).putInt(sendData.length).array();
			byte[] headerInt = new byte[4];
			headerInt[3] = (byte)((sendData.length & 0x000000FF) >> 0*8);
			headerInt[2] = (byte)((sendData.length & 0x0000FF00) >> 1*8);
			headerInt[1] = (byte)((sendData.length & 0x00FF0000) >> 2*8);
			headerInt[0] = (byte)((sendData.length & 0xFF000000) >> 3*8);
			
			DatagramPacket sendInteger = new DatagramPacket(headerInt,headerInt.length,IPAddress,port);
			clientSocket.send(sendInteger);
			DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, IPAddress,port);
			clientSocket.send(sendPacket);
			
			//System.out.println("* Sending message: [" + sendData.length + "]" + sentence);
		}
		catch (Exception e) { retval = "<status>KO</status>"; e.printStackTrace(); }
		
		return retval;
	}

	private void msgToMe (String sentence)
	{
		try
		{
			DatagramSocket clientSocket = new DatagramSocket();
			InetAddress IPAddress = InetAddress.getByName("localhost");
			byte[] sendData = sentence.getBytes();

			DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length,IPAddress,portlistener);
			clientSocket.send(sendPacket);
		}
		catch (Exception e) { e.printStackTrace(); }
	}

	private class NotifyListener extends Thread
	{
		public boolean keepgoing = true;

		public void run ()
		{
			try
			{
				while (keepgoing)
				{
					String reply = udpListener();
					synchronized (lock) 
					{
						if (reply.contains("<Notify type=\"1\">"))
						{
							continue;
							/*
							// End of TX
							Matcher matcher = msgConnID.matcher(reply);
							matcher.find();
							int id = (new Integer(matcher.group(1))).intValue();

							matcher = msgAssBWD.matcher(reply);
							matcher.find();
							double bwd = (new Double(matcher.group(1))).doubleValue();

							// Get connections for id
							String aux = "";
							for (String key : setOfTCID())
							{
								if (timedConns.get(key).intValue() == id) aux = key;
							}	
							String[] aux2 = aux.split("-");

							// Mata la connexió tipus 4
							boolean baux = closeConn(id);
							removeTCID(aux2[0],aux2[1],aux2[2]);
							removeTCBWD(id);

							System.out.println("Transmission Finished at " + currentTS + " for " + aux2[1] + " -> " + aux2[2]);
							*/					
						}
						else if (reply.contains("<Notify type=\"2\">"))
						{
							//System.out.println("NOTIFY> " + reply);

							// Change of BWD
							Matcher matcher = msgConnID.matcher(reply);
							matcher.find();
							int id = (new Integer(matcher.group(1))).intValue();

							matcher = msgAssBWD.matcher(reply);
							matcher.find();
							double bwd = (new Double(matcher.group(1))).doubleValue();

							// Get connections for id
							String aux = "";
							for (String key : setOfTCID())
							{
								if (timedConns.get(key).intValue() == id) aux = key;
							}	
							String[] aux2 = aux.split("-");
						
							provideTCID(aux2[0],aux2[1],aux2[2],id);
							provideTCBWD(id,bwd);
						}
						else if (reply.contains("<Notify type=\"0\">"))
						{
							keepgoing = false;
						}
						else
						{
							System.out.println("* Error on receiving UDP: " + reply);
						}
					}
				}
			}
 			catch (InterruptedException iex) { }
			catch (InterruptedIOException iioex) { }
			finally
			{
				System.out.println("* Ending UDP Listener for Notify");
			}
		}
	}
}
