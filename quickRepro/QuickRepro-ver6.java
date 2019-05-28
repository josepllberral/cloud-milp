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

// TODO - Min-Conn desde les Estadístiques...

class QuickRepro
{
	public static void main(String args[])
	{
		String a = (args.length > 0)?args[0]:"localhost";
		int t = (args.length > 1)?(new Integer(args[1])).intValue():0;
		boolean c = false;
		boolean p = false;
		if (args.length > 2)
		{
			if (args[2].compareTo("coop")==0)
			{
				c = true;
				System.out.println("* MODE COOP");
			}
			else if (args[2].compareTo("pre")==0)
			{
				p = true;
				System.out.println("* MODE PREEMPTIVE");
			}
		}
		if (args.length > 3)
		{
			if (args[3].compareTo("coop")==0)
			{
				c = true;
				System.out.println("* MODE COOP");
			}
			else if (args[3].compareTo("pre")==0)
			{
				p = true;
				System.out.println("* MODE PREEMPTIVE");
			}
		}

		QuickRepro qr = new QuickRepro(a,t,c,p);
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
	private int maxTimeRound;

	private String address = "localhost";
	private int port = 5555;
	private int portlistener = 5550;
	private boolean coop = false;	// True -> Coop; False -> Master/Slave
	private int typeExps = 0;
	private boolean preemptive = false;

	private Object lock = new Object();

	public QuickRepro (String addr, int typeExps, boolean coop, boolean pre)
	{
		ListOfVMs = new HashMap<Integer,Map>();
		ListOfVMc = new HashMap<Integer,Map>();
		DNSMap = new HashMap<String,String>();
		vmDistributionS = new HashMap<Integer,Map>();
		vmDistributionC = new HashMap<Integer,Map>();
		vmAvgDown = new HashMap<String,Double>();
		hostList = new ArrayList<String>();

		// Info VMs - Configuration
		numVMs = 5000;
		numVMc = 5000;
		if (typeExps == 0) numVMc = 0;				// Only VM-S
		if (typeExps == 1) numVMs = 0;				// Only VM-C
		
		volumeVMs = BigInteger.valueOf(5);			// BITS per VM FIXME - Default 5GB/VM
		volumeVMs = volumeVMs.multiply(BigInteger.valueOf(1000*1000));
		volumeVMs = volumeVMs.multiply(BigInteger.valueOf(1000*8));
		maxTimeRound = 3600;						// in Seconds FIXME - Trade-off between foreign-time penalty and cost of bwd...

		volumeData = BigInteger.valueOf(5);			// BITS per Generated VM Data FIXME - Default 5GB/VM
		volumeData = volumeData.multiply(BigInteger.valueOf(1000*1000));
		volumeData = volumeData.multiply(BigInteger.valueOf(1000*8));

		// Simulator Address
		address = addr; // "idealist.pc.ac.upc.edu";
		port = 5555;
		portlistener = 5550;

		this.typeExps = typeExps;
		this.coop = coop;
		this.preemptive = pre;
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

					numDCs++;
					DNSMap.put(aux[4],aux[7]);
					
					hostList.add(aux[4]);
				}
				else if (strLine.contains("Built proxy for"))
				{
					String[] aux = strLine.split("\\s+");
					ListOfVMs.put(new Integer(aux[4]),new HashMap<String,Integer>());
					ListOfVMc.put(new Integer(aux[4]),new HashMap<String,Integer>());

					// Inicialization of Load VM Distribution (Singularity)
					Map <String,Integer> vmInDCS = new HashMap<String,Integer>();
					for (String dckey : DNSMap.keySet()) vmInDCS.put(dckey,0);
					vmInDCS.put("atom00",numVMs);
					vmDistributionS.put(new Integer(aux[4]),vmInDCS);

					// Inicialization of Load VM Distribution (Clonic VMs)
					Map <String,Integer> vmInDCC = new HashMap<String,Integer>();
					for (String dckey : DNSMap.keySet()) vmInDCC.put(dckey,0);
					vmInDCC.put("atom00",numVMc);
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
		boolean warmup = true;

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
			}
			
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

						int bwd = 1; // FIXME - Open only to 1Gbps... must adjust to common traffic
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
			boolean baux = closeConn (id);
		}
		String reply = udpSender ("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"999\"></Request>");
	}

	private void schedule()
	{
		System.out.println("Iteration: " + numIter);

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

			// Print Values of Load
			for (String key : keys)
			{ 
				int value = LoadLocation.get(key).intValue();
				//System.out.println("SVM> VMs: " + vmkey + " Load from DC: " + key + " -> " + value);
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
				//System.out.println("VM: " + vmkey + " -> Must move " + numVMInDC +  " VMs from " + currentHost + " to " + bestHost + " (max load here: " + bestValue + ")");	

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
		
		// --------------------------------------------------------------------
		// Clonic VMs (DB sync)
		// --------------------------------------------------------------------
		Map <String,Integer> aggregatedTrafficTotal = new HashMap<String,Integer>();
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
					if (!aggregatedTrafficTotal.containsKey(key+"-"+key2)) aggregatedTrafficTotal.put(key+"-"+key2,new Integer(0));
				
					int valueS = aggregatedTraffic.get(key+"-"+key2);
					int valueT = aggregatedTrafficTotal.get(key+"-"+key2);
					aggregatedTrafficTotal.put(key+"-"+key2,valueS + valueT);
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
				double percent = LoadLocation.get(key).intValue() / (new Double(sumLoad)).doubleValue();
				int value = (int)Math.round(numVMs * percent);
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
		}

		// From aggregated Traffic VM-C, get what to move
		for (String key : aggregatedTrafficTotal.keySet())
		{
			int move = aggregatedTrafficTotal.get(key).intValue();
			movementNumDB.put(key,move);
		}
		
		// --------------------------------------------------------------------
		// Policies to be applied
		// --------------------------------------------------------------------

		if (!coop) policyMS (aggregatedVMTotal,movementNumVM,aggregatedTrafficTotal,movementNumDB);
		else policyCC (aggregatedVMTotal,movementNumVM,aggregatedTrafficTotal,movementNumDB);

		// TODO - Estadístiques

		// --------------------------------------------------------------------
		// Preemptive Scheduling 
		// --------------------------------------------------------------------

		if ((nextSchedTS - currentTS > 15*60) && preemptive)
		{
			currentTS = nextSchedTS - 15*60;
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
		Map <String,Integer> aggregatedVMTotal = new HashMap<String,Integer>();
		Map <String,Integer> movementNumVM = new HashMap<String,Integer>();

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
		
		// Execute Negotiation and Transference
		int auxmtr = maxTimeRound;
		maxTimeRound = 15 * 60;
		policyMS (aggregatedVMTotal,movementNumVM,new HashMap<String,Integer>(),new HashMap<String,Integer>());
		maxTimeRound = auxmtr;
	}

	/**************************************************************************/
	/* TADS                                                                   */
	/**************************************************************************/

	private Map <String,Integer> connections = new HashMap<String,Integer>();
	private Map <String,Double> availableBWD = new HashMap<String,Double>();
	
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

	private void policyMS (Map <String,Integer> aggregatedVMTotal, Map <String,Integer> movementNumVM, Map <String,Integer> aggregatedTrafficTotal, Map <String,Integer> movementNumDB)
	{
		// --------------------------------------------------------------------
		// BWD Negotiation per Path
		// --------------------------------------------------------------------

		// Perform BWD negotiations for VM-S
		Map <String,Integer> requiredPathVMBWD = new HashMap<String,Integer>();
		for (String key : aggregatedVMTotal.keySet())
		{
				// Get BWD per path
				int move = aggregatedVMTotal.get(key);
				String[] aux = key.split("-");
				int bwd = calculateBWD("VM",move,aux[0],aux[1],volumeVMs,(maxTimeRound/2));				
				requiredPathVMBWD.put(key,bwd);

				// Negotiate VM Sending
				boolean done = negotiateBWD("VM",aux[0],aux[1],bwd,true);
		}

		// Perform BWD negotiations for VM-C
		Map <String,Integer> requiredPathDBBWD = new HashMap<String,Integer>();
		for(String bwdkey : aggregatedTrafficTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			
			// Calculate estimated BWD
			int move = aggregatedTrafficTotal.get(aux[0]+"-"+aux[1]);
			int bwd = calculateBWD("DB",move,aux[0],aux[1],volumeData,maxTimeRound);
			requiredPathDBBWD.put(bwdkey,bwd);

			// Negotiate Data Sending
			boolean done = negotiateBWD("DB",aux[0],aux[1],bwd,true);
		}

		// sleep for 1000 ms (Just in case...)
		try	{ Thread.currentThread().sleep(1000); }
		catch(InterruptedException ie) { System.err.println("ERROR: " + ie); ie.printStackTrace(); }
		
		// --------------------------------------------------------------------
		// Solving Movements for Transmissions
		// --------------------------------------------------------------------
		
		// Resolve number of VMs moved or left behind
		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());

			Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);
			for (String key : requiredPathVMBWD.keySet())
			{
				String aux[] = key.split("-");
				double auxbwd = retrieveBWD("VM",aux[0],aux[1]);			// Available BWD
				double reqbwd = requiredPathVMBWD.get(key).doubleValue();	// Required BWD
				int move = movementNumVM.get(vmkey+"-"+key).intValue();		// VMs to move from key -> key2

				int moved = move;
				if (auxbwd < reqbwd)
				{
					double ratio = (reqbwd>0)?(auxbwd/reqbwd):0;
					moved = (new Double(move*ratio)).intValue(); 		// Some VMs are behind
				}
				vmInDC.put(aux[0],vmInDC.get(aux[0]).intValue()-moved);
				vmInDC.put(aux[1],vmInDC.get(aux[1]).intValue()+moved);
				System.out.println("Movement of VM-S> Path: " + key + " Required_BWD: " + reqbwd + " Obtained_BWD: " + auxbwd + " VMs_To_Move: " + move + " VMs_Moved: " + moved);			
			}
		}

		// Resolve number of DB data communicated
		for (String key : requiredPathDBBWD.keySet())
		{
			String aux[] = key.split("-");
			double auxbwd = retrieveBWD("DB",aux[0],aux[1]);			// Available BWD
			double reqbwd = requiredPathDBBWD.get(key).doubleValue();	// Required BWD
			int move = movementNumDB.get(key).intValue();				// Data to move from orig -> dest

			int moved = move;
			if (auxbwd < reqbwd)
			{
				// Some VMs are behind
				double ratio = (reqbwd>0)?(auxbwd/reqbwd):0;
				moved = (new Double(move*ratio)).intValue();
				// INFO - Data was not finished to transmit
			}
			System.out.println("Movement of VM-C> Path: " + key + " Required_BWD: " + reqbwd + " Obtained_BWD: " + auxbwd + " Images_To_Update: " + move + " Images_Updated: " + moved);				
		}
		
		// --------------------------------------------------------------------
		// Solving Timmings for Transmissions
		// --------------------------------------------------------------------

		Map<String,Long> endingMap = new HashMap<String,Long>();
		for(String bwdkey : setOfBWD())
		{
			String[] aux = bwdkey.split("-");
			String kname = aux[0];
			String key = aux[1];
			String key2 = aux[2];
		
			BigDecimal spentTime = BigDecimal.ZERO;
			if (kname.compareTo("VM")==0 && aggregatedVMTotal.containsKey(key+"-"+key2))
			{
				BigDecimal bigvolume = new BigDecimal(volumeVMs);													// bits/VM
				BigDecimal totalVol =  BigDecimal.valueOf(aggregatedVMTotal.get(key+"-"+key2).doubleValue());		// VMs/path
				totalVol = totalVol.multiply(bigvolume);
				
				BigDecimal bwd = BigDecimal.valueOf(retrieveBWD("VM",key,key2));									// Gbps/path
				bwd = bwd.multiply(BigDecimal.valueOf(1000 * 1000));
				bwd = bwd.multiply(BigDecimal.valueOf(1000));														// bits/path

				if (bwd.compareTo(BigDecimal.ZERO) > 0.0) spentTime = totalVol.divide(bwd, 2, RoundingMode.HALF_UP);
			}
			if (kname.compareTo("DB")==0 && aggregatedTrafficTotal.containsKey(key+"-"+key2))
			{
				BigDecimal bigvolume = new BigDecimal(volumeData);													// bits/VM-C
				BigDecimal totalVol =  BigDecimal.valueOf(aggregatedTrafficTotal.get(key+"-"+key2).doubleValue());	// VM-C/path
				totalVol = totalVol.multiply(bigvolume);															// bits/path

				BigDecimal bwd = BigDecimal.valueOf(retrieveBWD("DB",key,key2));									// Gbps/path
				bwd = bwd.multiply(BigDecimal.valueOf(1000 * 1000));
				bwd = bwd.multiply(BigDecimal.valueOf(1000));	

				if (bwd.compareTo(BigDecimal.ZERO) > 0.0) spentTime = totalVol.divide(bwd, 2, RoundingMode.HALF_UP);
			}

			long ending = currentTS + spentTime.longValue();
			endingMap.put(kname+"-"+key+"-"+key2,(new Long(ending)));
		}

		// --------------------------------------------------------------------
		// Reduce bandwidths after Transmission
		// --------------------------------------------------------------------

		// Sort ending times
		String[] sortedList = new String[hostList.size() * hostList.size() * 2];
		int count = 0;
		for(int i = 0; i < (hostList.size() * hostList.size() * 2); i++) sortedList[i] = "";
		for (String key: endingMap.keySet())
		{
			long value = endingMap.get(key).longValue();
			sortedList[count] = key;
			
			for (int i = count; i > 0; i--)
			{
				long valaux1 = endingMap.get(sortedList[i]).longValue();
				long valaux2 = endingMap.get(sortedList[i-1]).longValue();
				if (valaux1 < valaux2)
				{
					String aux = sortedList[i];
					sortedList[i] = sortedList[i-1];
					sortedList[i-1] = aux;
				}
			}
			count = count + 1;
		}

		// Reduce BWD to minimal conditions
		for (int i = 0; i < count; i++ )
		{
			String[] aux = sortedList[i].split("-");
			String kname = aux[0];
			String orig = aux[1];
			String dest = aux[2];
			
			if (orig.compareTo(dest) == 0) continue;
			
			long simTS = endingMap.get(sortedList[i]).longValue();
			if (simTS > nextSchedTS) currentTS = nextSchedTS;
			else currentTS = simTS;
			boolean exists = connections.containsKey("VM-"+orig+"-"+dest);
			if (exists)
			{
				int bwd = 1; // FIXME - Reduced only to 1Gbps... must adjust to common traffic
				boolean done = negotiateBWD("VM",orig,dest,bwd,true);
				//System.out.println("Transmission Finished at " + simTS + " for " + aux[0] + " -> " + aux[1] + "  Reduce to " + bwd + " Gbps");
			}
		}
	}
	
	private void policyCC (Map <String,Integer> aggregatedVMTotal, Map <String,Integer> movementNumVM, Map <String,Integer> aggregatedTrafficTotal, Map <String,Integer> movementNumDB)
	{
		// Calculate BWD and negotiate each connection for VM-S
		for(String bwdkey : aggregatedVMTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			String key = aux[0];
			String key2 = aux[1];
			
			// Calculate estimated BWD
			int move = aggregatedVMTotal.get(bwdkey);
			BigInteger totalVol = BigInteger.valueOf(move);
			totalVol = totalVol.multiply(volumeData);	

			System.out.println("VMS> Policy CC: " + key+"-"+key2 + " " + move + " " + aggregatedVMTotal.get(bwdkey) + " " + totalVol);

			// Negotiate VM Sending
			if (move > 0)
			{
				boolean done = negotiateTime("VM",key,key2,totalVol,maxTimeRound);
			}
		}

		// Calculate BWD and negotiate each connection for VM-C
		for(String bwdkey : aggregatedTrafficTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			String key = aux[0];
			String key2 = aux[1];
			
			// Calculate estimated BWD
			int move = aggregatedTrafficTotal.get(bwdkey);
			BigInteger totalVol = BigInteger.valueOf(move);
			totalVol = totalVol.multiply(volumeData);	

			// Negotiate Data Sending
			if (move > 0)
			{
				System.out.println("VMC> Policy CC: " + key+"-"+key2 + " " + move + " " + aggregatedTrafficTotal.get(bwdkey) + " " + totalVol);
				boolean done = negotiateTime("DB",key,key2,totalVol,maxTimeRound);
			}
		}

		Map <String,Double> timedVMs = new HashMap<String,Double>();	// <Origin-Dest,vms>
		Map <String,Double> timedVMc = new HashMap<String,Double>();	// <Origin-Dest,dbs>
		for (String key : aggregatedVMTotal.keySet()) {	timedVMs.put(key,0.0); }
		for (String key : aggregatedTrafficTotal.keySet()) { timedVMc.put(key,0.0); }

		// Start listener for Notify
		NotifyListener ntfl = new NotifyListener();
  		ntfl.start();

		// Loop for Time Steps
		int count = timedConns.size();
		while (count > 0 && currentTS < nextSchedTS)
		{
			// How much VM-S have been transmited in this time
			for (String key: aggregatedVMTotal.keySet())
			{
				String[] aux = key.split("-");
				double auxbwd = retrieveTCBWD("VM",aux[0],aux[1]);

				if (auxbwd != -1.0)
				{
					double gbpstx = auxbwd * 60; // Step Time 60 seconds
					BigDecimal vmaux = new BigDecimal(volumeVMs);
					vmaux = vmaux.divide(BigDecimal.valueOf(1000 * 1000));
					vmaux = vmaux.divide(BigDecimal.valueOf(1000));
					double vmsmoved = gbpstx / vmaux.doubleValue();

					double prevmoved = timedVMs.get(key);
					timedVMs.put(key,prevmoved+vmsmoved);
				}
			}

			// How much VM-C have been transmited in this time
			for (String key: aggregatedTrafficTotal.keySet())
			{
				String[] aux = key.split("-");
				double auxbwd = retrieveTCBWD("DB",aux[0],aux[1]);

				if (auxbwd != -1.0)
				{
					double gbpstx = auxbwd * 60; // Step Time 60 seconds
					BigDecimal dbaux = new BigDecimal(volumeData);
					dbaux = dbaux.divide(BigDecimal.valueOf(1000 * 1000));
					dbaux = dbaux.divide(BigDecimal.valueOf(1000));
					double bdsmoved = gbpstx / dbaux.doubleValue();

					double prevmoved = timedVMc.get(key);
					timedVMc.put(key,prevmoved+bdsmoved);
				}
			}

			// Adavance to Next Step, Blocked until receiving notification of Advance Time
			//System.out.println("* Waiting for Responses: " + count);
			currentTS = currentTS + 60; // Step Time 60 seconds
			advanceTime(currentTS);

			count = timedConns.size();
		}
		try	{ Thread.currentThread().sleep(10); } // Just in Case...
		catch(InterruptedException ie) { System.err.println("ERROR: " + ie); ie.printStackTrace(); }

		synchronized (lock) 
		{
			try
			{
				String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Notify type=\"0\"><timestamp>" + currentTS + "</timestamp></Notify>\0";
				msgToMe(request);
			}
			catch (Exception e) { System.err.println("EXCEPTION: " + e); e.printStackTrace(); }
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

			if (ratio < 1.0) move = (new Double(move*ratio)).intValue(); // Some VMs are behind
			vmInDC.put(orig,vmInDC.get(orig).intValue()-move);
			vmInDC.put(dest,vmInDC.get(dest).intValue()+move);
		}

		// How much VM-C data have been moved sucessfully
		for (String key: movementNumDB.keySet())
		{
			String[] aux = key.split("-");
			int move = movementNumDB.get(key);
			String orig = aux[0];
			String dest = aux[1];

			int toMigPath = aggregatedTrafficTotal.get(orig+"-"+dest);
			double migrPath = timedVMc.get(orig+"-"+dest).doubleValue();
			double ratio = migrPath / (new Double(toMigPath)).doubleValue();

			if (ratio < 1.0)
			{
				move = (new Double(move*ratio)).intValue();
				// TODO - Some data was left behind
			}
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
		if (move < 1) return 1;

		if (keyname.compareTo("DB") != 0)
		{
			System.out.println("Migrate VMs/Data " + keyname + " from " + origin + " to " + destination);
			System.out.println("* Num VMs/Data: " + move + " ( " + volume + " bits per VM )");
		}

		// Calculate MaxTime in total... and per VM
		BigInteger reqBWD = BigInteger.valueOf(move);
		reqBWD = reqBWD.multiply(volume);
		reqBWD = reqBWD.divide(BigInteger.valueOf(maxTimeRound));

		BigInteger reqGbps = reqBWD.divide(BigInteger.valueOf(1000*1000));
		reqGbps = reqGbps.divide(BigInteger.valueOf(1000));

		BigDecimal VMps = new BigDecimal(reqBWD);
		VMps = VMps.divide(new BigDecimal(volumeVMs));
		//System.out.println("* MaxTime: " + maxTime + " -> Required BWD: " + reqBWD + " bps ( " + reqGbps + " Gbps )");

		return Math.max(reqGbps.intValue(),1);
	}
	
	private boolean negotiateBWD (String keyname, String origin, String destination, double bwd, boolean fixed)
	{
		boolean retval = false;
		if (bwd < 1) bwd = 1;

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
			System.out.println("* Current Negotiation " + origin + " -> " + destination + " " + testBWD + " Gbps");
			double resaux = 0.0;
			if (!exists) resaux = openConn(origin,destination,testBWD);
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
				if (testBWD < 1.0) keepGoing = false;	// Game Over, we keep old bwd
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
			long resaux = timedConn(keyname,origin,destination,newVolume,time);
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
	private double openConn(String origin, String destination, double bwd)
	{
		long unixTime = currentTS;
		String ip1 = DNSMap.get(origin);
		String ip2 = DNSMap.get(destination);

		double retval = 0.0;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"1\"><sourceip>" + ip1 + "</sourceip><destinationip>" + ip2 + "</destinationip><requestedbw>" + bwd + "</requestedbw><timestamp>" + unixTime + "</timestamp></Request>";
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
	private long timedConn(String keyname, String origin, String destination, BigInteger volume, long time)
	{
		long unixTime = currentTS;
		String ip1 = DNSMap.get(origin);
		String ip2 = DNSMap.get(destination);

		long retval = 1;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"4\"><sourceip>" + ip1 + "</sourceip><destinationip>" + ip2 + "</destinationip><requestedTransfer>"+volume+"</requestedTransfer><completion>"+time+"</completion><timestamp>" + unixTime + "</timestamp></Request>";
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

			System.out.println("ERROR de TIMEDCONN: " + reply);

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

			System.out.println("* Received message: " + retval);
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
						}
						else if (reply.contains("<Notify type=\"2\">"))
						{
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
