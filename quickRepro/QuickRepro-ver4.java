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

class QuickRepro
{
	public static void main(String args[])
	{
		String a = (args.length > 0)?args[0]:"localhost";
		int t = (args.length > 1)?(new Integer(args[1])).intValue():0;
		boolean c = (args.length > 2)?(args[2].compareTo("coop")==0):false;
		QuickRepro qr = new QuickRepro(a,t,c);
		qr.run();
	}

	private Map <Integer,Map> 	ListOfVMs;
	private Map <String,String> DNSMap;
 	private Map <Integer,Map> 	vmDistributionS;
 	private Map <Integer,Map> 	vmDistributionD;
 	private Map <Integer,Double> vmAvgDown;
 	
	private int numDCs = 0;
	private int numIter = 0;
	private long currentTS;

	private int numVMs;
	private BigInteger volumeVMs;
	private BigInteger volumeData;
	private int maxTimeVM;
	private int maxTimeRound;
	private int maxReqsVM;

	private String address = "localhost";
	private int port;
	private int portlistener;
	private boolean coop = false;	// True -> Coop; False -> Master/Slave
	private int typeExps = 0;		// 0 -> VMs; 1 -> DBs; 2 -> All

	public QuickRepro (String addr, int typeExps, boolean coop)
	{
		ListOfVMs = new HashMap<Integer,Map>();
		DNSMap = new HashMap<String,String>();
		vmDistributionS = new HashMap<Integer,Map>();
		vmDistributionD = new HashMap<Integer,Map>();
		vmAvgDown = new HashMap<Integer,Double>();

		// Info VMs - Configuration
		numVMs = 5000;
		
		volumeVMs = BigInteger.valueOf(5);			// BITS per VM FIXME - Default 5GB/VM
		volumeVMs = volumeVMs.multiply(BigInteger.valueOf(1000*1000));
		volumeVMs = volumeVMs.multiply(BigInteger.valueOf(1000*8));
		maxTimeVM = 120;							// in Seconds
		maxTimeRound = 3600;						// in Seconds TODO - Trade-off between foreign-time penalty and cost of bwd...
		maxReqsVM = 8000;							// in Reqs/hour per VM FIXME - Posar-ho en usuaris/VM i fer reqs/usuari x hora

		volumeData = BigInteger.valueOf(5);			// BITS per Generated VM Data FIXME - Default 1GB/VM
		volumeData = volumeData.multiply(BigInteger.valueOf(1000*1000));
		volumeData = volumeData.multiply(BigInteger.valueOf(1000*8));

		// Simulator Address
		address = addr; // "idealist.pc.ac.upc.edu";
		port = 5555;
		portlistener = 5550;

		this.typeExps = typeExps;
		this.coop = coop;
	}

	public void run()
	{
		try
		{
			FileInputStream fstream = new FileInputStream("qr-scheduler.log");
			DataInputStream in = new DataInputStream(fstream);
			BufferedReader br = new BufferedReader(new InputStreamReader(in));

			String strLine;
			while ((strLine = br.readLine()) != null)
			{
				if (strLine.contains("load from it:"))
				{
					String[] aux = strLine.split("\\s+");

					Map <String,Integer> LoadLocation;
					if (ListOfVMs.containsKey(new Integer(aux[2])))	LoadLocation = ListOfVMs.get(new Integer(aux[2]));
					else LoadLocation = new HashMap<String,Integer>();

					LoadLocation.put(aux[4],(new Integer((aux[10].split("\\."))[0])));
					ListOfVMs.put(new Integer(aux[2]),LoadLocation);
				}
				else if (strLine.contains("Iteration:"))
				{
					// Schedule Previous
					String[] aux = strLine.split("\\s+");
					DateFormat dfm = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
					dfm.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
					currentTS = dfm.parse(aux[3] + " " + aux[4]).getTime() / 1000;

					if (numIter == 0)
					{
						// Open Minimal Connections
						for (Integer vmkey : ListOfVMs.keySet())
						{
							SortedSet<String> keys = new TreeSet<String>(DNSMap.keySet());
							for (String key : keys)
							{
								for (String key2 : keys)
								{
									if (key2.compareTo(key) == 0) continue;
									System.out.println("Minimal Conn: " + vmkey + " " + key + "-" + key2);

									int bwd = 1; // FIXME - Open only to 1Gbps... must adjust to common traffic
									boolean done = negotiateBWD(vmkey.toString(),key,key2,bwd,true);
								}
							}
						}
					}
					else
					{
						if (typeExps == 0 || typeExps == 2) scheduleDirect();
						if (typeExps == 1 || typeExps == 2) scheduleLoadBalance();
						
					}

					// Start New Iteration
					numIter = (new Integer(aux[2])).intValue();
					ListOfVMs.clear();
				}
				else if (strLine.contains("Found ISP for"))
				{
					String[] aux = strLine.split("\\s+");
					System.out.println("Location: " + aux[4] + " with IP " + aux[7]);

					numDCs++;
					DNSMap.put(aux[4],aux[7]);
				}
				else if (strLine.contains("Built proxy for"))
				{
					String[] aux = strLine.split("\\s+");
					ListOfVMs.put(new Integer(aux[4]),new HashMap<String,Integer>());

					// Inicialization of Load VM Distribution (Singularity)
					Map <String,Integer> vmInDCS = new HashMap<String,Integer>();
					for (String dckey : DNSMap.keySet()) vmInDCS.put(dckey,0);
					vmInDCS.put("atom00",numVMs);
					vmDistributionS.put(new Integer(aux[4]),vmInDCS);

					// Inicialization of Load VM Distribution (Clonic VMs)
					Map <String,Integer> vmInDCD = new HashMap<String,Integer>();
					for (String dckey : DNSMap.keySet()) vmInDCD.put(dckey,0);
					vmInDCD.put("atom00",numVMs);
					vmDistributionD.put(new Integer(aux[4]),vmInDCD);
				}
				else if (strLine.contains("BPR:"))
				{
					String[] aux = strLine.split("\\s+");
					vmAvgDown.put(new Integer(aux[3]),new Double(aux[30].substring(0, aux[30].length()-1)));
				}
			}
			in.close();

			// Closing Time
			for (String key : connections.keySet())
			{
				String[] saux = key.split("-");
				int id = connections.get(saux[0]+"-"+saux[1]+"-"+saux[2]);
				boolean baux = closeConn (id);
			}
		}
		catch (Exception e) { System.err.println("Error: " + e.getMessage()); e.printStackTrace(); }

		String reply = udpSender ("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"999\"></Request>");
	}

	private Map <String,Integer> connections = new HashMap<String,Integer>();
	private Map <String,Double> availableBWD = new HashMap<String,Double>();

	/**************************************************************************/
	/* Direct VM Management (for Singular VMs)                                */
	/**************************************************************************/
	
	// Política actual : Demanar migrar les VMs en 1/2 del scheduling round
	//                   No es negocia BWD a l'alça. Es demana el bwd fixat i prou

	private void scheduleDirect ()
	{
		System.out.println("Iteration: " + numIter);

		Map <String,Integer> aggregatedVMTotal = new HashMap<String,Integer>();
		Map <String,Integer> movementNumVM = new HashMap<String,Integer>();
		Map <String,Integer> requiredPathBWD = new HashMap<String,Integer>();

		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);

			// Print Values of Load
			for (String key : keys)
			{ 
				int value = LoadLocation.get(key).intValue();
				System.out.println("SVM> VMs: " + vmkey + " Load from DC: " + key + " -> " + value);
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

		if (coop) vmPolicyCC(aggregatedVMTotal,movementNumVM,requiredPathBWD);
		else vmPolicyMS(aggregatedVMTotal,movementNumVM,requiredPathBWD);
	}

	private void vmPolicyMS (Map <String,Integer> aggregatedVMTotal, Map <String,Integer> movementNumVM, Map <String,Integer> requiredPathBWD)
	{
		// Perform BWD negotiations
		for (String key : aggregatedVMTotal.keySet())
		{
				// Get BWD per path
				int move = aggregatedVMTotal.get(key);
				String[] aux = key.split("-");
				int bwd = calculateBWD("VM",move,aux[0],aux[1],volumeVMs,(maxTimeRound/2));				
				requiredPathBWD.put(key,bwd);

				// Negotiate VM Sending
				boolean done = negotiateBWD("VM",aux[0],aux[1],bwd,true);
		}
		
		// sleep for 1000 ms -> should be time of transmission (for each!)
		try	{ Thread.currentThread().sleep(1000); }
		catch(InterruptedException ie) { System.err.println("ERROR: " + ie); ie.printStackTrace(); }

		// Resolve number of VMs moved or left behind
		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());

			Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);
			for (String key : requiredPathBWD.keySet())
			{
				String aux[] = key.split("-");
				double auxbwd = retrieveBWD("VM",aux[0],aux[1]);		// Available BWD
				double reqbwd = requiredPathBWD.get(key).doubleValue();	// Required BWD
				int move = movementNumVM.get(vmkey+"-"+key).intValue();	// VMs to move from key -> key2

				if (auxbwd >= reqbwd)
				{
					// All VMs reached destination
					vmInDC.put(aux[0],vmInDC.get(aux[0]).intValue()-move);
					vmInDC.put(aux[1],vmInDC.get(aux[1]).intValue()+move);
				}
				else
				{
					// Some VMs are behind
					double ratio = (reqbwd>0)?(auxbwd/reqbwd):0;
					move = (new Double(move*ratio)).intValue();
					vmInDC.put(aux[0],vmInDC.get(aux[0]).intValue()-move);
					vmInDC.put(aux[1],vmInDC.get(aux[1]).intValue()+move);
				}
			}
		}
	
		// Reduce BWD after data sending
		for (String key : aggregatedVMTotal.keySet())
		{
				// Get BWD per path
				int move = aggregatedVMTotal.get(key);
				String[] aux = key.split("-");

				// Negotiate VM Sending
				int bwd = 1; // FIXME - Reduced only to 1Gbps... must adjust to common traffic
				boolean done = negotiateBWD("VM",aux[0],aux[1],bwd,true);
		}
	}

	private Map <String,String> timedVMConns = new HashMap<String,String>();	// <Origin-Dest,id-bwd>
	private Map <Integer,Double> timedVMIDBWD = new HashMap<Integer,Double>();	// <id,bwd>

	private void vmPolicyCC (Map <String,Integer> aggregatedVMTotal, Map <String,Integer> movementNumVM, Map <String,Integer> requiredPathBWD)
	{
		// With aggregated traffic, calculate BWD and negotiate each connection
		for(String bwdkey : aggregatedVMTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			String key = aux[0];
			String key2 = aux[1];
			
			// Calculate estimated BWD
			int move = aggregatedVMTotal.get(key+"-"+key2);
			BigInteger totalVol = BigInteger.valueOf(move);
			totalVol = totalVol.multiply(volumeData);	

			System.out.println("SVM> Policy CC: " + key+"-"+key2 + " " + move + " " + aggregatedVMTotal.get(key+"-"+key2) + " " + totalVol);

			// Negotiate VM Sending
			if (move > 0)
			{
				boolean done = negotiateTime(key,key2,totalVol,maxTimeRound,timedVMConns,timedVMIDBWD);
			}
		}
		String confirm = udpSender ("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Notify type=\"0\"><timestamp>" + currentTS + "</timestamp></Notify>");

		// Wait for Notifications per connection
		int count = timedVMConns.size();
		System.out.println("Waiting for Responses: " + count);
		while (count > 0)
		{
			String reply = udpListener();
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
				for (Integer key : timedVMIDBWD.keySet())
				{
					if (key.intValue() == id)
					{
						double bwdaux = timedVMIDBWD.get(key);
						for (String key2 : timedVMConns.keySet())
						{
							if (timedVMConns.get(key2).compareTo(id+"-"+bwdaux) == 0) aux = key2;
						}
					}
				}	
				String[] aux2 = aux.split("-");

				// Resolve number of VMs moved (No one should left behind except all are)
				for (Integer vmkey : ListOfVMs.keySet())
				{
					Map <String,Integer> vmInDC = vmDistributionS.get(vmkey);

					int move = movementNumVM.get(vmkey+"-"+aux2[0]+"-"+aux2[1]).intValue();

					// All VMs reached destination
					vmInDC.put(aux2[0],vmInDC.get(aux2[0]).intValue()-move);
					vmInDC.put(aux2[1],vmInDC.get(aux2[1]).intValue()+move);
				}

				// Mata la connexió tipus 4
				boolean baux = closeTimeConn(aux2[0],aux2[1],timedVMConns);

				count = count - 1;

				System.out.println("Transmission Finished at " + currentTS + " for " + aux2[1] + " -> " + aux2[2]);
				System.out.println("Waiting for Responses: " + count);
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
				for (Integer key : timedVMIDBWD.keySet())
				{
					if (key.intValue() == id)
					{
						double bwdaux = timedVMIDBWD.get(key);
						for (String key2 : timedVMConns.keySet())
						{
							if (timedVMConns.get(key2).compareTo(id+"-"+bwdaux) == 0) aux = key2;
						}
					}
				}				
				availableBWD.put(aux,new Double(bwd));
				timedVMConns.put(aux,id+"-"+bwd);
				timedVMIDBWD.put(id,bwd);
			}
			else
			{
				System.out.println("* Error on receiving UDP: " + reply);
			}

			timedVMConns.clear();
			timedVMIDBWD.clear();
		}
	}
/*----------------------------------------------------------------------------*/

	/**************************************************************************/
	/* Load balance (DB Traffic for Clonic VMs)                               */
	/**************************************************************************/

	private void scheduleLoadBalance ()
	{
		System.out.println("Iteration: " + numIter + " " + currentTS);

		Map <String,Integer> aggregatedTrafficTotal = new HashMap<String,Integer>();
		List<String> hostList = new ArrayList<String>();
		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistributionD.get(vmkey);
			
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
				if (!hostList.contains(key)) hostList.add(key);
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

			// Schedule VMs - And here goes the HARD SCHEDULING PROBLEM!...
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

		if (coop) policyCC (aggregatedTrafficTotal);
		else policyMS (aggregatedTrafficTotal,hostList);
	}

	private void policyMS (Map <String,Integer> aggregatedTrafficTotal, List<String> hostList)
	{
		// With aggregated traffic, calculate BWD and negotiate each connection
		for(String bwdkey : aggregatedTrafficTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			String key = aux[0];
			String key2 = aux[1];
			
			// Calculate estimated BWD
			int move = aggregatedTrafficTotal.get(key+"-"+key2);
			int bwd = calculateBWD(move,key,key2,volumeData,maxTimeRound);
		
			// Negotiate Data Sending
			boolean done = negotiateBWD(key,key2,bwd);
		}
		
		// Sleep for 1000 ms -> should be time of transmission (for each!)
		try	{ Thread.currentThread().sleep(1000); }
		catch(InterruptedException ie) { System.err.println("ERROR: " + ie); ie.printStackTrace(); }
				
		Map<String,Long> endingMap = new HashMap<String,Long>();
		for(String bwdkey : availableBWD.keySet())
		{
			String[] aux = bwdkey.split("-");
			String key = aux[1];
			String key2 = aux[2];
			
			BigDecimal bigvolume = new BigDecimal(volumeData);													// bits/VM
			BigDecimal totalVol =  BigDecimal.valueOf(aggregatedTrafficTotal.get(key+"-"+key2).doubleValue());	// VMs/path
			totalVol = totalVol.multiply(bigvolume);															// bits/path

			BigDecimal bwd = BigDecimal.valueOf(availableBWD.get("DB-"+key+"-"+key2).doubleValue());			// Gbps/path
			bwd = bwd.multiply(BigDecimal.valueOf(1000 * 1000));
			bwd = bwd.multiply(BigDecimal.valueOf(1000));
							
			BigDecimal spentTime = BigDecimal.ZERO;
			if (bwd.compareTo(BigDecimal.ZERO) > 0.0) spentTime = totalVol.divide(bwd, 2, RoundingMode.HALF_UP);

			long ending = currentTS + spentTime.longValue();
			endingMap.put(key+"-"+key2,(new Long(ending)));
			
			//System.out.println("== new ending time: " + ending + " (" + key + "->" + key2 + ") " + connections.get(key + "-" + key2) + " " + spentTime.longValue() + " " + bwd + " " + totalVol);
		}

		// Insertion Sort (wa yeah!)
		String[] sortedList = new String[hostList.size() * hostList.size()];
		int count = 0;
		for(int i = 0; i < (hostList.size() * hostList.size()); i++) sortedList[i] = "";
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
			
		for (int i = 0; i < count; i++ )
		{
			String[] aux = sortedList[i].split("-");
			if (aux[0].compareTo(aux[1]) == 0) continue;
			
			long simTS = endingMap.get(sortedList[i]).longValue();
			currentTS = simTS;
			boolean exists = connections.containsKey("DB-"+aux[0]+"-"+aux[1]);
			if (exists)
			{
				int bwd = 1; // FIXME - Reduced only to 1Gbps... must adjust to common traffic
				boolean done = negotiateBWD(aux[0],aux[1],bwd,true);
				//System.out.println("Transmission Finished at " + simTS + " for " + aux[0] + " -> " + aux[1] + "  Reduce to " + bwd + " Gbps");
			}
		}
	}

	private Map <String,String> timedConns = new HashMap<String,String>();		// <Origin-Dest,id-bwd>
	private Map <Integer,Double> timedIDBWD = new HashMap<Integer,Double>();	// <id,bwd>

	private void policyCC (Map <String,Integer> aggregatedTrafficTotal)
	{
		// With aggregated traffic, calculate BWD and negotiate each connection
		for(String bwdkey : aggregatedTrafficTotal.keySet())
		{
			String[] aux = bwdkey.split("-");
			String key = aux[0];
			String key2 = aux[1];
			
			// Calculate estimated BWD
			int move = aggregatedTrafficTotal.get(key+"-"+key2);
			BigInteger totalVol = BigInteger.valueOf(move);
			totalVol = totalVol.multiply(volumeData);	

			System.out.println("Policy CC: " + key+"-"+key2 + " " + move + " " + aggregatedTrafficTotal.get(key+"-"+key2) + " " + totalVol);

			// Negotiate Data Sending
			if (move > 0)
			{
				boolean done = negotiateTime(key,key2,totalVol,maxTimeRound,timedConns,timedIDBWD);
			}
		}
		String confirm = udpSender ("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Notify type=\"0\"><timestamp>" + currentTS + "</timestamp></Notify>");

		int count = timedConns.size();
		System.out.println("Waiting for Responses: " + count);
		while (count > 0)
		{
			String reply = udpListener();
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
				for (Integer key : timedIDBWD.keySet())
				{
					if (key.intValue() == id)
					{
						double bwdaux = timedIDBWD.get(key);
						for (String key2 : timedConns.keySet())
						{
							if (timedConns.get(key2).compareTo(id+"-"+bwdaux) == 0) aux = key2;
						}
					}
				}	
				String[] aux2 = aux.split("-");

				// Mata la connexió tipus 4
				boolean baux = closeTimeConn(aux2[0],aux2[1],timedConns);

				count = count - 1;

				System.out.println("Transmission Finished at " + currentTS + " for " + aux2[1] + " -> " + aux2[2]);
				System.out.println("Waiting for Responses: " + count);
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
				for (Integer key : timedIDBWD.keySet())
				{
					if (key.intValue() == id)
					{
						double bwdaux = timedIDBWD.get(key);
						for (String key2 : timedConns.keySet())
						{
							if (timedConns.get(key2).compareTo(id+"-"+bwdaux) == 0) aux = key2;
						}
					}
				}				
				availableBWD.put(aux,new Double(bwd));
				timedConns.put(aux,id+"-"+bwd);
				timedIDBWD.put(id,bwd);
			}
			else
			{
				System.out.println("* Error on receiving UDP: " + reply);
			}

			timedConns.clear();
			timedIDBWD.clear();
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

	private double retrieveBWD (String keyname, String origin, String destination)
	{
		if (!availableBWD.containsKey(keyname+"-"+origin+"-"+destination)) return -1.0;
		return availableBWD.get(keyname+"-"+origin+"-"+destination);
	}

	private int calculateBWD (int move, String origin, String destination, BigInteger volume, int maxTime)
	{
		return calculateBWD("DB",move,origin,destination,volume,maxTime);
	}

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

	private boolean negotiateBWD (String origin, String destination, double bwd)
	{
		return negotiateBWD("DB",origin,destination,bwd,false);
	}

	private boolean negotiateBWD (String origin, String destination, double bwd, boolean fixed)
	{
		return negotiateBWD("DB",origin,destination,bwd,fixed);
	}

	private boolean negotiateBWD (String keyname, String origin, String destination, double bwd)
	{
		return negotiateBWD(keyname,origin,destination,bwd,false);
	}

	private boolean negotiateBWD (String keyname, String origin, String destination, double bwd, boolean fixed)
	{
		boolean retval = false;
		if (bwd < 1) bwd = 1;

		boolean exists = connections.containsKey(keyname+"-"+origin+"-"+destination);
		double testBWD = (fixed)?bwd:200;//Double.MAX_VALUE;
		if (exists)
		{
			if (bwd < availableBWD.get(keyname+"-"+origin+"-"+destination)) testBWD = bwd;	// We want to reduce, not negotiate above bwd.
			if (bwd == availableBWD.get(keyname+"-"+origin+"-"+destination))
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
				availableBWD.put(keyname+"-"+origin+"-"+destination,testBWD);
				keepGoing = false;
				done = true;
				retval = true;
				System.out.println("* Negotiation Success " + origin + " -> " + destination + " " + availableBWD.get(keyname+"-"+origin+"-"+destination) + " Gbps"); 
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
				availableBWD.put(keyname+"-"+origin+"-"+destination,0.0);
			}
		}

		return retval;
	}

	private boolean negotiateTime (String origin, String destination, BigInteger volume, long time, Map <String,String> tConns, Map <Integer,Double> tIDBWD)
	{
		boolean retval = false;
		boolean done = false;

		System.out.println("* Negotiate connection " + origin + " -> " + destination + " " + volume + " Gbits in " + time + " seconds");

		double resaux = timedConn(origin,destination,volume,time,tConns,tIDBWD);
		if (resaux > 0)
		{
			done = true;
			retval = true;
			String aux = tConns.get(origin+"-"+destination);
			String[] aux2 = aux.split("-");
			System.out.println("* Negotiation Success " + origin + " -> " + destination + " " + aux2[0] + " id " + aux2[1] + " Gbps");
		}		
		else if (resaux <= 0)
		{
			double newTime = -1 * resaux;					// Proposed new Time -> Game Over
			//System.out.println("* Proposed new BWD " + origin + " -> " + destination + " " + testBWD + " Gbps");
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


	// Return: 1 if OK;  <= 0 with new time * -1
	private double timedConn(String origin, String destination, BigInteger volume, long time, Map <String,String> tConns, Map <Integer,Double> tIDBWD)
	{
		long unixTime = currentTS;
		String ip1 = DNSMap.get(origin);
		String ip2 = DNSMap.get(destination);

		double retval = 1.0;
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

			tConns.put(origin+"-"+destination,id+"-"+bwd);
			tIDBWD.put(new Integer(id),new Double(bwd));
			retval = 1.0;	// All OK
		}
		else if (reply.contains("<status>KO</status>"))
		{
			retval = -1;
		}
		return retval;
	}

	private boolean closeTimeConn (String origin, String destination, Map <String,String> tConns)
	{
		String aux = tConns.get(origin+"-"+destination);
		int id = (new Integer(aux.split("-")[0])).intValue();
		long unixTime = currentTS;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"3\"><id>" + id + "</id><timestamp>" + unixTime + "</timestamp></Request>";

		boolean retval = false;
		String reply = sendMessage (request);
		if (reply.contains("<status>OK</status>")) retval = true;
		
		return retval;
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

		try
		{
			Matcher matcher = msgTimestamp.matcher(retval);
			matcher.find();
			currentTS = (new Long(matcher.group(1))).longValue();
		}
		catch (Exception e) { /*System.err.println("Error in getting Timestamp"); /*e.printStackTrace();*/ }
		
		return retval;
	}

	private String udpListener()
	{
		String retval = "";
		try
		{
			DatagramSocket clientSocket = new DatagramSocket(portlistener);

			byte[] buffer = new byte[2048];
			DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
			clientSocket.receive(packet);
			retval = new String(buffer, 0, packet.getLength());

			System.out.println("* Received message: " + retval);
		}
		catch (Exception e) { retval = "<status>KO</status>"; e.printStackTrace(); }

		try
		{
			Matcher matcher = msgTimestamp.matcher(retval);
			matcher.find();
			currentTS = (new Long(matcher.group(1))).longValue();
		}
		catch (Exception e) { /*System.err.println("Error in getting Timestamp"); /*e.printStackTrace();*/ }
		
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
}


