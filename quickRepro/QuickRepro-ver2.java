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
		QuickRepro qr = new QuickRepro();
		qr.run();
	}

	private Map <Integer,Map> ListOfVMs;
	private Map <String,String> DNSMap;
 	private Map <Integer,Map> vmDistribution;
 	private Map <Integer,String> vmLocation;
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

	public QuickRepro ()
	{
		ListOfVMs = new HashMap<Integer,Map>();
		DNSMap = new HashMap<String,String>();
		vmDistribution = new HashMap<Integer,Map>();	// Load Balance
		vmLocation = new HashMap<Integer,String>();		// Direct
		vmAvgDown = new HashMap<Integer,Double>();

		// Info VMs - Configuration
		numVMs = 5000;
		
		volumeVMs = BigInteger.valueOf(5);			// BITS per VM FIXME - Default 5GB/VM
		volumeVMs = volumeVMs.multiply(BigInteger.valueOf(1024*1024));
		volumeVMs = volumeVMs.multiply(BigInteger.valueOf(1024*8));
		maxTimeVM = 120;							// in Seconds
		maxTimeRound = 3600;						// in Seconds TODO - Trade-off between foreign-time penalty and cost of bwd...
		maxReqsVM = 8000;							// in Reqs/hour per VM FIXME - Posar-ho en usuaris/VM i fer reqs/usuari x hora

		volumeData = BigInteger.valueOf(5);			// BITS per Generated VM Data FIXME - Default 1GB/VM
		volumeData = volumeData.multiply(BigInteger.valueOf(1024*1024));
		volumeData = volumeData.multiply(BigInteger.valueOf(1024*8));
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

					System.out.println("TSCHANGE> " + currentTS);

					if (numIter == 0)
					{
						// Open Minimal Connections
						for (Integer vmkey : ListOfVMs.keySet())
						{
							SortedSet<String> keys = new TreeSet<String>(ListOfVMs.get(vmkey).keySet());
							for (String key : keys)
							{
								for (String key2 : keys)
								{
									if (key2.compareTo(key) == 0) continue;
									int bwd = 1; // FIXME - Open only to 1Gbps... must adjust to common traffic
									boolean done = negotiateBWD(key,key2,bwd);
								}
							}
						}
					}
					//else scheduleDirect();
					else scheduleLoadBalance();

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

					// Inicialization of Load VM Distribution
					Map <String,Integer> vmInDC = new HashMap<String,Integer>();
					for (String dckey : DNSMap.keySet()) vmInDC.put(dckey,0);
					vmInDC.put("atom00",numVMs);
					vmDistribution.put(new Integer(aux[4]),vmInDC);
					vmLocation.put(new Integer(aux[4]),"atom00");
				}
				else if (strLine.contains("BPR:"))
				{
					String[] aux = strLine.split("\\s+");
					vmAvgDown.put(new Integer(aux[3]),new Double(aux[30].substring(0, aux[30].length()-1)));
				}
			}
			in.close();
		}
		catch (Exception e) { System.err.println("Error: " + e.getMessage()); e.printStackTrace(); }

		for (String key : connections.keySet())
		{
			String[] saux = key.split("-");
			boolean baux = closeConn (saux[0], saux[1]);
		}
	}
/*
	private void scheduleDirect ()
	{
		System.out.println("Iteration: " + numIter);

		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistribution.get(vmkey);
			SortedSet<String> dckeys = new TreeSet<String>(vmInDC.keySet());

			// Print Values of Load
			for (String key : keys)
			{ 
				int value = LoadLocation.get(key).intValue();
				System.out.println("DC: " + key + " -> Load To " + vmkey + " From There: " + value);
			}
			
			// Move Load
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
			
			// Print New Distribution
			System.out.println("VM: " + vmkey + " -> Must move " + numVMs +  " VMs to " + bestHost + " (max load here: " + bestValue + ")");			
			
			String currentHost = vmLocation.get(vmkey);
			if (currentHost.compareTo(bestHost) != 0 && bestHost.compareTo("") != 0)
			{
				String origin = currentHost;
				String destination = bestHost;
				int move = numVMs;
				
				int bwd = calculateBWD(vmkey.intValue(),move,origin,destination,volumeVMs,(maxTimeRound/2));
				
				// Negotiate VM Sending
				boolean done = negotiateBWD(vmkey.intValue(),origin,destination,bwd);
				if (done)
				{
					vmInDC.put(origin,vmInDC.get(origin).intValue()-move);
					vmInDC.put(destination,vmInDC.get(destination).intValue()+move);
				}
			}
		}
		
		// sleep for 1000 ms -> should be time of transmission (for each!)
		try	{ Thread.currentThread().sleep(1000); }
		catch(InterruptedException ie) { System.err.println("ERROR: " + ie); ie.printStackTrace(); }
		
		// Reduce BWD after data sending
		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			
			for (String key : keys)
			{
				for (String key2 : keys)
				{
					if (key2.compareTo(key) == 0) continue;
					int bwd = 1; // FIXME - Reduced only to 1Gbps... must adjust to common traffic
					boolean done = negotiateBWD(vmkey.intValue(),key,key2,bwd);
				}
			}
		}

	}
*/

	/* Load Balancing amb Tot */

	private void scheduleLoadBalance ()
	{
		System.out.println("Iteration: " + numIter + " " + currentTS);

		Map <String,Integer> aggregatedTrafficTotal = new HashMap<String,Integer>();
		List<String> hostList = new ArrayList<String>();
		for (Integer vmkey : ListOfVMs.keySet())
		{
			Map <String,Integer> LoadLocation = ListOfVMs.get(vmkey);
			SortedSet<String> keys = new TreeSet<String>(LoadLocation.keySet());
			Map <String,Integer> vmInDC = vmDistribution.get(vmkey);
			SortedSet<String> dckeys = new TreeSet<String>(vmInDC.keySet());
			
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
			
			// Print Values of Load
			for (String key : keys)
			{ 
				for (String key2 : keys)
				{
					if (key2.compareTo(key) == 0) continue;
					int value = aggregatedTraffic.get(key+"-"+key2);
					//System.out.println("VM: " + vmkey + " -> Load From " + key + " To " + key2 + " : " + value + " VMs data");
				}
			}
		
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

			// Print Values of Load
			for (String key : keys)
			{ 
				int value = LoadLocation.get(key).intValue();
				//System.out.println("DC: " + key + " -> Load To " + vmkey + " From There: " + value + " reqs/h [ avg " + 512 + " bytes up/req " + vmAvgDown.get(vmkey) + " bytes down/req]");
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

			// Print New Distribution
			for (String key : keys)
			{ 
				int value1 = vmInDC.get(key).intValue();
				int value2 = newBalance.get(key).intValue();
				//System.out.println("VM: " + vmkey + " -> In DC " + key + " are " + value1 + " and should have " + value2);
			}

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
			String key = aux[0];
			String key2 = aux[1];
			
			BigDecimal bigvolume = new BigDecimal(volumeData);													// bits/VM
			BigDecimal totalVol =  BigDecimal.valueOf(aggregatedTrafficTotal.get(key+"-"+key2).doubleValue());	// VMs/path
			totalVol = totalVol.multiply(bigvolume);															// bits/path

			BigDecimal bwd = BigDecimal.valueOf(availableBWD.get(key+"-"+key2).doubleValue());					// Gbps/path
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
			boolean exists = connections.containsKey(aux[0]+"-"+aux[1]);
			if (exists)
			{
				int bwd = 1; // FIXME - Reduced only to 1Gbps... must adjust to common traffic
				boolean done = negotiateBWD(aux[0],aux[1],bwd);
				//System.out.println("Transmission Finished at " + simTS + " for " + aux[0] + " -> " + aux[1] + "  Reduce to " + bwd + " Gbps");
			}
		}
	}

	private int calculateBWD (int move, String origin, String destination, BigInteger volume, int maxTime)
	{
		//System.out.println("Migrate VMs/Data from " + origin + " to " + destination);
		//System.out.println("* Num VMs/Data: " + move + " ( " + volume + " bits per VM )");

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

	/* Coses del Simulador */
	Map <String,Integer> connections = new HashMap<String,Integer>();
	Map <String,Double> availableBWD = new HashMap<String,Double>();

	private final Pattern msgMaxBWD = Pattern.compile("<maxbw>(.+?)</maxbw>");
	private final Pattern msgConnID = Pattern.compile("<id>(.+?)</id>");

	// FIXME - Ara mateix es negocia un bwd igual o superior al que permeti teni-ho en 1/2 hora
	private boolean negotiateBWD (String origin, String destination, double bwd)
	{
		boolean retval = false;
		if (bwd < 1) bwd = 1;

		boolean exists = connections.containsKey(origin+"-"+destination);
		double testBWD = 200;//Double.MAX_VALUE;
		if (exists)
		{
			if (bwd < availableBWD.get(origin+"-"+destination)) testBWD = bwd;	// We want to reduce, not negotiate above bwd.
			if (bwd == availableBWD.get(origin+"-"+destination))
			{
				System.out.println("* No Negotiation Needed " + origin + " -> " + destination + " " + bwd + " Gbps"); 
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
			else  		 resaux = modifyConn(origin,destination,testBWD);
			
			if (resaux > 0)
			{
				if (!exists) connections.put(origin+"-"+destination,((new Double(resaux)).intValue()-1)); // FIXME - IDs i coses. Fer estructura de resposta
				availableBWD.put(origin+"-"+destination,testBWD);
				keepGoing = false;
				done = true;
				retval = true;
				System.out.println("* Negotiation Success " + origin + " -> " + destination + " " + availableBWD.get(origin+"-"+destination) + " Gbps"); 
			}		
			else if (resaux <= 0)
			{
				testBWD = -1 * resaux;					// Proposed new BWD
				if (testBWD < bwd) keepGoing = false;	// Game Over, we keep old bwd
				System.out.println("* Proposed new BWD " + origin + " -> " + destination + " " + testBWD + " Gbps");
			}
		}
		if (!done)
		{
			System.out.println("* Failed Negotiation " + origin + " -> " + destination + " " + bwd + " Gbps");
			if (!exists)
			{
				availableBWD.put(origin+"-"+destination,0.0);
			}
		}

		return retval;
	}

	// Return (ID+1) if OK, -1 * Proposed BWD if KO
	private double openConn(String origin, String destination, double bwd)
	{
		long unixTime = currentTS;
		String ip1 = DNSMap.get(origin);
		String ip2 = DNSMap.get(destination);

		double retval = 0.0;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"1\"><sourceip>" + ip1 + "</sourceip><destinationip>" + ip2 + "</destinationip><requestedbw>" + bwd + "</requestedbw><timestamp>" + unixTime + "</timestamp></Request>";
		String reply = sendMessage (request,origin,destination);
			
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

	private boolean closeConn (String origin, String destination)
	{
		int id = connections.get(origin+"-"+destination);
		long unixTime = currentTS;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"3\"><id>" + id + "</id><timestamp>" + unixTime + "</timestamp></Request>";

		boolean retval = false;
		String reply = sendMessage (request,origin,destination);
		if (reply.contains("<status>OK</status>")) retval = true;
		
		return retval;
	}

	// Return 1 if OK, -1 * Proposed BWD if KO
	private double modifyConn (String origin, String destination, double bwd)
	{
		int id = connections.get(origin+"-"+destination);
		long unixTime = currentTS;

		double retval = 0.0;
		String request = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><Request command=\"2\"><id>" + id + "</id><requestedbw>" + bwd + "</requestedbw><timestamp>" + unixTime + "</timestamp></Request>";
		String reply = sendMessage (request,origin,destination); // FIXME - Treure al definitiu

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

	private String sendMessage (String sentence,String a, String b)
	{
		/*String aux = a+"-"+b;
		int hash=7;
		for (int i=0; i < aux.length(); i++) {
		    hash = hash*31+aux.charAt(i);
		}
		return "<status>OK</status><id>" + hash + "</id>";*/

		String retval = "";
		try
		{
			DatagramSocket clientSocket = new DatagramSocket();
			InetAddress IPAddress = InetAddress.getByName("idealist.pc.ac.upc.edu");
			//InetAddress IPAddress = InetAddress.getByName("localhost");
			byte[] sendData = sentence.getBytes();

			//byte[] headerInt = ByteBuffer.allocate(4).putInt(sendData.length).array();
			byte[] headerInt = new byte[4];
			headerInt[3] = (byte)((sendData.length & 0x000000FF) >> 0*8);
			headerInt[2] = (byte)((sendData.length & 0x0000FF00) >> 1*8);
			headerInt[1] = (byte)((sendData.length & 0x00FF0000) >> 2*8);
			headerInt[0] = (byte)((sendData.length & 0xFF000000) >> 3*8);
			
			DatagramPacket sendInteger = new DatagramPacket(headerInt,headerInt.length,IPAddress,5555);
			clientSocket.send(sendInteger);
			DatagramPacket sendPacket = new DatagramPacket(sendData, sendData.length, IPAddress, 5555);
			clientSocket.send(sendPacket);
			
			System.out.println("* Sending message: [" + sendData.length + "]" + sentence);

			byte[] receiveData = new byte[1024];
			DatagramPacket receivePacket = new DatagramPacket(receiveData,receiveData.length,IPAddress,5555);
			clientSocket.receive(receivePacket);

			clientSocket.close();
			retval = new String(receivePacket.getData());
			
			System.out.println("* Received message: " + retval);
		}
		catch (Exception e) { retval = "<status>KO</status>"; e.printStackTrace(); }
		
		
		return retval;
	}
}


