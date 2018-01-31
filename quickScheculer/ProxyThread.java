
import java.net.*;
import java.io.*;
import java.util.*;
import java.util.Calendar;
import java.text.SimpleDateFormat;

public class ProxyThread extends Thread
{
	private java.util.Vector<StreamCopyThread> connections = new java.util.Vector<StreamCopyThread>();	// Holds all the currently active StreamCopyThreads
	protected Object lock = new Object();							// Used to synchronize the connection-handling threads with this thread
	private InetAddress dstAddr;									// The address to forward connections to
	private int dstPort;											// The port to forward connections to
	protected static final int backLog = 100;						// Backlog parameter used when creating the ServerSocket
	public static final int threadTimeout = 2000; //ms				// Timeout waiting for a StreamCopyThread to finish
	public static final int lingerTime = 180; // seconds (?) orig: 180 	// Linger time
	public static final int bufSize = 32768; // orig: 2048;			// Size of receive buffer
	private String header;											// Header to prepend to log messages
	private ServerSocket srvSock;									// This proxy's server socket
	private boolean debug = false;									// Debug flag

	private PrintStream out;										// Log streams for output and error messages
	private PrintStream err;
    
	//private static final String argsMessage="Arguments: ( [source_address] source_port dest_address dest_port ) | config_file";
	//private static final String propertyPrefix="proxy";

	// Log GateWay [qdec]
	protected int requests = 0;
	protected int queue = 0;
	protected int bytestotal = 0;
	protected long timetotal = 0;
	protected long tproctotal = 0;
	protected int remote = 0;
	protected int localr = 0;
	protected GatewayLogger gl;
	protected SimpleDateFormat sdf;
	protected Calendar cal;
	protected String ruleName = "";
	protected int vmid = 0;
	protected double slacount = 0;		// SLA com a Target (0,1)
	protected double slainteg = 0;		// SLA com a Target [0,1]

	protected int latency = 0;										// Delay latency in millis
	protected String currentISP = "0.0.0.0";
	protected String currentHost = "localhost";
	protected HashMap<String,Integer> mapRemote = new HashMap<String,Integer>();

	protected Map <String,Integer> latencyMap;	// This is <hostInfo.name+"-"+hostInfo.name,Value>
	protected Map <String,String> mapISP;		// This is <hostInfo.name,ISP_IP>

	public ProxyThread(InetAddress srcAddr,int srcPort,InetAddress dstAddr,int dstPort, PrintStream out, PrintStream err, String rname, int vmid, int latency, String isp, String currentHost) throws IOException
	{
		this.out = out;
		this.err = err;
		this.srvSock = (srcAddr==null) ? new ServerSocket(srcPort,backLog) : new ServerSocket(srcPort,backLog,srcAddr);
		this.dstAddr = dstAddr;
		this.dstPort = dstPort;
		this.header = (srcAddr==null ? "" : srcAddr.toString())+":"+srcPort+" <-> "+dstAddr+":"+dstPort;
		this.sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
		this.ruleName = rname;
		this.vmid = vmid;
		this.latency = latency;
		this.currentISP = isp;
		this.currentHost = currentHost;
		this.latencyMap = null;
		this.mapISP = null;
		start();
	}

	public ProxyThread(InetAddress srcAddr,int srcPort,InetAddress dstAddr,int dstPort, PrintStream out, PrintStream err, String rname, int vmid, int latency, String isp, String currentHost, Map <String,Integer> latencyMap, Map <String,String> mapISP) throws IOException
	{
		this.out = out;
		this.err = err;
		this.srvSock = (srcAddr==null) ? new ServerSocket(srcPort,backLog) : new ServerSocket(srcPort,backLog,srcAddr);
		this.dstAddr = dstAddr;
		this.dstPort = dstPort;
		this.header = (srcAddr==null ? "" : srcAddr.toString())+":"+srcPort+" <-> "+dstAddr+":"+dstPort;
		this.sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
		this.ruleName = rname;
		this.vmid = vmid;
		this.latency = latency;
		this.currentISP = isp;
		this.currentHost = currentHost;
		this.latencyMap = null;
		this.mapISP = null;
		this.latencyMap = latencyMap;
		this.mapISP = mapISP;
		start();
	}

	public void run()
	{
		gl = new GatewayLogger();
		if (out != null) out.println(header+" : starting");
		try
		{
			while(!isInterrupted())
			{
				Socket serverSocket = srvSock.accept();
				synchronized(lock) { requests++; queue++; }
				try
				{
					// Where Reqs come from...
					int latencyaux = 0;
					String incommingAddr = serverSocket.getInetAddress().toString().replaceAll("/","");
					if (currentISP.compareTo(incommingAddr) != 0) 
					{
						synchronized(lock) { remote++; }
						latencyaux = latency;

						if (latencyMap != null)
						{
							String originDC = "";
							String destinationDC = "";
							for (String key : mapISP.keySet())
							{
								if ((mapISP.get(key)).compareTo(incommingAddr) == 0) originDC = key;
							}
							if (originDC.compareTo("") != 0) latencyaux = latencyMap.get(originDC+"-"+currentHost).intValue();
						}
					}
					/*if (currentHost.compareTo("atom00") != 0)
					{
						synchronized(lock) { remote++; }
						latencyaux = latency;

						if (latencyMap != null)
						{
							latencyaux = latencyMap.get("atom00"+"-"+currentHost).intValue();
						}

					}*/
					else
					{
						synchronized(lock) { localr++; }
					}
					if (mapRemote.get(incommingAddr) == null) mapRemote.put(incommingAddr,1);
					else
					{
						int aux = (mapRemote.get(incommingAddr)).intValue();
						mapRemote.put(incommingAddr,(aux+1));
					}

					// Real stuff to be done...
					serverSocket.setSoLinger(true,lingerTime);
					Socket clientSocket = new Socket(dstAddr,dstPort);
					clientSocket.setSoLinger(true,lingerTime);
					StreamCopyThread sToC = new StreamCopyThread(serverSocket,clientSocket,true,latencyaux);
					StreamCopyThread cToS = new StreamCopyThread(clientSocket,serverSocket,false,latencyaux);
					sToC.setPeer(cToS);
					cToS.setPeer(sToC);
					synchronized(lock)
					{
						connections.addElement(cToS);
						connections.addElement(sToC);
						sToC.start();
						cToS.start();
					}
				}
				catch(ConnectException xc) { err.print("C"); }
				catch(NoRouteToHostException xc) { err.print("N"); }
				catch(Exception xc) { err.println("1 "+header+":"+xc); if(debug) xc.printStackTrace(); }
			}
			srvSock.close();
		}
		catch(IOException xc) { err.println("2 "+header+":"+xc); if(debug) xc.printStackTrace(); }
		finally
		{
			cleanup();
			if (out != null) out.println(header+" : stopped");
			try { Thread.currentThread().sleep(120000); }
            catch (Exception e) { err.println("Error at sleeping in Exception"); }
			start();
		}
	}

	public void cleanup()
	{
		synchronized(lock)
		{
			try
			{
				while(connections.size() > 0)
				{
					StreamCopyThread sct=(StreamCopyThread)connections.elementAt(0);
					sct.interrupt();
					sct.join(threadTimeout);
				}
			}
			catch(InterruptedException xc) { err.println("3 "+xc); }
		}
	}

	public void shutdown()
	{
		cleanup();
		gl.down();
		try
		{
			srvSock.close();
			this.interrupt();
		}
		catch (Exception e) { System.err.println("Warning at closing thread"); e.printStackTrace(); }
	}

	public String gatewayLastMeasure()
	{
		return gl.getLastMeasure();
	}

	public String gatewayAvgMeasure()
	{
		return gl.getAvgMeasure();
	}

	public int gatewaySleepTime()
	{
		return gl.getSleepTime();
	}

	public List<HashMap<String,Integer>> gatewaySources()
	{
		return gl.getSources();
	}

	public void cleanMeasures()
	{
		gl.cleanMeasures();
	}

	public void changeISP(String isp)
	{
		currentISP = isp;
	}

	public void changeHost(String newHost)
	{
		currentHost = newHost;
	}

	public static void main (String[] args)
	{
		if (args.length < 3) { System.err.println("Gateway> Error at MAIN"); System.exit(-1); }

		ProxyThread ptx;
		try
		{
			ptx = new ProxyThread(InetAddress.getByName(args[0]),(new Integer(args[1])).intValue(),InetAddress.getByName(args[2]),80,System.out,System.err,"PTX",(new Integer(args[3])).intValue(),0,"0.0.0.0","localhost");
			while(true)
			{
				Thread.currentThread().sleep(60000);
			}
		}
		catch (Exception e) { System.err.println("Gateway> Error at MAIN"); e.printStackTrace(); }
	}


	/**********************************************************************/
	/* GATEWAY LOGGING THREAD                                             */
	/**********************************************************************/

	protected class GatewayLogger extends Thread
	{
		private Calendar cal;
		private String lastMeasure = "";
		private boolean up = true;
		private int sleepTime = 0;	// In millis (Value - 15000 from "sar + collect")
		private int oldQueue = 0;

		public GatewayLogger()
		{
			start();
		}

		private List<String> listMeasuresGW = new ArrayList<String>();
		private List<HashMap<String,Integer>> listISPConnGW = new ArrayList<HashMap<String,Integer>>();
		private int maxMeasureTimegGW = 240; // Time Window for Average Measures (Each obs = 15 sec; e.g. 240 -> 60 min)

		public void run()
		{
			while(up)
			{
				String command = "ssh " + currentHost + " '/srv/one/cpurecord.sh one-" + vmid + " '";
				String [] output = new String[2];
				execCommand(new String[]{"/bin/bash", "-c", command}, output);

				int localreq = 0; int localbytes = 0; long localtime = 0;  int localqueue = 0; int localremote = 0; int locallocalr = 0; HashMap<String,Integer> localmapremote = null; long localtproc = 0; double localslacount = 0.0; double localslainteg = 0.0;
				synchronized(lock)
				{
					localreq = requests; localbytes = bytestotal; localtime = timetotal; localqueue = queue; localremote = remote; locallocalr = localr; localmapremote = mapRemote; localtproc = tproctotal; localslacount = slacount; localslainteg = slainteg;
					requests = 0; bytestotal = 0; timetotal = 0; remote = 0; localr = 0; mapRemote = new HashMap<String,Integer>(); tproctotal = 0; slacount = 0.0; slainteg = 0.0;
				}
				int served = (localreq + oldQueue) - localqueue;

				lastMeasure = ruleName + " " + localreq + " " + oldQueue + " " + served + " " + (localbytes/((served == 0)?1:served)) + " " + (localtime/((served == 0)?1:served)) + " " + (output[0].replaceAll("\n","")) + " " + localremote + " " + locallocalr + " " + (localtproc/((served == 0)?1:served)) + " " + (localslacount/((served == 0)?1:served)) + " " + (localslainteg/((served == 0)?1:served));
				if (out != null) out.print(lastMeasure+" " + latency + "\n");

				synchronized(lock)
				{
					if (localreq > 0 || served > 0 || localqueue > 0 || listMeasuresGW.size() == 0)
					{
						listMeasuresGW.add(lastMeasure.replace("\n",""));
						listISPConnGW.add(localmapremote);
					}
					if (listMeasuresGW.size() > maxMeasureTimegGW) { listMeasuresGW.remove(0); listISPConnGW.remove(0); }
				}

				// Clean Queue when degraded... [no reqs, no served, cpupm low...]
				//if (served == 0 && localreq == 0 && (new Double((output[0].replaceAll("\n","").split("\\s+"))[1])).doubleValue() < 50)
				//{
				//	synchronized(lock) { oldQueue = 0; queue = 0; localqueue = 0; }
				//}

				oldQueue = localqueue;

				if (sleepTime > 0)
				{
					try { Thread.currentThread().sleep(sleepTime); }
					catch (Exception e) { System.err.println("Gateway> Error at sleeping thread [R]"); e.printStackTrace(); }
				}
			}
		}

		public void down()
		{
			up = false;
		}

		public int getSleepTime()
		{
			return sleepTime + 15000;	// 15 segons d'observació
		}

		public String getLastMeasure()
		{
			return lastMeasure;
		}

		public List<HashMap<String,Integer>> getSources()
		{
			return listISPConnGW;
		}

		public String getAvgMeasure()
		{
			/* RULE REQS QUEUE SERVED BYTESPR RTPR TIMESTAMP CPUPM MEMPM RCPUVM RMEMVM DKBREAD DREADS DKBWRITE DWRITES NKBIN NPKTIN NKBOUT NPKTOUT NFSREAD NFSWRITE NFSMETA NFSCOMM REMOTE LOCALR RTPROCPR SLA SLAINT <RTPROCPR0> */
			String retval = "";
			try
			{
				double [] avgMeasure = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
				double avgRTPR0 = 0;
				int countRTPR0 = 0;

				synchronized(lock)
				{
					int numObservacions = listMeasuresGW.size();
					int numItems = avgMeasure.length;

					for (int i=0; i < listMeasuresGW.size(); i++)
					{
						String[] aux = ((String)listMeasuresGW.get(i)).split("\\s+");

						for (int j=1; j < numItems; j++) avgMeasure[j] += Double.parseDouble(aux[j]);
						avgMeasure[6]  = Double.parseDouble(aux[6]) * numObservacions;			// Last Timestamp
						if (avgMeasure[2] == 0) { avgRTPR0 += avgMeasure[25]; countRTPR0++; }	// "Average_RTPR" in Void
					}

					retval = ruleName + " ";
					for (int j = 1; j < numItems; j++) retval += avgMeasure[j]/((numObservacions>0)?numObservacions:1) + " ";	// Average Measure per Time Unit
					retval += avgRTPR0 / ((countRTPR0 > 0)?countRTPR0:1);
				}
				//synchronized(lock) { oldQueue = 0; } // CleanUp for longrun experiments. Triggered when demanded Measures...
			}
			catch (ArrayIndexOutOfBoundsException aioobe) { System.err.println("Gateway> WARNING " + aioobe + ". Possibly VM not ready yet..."); retval = "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"; }
			catch (Exception e) { System.err.println("Gateway> ERROR at AvgMeasure " + e); e.printStackTrace(); retval = "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"; }
			return retval;
		}

		public void cleanMeasures()
		{
			listMeasuresGW = new ArrayList<String>();
			listISPConnGW = new ArrayList<HashMap<String,Integer>>();
		}

		public int execCommand (String[] command, String[] output)
		{
			String retval = "";
			try
			{
				Process child = Runtime.getRuntime().exec(command);
				child.waitFor();

				BufferedReader stdInput = new BufferedReader(new InputStreamReader(child.getInputStream()));
				BufferedReader stdError = new BufferedReader(new InputStreamReader(child.getErrorStream()));

				output[0] = ""; output[1] = "";

				String aux = "";
				while ((aux = stdInput.readLine()) != null) { output[0] += aux + "\n"; }
				while ((aux = stdError.readLine()) != null) { output[1] += aux + "\n"; }

				child.getInputStream().close();
				child.getErrorStream().close();
				child.getOutputStream().close();

				child.destroy();
				child = null;

			}
			catch (Exception e) { System.err.println("ERROR> Command Error at " + command); e.printStackTrace(); }
			finally { System.gc(); }

			return 0;
		}
	}

	/**********************************************************************/
	/* STREAM COPY THREAD                                                 */
	/**********************************************************************/

	protected class StreamCopyThread extends Thread
	{
		private Socket inSock;
		private Socket outSock;
		private boolean done=false;
		private StreamCopyThread peer;
		private long startTime = 0;
		private int latency = 0;

		public StreamCopyThread(Socket inSock, Socket outSock, boolean recTime, int latency)
		{
			this.inSock = inSock;
			this.outSock = outSock;
			this.startTime = (recTime)?(Calendar.getInstance()).getTimeInMillis():-1;
			this.latency = latency;
		}

		public void run()
		{
			byte[] buf=new byte[bufSize];
			int count=-1;
			try
			{
				InputStream in=inSock.getInputStream();
				OutputStream out=outSock.getOutputStream();
				int bcount = 0;
				try
				{
					if (latency > 0) 
					{
						try { Thread.currentThread().sleep(latency); }
						catch (Exception e) { System.err.println("Gateway> Error at delaying communication"); e.printStackTrace(); }
					}
					while(((count=in.read(buf))>0) && !isInterrupted())
					{
						out.write(buf,0,count);
						bcount += count;
					}
				} catch(Exception xc) {
					if(debug) {
						// FIXME
						// It's very difficult to sort out between "normal"
						// exceptions (occuring when one end closes the connection
						// normally), and "exceptional" exceptions (when something
						// really goes wrong)
						// Therefore we only log exceptions occuring here if the debug flag
						// is true, in order to avoid cluttering up the log.
						System.err.println("4 "+header+":"+xc);
						xc.printStackTrace();
					}
				} finally {
					synchronized(lock) { bytestotal += bcount; }
					// The input and output streams will be closed when the sockets themselves
					// are closed.
					out.flush();
				}
			}
			catch(SocketException xc) { err.print("S"); }
			catch(IOException xc) { err.print("I"); }
			catch(Exception xc) { System.err.println("5 "+header+":"+xc); xc.printStackTrace(); }

			synchronized(lock)
			{
				done=true;
				try
				{
					if((peer==null) || peer.isDone())
					{
						// Cleanup if there is only one peer OR if _both_ peers are done
						inSock.close();
						outSock.close();
					}
					else 
					// Signal the peer (if any) that we're done on this side of the connection
					peer.interrupt();
				}
				catch(Exception xc) { System.err.println("6 "+header+":"+xc); xc.printStackTrace(); }
				finally
				{
					connections.removeElement(this);
					try
					{
						inSock.close();		// FIXME - Added as a trick to avoid File Leaks
						outSock.close();	// FIXME - Added as a trick to avoid File Leaks
					}
					catch(Exception xc) { System.err.println("7 "+header+": AAA "+xc); xc.printStackTrace(); }
				}
			}

			if (startTime != -1)
			{
				long diff = (Calendar.getInstance()).getTimeInMillis() - startTime;
				synchronized(lock) { timetotal += ((diff > 0)?diff:0); }
				synchronized(lock) { tproctotal += ((diff > 0)?diff:0) - latency; }
				synchronized(lock) { queue = queue - 1; }

				synchronized(lock)
				{
					double jOriginalRT = 100; // (millis)
					double beta = 9.0;
					double sla_aux = (1.0 - ((diff - jOriginalRT) / (beta * jOriginalRT)));
					if (sla_aux < 0) sla_aux = 0.0;
					if (sla_aux > 1) sla_aux = 1.0;

					slacount += sla_aux;
					slainteg += (diff <= jOriginalRT)?1:0;
				}

			}
		}

		public boolean isDone()
		{
			return done;
		}

		public void setPeer(StreamCopyThread peer)
		{
			this.peer=peer;
		}
	}
}
