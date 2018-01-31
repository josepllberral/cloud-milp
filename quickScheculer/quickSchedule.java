import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.String;

public class quickSchedule
{
	protected static final int STOPVM			= 0;
	protected static final int DEPLOYVM			= 1;
	protected static final int DELETEVM			= 2;
	protected static final int HOLDVM			= 3;
	protected static final int MIGRATEVM		= 4;
	protected static final int LIVEMIGRATEVM	= 5;
	protected static final int CANCELVM			= 6;
	protected static final int SHUTDOWNVM		= 7;
	protected static final int RESUBMITVM		= 8;

	protected Map <Integer,hostInfo> hosts;
	protected Map <Integer,vmInfo> vms;

	protected Map <Integer,String> prevIP;

	protected int stepTime = 120000;			// in milli-seconds
	protected int stepDiffTime = 0;				// in milli-seconds
	protected boolean debugInfo = false;
	protected boolean testMode = false;
	protected boolean fakeMode = false;

	protected boolean isVBox = false;
	private String uservbox = "username";
	private String pwdvbox = "vbox-guest-sudo-pwd";

	protected Map<Integer,ProxyThread> mapProxy;
	protected String dirhost = "xxxx";
	protected PrintStream printOut;
	protected PrintStream printLog;
	protected PrintStream printErr;

	protected int latency = 0;							// in milli-seconds
	protected Map <String, String> mapISP;				// This is <hostInfo.name,ISP_IP>

	protected Map <String,Integer> latencyMap;			// This is <hostInfo.name+"-"+hostInfo.name,Value>

	/* ---------------------------------------------------------- */
	/*  Configuration of the Full System                          */
	/* ---------------------------------------------------------- */
	protected double velocity = 10 * (1024 * 1024 * 1024 / 8); // 10 Gb/s

	/* ---------------------------------------------------------- */
	/*  Main and Constructors                                     */
	/* ---------------------------------------------------------- */
	
	public static void main (String [] args)
	{
		quickSchedule qs;
		if (args.length > 0) qs = new quickSchedule(args);
		else qs = new quickSchedule();
		qs.controlLoop();
	}

	public quickSchedule()
	{
		return;
	}

	public quickSchedule(String [] args)
	{
		// Parse General Parameters
		try
		{
			for (int i = 0; i < args.length; i++)
			{
				if (args[i].compareTo("-vbox") == 0)
				{
					this.isVBox = true;
					this.uservbox = args[++i];
					this.pwdvbox = args[++i];
				}
				if (args[i].compareTo("-step") == 0) this.stepTime = (new Integer(args[++i])).intValue();
				if (args[i].compareTo("-debug") == 0) this.debugInfo = true;
				if (args[i].compareTo("-test") == 0) this.testMode = true;
				if (args[i].compareTo("-fake") == 0) this.fakeMode = true;
				if (args[i].compareTo("-dirhost") == 0) this.dirhost = args[++i];
				if (args[i].compareTo("-latency") == 0) this.latency = (new Integer(args[++i])).intValue();
				if (args[i].compareTo("-help") == 0) System.err.println("scheduler> java quickSchedule -dirhost <this hostname> [-debug] [-step <millis>] [-vbox <user> <passwd>] [-help] [-test | -fake] [-latency <millis>]");
			}
			if (this.debugInfo) { for (int i = 0; i < args.length; i++) { System.out.print(args[i] + " "); } System.out.print("\n"); }
		}
		catch (Exception e) { System.err.println("scheduler> Error Parsing Parameters\n" + e.getMessage()); }

		// Creation of Gateway Map
		this.mapProxy = new HashMap<Integer,ProxyThread>();
		try
		{
			/*if (debugInfo)*/ printOut = new PrintStream(new FileOutputStream("proxy.out"));
			/*if (debugInfo)*/ printLog = new PrintStream(new FileOutputStream("results.out"));
			/*if (debugInfo)*/ printErr = new PrintStream(new FileOutputStream("proxy.err"));
		}
		catch (Exception e) { System.err.println("scheduler> Error at opening log files for Proxies"); System.exit(0); }

		// Read map of ISPs
		this.mapISP = new HashMap<String, String>();
		try
		{
			DataInputStream in = new DataInputStream(new FileInputStream("isp.data"));
			BufferedReader br = new BufferedReader(new InputStreamReader(in));

			String strLine;
			while ((strLine = br.readLine()) != null)
			{
				String[] aux = strLine.split(" ");
				mapISP.put(aux[0],aux[1]);
				/*if (debugInfo)*/ System.out.println("scheduler> Found ISP for " + aux[0] + " with IP " + aux[1]);
			}
			in.close();
		}
		catch (Exception e){ System.err.println("scheduler> Error in ISP parsing: " + e.getMessage()); e.printStackTrace(); }

		// Load Information about latencies and wattprices
		this.latencyMap = new HashMap<String,Integer>();
		try
		{
				FileInputStream fstream = new FileInputStream("latency.data");
				DataInputStream in = new DataInputStream(fstream);
				BufferedReader br = new BufferedReader(new InputStreamReader(in));
				String strLine = "";
				while ((strLine = br.readLine()) != null)
				{
					if (strLine.compareTo("") == 0) continue;
					String[] aux = strLine.split("\\s+");
					latencyMap.put(aux[0],new Integer(aux[1]));
					/*if (debugInfo)*/ System.out.println("scheduler> Latency for " + aux[0] + " with ms " + aux[1]);
				}
				in.close();
		}
		catch (Exception e) { System.err.println("scheduler> Error at opening latency data"); e.printStackTrace(); System.exit(-1); }
	}

	/* ---------------------------------------------------------- */
	/*  Control Loop                                              */
	/* ---------------------------------------------------------- */

	protected void controlLoop ()
	{
		hosts = new HashMap <Integer,hostInfo>();
		vms = new HashMap <Integer,vmInfo>();

		prevIP = new HashMap <Integer,String>();

		boolean keepLooping = true;

		while (keepLooping)
		{
			/* Update Info From Hosts and VMs */
			readHosts();
			readVMs();

			/* Debug and Info*/
			if (debugInfo) for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet()) (entry.getValue()).printHostInfo();
			if (debugInfo) for (Map.Entry<Integer,vmInfo> entry : vms.entrySet()) (entry.getValue()).printVMInfo();

			/* Scheduler is done Here */
			schedule();

			/* Sleep Waiting for all being set up... */
			try { Thread.currentThread().sleep(120000); stepDiffTime += 120000; }
			catch (Exception e) { System.err.println("scheduler> Error on Running Sleep\n" + e.getMessage()); }

			/* Check All VMs are OK... */
			checkAliveVMs();

			/* Deploy and Update Proxy Gateway */
			deployProxys();

			/* Sleep */
			try { Thread.currentThread().sleep(stepTime - stepDiffTime); stepDiffTime = 0; }
			catch (Exception e) { System.err.println("scheduler> Error on Running Sleep\n" + e.getMessage()); }
		}
	}

	/* ---------------------------------------------------------- */
	/*  Scheduler (Void Scheduler)                                */
	/* ---------------------------------------------------------- */

	protected void schedule ()
	{
		for (Map.Entry<Integer,vmInfo> entry : vms.entrySet())
		{
			vmInfo auxvm = entry.getValue();

			double rt1 = (auxvm.avg_tpr/1000);
			double rtt1 = (auxvm.avg_remote > 0)?((Double.valueOf(latency)/1000) * (auxvm.avg_remote/((auxvm.avg_reqs > 0)?auxvm.avg_reqs:1)) + (auxvm.avg_bpr * auxvm.avg_remote) / velocity):0;	// RT_Transport - Supposing all goes to the same location or in serie.
			System.out.println("scheduler-Void> A VM " + auxvm.id + " in " + auxvm.state + " status found at " + auxvm.hostname + " -> (per 15s): [ RTPR: " + rt1 + " (Local: " + (rt1-rtt1) + " Remote:" + rtt1 + "); Req: " + auxvm.avg_reqs + " (Local: " + auxvm.avg_localr + " Remote: " + auxvm.avg_remote + "); Queue: " + auxvm.avg_queue + "; Served: " + auxvm.avg_served + "; BPR: " + auxvm.avg_bpr + "]");
		}
		for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet())
		{
			hostInfo auxhost = entry.getValue();
			System.out.println("scheduler-Void> A HOST " + auxhost.name + " in " + auxhost.state + " status found with " + auxhost.running_vms + " VMs");
		}
	}

	/* ---------------------------------------------------------- */
	/*  OpenNebula Operations                                     */
	/* ---------------------------------------------------------- */

	protected void readHosts ()
	{
		hosts.clear();

		String[] retval = new String[2];
		if (fakeMode)	execCommand("cat fake/onehost.fake | grep -v ID | grep -v off |  awk '{ printf \"%s \",$1}'",retval);
		else		execCommandVM("onehost list | grep -v ID | grep -v off |  awk '{ printf \"%s \",$1}'",retval);

		String[] aux = retval[0].split("\\s+");
		for(int i=0; i < aux.length; i++)
		{
			if (aux[i].compareTo("")==0) continue;
			if (fakeMode)	execCommand("cat fake/onehost-" + Integer.parseInt(aux[i]) + ".fake", retval);
			else		execCommandVM("onehost show " + Integer.parseInt(aux[i]), retval);
			hosts.put(new Integer(aux[i]),new hostInfo(retval[0]));
		}
	}

	protected void readVMs ()
	{
		vms.clear();

		String[] retval = new String[2];
		if (fakeMode)	execCommand("cat fake/onevm.fake | grep -v ID |  awk '{ printf \"%s \",$1}'",retval);
		else 			execCommandVM("onevm list | grep -v ID | grep -v stop | grep -v fail | awk '{ printf \"%s \",$1}'",retval);	// FIXME - Get only the non stopped (Get better way to detect stopped ones)

		String[] aux = retval[0].split("\\s+");
		for(int i=0; i < aux.length; i++)
		{
			if (aux[i].compareTo("")==0) continue;
			if (fakeMode)	execCommand("cat fake/onevm-" + Integer.parseInt(aux[i]) + ".fake", retval);
			else			execCommandVM("onevm show " + Integer.parseInt(aux[i]), retval);
			vms.put(new Integer(aux[i]),new vmInfo(retval[0]));
		}
	}

	protected void deployProxys ()
	{
		if (debugInfo) System.out.println("scheduler> Deploying Proxies...");

		String[] retval = new String[2];
		if (fakeMode)	execCommand("cat fake/onevm.fake | grep -v ID |  awk '{print $1 \" \" $5 \" \" $8}'",retval);
		else 			execCommandVM("onevm list | grep -v ID | grep -v stop | grep -v fail | awk '{print $1 \" \" $5 \" \" $8}'",retval);

		String[] auxvms = retval[0].split("\n");
		for(int i=0; i < auxvms.length; i++)
		{
			String aux[] = auxvms[i].split("\\s+");
			int vmauxid = (new Integer(aux[0])).intValue();
			String vmauxip = retrieveIPVM (vmauxid,true);
			String currentHost = aux[2];
			String currentISP = mapISP.get(currentHost);

			if (aux[1].compareTo("runn") == 0)
			{
				if (mapProxy.get(new Integer(vmauxid)) == null)
				{
					try
					{
						mapProxy.put(new Integer(vmauxid), new ProxyThread(InetAddress.getByName(dirhost),(8000+vmauxid),InetAddress.getByName(vmauxip),80,printOut,printErr,"Web-"+vmauxid,vmauxid,latency,currentISP,currentHost,latencyMap,mapISP));
						/*if (debugInfo)*/ System.out.println("scheduler> Built proxy for " + vmauxid + " with IP " + vmauxip + ":80 at " + dirhost + ":" + (8000+vmauxid) + " (Local ISP: " + currentISP + " - " + currentHost+ ")");
					}
					catch(Exception e) { System.err.println("scheduler> ERROR Adding Proxy: " + e.getMessage()); e.printStackTrace(); }
				}
				else
				{
					(mapProxy.get(new Integer(vmauxid))).changeISP(currentISP);
					(mapProxy.get(new Integer(vmauxid))).changeHost(currentHost);
				}
			}
		}
		if (debugInfo) System.out.println("scheduler> Proxies Deployed...");
	}

	protected String retrieveIPVM (int id, boolean fromfile)
	{
		if (debugInfo) System.out.println("scheduler> Getting IP for " + id + "...");

		/* Get Hostname */
		String[] retval = new String[2];
		if (fakeMode)	execCommand("cat fake/onevm.fake | grep HOSTNAME | head -n 1 | awk '{print $3}'",retval);
		else 			execCommandVM("onevm show 111 | grep HOSTNAME | head -n 1 | awk '{print $3}'",retval);

		String hostname = retval[0];
		String ip = "0.0.0.0";

		try
		{
			if (!fakeMode && !fromfile)
			{
				if (isVBox)
				{
					/* Retrieve directly from VMs */
					String [] output = new String[2];									
					execCommand("ssh "+hostname+" 'VBoxManage guestcontrol one-"+id+" execute \"/sbin/ifconfig\" \"eth0\" --username "+uservbox+" --password "+pwdvbox+" --wait-stdout '",output);
					String[] aux = output[0].split("\n");

					String[] auxerr = output[1].split(" ");
					if ((auxerr[0].compareTo("VBoxManage:")==0) && (auxerr[1].compareTo("error:")==0))
					{
						System.err.println("scheduler> Error getting IP for machine one-" + id + " : " + output[1]);
						/*if (debugInfo)*/ System.err.println("scheduler> Warning! Falling back to previous IP for machine one-" + id);
						ip = prevIP.get(new Integer(id));
						if (ip == null) { if (debugInfo) System.err.println("scheduler> Error! Can't fallback IP for one-" + id + ". Getting from file."); execCommand("grep \"one-" + id + "\" iporig.data",output); ip = output[0].split("\\s+")[1]; prevIP.put(new Integer(id),ip);}
					}
					else
					{
						int i = 0; boolean foundip = false;
						try
						{
							if (debugInfo) System.err.println("scheduler> Requesting IP for machine one-" + id + " : \n" + output[0]);
							while(i < aux.length && foundip == false)
							{
								String[] outaux = aux[i].trim().split("\\s+");
								if ((outaux[0].compareTo("inet") == 0))
								{
									String ipaux = (outaux[1].split(":"))[1];
									if (ipaux.compareTo("127.0.0.1") != 0)
									{
										ip = ipaux;
										foundip = true;
									}
								}
								i = i + 1;
							}
						}
						catch (ArrayIndexOutOfBoundsException aioobe)
						{
							if (debugInfo) System.err.println("scheduler> Warning! Error requesting IP for machine one-" + id + " : " + output[1]);
							if (debugInfo) System.err.println("scheduler> Warning! Falling back to previous IP for machine one-" + id);
							ip = prevIP.get(new Integer(id));
							if (ip == null) { if (debugInfo) System.err.println("scheduler> Warning! Can't fallback IP for one-" + id + ". Getting from file [2]"); execCommand("grep \"one-" + id + "\" iporig.data",output); ip = output[0].split("\\s+")[1]; }
							foundip = true;
						}

						if (foundip)
						{
							prevIP.put(new Integer(id),ip);
							if (debugInfo) System.err.println("scheduler> Getting IP for machine one-" + id + " : " + ip);
						}
						else
						{
							try
							{
								if (debugInfo) System.err.println("scheduler> Warning! VM machine one-" + id + " online without IP! Asking for IP...");
								if (debugInfo) System.err.println("scheduler> Warning! Falling back to previous IP for machine one-" + id);
								execCommand("ssh "+hostname+" 'VBoxManage guestcontrol one-"+id+" execute \"/etc/network/if-up.d\" \"eth0\" --username "+uservbox+" --password "+pwdvbox+" --wait-stdout '",output);
								ip = prevIP.get(new Integer(id));
								if (ip == null) { if (debugInfo) System.err.println("scheduler> Error! Can't fallback IP for one-" + id + ". Getting from file [3]"); execCommand("grep \"one-" + id + "\" iporig.data",output); ip = output[0].split("\\s+")[1]; prevIP.put(new Integer(id),ip);}
								if (debugInfo) System.err.println("scheduler> Rescuing IP for machine one-" + id + " : " + ip);
							}
							catch (Exception e2) { System.err.println("scheduler> Error managing machine one-" + id + " Operator assistance required!"); }
						}
					}
				}
				// TODO - Posar el cas !isVBox
			}
			else
			{
				String [] output = new String[2];
				execCommand("grep \"one-" + id + "\" iporig.data",output);
				ip = output[0].split("\\s+")[1];

				prevIP.put(new Integer(id),ip);
				if (debugInfo) System.err.println("scheduler> Reading IP for machine one-" + id + " : " + ip);
			}
		}
		catch (Exception e) { System.err.println("scheduler> Error managing machine one-" + id + " Operator assistance required!"); e.printStackTrace(); }

		if (debugInfo) System.out.println("scheduler> " + id + " has IP " + ip+ "...");

		return ip;
	}

	protected void checkAliveVMs()
	{
		String[] retval = new String[2];
		if (fakeMode)	execCommand("cat fake/onevm.fake | grep -v ID |  awk '{print $1 \" \" $5 \" \" $8}'",retval);
		else 			execCommandVM("onevm list | grep -v ID | grep -v stop | awk '{print $1 \" \" $5 \" \" $8}'",retval);

		String[] aux = retval[0].split("\\s+");
		for(int i=0; i < aux.length; i++)
		{
			String hostname = aux[2];
			int id = (new Integer(aux[0])).intValue();

			if (aux[1].compareTo("runn") == 0 && !fakeMode)
			{
				if (isVBox)
				{
					try
					{
						if (!isAliveVMs(id,hostname))
						{
							System.err.println("scheduler> Warning! VM machine one-" + id + " not started. Reanimation started");
							try
							{
								String [] output = new String[2];									
								execCommand("ssh "+hostname+" 'VBoxManage discardstate  one-"+id+"'",output);
								execCommand("ssh "+hostname+" 'cp checkpoint/one-"+id+".sav checkpoint/rd-one"+id+".sav'",output);
								execCommand("ssh "+hostname+" 'VBoxManage adoptstate  one-"+id+" checkpoint/rd-one-"+id+".sav '",output);
								execCommand("ssh "+hostname+" 'VBoxManage startvm one-"+id+" --type headless"+"'",output);
							}
							catch (Exception e2) { System.err.println("scheduler> Error rescuing VM machine one-" + id + " Operator assistance required!"); }						
						}
					}
					catch (Exception e) { System.err.println("scheduler> Error checking status of VM machine one-" + id + " Operator assistance required!"); }
				}
				// TODO - Posar el cas !isVBox
			}
		}
	}

	protected boolean isAliveVMs(int id, String hostname)
	{
		boolean retval = false;
		try
		{
			if (isVBox)
			{
				String [] output = new String[2];									
				execCommand("ssh "+hostname+" 'VBoxManage list runningvms | grep one-"+id+" '",output);
				if (((output[0].split("\\s+"))[0]).compareTo("\"one-"+id+"\"") != 0) retval = false;
				else retval = true;
			}
			// TODO - Posar el cas !isVBox
		}
		catch (Exception e) { System.err.println("scheduler> Error checking status of VM machine one-" + id + " Operator assistance required!"); }
		return retval;
	}

	protected String[] createVM (String file)
	{
		String[] retval = new String[2];
		execCommandVM("onevm create " + file, retval);
		return retval;
	}

	protected String[] operateVM (int [] ids, int operation)
	{
		String command = "";
		String[] retval = new String[2];

		try
		{
			switch (operation)
			{
				case STOPVM:
					command = "onevm stop " + ids[0];
					break;
				case DELETEVM:
					command = "onevm delete " + ids[0];
					break;
				case DEPLOYVM:
					command = "onevm deploy " + ids[0] + " " + ids[1];
					break;
				case HOLDVM:
					command = "onevm hold " + ids[0];
					break;
				case MIGRATEVM:
					command = "onevm migrate " + ids[0] + " " + ids[1];
					break;
				case LIVEMIGRATEVM:
					command = "onevm livemigrate " + ids[0] + " " + ids[1];
					break;
				case CANCELVM:
					command = "onevm cancel " + ids[0];
					break;
				case SHUTDOWNVM:
					command = "onevm shutdown " + ids[0];
					break;
				case RESUBMITVM:
					command = "onevm resubmit " + ids[0];
					break;
				default:
					throw new UnsupportedOperationException();
			}
			if (testMode) System.out.println("scheduler> TEST: Request for operate " + command);
			else if (fakeMode) fakeCommandVM(command);
			else execCommandVM(command,retval);
		}
		catch (ArrayIndexOutOfBoundsException aioobe)	{ System.err.println("operate-vm> Parameter Error on Operation " + operation + "\n"); }
		catch (UnsupportedOperationException uoe)	{ System.err.println("operate-vm> Not a valid operation\n"); }

		return retval;
	}

	/* ---------------------------------------------------------- */
	/*  System Operations                                         */
	/* ---------------------------------------------------------- */

	protected void execCommand (String command, String[] output)
	{
		execCommand(new String[]{"/bin/bash", "-c", command}, output);
	}

	protected void execCommandVM (String command, String[] output)
	{

		execCommand(new String[]{"/bin/bash", "-c", command}, output);
		if ((output[0].split("\\s+"))[0].compareTo("execution") != 0) return;

		do
		{
			try { if (!testMode) { Thread.currentThread().sleep(60000); stepDiffTime += 60000; } }
			catch (Exception e) { System.err.println("scheduler> Error on Running Sleep (EXECCOMMANDVM)\n" + e.getMessage()); }

			execCommand(new String[]{"/bin/bash", "-c", command}, output);
		}
		while ((output[0].split("\\s+"))[0].compareTo("execution") == 0);
	}

	protected void execCommand (String[] command, String[] output)
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
			while ((aux = stdInput.readLine()) != null) output[0] += aux + "\n";
			while ((aux = stdError.readLine()) != null) output[1] += aux + "\n";

			child.getInputStream().close();
			child.getErrorStream().close();
			child.getOutputStream().close();

			child.destroy();
			child = null;

		}
		catch (Exception e)	{ System.err.println("ERROR> Command Error at " + command); e.printStackTrace(); }
		finally			{ System.gc(); }

		return;
	}

	/* ---------------------------------------------------------- */
	/*  Private Classes and Structures                            */
	/* ---------------------------------------------------------- */

	protected class hostInfo
	{
		/* HOST INFORMATION */
		public int id ;						// 14
		public String name;					// atom00             
		public String state;				// MONITORED           
		public String im_mad;				// im_kvm              
		public String vm_mad;				// vmm_kvm             
		public String vn_mad;				// fw                  
		public String tm_mad;				// tm_shared           
		public long lastMonTime;			// 1328738444          

		/* HOST SHARES */
		public long max_mem;				// 8185060  (KB)      
		public double used_mem_real = 0;	// 1160752  (KB)           
		public double used_mem_alloc;		// 1572864  (KB)       
		public double max_cpu;				// 800      (0 - 100*n)   
		public double used_cpu_real = 0;	// 0        (0 - 100*n)        
		public double used_cpu_alloc;		// 900      (0 - 100*n)           
		public long max_disk;				// 0                   
		public long used_disk_real;			// 0                   
		public long used_disk_alloc;		// 0                   
		public int running_vms;				// 2                   

		/* MONITORING INFORMATION */
		public String arch;					// x86_64
		public int cpuspeed;				// 2400
		public String error;				// [MESSAGE="Error monitoring host 14 : MONITOR FAILURE 14 Could not update remotes",TIMESTAMP="Fri Feb  3 19:47:12 2012" ]
		public double freecpu;				// 750.0	(0 - 100*n)
		public long freememory;				// 7024308	(KB)
		public String hostname;				// atom00
		public String hypervisor;			// vbox
		public String modelname;			// "Intel(R) ATOM(R) CPU D525 @ 1.20GHz"
		public long netrx;					// 0
		public long nettx;					// 0
		public int totalcpu;				// 800		(0 - 100*n)
		public long totalmemory;			// 8185060	(KB)
		public double usedcpu;				// 50.0		(0 - 100*n)
		public long usedmemory;				// 1160752	(KB)

		public hostInfo (String obtained)
		{
			String[] aux = obtained.split("\n");

			// Host Information
			id = Integer.parseInt((aux[1].split("\\s+"))[2]);
			name = ((aux[2].split(" : "))[1]).replaceAll(" ","");
			state = ((aux[3].split(" : "))[1]).replaceAll("\\s+","");
			im_mad = (aux[4].split(" : "))[1];
			vm_mad = (aux[5].split(" : "))[1];
			vn_mad = (aux[6].split(" : "))[1];
			tm_mad = (aux[7].split(" : "))[1];
			lastMonTime = Long.parseLong((aux[8].split(" : ")[1]).replace(" ",""));
			if (vm_mad.compareTo("vmm_vbox") == 0) isVBox = true;

			// Host Shares
			max_mem = Long.parseLong((aux[11].split(" : ")[1]).replace(" ",""));;
			used_mem_real = Double.parseDouble((aux[12].split(" : ")[1]).replace(" ",""));
			used_mem_alloc = Double.parseDouble((aux[13].split(" : ")[1]).replace(" ",""));
			max_cpu = Double.parseDouble((aux[14].split(" : ")[1]).replace(" ",""));
			used_cpu_real = Double.parseDouble((aux[15].split(" : ")[1]).replace(" ",""));
			used_cpu_alloc = Double.parseDouble((aux[16].split(" : ")[1]).replace(" ",""));
			max_disk = Long.parseLong((aux[17].split(" : ")[1]).replace(" ",""));
			used_disk_real = Long.parseLong((aux[18].split(" : ")[1]).replace(" ",""));
			used_disk_alloc = Long.parseLong((aux[19].split(" : ")[1]).replace(" ",""));
			running_vms= Integer.parseInt((aux[20].split(" : ")[1]).replace(" ",""));

			// Monitoring Information
			for(int i=23; i < aux.length; i++)
			{
				String[] aux2 = aux[i].split("=");
				if (aux2.length < 2 || aux2[0].contains("ERROR") || aux2[0].contains("MESSAGE") || aux2[0].contains("TIMESTAMP")) continue;
				if (aux2[0].contains("ARCH")) arch = aux2[1];
				if (aux2[0].contains("CPUSPEED")) cpuspeed = Integer.parseInt(aux2[1]);
				if (aux2[0].contains("FREECPU")) freecpu = Double.parseDouble(aux2[1]);
				if (aux2[0].contains("FREEMEMORY")) freememory = Long.parseLong(aux2[1]);
				if (aux2[0].contains("HOSTNAME")) hostname = aux2[1].replaceAll(" ","");
				if (aux2[0].contains("HYPERVISOR")) hypervisor = aux2[1];
				if (aux2[0].contains("MODELNAME")) modelname = aux2[1];
				if (aux2[0].contains("NETRX")) netrx = Long.parseLong(aux2[1]);
				if (aux2[0].contains("NETTX")) nettx = Long.parseLong(aux2[1]);
				if (aux2[0].contains("TOTALCPU")) totalcpu = Integer.parseInt(aux2[1]);
				if (aux2[0].contains("TOTALMEMORY")) totalmemory = Long.parseLong(aux2[1]);
				if (aux2[0].contains("USEDCPU")) usedcpu = Double.parseDouble(aux2[1]);
				if (aux2[0].contains("USEDMEMORY")) usedmemory = Long.parseLong(aux2[1]);
			}
		}

		public void printHostInfo ()
		{
			String retval = "";
			retval += "ID: " + id + "\n";
			retval += "NAME: " + name + "\n";
			retval += "STATE: " + state + "\n";
			retval += "IM_MAD: " + im_mad + "\n";
			retval += "VM_MAD: " + vm_mad + "\n";
			retval += "VN_MAD: " + vn_mad + "\n";
			retval += "TM_MAD: " + tm_mad + "\n";
			retval += "LAST MON TIME: " + lastMonTime + "\n";

			retval += "\n";
			retval += "MAX_MEM: " + max_mem + "\n";
			retval += "USED_MEM_REAL: " + used_mem_real + "\n";
			retval += "USED_MEM_ALLOC: " + used_mem_alloc + "\n";
			retval += "MAX_CPU: " + max_cpu + "\n";
			retval += "USED_CPU_REAL: " + used_cpu_real + "\n";
			retval += "USED_CPU_ALLOC: " + used_cpu_alloc + "\n";
			retval += "MAX_DISK: " + max_disk + "\n";
			retval += "USED_DISK_REAL: " + used_disk_real + "\n";
			retval += "USED_DISK_ALLOC: " + used_disk_alloc + "\n";
			retval += "RUNNING_VMS: " + running_vms + "\n";

			retval += "\n";
			retval += "ARCH: " + arch + "\n";
			retval += "CPUSPEED: " + cpuspeed + "\n";
			retval += "FREECPU: " + freecpu + "\n";
			retval += "FREEMEM: " + freememory + "\n";
			retval += "HOSTNAME: " + hostname + "\n";
			retval += "HYPERVISOR: " + hypervisor + "\n";
			retval += "MODELNAME: " + modelname + "\n";
			retval += "NETRX: " + netrx + "\n";
			retval += "NETTX: " + nettx + "\n";
			retval += "TOTALCPU: " + totalcpu + "\n";
			retval += "TOTALMEM: " + totalmemory + "\n";
			retval += "USEDCPU: " + usedcpu + "\n";
			retval += "USEDMEM: " + usedmemory + "\n";
			retval += "----------------------------------";

			System.out.println(retval);
		}
	}

	protected class vmInfo
	{
		/* VIRTUAL MACHINE INFORMATION */
		public int id;						// 40
		public String name;					// VMTest2
		public String user;					// oneadmin
		public String group;				// oneadmin
		public String state = "NA";			// ACTIVE
		public String lcm_state = "NA";		// RUNNING
		public String hostname = "NONE";	// atom00
		public String hostid = "-1";		// 4
		public String start_time;			// 02/08 16:21:03
		public String end_time;				// -
		public String deploy_id;			// one-40

		/* VIRTUAL MACHINE MONITORING */
		public long nettx 	   = 0;			// 60163
		public long netrx 	   = 0;			// 8162999
		public long usedmemory = 0;			// 1048576 (KB)
		public double usedcpu  = 0.0;		// 1.0 (#)

		/* PERMISSIONS */
		public String powner;				// um-
		public String pgroup;				// ---
		public String pother;				// ---

		/* VIRTUAL MACHINE TEMPLATE */
		public int cpu;						// 6
		public long memory;					// 1024
		public String ip;					// 172.16.28.205
		public String network;				// Network1
		public int network_id;				// 12

		/* VIRTUAL MACHINE GATEWAY MONITORING */
		public double avg_reqs   = 0;		// 102
		public double avg_queue  = 0;		// 35
		public double avg_served = 0;		// 70
		public double avg_bpr 	 = 0;		// 54545
		public double avg_tpr 	 = 0;		// 1020 (millis)
		public double avg_tpr0 	 = 0;		// 64 (millis)
		public double avg_tppr 	 = 0;		// 64 (millis)
		public double avg_remote = 0;		// 50 (millis)
		public double avg_localr = 0;		// 52 (millis)
		public double avg_sla    = 0;		// (0,1)
		public double avg_slaint = 0;		// [0,1]
		public List<HashMap<String,Integer>> sources = null;

		/* HOSTING MACHINE INFO */
		public double cpupm = 0;			// (0-100*n)
		public double mempm = 0;			// (MB)
		public double ioinpm = 0;			// (KB)
		public double iooutpm = 0;			// (KB)


		public vmInfo (String obtained)
		{
			String[] aux = obtained.split("\n");
			boolean userGroup = true;

			for(int i=0; i < aux.length; i++)
			{
				if (aux[i].contains(" : "))
				{
					String[] aux2 = aux[i].split(" : ",2);

					// Virtual Machine Information
					if (aux2[0].contains("ID") && !aux2[0].contains("DEPLOY ID")) id = Integer.parseInt(aux2[1].replace(" ",""));
					if (aux2[0].contains("NAME") && !aux2[0].contains("HOSTNAME")) name = aux2[1];
					if (aux2[0].contains("USER")) user = aux2[1].replace(" ","");
					if (aux2[0].contains("GROUP") && userGroup) { group = aux2[1].replace(" ",""); userGroup = false; }
					if (aux2[0].contains("STATE") && !aux2[0].contains("LCM_STATE")) state = aux2[1].replaceAll(" ","");
					if (aux2[0].contains("LCM_STATE")) lcm_state = aux2[1].replaceAll(" ","");
					if (aux2[0].contains("HOSTNAME")) hostname = aux2[1].replaceAll(" ","");
					if (aux2[0].contains("START TIME")) start_time = aux2[1];
					if (aux2[0].contains("END TIME")) end_time = aux2[1];
					if (aux2[0].contains("DEPLOY ID")) deploy_id = aux2[1].replace(" ","");

					// Virtual Machine Monitoring
					if (aux2[0].contains("NET_TX")) nettx = Long.parseLong(aux2[1].replace(" ",""));
					if (aux2[0].contains("NET_RX")) netrx = Long.parseLong(aux2[1].replace(" ",""));
					if (aux2[0].contains("USED MEMORY")) usedmemory = Long.parseLong(aux2[1].replace(" ",""));
					if (aux2[0].contains("USED CPU")) usedcpu = Integer.parseInt(aux2[1].replace(" ",""));

					// Permissions
					if (aux2[0].contains("OWNER")) powner = aux2[1].replace(" ","");
					if (aux2[0].contains("GROUP") && !userGroup) pgroup = aux2[1].replace(" ","");
					if (aux2[0].contains("OTHER")) pother = aux2[1].replace(" ","");
				}
				else
				{
					String[] aux2 = aux[i].split("=",2);

					// Virtual Machine Template [By definition]
					if (aux2.length < 2 || aux2[0].contains("ERROR") || aux2[0].contains("MESSAGE") || aux2[0].contains("TIMESTAMP")) continue;
					if (aux2[0].contains("CPU") && !aux2[0].contains("USED CPU")) cpu = Integer.parseInt(aux2[1].replace(" ",""));
					if (aux2[0].contains("MEMORY") && !aux2[0].contains("USED MEMORY")) memory = Long.parseLong(aux2[1].replace(" ",""));
					if (aux2[0].contains("IP")) ip = aux2[1].replace(",","");
					if (aux2[0].contains("NETWORK") && !aux2[0].contains("NETWORK_ID")) network = aux2[1].replace(",","");
					if (aux2[0].contains("NETWORK_ID")) network_id = Integer.parseInt(aux2[1].replace(",",""));
				}
			}

			/* Values from Monitors and Statistics (Overrule ONE monitors) */
			if (state.compareTo("ACTIVE") == 0 && lcm_state.compareTo("RUNNING") == 0)
			{
				ProxyThread paux = mapProxy.get(new Integer(id));
				if (paux == null)
				{
					avg_reqs   = 0;
					avg_queue  = 0;
					avg_served = 0;
					avg_bpr    = 0;
					avg_tpr    = 0;
					avg_remote = 0;
					avg_localr = 0;
					avg_tpr0   = 0;
					avg_tppr   = 0;
					avg_sla	   = 0;
					avg_slaint = 0;
					sources	   = new ArrayList<HashMap<String,Integer>>();
				}
				else
				{
					String gwaux = paux.gatewayAvgMeasure();
					try
					{
						if (gwaux != null) 
						{
							String[] monitoraux = gwaux.split("\\s+");

							avg_reqs   = Double.valueOf(monitoraux[1]);
							avg_queue  = Double.valueOf(monitoraux[2]);
							avg_served = Double.valueOf(monitoraux[3]);
							avg_bpr    = Double.valueOf(monitoraux[4]);
							avg_tpr    = Double.valueOf(monitoraux[5]);
							usedcpu	   = Double.valueOf(monitoraux[9]);									// CPU (0-100*n) <- top retorna el % per single CPU (p.e. 0%-400%)
							usedmemory = (new Double(Double.valueOf(monitoraux[10])*1024)).longValue();	// MEM monitor (MB) -> Mem ONE (KB)
							avg_remote = Double.valueOf(monitoraux[23]);
							avg_localr = Double.valueOf(monitoraux[24]);
							avg_tppr   = Double.valueOf(monitoraux[25]);
							avg_tpr0   = Double.valueOf(monitoraux[28]); // <- Index := last element
							avg_sla    = Double.valueOf(monitoraux[26]);
							avg_slaint = Double.valueOf(monitoraux[27]);
							sources	   = paux.gatewaySources();

							cpupm	   = Double.valueOf(monitoraux[7]) * 4;								// CPU (0-100) <- sar retorna (0-100%); FIXME - que multipliqui per #CPUs
							mempm 	   = (new Double(Double.valueOf(monitoraux[8]))).longValue();
							iooutpm	   = (new Double(Double.valueOf(monitoraux[17]))).longValue();
							ioinpm	   = (new Double(Double.valueOf(monitoraux[15]))).longValue();
						}
					}
					catch (Exception e) { System.err.println("ERROR> Error in retieving ProxyInfo for " + id); e.printStackTrace(); }
					paux.cleanMeasures();
				}

				/* Host ID holding VM */	
				try
				{
					String[] aux3 = new String[2];
					if (fakeMode)	execCommand("cat fake/onehost.fake | grep " + hostname + " | awk '{print $1}'", aux3);
					else 			execCommandVM("onehost show " + hostname + " | grep ID | awk '{print $3}'", aux3);			
					hostid = aux3[0].replaceAll("\n","");
				}
				catch (Exception e) { System.err.println("ERROR> Unexpected Error in vmInfo"); e.printStackTrace(); }
			}

			/* If VBox -> Get IP from GuestControl */
			if (isVBox && state.compareTo("ACTIVE") == 0 && lcm_state.compareTo("RUNNING") == 0 && !fakeMode)
			{
				/* Check VM Started */
				if (!isAliveVMs(id,hostname))
				{
					System.err.println("scheduler> Warning! VM machine one-" + id + " not started. Reanimation started");
					try
					{
						String [] output = new String[2];									
						execCommand("ssh "+hostname+" 'VBoxManage discardstate  one-"+id+"'",output);
						execCommand("ssh "+hostname+" 'VBoxManage startvm one-"+id+" --type headless"+"'",output);
					}
					catch (Exception e2) { System.err.println("scheduler> Error rescuing VM machine one-" + id + " Operator assistance required!"); }						
				}

				/* Get VM IP from GuestControl */
				ip = retrieveIPVM(id,true);

/*				try
				{
					if (false) // FIXME - Get IP from VM directly...
					{
						/* Retrieve directly from VMs 
						String [] output = new String[2];									
						execCommand("ssh "+hostname+" 'VBoxManage guestcontrol one-"+id+" execute \"/sbin/ifconfig\" \"eth0\" --username "+uservbox+" --password "+pwdvbox+" --wait-stdout '",output);
						aux = output[0].split("\n");

						String[] auxerr = output[1].split(" ");
						if ((auxerr[0].compareTo("VBoxManage:")==0) && (auxerr[1].compareTo("error:")==0))
						{
							System.err.println("scheduler> Error getting IP for machine one-" + id + " : " + output[1]);
							if (debugInfo) System.err.println("scheduler> Warning! Falling back to previous IP for machine one-" + id);
							ip = prevIP.get(new Integer(id));
							if (ip == null) { if (debugInfo) System.err.println("scheduler> Error! Can't fallback IP for one-" + id + ". Getting from file."); execCommand("grep \"one-" + id + "\" iporig.data",output); ip = output[0].split("\\s+")[1]; }
						}
						else
						{
							int i = 0; boolean foundip = false;
							try
							{
								if (debugInfo) System.err.println("scheduler> Requesting IP for machine one-" + id + " : \n" + output[0]);
								while(i < aux.length && foundip == false)
								{
									String[] outaux = aux[i].trim().split("\\s+");
									if ((outaux[0].compareTo("inet") == 0))
									{
										String ipaux = (outaux[1].split(":"))[1];
										if (ipaux.compareTo("127.0.0.1") != 0)
										{
											ip = ipaux;
											foundip = true;
										}
									}
									i = i + 1;
								}
							}
							catch (ArrayIndexOutOfBoundsException aioobe)
							{
								if (debugInfo) System.err.println("scheduler> Warning! Error requesting IP for machine one-" + id + " : " + output[1]);
								if (debugInfo) System.err.println("scheduler> Warning! Falling back to previous IP for machine one-" + id);
								ip = prevIP.get(new Integer(id));
								if (ip == null) { if (debugInfo) System.err.println("scheduler> Warning! Can't fallback IP for one-" + id + ". Getting from file [2]"); execCommand("grep \"one-" + id + "\" iporig.data",output); ip = output[0].split("\\s+")[1]; }
								foundip = true;
							}

							if (foundip)
							{
								prevIP.put(new Integer(id),ip);
								if (debugInfo) System.err.println("scheduler> Getting IP for machine one-" + id + " : " + ip);
							}
							else
							{
								try
								{
									if (debugInfo) System.err.println("scheduler> Warning! VM machine one-" + id + " online without IP! Asking for IP...");
									if (debugInfo) System.err.println("scheduler> Warning! Falling back to previous IP for machine one-" + id);
									execCommand("ssh "+hostname+" 'VBoxManage guestcontrol one-"+id+" execute \"/etc/network/if-up.d\" \"eth0\" --username "+uservbox+" --password "+pwdvbox+" --wait-stdout '",output);
									ip = prevIP.get(new Integer(id));
									if (ip == null) { if (debugInfo) System.err.println("scheduler> Error! Can't fallback IP for one-" + id + ". Getting from file [3]"); execCommand("grep \"one-" + id + "\" iporig.data",output); ip = output[0].split("\\s+")[1]; }
									if (debugInfo) System.err.println("scheduler> Rescuing IP for machine one-" + id + " : " + ip);
								}
								catch (Exception e2) { System.err.println("scheduler> Error managing machine one-" + id + " Operator assistance required!"); }
							}
						}
					}
					else
					{
						String [] output = new String[2];
						execCommand("grep \"one-" + id + "\" iporig.data",output);
						ip = output[0].split("\\s+")[1];

						prevIP.put(new Integer(id),ip);
						if (debugInfo) System.err.println("scheduler> Reading IP for machine one-" + id + " : " + ip);
					}
				}
				catch (Exception e) { System.err.println("scheduler> Error managing machine one-" + id + " Operator assistance required!"); e.printStackTrace(); }*/
			}
		}

		public void printVMInfo ()
		{
			String retval = "";
			retval += "ID: " + id + "\n";
			retval += "NAME: " + name + "\n";
			retval += "USER: " + user + "\n";
			retval += "GROUP: " + group + "\n";
			retval += "STATE: " + state + "\n";
			retval += "LCM_STATE: " + lcm_state + "\n";
			retval += "HOSTNAME: " + hostname + "\n";
			retval += "START_TIME: " + start_time + "\n";
			retval += "END_TIME: " + end_time + "\n";
			retval += "DEPLOY_ID: " + deploy_id + "\n";

			retval += "\n";
			retval += "NETTX: " + nettx + "\n";
			retval += "NETRX: " + netrx + "\n";
			retval += "USED_MEMORY: " + usedmemory + "\n";
			retval += "USED_CPU: " + usedcpu + "\n";

			retval += "\n";
			retval += "OWNER: " + powner + "\n";
			retval += "GROUP: " + pgroup + "\n";
			retval += "OTHER: " + pother + "\n";

			retval += "\n";
			retval += "CPU: " + cpu + "\n";
			retval += "MEMORY: " + memory + "\n";
			retval += "IP: " + ip + "\n";
			retval += "NETWORK: " + network + "\n";
			retval += "NETWORK_ID: " + network_id + "\n";

			retval += "\n";
			retval += "REQUESTS (per 15s): " + avg_reqs + "\n";
			retval += "QUEUE (per 15s):" + avg_queue + "\n";
			retval += "SERVED (per 15s):" + avg_served + "\n";
			retval += "BYTESPR (per 15s):" + avg_bpr + "\n";
			retval += "TIMEPR (per 15s):" + avg_tpr + "\n";
			retval += "TIMEPR_0 (per 15s):" + avg_tpr0 + "\n";
			retval += "REMOTE REQS (per 15s):" + avg_remote + "\n";
			retval += "----------------------------------";

			System.out.println(retval);
		}
	}

	/* ---------------------------------------------------------- */
	/*  Fake Mode                                                 */
	/* ---------------------------------------------------------- */

	protected void fakeCommandVM (String command)
	{
		String[] aux = command.split("\\s+");

		if (command.contains("stop"))
		{
			String hostname = vms.get(new Integer(aux[2])).hostname;
			String hostid = vms.get(new Integer(aux[2])).hostid;

			String[] aux2 = new String[2];
			// onevm-ID.fake -> Treu Host i posa Stop
			execCommand("cd fake; sed -i 's/RUNNING/STOPPED/g' onevm-" + aux[2] + ".fake", aux2);
			execCommand("cd fake; sed -i 's/" + hostname + "/NOHOST/g' onevm-" + aux[2] + ".fake", aux2);
			// onevm.fake -> Treu Host i posa Stop
			execCommand("cd fake; export IFS='%'; AUX=`grep '" + aux[2] + "' onevm.fake`; AUX4=`echo $AUX | awk '{$5=\"stop\"; $8=\"nohost\"; print}'`; sed -i 's/'$AUX'/'\" \"$AUX4'/g' onevm.fake;", aux2);
			// onehost-HID.fake -> Decrementa VMs
			execCommand("cd fake; export IFS='%'; AUX=`grep 'RUNNING VMS' onehost-" + hostid + ".fake`; AUX2=`echo $AUX | awk '{print \": \" $4-1}'`; AUX3=`echo $AUX | awk '{print \": \" $4}'`; AUX4=`echo ${AUX/$AUX3/$AUX2}`; sed -i 's/'$AUX'/'$AUX4'/g' onehost-" + hostid + ".fake", aux2);
			// onehost.fake -> Decrementa VMs
			execCommand("cd fake; export IFS='%'; AUX=`grep '" + hostname + "' onehost.fake`; AUX4=`echo $AUX | awk '{$3=$3-1; print}'`; sed -i 's/'$AUX'/'\" \"$AUX4'/g' onehost.fake;", aux2);
		}
		else if  (command.contains("deploy"))
		{
			String hostname = hosts.get(new Integer(aux[3])).hostname;
			String hostid = new Integer(hosts.get(new Integer(aux[3])).id).toString();

			String[] aux2 = new String[2];
			// onevm-ID.fake -> Posa Host i posa Running
			execCommand("cd fake; sed -i 's/STOPPED/RUNNING/g' onevm-" + aux[2] + ".fake", aux2);
			execCommand("cd fake; sed -i 's/NOHOST/" + hostname + "/g' onevm-" + aux[2] + ".fake", aux2);
			// onevm.fake -> Posa Host i posa Running
			execCommand("cd fake; export IFS='%'; AUX=`grep '" + aux[2] + "' onevm.fake`; AUX4=`echo $AUX | awk '{$5=\"runn\"; $8=\"" + hostname + "\"; print}'`; sed -i 's/'$AUX'/'\" \"$AUX4'/g' onevm.fake;", aux2);
			// onehost-HID.fake -> Incrementa VMs
			execCommand("cd fake; export IFS='%'; AUX=`grep 'RUNNING VMS' onehost-" + hostid + ".fake`; AUX2=`echo $AUX | awk '{print \": \" $4+1}'`; AUX3=`echo $AUX | awk '{print \": \" $4}'`; AUX4=`echo ${AUX/$AUX3/$AUX2}`; sed -i 's/'$AUX'/'$AUX4'/g' onehost-" + hostid + ".fake", aux2);
			// onehost.fake -> INcrementa VMs
			execCommand("cd fake; export IFS='%'; AUX=`grep '" + hostname + "' onehost.fake`; AUX4=`echo $AUX | awk '{$3=$3+1; print}'`; sed -i 's/'$AUX'/'\" \"$AUX4'/g' onehost.fake;", aux2);
		}
		else if (command.contains("migrate"))
		{
			String hostname = hosts.get(new Integer(aux[3])).hostname;
			String hostid = (new Integer(hosts.get(new Integer(aux[3])).id)).toString();

			String[] aux2 = new String[2];
			// onehost-HID.fake -> Incrementa Desti i Decrementa Origen (L'Ordre d'Execució Importa!)
			execCommand("cd fake; export IFS='%'; AUXHOST=`grep '" + aux[2] + "' onevm.fake | awk '{print $8}'`; AUXID=`grep $AUXHOST onehost.fake | awk '{print $1}'`; AUX=`grep 'RUNNING VMS' onehost-$AUXID.fake`; AUX2=`echo $AUX | awk '{print \": \" $4-1}'`; AUX3=`echo $AUX | awk '{print \": \" $4}'`; AUX4=`echo ${AUX/$AUX3/$AUX2}`; sed -i 's/'$AUX'/'$AUX4'/g' onehost-$AUXID.fake", aux2);
			execCommand("cd fake; export IFS='%'; AUX=`grep 'RUNNING VMS' onehost-" + hostid + ".fake`; AUX2=`echo $AUX | awk '{print \": \" $4+1}'`; AUX3=`echo $AUX | awk '{print \": \" $4}'`; AUX4=`echo ${AUX/$AUX3/$AUX2}`; sed -i 's/'$AUX'/'$AUX4'/g' onehost-" + hostid + ".fake", aux2);
			// onehost.fake -> Incrementa Desti i Decrementa Origen
			execCommand("cd fake; export IFS='%'; AUXHOST=`grep '" + aux[2] + "' onevm.fake | awk '{print $8}'`; AUX=`grep $AUXHOST onehost.fake`; AUX4=`echo $AUX | awk '{$3=$3-1; print}'`; sed -i 's/'$AUX'/'\" \"$AUX4'/g' onehost.fake;", aux2);
			execCommand("cd fake; export IFS='%'; AUX=`grep '" + hostname + "' onehost.fake`; AUX4=`echo $AUX | awk '{$3=$3+1; print}'`; sed -i 's/'$AUX'/'\" \"$AUX4'/g' onehost.fake;", aux2);
			// onevm-ID.fake -> Canvia Host
			execCommand("cd fake; export IFS='%'; AUXHOST=`grep '" + aux[2] + "' onevm.fake | awk '{print $8}'`; sed -i 's/'$AUXHOST'/" + hostname + "/g' onevm-" + aux[2] + ".fake", aux2);
			// onevm.fake -> Canvia Host
			execCommand("cd fake; export IFS='%'; AUX=`grep '" + aux[2] + "' onevm.fake`; AUX4=`echo $AUX | awk '{$8=\"" + hostname + "\"; print}'`; sed -i 's/'$AUX'/'\" \"$AUX4'/g' onevm.fake;", aux2);
		}
		else
		{
			System.err.println("scheduler> Operation not allowed on FakeMode: " + command);
		}
	}

}

/*
VBoxManage Commands:
	VBoxManage createvm --name Genload0X --register
	VBoxManage modifyvm Genload0X --memory 512 --acpi on --boot1 dvd --nic1 bridged --bridgeadapter1 eth0 --vram 128
	VBoxManage clonevdi Linux-rdy.vdi VirtualBox\ VMs/Genload0X/Genload0X.vdi
	VBoxManage storagectl Genload0X --name "IDE Controller" --add ide
	VBoxManage storageattach Genload0X --storagectl "IDE Controller" --port 0 --device 0 --type hdd --medium VirtualBox\ VMs/Genload0X/Genload0X.vdi 

	VBoxManage --startvm Genload0X --type headless
	VBoxManage guestcontrol Genload0X execute "/sbin/ifconfig" "eth0" --username josep --password PWD --verbose --wait-stdout

Individual:
	ps aux | grep VBoxHeadless | grep one-111 | awk '{sum+=$3; sum2+=$5/1024} END {print sum+0 " " sum2+0}'

	ssh atom00 'ps aux | grep VBoxHeadless | grep one-111 | grep -v grep'
	ssh atom00 'VBoxManage guestcontrol one-111 execute  "/sbin/ifconfig" "eth0" --username josep --password PWD --verbose --wait-stdout | grep "inet addr" | grep -v 127 '

Global:
	top -b -n 1 | grep "VBoxHeadless" | awk '{sum+=$9; sum2+=($6-1)} END {print sum+0 " " sum2+0}'

Execucio:
	java -cp quickSchedule.jar quickSchedule -vbox josep PWD -step 600000 -latency 200 -dirhost atom01 -test
	java -cp quickSchedule.jar:/home/jlberral/gurobi501/linux64/lib/gurobi.jar greenNebula -vbox josep PWD -step 60000 -dirhost atom01 -latency 200 &> gn-scheduler.log &
	java -cp quickSchedule.jar:/home/jlberral/weka.jar wekaScheduler -vbox josep PWD -step 600000 -dirhost atom01 -latency 200 &> scheduler.log &

	wget http://atom01:8115; wget http://atom01:8114; wget http://atom01:8113; wget http://atom01:8112; wget http://atom01:8111; rm index.html*

Recuperació:
	tgvm=113
	VBoxManage discardstate one-$tgvm
	VBoxManage storageattach one-$tgvm --storagectl "ONE-sata" --port 0 --device 0 --medium none
	VBoxManage closemedium disk `VBoxManage list hdds | grep one/$tgvm/ | awk '{print $2}'`
	#VBoxManage closemedium disk `VBoxManage list hdds | grep one/$tgvm/ -B 3 | head -1 | awk '{print $2}'`
	VBoxManage unregistervm one-$tgvm
	rm -rf .VirtualBox/Machines/one-$tgvm

	onevm resubmit $tgvm
	onevm deploy $tgvm NEW_HOST
*/
