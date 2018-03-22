
import java.io.*;
import java.text.*;
import java.util.regex.*;
import java.util.Date;
import java.lang.String;
import java.util.Calendar;

import java.net.*;

import java.lang.management.ManagementFactory;
import java.lang.management.OperatingSystemMXBean;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public class GenLoad extends Thread
{
	private static int NUM_FIELDS = 14;
	private static String [] log_file;
	private static String base = "/home/usuaris/jlberral/records/";
	private static String address;
	private static long memCentinel;
	private static DateFormat formatter;
	private static OperatingSystemMXBean operatingSystemMXBean;
	protected Object lock = new Object();

	private static Pattern paux0 = Pattern.compile("captcha");
	private static Pattern paux1 = Pattern.compile("(.*?)=(\\d+,\\d+|\\d+.\\d+|\\d+)s$");
	private static Pattern paux2 = Pattern.compile("(\\d+-\\d+-\\d+ \\d+:\\d+:\\d+) \\((\\d+.\\d+|\\d+) (MB|KB|B)\\/s\\) - (.*?) saved \\[(\\d+)(.*?)\\]");
	private static Pattern paux3 = Pattern.compile("^([\\d.]+) (\\S+) (\\S+) \\[([\\w:/]+):(\\d+:\\d+:\\d+) ([+\\-]\\d{4})\\] \"(\\S+) (.+?) (\\S+)\" (\\d{3}) (\\d+) \"([^\"]+)\" \"([^\"]+)\"(.*?)");

	public static String formatLine1(String logEntryLine)
	{
		Matcher matcher = paux3.matcher(logEntryLine);
		if (!matcher.matches())
		{
			System.err.println("Bad log entry (or problem with RE?) 1:");
			System.err.println(logEntryLine);
			return null;
		}
		if (NUM_FIELDS < matcher.groupCount())
		{
			System.err.println("Bad log entry (or problem with RE?) 2:");
			System.err.println(logEntryLine);
			return null;
		}
		String retval = "";
		retval = retval + matcher.group(1) + " "; // IP Address	0
		retval = retval + matcher.group(4) + " "; // Date	1
		retval = retval + matcher.group(5) + " "; // Time	2
		retval = retval + matcher.group(6) + " "; // TimeZone	3
		retval = retval + matcher.group(7) + " "; // Method	4
		retval = retval + matcher.group(8) + " "; // Path	5
		retval = retval + matcher.group(9) + " "; // Protocol	6

		return retval;
	}

	private int id;

	public GenLoad (int local_id)
	{
		this.id = local_id;
		this.formatter = new SimpleDateFormat("dd/MMM/yyyy hh:mm:ss z");
		//this.operatingSystemMXBean = ManagementFactory.getOperatingSystemMXBean();
		start();
	}

	public void run()
	{
		int id = this.id;

		String logging = "";
		long old_time = -1;
		double mbs = 0;
		int queries = 0;
		double rt = 0;
		double bytes = 0;
		int timeunit = 0;

		// CLAI // String myFile = base + "testFile-" + id + ".txt";

		try {
			// CLAI // 
			/* BufferedWriter out = new BufferedWriter(new FileWriter(myFile));
			out.write("Timestamp QueriesTU AvgRT(s) TotalBytes Speed(MB/s)\n");
			out.close(); out = null;*/

			String line;  
			int i = 0;
			while (i < log_file.length)
			{
				line = log_file[i++];
			
				String [] parsed_line = line.split(" ");
				String target = parsed_line[5];

				Matcher maux = paux0.matcher(target);
				if (!maux.matches())
				{
					Date date2 = (Date)formatter.parse(parsed_line[1]+" "+parsed_line[2]+" "+parsed_line[3]);
					long time1 = date2.getTime();

					if (old_time == -1) old_time = time1;

					if (time1 - old_time > 0)
					{
						// CLAI // 
						/* if (queries > 0)
						{
							SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
							logging = sdf.format(Calendar.getInstance().getTime()) + " " + queries + " " + (rt/queries) + " " + (bytes/queries) + " " + (mbs/queries) + "\n";

							BufferedWriter out1 = new BufferedWriter(new FileWriter(myFile,true));
							out1.write(logging);
							out1.close(); out1 = null;
						}
						mbs = 0;
						rt = 0;
						bytes = 0;*/
						queries = 0;

						Thread.currentThread().sleep((time1 - old_time));
					}
					old_time = time1;

					if (parsed_line[4].compareTo("GET") == 0)
					{
				
						// Jget //
						URL u;
						InputStream is = null;
						DataInputStream dis;
						String s;
						String test = "";

						try
						{
							u = new URL(address + target);
							is = u.openStream();
							dis = new DataInputStream(new BufferedInputStream(is));
							while ((s = dis.readLine()) != null)
							{
								test += s;
							}
						}
						catch (MalformedURLException mue)
						{
							System.err.println("Ouch - a MalformedURLException happened.");
							mue.printStackTrace();
						}
						catch (IOException ioe)
						{
							System.err.println("Oops- an IOException happened.");
							ioe.printStackTrace();
						}
						finally
						{
							try
							{
								is.close();
							}
							catch (IOException ioe)
							{
								System.err.println("Closing Error: Possible leakage expected!");
							}
						}

						// Substituit pel Jget //

						/*String aux = "";
						String command = "wget " + address + target + " --user-agent=\"" + id + "." + parsed_line[0]  + "\" -O/dev/null";

						try {
							long b = 0;
							synchronized(lock) { b = ((com.sun.management.OperatingSystemMXBean) operatingSystemMXBean).getFreePhysicalMemorySize(); }
							while ((b/1048576) <= memCentinel)
							{
								System.err.println("MOTHERFUCKING OUT OF MEMORY " + (b/1048576) );
								Thread.currentThread().sleep(10000);
								synchronized(lock) { b = ((com.sun.management.OperatingSystemMXBean) operatingSystemMXBean).getFreePhysicalMemorySize(); }
							}
							
							Process child = Runtime.getRuntime().exec(command);

							// CLAI // 
							child.getInputStream().close();
							child.getErrorStream().close();
							child.getOutputStream().close();

							child.waitFor();

							// CLAI // 
							///InputStream in1 = child.getInputStream();
							//InputStream es1 = child.getErrorStream();
							//OutputStream os1 = child.getOutputStream();
							//
							//int c;
							//while ((c = es1.read()) != -1) {
							//	aux = aux + (char)c;
							//}
							//in1.close(); in1 = null;
							//es1.close(); es1 = null;
							//os1.close(); os1 = null;
							child.destroy();
							child = null;

						} catch (java.io.IOException e) {
							System.err.println("FS Error " + id + " at wget");
							System.gc();
						}*/

						//System.err.println(id + " " + target);
						//System.err.println(id + " " + command);
						//System.err.println(id + " " + aux);

						queries++;

						// CLAI // 
						/*String aux_rt = "", aux_bytes = "", aux_speed = "";
						String [] output = aux.split("\n");
						for (int j=0; j < output.length; j++)
						{
							maux = paux1.matcher(output[j]);
							if (maux.matches())
							{
								aux_rt = maux.group(2);
								aux_rt = aux_rt.replace(",",".");
								aux_rt = aux_rt.replace("s","");
								rt += Double.valueOf(aux_rt);
								continue;
							}
							maux = paux2.matcher(output[j]);
							if (maux.matches())
							{
								aux_bytes = maux.group(5);
								bytes += Double.valueOf(aux_bytes);

								aux_speed = maux.group(2);
								aux_speed = aux_speed.replace(",",".");

								if ((maux.group(3)).compareTo("MB")==0)		mbs += Double.valueOf(aux_speed);
								else if ((maux.group(3)).compareTo("KB")==0)	mbs += Double.valueOf(aux_speed)/1024;
								else if ((maux.group(3)).compareTo("B")==0)	mbs += Double.valueOf(aux_speed)/(1024*1024);

							}
						}*/
						//System.err.println(id + " " + aux_rt + " " + aux_bytes + " " + aux_speed);
					}
				}
			}
			System.out.println("Thread " + id + " Executades " + i + " linies");
		} catch (Exception e){
			System.err.println("Error " + id + " : " + e.getMessage());
		}
	}

	public static void main ( String argv[] )
	{
		String log_file_arg = argv[0];		// "./imageboard-test-27-11-2010.log";
		String target = argv[1];		// "http://blade3:8080";
		int mult = Integer.valueOf(argv[2]);	// 1000
		int offset = Integer.valueOf(argv[3]);	// 100
		memCentinel = Long.valueOf(argv[4]);	// 100 (Mb)

		String line = "", aux;
		try {
			DataInputStream in = new DataInputStream(new FileInputStream(log_file_arg));
			BufferedReader br = new BufferedReader(new InputStreamReader(in));

			while ((aux = br.readLine()) != null)
			{
				line = line + formatLine1(aux) + "\n";
			}
			in.close();
		} catch (Exception e){
			System.err.println("Error Fatal: " + e.getMessage());
			System.exit(-1);
		}
		log_file = line.split("\n");
		address = target;

		GenLoad [] gl = new GenLoad[mult];
		for(int i = 0; i < mult; i++)
		{
			gl[i] = new GenLoad((i+offset));
			try {
				Thread.currentThread().sleep(50);
			} catch (Exception e) {
				System.err.println(e);
			}
		}

		for(int i = 0; i < mult; i++)
		{
			try {
		            gl[i].join();
			} catch (InterruptedException e) {
				System.out.print("Join interrupted\n");
			}
		}

		System.exit(0);
	}
}

