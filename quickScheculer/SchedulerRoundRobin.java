import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.String;

public class SchedulerRoundRobin extends quickSchedule
{
	public static void main (String [] args)
	{
		SchedulerRoundRobin qs = new SchedulerRoundRobin(args);
		qs.controlLoop();
	}

	public SchedulerRoundRobin (String [] args)
	{
		super(args);
	}

	/* ---------------------------------------------------------- */
	/*  Scheduler                                                 */
	/* ---------------------------------------------------------- */

	private int counterRR = 0;

	protected void schedule ()
	{
		for (Map.Entry<Integer,vmInfo> entry : vms.entrySet())
		{
			vmInfo auxvm = entry.getValue();
			/*if (debugInfo)*/ System.out.println("scheduler-RR> A VM " + auxvm.id + " in " + auxvm.state + " status found...");

			if ((auxvm.state).compareTo("FAILED") == 0)
			{
				if (isVBox) execCommand ("VBoxManage unregistervm one-"+auxvm.id+"; rm -rf .VirtualBox/Machines/one-"+auxvm.id, new String[2]);	// FIXME - This should be fixed in onevbox, not here ...
				operateVM(new int[]{auxvm.id},RESUBMITVM);
				System.out.println("scheduler-RR> The VM " + auxvm.id + " is resubmited");
			}

			if ((auxvm.state).compareTo("PENDING") == 0)
			{
				boolean allocated = false;
				int counterInt = 0;
				Object [] candidateHosts = (hosts.keySet()).toArray();
				while (!allocated)
				{
					hostInfo auxhi = hosts.get((Integer)candidateHosts[counterRR]);
					if (auxhi.max_cpu >= auxhi.used_cpu_alloc + auxvm.cpu*100 && auxhi.max_mem >= auxhi.used_mem_alloc + auxvm.memory*1024)
					{
						operateVM(new int[]{auxvm.id, auxhi.id},DEPLOYVM);

						auxhi.used_cpu_alloc += auxvm.cpu*100;
						auxhi.used_mem_alloc += auxvm.memory*1024;

						allocated = true;

						/*if (debugInfo)*/ System.out.println("scheduler-RR> The VM " + auxvm.id + " is deployed in " + auxhi.id);
					}
					counterRR = (counterRR==hosts.size()-1)?0:counterRR+1;
					counterInt++;

					if (counterInt==hosts.size())
					{
						allocated = true;	// NOT true... it can not be allocated anywhere...
						/*if (debugInfo)*/ System.out.println("scheduler-RR> The VM " + auxvm.id + " not deployed...");
					}
				}
			}
		}
	}
}
