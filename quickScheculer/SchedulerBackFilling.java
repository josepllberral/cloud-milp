import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.String;

public class SchedulerBackFilling extends quickSchedule
{
	public static void main (String [] args)
	{
		SchedulerBackFilling qs = new SchedulerBackFilling(args);
		qs.controlLoop();
	}

	public SchedulerBackFilling (String [] args)
	{
		super(args);
	}

	/* ---------------------------------------------------------- */
	/*  Scheduler                                                 */
	/* ---------------------------------------------------------- */

	protected void schedule ()
	{
		/* Order Hosts by CPU Occupation */
		int [] order = new int[hosts.size()];
		for (int i=0; i < hosts.size(); i++) order[i] = i;
		Object [] candidateHosts = (hosts.keySet()).toArray();

		/* Insertion Sort: Replace if Host Size becomes >>>50 */
		for (int i=0; i < hosts.size(); i++)
		{
			for (int j=i; j > 0; j--)
			{
				hostInfo auxhj1 = hosts.get((Integer)candidateHosts[order[j-1]]);
				hostInfo auxhj2 = hosts.get((Integer)candidateHosts[order[j]]);
				
				if (auxhj1.used_cpu_alloc > auxhj2.used_cpu_alloc)
				{
					int swap = order[j-1];
					order[j-1] = order[j];
					order[j] = swap;
				}
			}
		}

		/* Schedule fent BackFilling simple */
		for (Map.Entry<Integer,vmInfo> entry : vms.entrySet())
		{
			vmInfo auxvm = entry.getValue();
			/*if (debugInfo)*/ System.out.println("scheduler-BF> A VM " + auxvm.id + " in " + auxvm.state + " status found...");

			if ((auxvm.state).compareTo("PENDING") == 0)
			{
				int currentCandidate = 0;
				boolean allocated = false;
				while (!allocated)
				{
					hostInfo auxhi = hosts.get((Integer)candidateHosts[order[currentCandidate]]);
					if (auxhi.max_cpu > auxhi.used_cpu_alloc + auxvm.usedcpu && auxhi.max_mem > auxhi.used_mem_alloc + auxvm.usedmemory)
					{
						operateVM(new int[]{auxvm.id, auxhi.id},DEPLOYVM);

						auxhi.used_cpu_alloc += auxvm.usedcpu;
						auxhi.used_mem_alloc += auxvm.usedmemory;

						allocated = true;

						/*if (debugInfo)*/ System.out.println("scheduler-BF> The VM " + auxvm.id + " is deployed in " + auxhi.id);
					}
					currentCandidate++;

					if (currentCandidate >= hosts.size()) allocated = true;	// NOT true... it can not be allocated anywhere...
				}

				/* Update Host Order */
				for (int j=currentCandidate; j > 0; j--)
				{
					hostInfo auxhj1 = hosts.get((Integer)candidateHosts[order[j-1]]);
					hostInfo auxhj2 = hosts.get((Integer)candidateHosts[order[j]]);
			
					if (auxhj1.used_cpu_alloc > auxhj2.used_cpu_alloc)
					{
						int swap = order[j-1];
						order[j-1] = order[j];
						order[j] = swap;
					}
				}
			}
		}
	}
}
