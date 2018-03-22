import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.String;

public class SchedulerDynamicBackFilling extends quickSchedule
{
	public static void main (String [] args)
	{
		SchedulerDynamicBackFilling qs = new SchedulerDynamicBackFilling(args);
		qs.controlLoop();
	}

	public SchedulerDynamicBackFilling (String [] args)
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

		/* Insertion Sort: Replace if Hosts Size becomes >>>50 */
		for (int i=0; i < hosts.size(); i++)
		{
			for (int j=i; j > 0; j--)
			{
				hostInfo auxhj1 = hosts.get((Integer)candidateHosts[order[j-1]]);
				hostInfo auxhj2 = hosts.get((Integer)candidateHosts[order[j]]);
				
				if (auxhj1.used_cpu_alloc < auxhj2.used_cpu_alloc)
				{
					int swap = order[j-1];
					order[j-1] = order[j];
					order[j] = swap;
				}
			}
		}

		/* Order VMs by CPU Demand */
		int [] vorder = new int[vms.size()];
		for (int i=0; i < vms.size(); i++) vorder[i] = i;
		Object [] cvms = (vms.keySet()).toArray();

		/* Insertion Sort: Replace if VMs Size becomes >>>50 */
		for (int i=0; i < vms.size(); i++)
		{
			for (int j=i; j > 0; j--)
			{
				vmInfo auxvmj1 = vms.get((Integer)cvms[vorder[j-1]]);
				vmInfo auxvmj2 = vms.get((Integer)cvms[vorder[j]]);
				
				if (auxvmj1.usedcpu > auxvmj2.usedcpu)
				{
					int swap = vorder[j-1];
					vorder[j-1] = vorder[j];
					vorder[j] = swap;
				}
			}
		}

		/* Schedule fent BackFilling Dinamic */
		for (int i=0; i < vms.size(); i++)
		{
			vmInfo auxvm = vms.get((Integer)cvms[vorder[i]]);
			/*if (debugInfo)*/ System.out.println("scheduler-DBF> A VM " + auxvm.id + " in " + auxvm.state + " status found...");

			if ((auxvm.state).compareTo("PENDING") == 0 || (auxvm.state).compareTo("ACTIVE") == 0)
			{
				int currentCandidate = 0;
				boolean allocated = false;
				while (!allocated)
				{
					hostInfo auxhi = hosts.get((Integer)candidateHosts[order[currentCandidate]]);

					if ((auxvm.state).compareTo("ACTIVE") == 0)
					{
						if (auxhi.id == Integer.parseInt(auxvm.hostid)) break;	// Reached the oldHost, nothing to do here...

						if (auxhi.max_cpu > auxhi.used_cpu_alloc + auxvm.usedcpu && auxhi.max_mem > auxhi.used_mem_alloc + auxvm.usedmemory)
						{
							if (isVBox) operateVM(new int[]{auxvm.id, auxhi.id},MIGRATEVM);
							else operateVM(new int[]{auxvm.id, auxhi.id},LIVEMIGRATEVM);

							auxhi.used_cpu_alloc += auxvm.usedcpu;
							auxhi.used_mem_alloc += auxvm.usedmemory;

							hostInfo auxho = hosts.get(new Integer(auxvm.hostid));
							auxho.used_cpu_alloc -= auxvm.usedcpu;
							auxho.used_mem_alloc -= auxvm.usedmemory;

							/*if (debugInfo)*/ System.out.println("scheduler-DBF> The VM " + auxvm.id + " is migrated from " + auxho.id + " to " + auxhi.id);

							allocated = true;
						}
					}
					else /* if ((auxvm.state).compareTo("PENDING") == 0) */
					{
						if (auxhi.max_cpu > auxhi.used_cpu_alloc + auxvm.usedcpu && auxhi.max_mem > auxhi.used_mem_alloc + auxvm.usedmemory)
						{
							operateVM(new int[]{auxvm.id, auxhi.id},DEPLOYVM);

							auxhi.used_cpu_alloc += auxvm.usedcpu;
							auxhi.used_mem_alloc += auxvm.usedmemory;

							/*if (debugInfo)*/ System.out.println("scheduler-DBF> The VM " + auxvm.id + " is deployed in " + auxhi.id);

							allocated = true;
						}
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

				if ((auxvm.state).compareTo("ACTIVE") == 0)
				{
					int oldHost = hosts.size();
					for (int j=0; j < hosts.size(); j++)
					{
						hostInfo auxhj = hosts.get((Integer)candidateHosts[order[j]]);
						if (auxhj.id == Integer.parseInt(auxvm.hostid)) oldHost = j;
					}

					for (int j=oldHost; j < hosts.size()-1; j++)
					{
						hostInfo auxhj1 = hosts.get((Integer)candidateHosts[order[j+1]]);
						hostInfo auxhj2 = hosts.get((Integer)candidateHosts[order[j]]);
			
						if (auxhj1.used_cpu_alloc < auxhj2.used_cpu_alloc)
						{
							int swap = order[j+1];
							order[j+1] = order[j];
							order[j] = swap;
						}
					}
				}
			}
		}
	}
}
