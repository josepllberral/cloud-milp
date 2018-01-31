import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.String;
import java.lang.Math;

import gurobi.*;

public class greenNebula extends quickSchedule
{
	public static void main (String [] args)
	{
		greenNebula qs = new greenNebula(args);
		qs.configureGN();
		qs.controlLoop();
	}

/*  static {
        System.loadLibrary("GurobiJni50");
    }*/

	public greenNebula (String [] args)
	{
		super(args);
	}

	protected void controlLoop ()
	{
		hosts = new HashMap <Integer,hostInfo>();
		vms = new HashMap <Integer,vmInfo>();

		boolean keepLooping = true;

		while (keepLooping)
		{
			System.out.println("scheduler-GN> Reading VMs and Hosts");

			/* Update Info From Hosts and VMs */
			readHosts();
			readVMs();

			if (debugInfo) for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet()) (entry.getValue()).printHostInfo();
			if (debugInfo) for (Map.Entry<Integer,vmInfo> entry : vms.entrySet()) (entry.getValue()).printVMInfo();

			System.out.println("scheduler-GN> Start Round " + timestamp);

			/* Scheduler Here */
			this.schedule();

			System.out.println("scheduler-GN> Sleeping for " + stepTime/1000 + " seconds");

			/* Sleep */
			try { Thread.currentThread().sleep(stepTime); }
			catch (Exception e) { System.err.println("scheduler-GN> Error on Running Sleep\n" + e.getMessage()); }

			System.out.println("scheduler-GN> Wake up");
		}
	}

	/* ---------------------------------------------------------- */
	/*  Configuration of the Scheduler                            */
	/* ---------------------------------------------------------- */

	private int locations;
	private int numHosts;
	private Map<String,Integer> locationNames;
	private Map<Integer,String> locationNamesInv;
	private Map<String,Integer> hostNames;
	private Map<Integer,String> hostNamesInv;
	//private double pricemigs;
	private int[][] isat;
	private double[] pricebrown;
	private double[] windsize;
	private double[] solarsize;
	private double[] locationCapacity;
	private	double[] nmAccount;

	private void configureGN ()
	{
		locationNames = new HashMap<String,Integer>();
		locationNamesInv = new HashMap<Integer,String>();
		hostNames = new HashMap<String,Integer>();
		hostNamesInv = new HashMap<Integer,String>();

		//pricemigs = 0;
		isat = null;
		pricebrown = null;
		windsize = null;
		solarsize = null;
		locationCapacity = null;

		try
		{
			DataInputStream in = new DataInputStream(new FileInputStream("greenNebula.conf"));
			BufferedReader br = new BufferedReader(new InputStreamReader(in));

			String strLine;
			while ((strLine = br.readLine()) != null)
			{
				if ((strLine.replaceAll("\\s+","")).length() == 0) continue;
				else if (strLine.compareTo("Locations:") == 0)
				{
					strLine = br.readLine();
					String[] aux = strLine.split(" ");
					for (int i = 0; i < aux.length; i++) { locationNames.put(aux[i],i); locationNamesInv.put(i,aux[i]); }
					locations = aux.length;
					pricebrown = new double[locations];
					windsize = new double[locations];
					solarsize = new double[locations];
					locationCapacity = new double[locations];
				}
				else if (strLine.compareTo("PriceBrown:") == 0)
				{
					strLine = br.readLine();
					String[] aux = strLine.split(" ");
					for (int i = 0; i < aux.length; i++) pricebrown[i] = 1;//Double.valueOf(aux[i]);
				}
				else if (strLine.compareTo("IsAt:") == 0)
				{
					strLine = br.readLine();
					String[] aux = strLine.split(" ");
					numHosts = aux.length;
					isat = new int[locations][numHosts];
					for (int i = 0; i < locations; i++) for (int j = 0; j < numHosts; j++) isat[i][j] = 0;
					for (int i = 0; i < numHosts; i++)
					{
						String[] aux2 = aux[i].split(":");
						hostNames.put(aux2[0],i); hostNamesInv.put(i,aux2[0]);
						int indexloct = (locationNames.get(aux2[1])).intValue();
						isat[indexloct][i] = 1;
					}
				}
				else if (strLine.compareTo("WindSize:") == 0)
				{
					strLine = br.readLine();
					String[] aux = strLine.split(" ");
					for (int i = 0; i < aux.length; i++) windsize[i] = Math.ceil(Double.valueOf(aux[i])/100.0);	// Aquests 100 son de pega
				}
				else if (strLine.compareTo("SolarSize:") == 0)
				{
					strLine = br.readLine();
					String[] aux = strLine.split(" ");
					for (int i = 0; i < aux.length; i++) solarsize[i] = Math.ceil(Double.valueOf(aux[i])/100.0);	// Aquests 100 son de pega
				}
				else if (strLine.compareTo("Capacity:") == 0)
				{
					strLine = br.readLine();
					String[] aux = strLine.split(" ");
					for (int i = 0; i < aux.length; i++) locationCapacity[i] = Math.ceil(Double.valueOf(aux[i])/100.0);	// Aquests 100 son de pega
				}
			}
			in.close();

			nmAccount = new double[locations];
			for (int i = 0; i < locations; i++) nmAccount[i] = 0;	// Set to zero the Netmetering account

			if (debugInfo)
			{
				System.out.println("scheduler-GN> Configuration Found\n");
				System.out.println("scheduler-GN> numHosts: " + numHosts + "locations:" + locations + "\n");
				String aux = "";
				for (int i = 0; i < locations; i++) for (int j = 0; j < numHosts; j++) if (isat[i][j] == 1) aux = aux + hostNamesInv.get(j) + " at " + locationNamesInv.get(i) + " (price KWh: " + pricebrown[i] + ")\n";
				System.out.println("scheduler-GN> Placements:\n" + aux);
			}
		}
		catch (Exception e) { System.err.println("Error: " + e.getMessage()); e.printStackTrace(); }
	}

	/* ---------------------------------------------------------- */
	/*  Scheduler                                                 */
	/* ---------------------------------------------------------- */
	private int timestamp = 1;

	protected void schedule ()
	{
		if (debugInfo)
		{
			System.out.print("Location Table: ");
			for (int i = 0; i < isat.length; i++) for (int j = 0; j < isat.length; j++)
			System.out.print(isat[i][j] + " ");
				
			System.out.print("\n");

			System.out.print("Price of Brown: ");
			for (int i = 0; i < pricebrown.length; i++) System.out.print(locationNamesInv.get(i) + ":" + pricebrown[i] + " ");
			System.out.print("\n");

			System.out.print("Wind Size: ");
			for (int i = 0; i < windsize.length; i++) System.out.print(locationNamesInv.get(i) + ":" + windsize[i] + " ");
			System.out.print("\n");

			System.out.print("Solar Size: ");
			for (int i = 0; i < solarsize.length; i++) System.out.print(locationNamesInv.get(i) + ":" + solarsize[i] + " ");
			System.out.print("\n");

			System.out.print("Capacity: ");
			for (int i = 0; i < locationCapacity.length; i++) System.out.print(locationNamesInv.get(i) + ":" + locationCapacity[i] + " ");
			System.out.print("\n");
		}

		if (numHosts != hosts.size()) System.err.println("scheduler-GN> Warning! Hosts in Configuration != Hosts in ONE");

		String[] output = new String[2];
		Map<String,Double> powerVM = new HashMap <String,Double>();

		/* Get Power for each host and average of its VMs */
		double maxLoadHost = 0;
		double[] prevload = new double[locations];
		double[] prevloadHost = new double[numHosts];
		for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet())
		{
			hostInfo auxhost = entry.getValue();
			double vmshare = 0.0;
			if (false)
			{
				/* FIXME - Change when needed: Command to get info from PowerMeter */
				execCommand("parasol monitor " + auxhost.id + " | grep \"power\"",output); 
				vmshare = Double.valueOf((output[0].split("\\s+"))[2])/auxhost.running_vms;
				maxLoadHost = 260; // FIXME - Collect max capacity for the host
			}
			else
			{
				vmshare = 60.0; // Watts in VM (1CPU) by default in Xeon 4Core Petecander
				maxLoadHost = 260; // Max Watts by default in Xeon 4Core Petecander
			}

			//if (auxhost.running_vms == 0) powerVM.put(auxhost.hostname,0.0);
			//else
			powerVM.put(auxhost.hostname,vmshare);

			//System.out.println("scheduler-GN> AAA [" + auxhost.hostname + "] " + auxhost.running_vms + " " + hostNames.get(auxhost.name));
			//System.out.println("scheduler-GN> AAA [" + auxhost.hostname + "] " + powerVM.get(auxhost.hostname));

			prevloadHost[hostNames.get(auxhost.name)] = auxhost.running_vms * vmshare;

			/*if (debugInfo)*/ System.out.println("scheduler-GN> A host " + auxhost.id + " with " + auxhost.state + " status found consuming " + (vmshare * auxhost.running_vms) + "...");
		}

		/* For each location get sum of prevloadHost */
		for(int i = 0; i < locations; i++) 
		{
			prevload[i] = 0;
			for(int j = 0; j < numHosts; j++) if (isat[i][j] == 1) prevload[i] = prevload[i] + prevloadHost[j];
		}

		/* Get Power produced and PUE Factor at each location for next round */
		double[][] auxLectures = receiveLectures();

		double[] production = new double[locations];
		double[] pueFactor = new double[locations];
		for(int i = 0; i < locations; i++)
		{
			production[i] = auxLectures[i][0];
			pueFactor[i] = auxLectures[i][1];
		}

		/* Run solver to put VMs into PMs */
		double[] retLocload = new double[locations];
		double[] migrLoad = new double[locations];
		double[] excessLoad = new double[locations];
		double[] greenLoad = new double[locations];
		double[] netMetLoad = new double[locations];

		double[] mpos = new double[locations];
		//double[] mneg = new double[locations];
		boolean[] mbin = new boolean[locations];

		try
		{
			GRBEnv env = new GRBEnv();
			env.set(GRB.IntParam.OutputFlag, 0);
			GRBModel model = new GRBModel(env);

			/* Variables */
			GRBVar[] load = new GRBVar[locations];
			GRBVar[] migration = new GRBVar[locations];
			GRBVar[] greend = new GRBVar[locations];
			GRBVar[] excess = new GRBVar[locations];
			GRBVar[] mpositive = new GRBVar[locations];
			GRBVar[] mbinary = new GRBVar[locations];
			for(int i = 0; i < locations; i++) 
			{
				load[i] = model.addVar(0.0,9E+15, 0.0, GRB.CONTINUOUS, "load("+String.valueOf(i)+")");
				migration[i] = model.addVar(-9E15,9E+15, 0.0, GRB.CONTINUOUS, "migration("+String.valueOf(i)+")");
				greend[i] = model.addVar(0.0,9E+15, 0.0, GRB.CONTINUOUS, "greend("+String.valueOf(i)+")");
				excess[i] = model.addVar(0.0,9E+15, 0.0, GRB.CONTINUOUS, "excess("+String.valueOf(i)+")");
				mpositive[i] = model.addVar(0.0,9E+15, 0.0, GRB.CONTINUOUS, "mpositive("+String.valueOf(i)+")");
				mbinary[i] = model.addVar(0.0,1.0, 0.0, GRB.BINARY, "mbinary("+String.valueOf(i)+")");
			}
			model.update();

			/* Objective Function */
			GRBLinExpr expr = new GRBLinExpr();
			for(int i = 0; i < locations; i++) 
			{
				expr.addTerm(pricebrown[i] * pueFactor[i], mpositive[i]);
				expr.addTerm(-1 * pricebrown[i], greend[i]);
				expr.addTerm(pricebrown[i], excess[i]);
			}
			model.setObjective(expr, GRB.MINIMIZE);

			/* Constraints */

			/* sum(Load) = sum(Previous Load) */
			expr = new GRBLinExpr();
			double auxsum = 0.0;
			for(int i = 0; i < locations; i++) 
			{
				expr.addTerm(1.0, load[i]);
				auxsum = auxsum + prevload[i];
			}
			model.addConstr(expr, GRB.EQUAL, auxsum, "conservation");

			for(int i = 0; i < locations; i++) 
			{
				/* Load + Migrated (from here) = Previous Load */
				expr = new GRBLinExpr();
				expr.addTerm(1.0, load[i]); expr.addTerm(1.0, migration[i]);
				model.addConstr(expr, GRB.EQUAL, prevload[i], "migration("+String.valueOf(i)+")");

				/* Load <= Location Capacity */
				expr = new GRBLinExpr();
				expr.addTerm(1.0, load[i]);
				model.addConstr(expr, GRB.LESS_EQUAL, locationCapacity[i], "capacity("+String.valueOf(i)+")");

				/* Load * PUE  + Green (for Migration) <= Production + Excess (energy from the grid) [+ NetMetering (in Account)] */
				expr = new GRBLinExpr();
				expr.addTerm(pueFactor[i], load[i]); expr.addTerm(-1.0, excess[i]); expr.addTerm(1.0, greend[i]);
				model.addConstr(expr, GRB.LESS_EQUAL, (production[i] + nmAccount[i]), "production("+String.valueOf(i)+")");

				/* Green (for Migration) <= Production */
				expr = new GRBLinExpr();
				expr.addTerm(1.0, greend[i]);
				model.addConstr(expr, GRB.LESS_EQUAL, production[i], "green-limit1("+String.valueOf(i)+")");

				/* Green (for Migration) <= MPositive (from here) * PUE */
				expr = new GRBLinExpr();
				expr.addTerm(pueFactor[i], mpositive[i]);
				model.addConstr(expr, GRB.GREATER_EQUAL, greend[i], "green-limit2("+String.valueOf(i)+")");

				/* MPositive <- Migration, only if Migration > 0 */

				/* IF mig > 0 THEN mbin = 1 */
				expr = new GRBLinExpr();
				expr.addTerm(9E+15, mbinary[i]);
				model.addConstr(expr, GRB.GREATER_EQUAL, migration[i], "positivemig1("+String.valueOf(i)+")");

				/* IF mig <= 0 THEN mbin = 0 */
				expr = new GRBLinExpr();
				expr.addTerm(1.0, migration[i]); expr.addTerm(-9E+15, mbinary[i]);
				model.addConstr(expr, GRB.GREATER_EQUAL, -9E+15, "positivemig2("+String.valueOf(i)+")");

				/* IF mbin = 1 THEN MPositive = Migration */
				expr = new GRBLinExpr();
				expr.addTerm(9E+15, mbinary[i]); expr.addTerm(1.0,mpositive[i]); expr.addTerm(-1.0,migration[i]);
				model.addConstr(expr, GRB.LESS_EQUAL, 9E+15, "positivemig3("+String.valueOf(i)+")");
				expr = new GRBLinExpr();
				expr.addTerm(9E+15, mbinary[i]); expr.addTerm(-1.0,mpositive[i]); expr.addTerm(1.0,migration[i]);
				model.addConstr(expr, GRB.LESS_EQUAL, 9E+15, "positivemig4("+String.valueOf(i)+")");

				/* IF mbin = 0 THEN MPositive = 0 */
				expr = new GRBLinExpr();
				expr.addTerm(9E+15, mbinary[i]);
				model.addConstr(expr, GRB.GREATER_EQUAL, mpositive[i], "positivemig5("+String.valueOf(i)+")");
			}
			model.optimize();

			/* Retrieve results */
			for(int i = 0; i < locations; i++)
			{
				retLocload[i] = load[i].get(GRB.DoubleAttr.X);
				migrLoad[i] = migration[i].get(GRB.DoubleAttr.X);
				excessLoad[i] = excess[i].get(GRB.DoubleAttr.X);
				greenLoad[i] = greend[i].get(GRB.DoubleAttr.X);

				mpos[i] = mpositive[i].get(GRB.DoubleAttr.X);
				mbin[i] = (mbinary[i].get(GRB.DoubleAttr.X) > 0)?true:false;

				/* NetMetering */
				netMetLoad[i] = 0;
//				netMetLoad[i] = production[i] + excessLoad[i] - retLocload[i] - greenLoad[i];
				if (netMetLoad[i] > 0 && excessLoad[i] == 0) nmAccount[i] = nmAccount[i] + netMetLoad[i];	// We had energy surplus
				if (netMetLoad[i] > 0 && excessLoad[i] > 0) nmAccount[i] = nmAccount[i] - netMetLoad[i];	// We got energy from netmetering
				if (netMetLoad[i] < 0) netMetLoad[i] = 0; // This should never happen		
			}

			/* Delete model from memory */
			model.dispose();
			env.dispose();
		}
		catch (GRBException e) { System.out.println("Error code: " + e.getErrorCode() + ". " + e.getMessage()); e.printStackTrace(); }

		System.out.println("Solution> PLACE: Capacity Production Load (Load*PUE) Excess GreenMig Migs (Migs*PUE) MPos MBin NetMet");
		for (int i = 0; i < locations; i++)
			System.out.println("Solution> " + locationNamesInv.get(i) + ": " + locationCapacity[i] + " " + production[i] + " " + retLocload[i] + " " + (retLocload[i] * pueFactor[i]) + " " + excessLoad[i] + " " + greenLoad[i] +  " " + migrLoad[i] + " " + (migrLoad[i] * pueFactor[i]) + " " + mpos[i] + " " + mbin[i] + " " + netMetLoad[i]);

		/* Redistribute Load */
		for(int i = 0; i < locations; i++)
		{
			if (migrLoad[i] >= powerVM.get(hostNamesInv.get(i)))// && (retLocload[i]*pueFactor[i]) > production[i])
			{
				System.out.println("Solving Migration for: " + locationNamesInv.get(i) + " M: " + (migrLoad[i]*pueFactor[i]) + " L: " + (retLocload[i]*pueFactor[i]) + " P: " + production[i]);

				List<vmInfo> ls = new ArrayList<vmInfo>();

				/* Get list of Hosts to empty and VMs to move */
				String hostname = hostNamesInv.get(i);
				for (Map.Entry<Integer,vmInfo> entry : vms.entrySet())
				{
					if (hostname.compareTo(entry.getValue().hostname) == 0) ls.add(entry.getValue());
				}
				
				/* PRINT list of VMs to move (tentatively) away */
				System.out.print("VMs to (tentative) move: ");
				Iterator iterator = ls.iterator();
				while(iterator.hasNext()) System.out.print(((vmInfo)iterator.next()).id + " ");
				System.out.print("\n");

				/* Number of VMs to move to migrate enough power */
				//int exitvms = new Double(Math.ceil(migrLoad[i]/powerVM.get(hostNamesInv.get(i)))).intValue();
				int exitvms = ls.size();

				/* For each VM to evac, search candidate */
				for (int j = 0; j < exitvms; j++)
				{
					if (!ls.isEmpty())
					{
						vmInfo auxvm = ls.remove(0);

						if ((auxvm.lcm_state).compareTo("RUNNING") == 0)
						{
							boolean found = false;

							/* Get target Host to fill (newload >= currentload + VM) */
							for(int k = 0; k < numHosts; k++)
							{
								if (hostNamesInv.get(k).compareTo(hostNamesInv.get(i))==0) continue;

								double powervmaux = powerVM.get(hostNamesInv.get(k));
								if (migrLoad[k] + powervmaux < (powervmaux-10) && !found)	// (powervmaux-10) -> marge de tolerancia marrano
								{
									migrLoad[k] = migrLoad[k] + powervmaux;
									migrLoad[i] = migrLoad[i] - powervmaux;
									
									String targethost = hostNamesInv.get(k);

									hostInfo auxhi = null;
									for (Map.Entry<Integer,hostInfo> entry : hosts.entrySet())
										if (targethost.compareTo(entry.getValue().name) == 0)
											auxhi = entry.getValue();

									/*if (debugInfo)*/ System.out.println("scheduler-GN> Moving VM " + auxvm.id + " from "  + locationNamesInv.get(i) + " (" + auxvm.hostname + ") to " + locationNamesInv.get(k) + " (" + targethost + ") " + migrLoad[i] + " " + migrLoad[k] + " " + powervmaux);

									if (isVBox) operateVM(new int[]{auxvm.id, auxhi.id},MIGRATEVM);
									else operateVM(new int[]{auxvm.id, auxhi.id},LIVEMIGRATEVM);

									found = true;
								}
							}
							if (!found) /*if(debugInfo)*/ System.out.println("scheduler-GN> No place for VM " + auxvm.id + " (" + auxvm.hostname + "). Keeping with brown...\n");
						}
					}
				}
				System.out.println("Migration Solved");
			}
		}
		timestamp = timestamp + 1;
	}

	/* ---------------------------------------------------------- */
	/* Function to get production info from all locations         */
	/* ---------------------------------------------------------- */

	private double[][] receiveLectures()
	{
		double[][] retval = new double[locations][2];
		for (int i = 0; i < locations; i++) { retval[i][0] = 0.0; retval[i][1] = 0.0; }

		try
		{
			if (!((new File("datasets/greenNebula.data."+timestamp)).exists()))
			{
				System.out.println("scheduler-GN> End of Data Input. Closing...\n");
				for (Map.Entry<Integer,vmInfo> entry : vms.entrySet()) operateVM(new int[]{entry.getKey().intValue()},STOPVM);
				System.exit(0);	// Run out of files, END
			}

			DataInputStream in = new DataInputStream(new FileInputStream("datasets/greenNebula.data."+timestamp));
			BufferedReader br = new BufferedReader(new InputStreamReader(in));

			String strLine;
			while ((strLine = br.readLine()) != null)
			{
				String[] aux = strLine.split(",");
				if (aux[0].charAt(0) != '#')
				{
					/* Solar Production */
					double ghi = Double.valueOf(aux[14]);										// Wh/m^2
					double temperature = Double.valueOf(aux[7]);								// Celsius
					double ghi_stc = 1000.0;													// W/m^2
					double temp_stc = 25.0;														// Celsius
					double temp_prime = temperature - temp_stc;									// difference Celsius
					double ghi_prime = ghi / ghi_stc;											// units
					double k1 = -0.017162;														// Coeff K1 c-Si
					double k2 = -0.040289;														// Coeff K2 c-Si
					double k3 = -0.004681;														// Coeff K3 c-Si
					double k4 = +1.48E-4;														// Coeff K4 c-Si
					double k5 = +1.69E-4;														// Coeff K5 c-Si
					double k6 = +5E-6;															// Coeff K6 c-Si
					double eff_rel = 1.0 + k1*Math.log(ghi_prime) + k2*(Math.pow((Math.log(ghi_prime)),2)) + temp_prime*(k3 + k4*Math.log(ghi_prime) + k5*(Math.pow((Math.log(ghi_prime)),2)) + k6*temp_prime);
					if (eff_rel > 1.0) eff_rel = 1.0;
					if (eff_rel < 0.0 || (new Double(eff_rel).isNaN())) eff_rel = 0.0;
					double g_factor = (ghi / ghi_stc);											// units
					double solarfactor = g_factor * eff_rel * 1.0 / 1.0;						// W / W[base] (Performance)

					/* Wind Production */
					double pressure = Double.valueOf(aux[10]);									// Pa
					temperature = Double.valueOf(aux[7]) + 273.15;								// Kelvin = Celsius + 273.15
					double velocity = Double.valueOf(aux[22]);									// m/s
					double molar_mass = 0.0289644;												// Kg/mol
					double univ_gas_ctan = 8.31447;												// J/(mol*K)
					double density = (pressure * molar_mass) / (univ_gas_ctan * temperature);	// Kg/m^3
					double area = 12668;														// m^2 (ROTOR E-126)
					double cutt_off = 25;														// m/s (ROTOR E-126)
					double[] lambda = {0,0.100,0.263,0.352,0.423,0.453,0.470,0.478,0.477,0.483,0.470,0.429,0.381,0.329,0.281,0.236,0.199,0.168,0.142,0.122,0.105,0.092,0.080,0.071,0.063,0.0}; //units (ROTOR E-126)
					velocity = (velocity > cutt_off)?0:velocity;								// m/s
					double selLambda = lambda[new Double(Math.floor(velocity)).intValue()];		// units
					double windfactor = (0.5 * area * Math.pow(velocity,3) * density * selLambda) / 7500000; // W / W[ROTOR E-126] (Performance)

					if (locationNames.containsKey(aux[0]))
					{
						int i = locationNames.get(aux[0]);
						retval[i][0] = Math.ceil(windsize[i] * windfactor * 0.82 + solarsize[i] * solarfactor * 0.82);
						retval[i][1] = findPUE(temperature);
						if (debugInfo) System.out.println(aux[0] + " " + eff_rel + " " + retval[i][0] + " " + windsize[i] + " " + windfactor + " " + solarsize[i] + " " + solarfactor);
					}
				}
			}
			in.close();
		}
		catch (Exception e) { System.err.println("Error Lectures [t=" + timestamp + "] :" + e.getMessage()); e.printStackTrace(); }

		return retval;
	}

	private double findPUE (double number)
	{
		if (number < 18) return 1.276236;
		if (number < 19) return 1.321002;
		if (number < 20) return 1.321002;
		if (number < 21) return 1.3554;
		return 1.693548;
	}
}
