# MILP Cloud Scheduling Model
ILP Models on my PhD Thesis (2009-2013)

This models a datacenter scheduling optimizer, where jobs are web-services (modeled after Li-BCN workloads) and machines are intel Xeon. The current files are a sample of the automatically adapted scenario models and scheduling rounds. The QoS and SLA implemented in the ILP are defined in the Thesis.

* PhD Thesis: http://hdl.handle.net/10803/134360
* Li-BCN: http://www.lsi.upc.edu/dept/techreps/llistat_detallat.php?id=1099

## Files

### MILP Model Examples
* **temp125.mod**: Model in MathProg of the given Cloud
* **temp125.dat**: A scheduling scenario (10 machines x 24 jobs)
* **wrapper_gurobi.c**: A wrapper created by Matteo Salani, modified by me to not trigger GLPK in case of sub-optimals

### Experiment Files
These files are uploaded only to be contemplated by posterity. They are mostly hardcoded for the experiments and files (e.g. workload paths and experiment parameters). Do not attempt to run them at home...

* **script.\*.r**: Experiment scripts. 0->ILP (no SLA), g1-> Round Robin, g3-> FirstFit, g4-> BestFit, new->ILP (with SLA)
* **functions.r**: Functions for experiment scripts
* **work-reader-xeon2.r**: Functions to parse monitoring data
* **lagrange.r**: Experiment optimizing MILP using Lagrange approach

### Workloads
These files contain traces about the used machines, services and jobs, reproducing their behavior

* **workload\/power.data**: Measured power data for Xeon servers
* **workload\/datacenter-\*.data**: Inventory of machines for experimentation
* **workload\/workload-\*.data**: List of files with workload for experimentation
* **workload\/http-\*.load**: Files with load data

### QuickScheduler
The included files are the implementation of the schedulers in a real Cloud environment (OpenNebula), after MILP experiments were done. They include QuickScheduler and GreenNebula experiments.

## MILP Usage
### GLPK
Execute the Solver
```
glpsol -m temp125.mod -d temp125.dat -o temp125.sol --mipgap 100 --log temp125.log
```

### GUROBI
Compile the wrapper, set-up licences, etc...
```
gcc wrapper_gurobi.c -I $GUROBI_PATH/include/ -L $GUROBI_PATH/lib/ -lgurobi75 -lm -lglpk -o wrapper_GLPK_GUROBI
export GRB_LICENSE_FILE=$GUROBI_PATH/gurobi.lic
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GUROBI_PATH/lib/
```
Execute the Solver
```
./wrapper_GLPK_GUROBI -m temp125.mod -d temp125.dat --glpk_out --grb_out -v --tmlim 100 > temp125.sol
```

## Datasets eEnergy'10
These files contain datasets belonging to the eEnergy'10 paper, with data for modeling Intel Xeon power consumption, and modeling SLAs on the Grid5000 benchmark using the EEFSIM.

The published data are a sample of the measured power outputs and SLAs on HPC tasks. You can use them under your own responsibility. In case of using them, cite our paper "Towards energy-aware scheduling in data centers using machine learning" 1st ACM/SIGCOM Intl. Conf. on Energy-Efficient Computing and Networking (eEnergy 2010). http://dx.doi.org/10.1145/1791314.1791349
