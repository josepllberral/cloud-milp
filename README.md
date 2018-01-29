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
These files are uploaded only to be contemplated by posterity. They are mostly hardcoded for the experiments and files. Do not attempt to run them at home...

* **script.\*.r**: Experiment scripts. 0->ILP (no SLA), g1-> Round Robin, g3-> FirstFit, g4-> BestFit, new->ILP (with SLA)
* **functions.r**: Functions for experiment scripts
* **work-reader-xeon2.r**: Functions to parse monitoring data

### Workloads
These files contain traces about the used machines, services and jobs, reproducing their behavior

* **power.data**: Measured power data for Xeon servers
* **datacenter\*.data**: Inventory of machines for experimentation
* **workload.\*.data**: List of files with workload for experimentation
* **workload\/\*.load**: Files with load data

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
