# MILP Cloud Scheduling Model
ILP Models on my PhD Thesis (2009-2013)

This models a datacenter scheduling optimizer, where jobs are web-services (modeled after Li-BCN workloads) and machines are intel Xeon. The current files are a sample of the automatically adapted scenario models and scheduling rounds. The QoS and SLA implemented in the ILP are defined in the Thesis.

* PhD Thesis: http://hdl.handle.net/10803/134360
* Li-BCN: http://www.lsi.upc.edu/dept/techreps/llistat_detallat.php?id=1099

## Files
* **temp125.mod**: Model in MathProg of the given Cloud
* **temp125.dat**: A scheduling scenario (10 machines x 24 jobs)
* **wrapper_gurobi.c**: A wrapper created by Matteo Salani, modified by me to not trigger GLPK in case of sub-optimals

## Usage
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
