/***************************************************************************
                          wrapper.c  -  description
                             -------------------
    begin                : May 2010
    copyright            : Matteo Salani
    email                : matteo.sal...@idsia.ch

    update               : March 2011
    author               : Josep Ll. Berral
    email                : jlberral@lsi-upc-edu

    description          : This is a wrapper to process a MILP model 
                           written in GNU - MathProg (http://www.gnu.org/software/glpk/)
                           solve it with Gurobi (http://gurobi.com/)
                           post-process the output with GLPK
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/** please, have GLPK and GUROBI installed **/
#include "glpk.h"
#include "gurobi_c.h"

#define GRB_INT 0
#define GRB_DB 1

/** GLPK: data structures **/
glp_prob *mip = NULL;
glp_tran *tran = NULL;
glp_smcp *parm;
glp_iocp *iparm;
int ret, col_index;
double glpk_iparm_mip_gap = 10E-4;
double glpk_iparm_tol_int = 10E-4;
double glpk_iparm_tol_obj = 10E-4;

/** GUROBI: data structures **/
GRBenv   *env   = NULL; 
GRBenv   *mipenv = NULL;
GRBmodel *model = NULL;
int      numvars, j, retGRB, GRB_IsMIP, GRB_noMIP;
double   db_tmp, bound;
char * nameGRB;
char type;
double grb_feasibilityTol = 10E-6;
double grb_intFeasTol = 10E-5;
double grb_mipGap = 10E-4;

/** GUROBI: user defined parameters **/
typedef struct _grb_udef {
  char * name;
  char type;
  double db_value;
  int int_value;
  struct _grb_udef * next;
} grb_udef;

grb_udef * grb_user_def_params;

/** execution flags **/
char GRB_out, GLPK_out, verbose;

int keep_tmp_mps;

double timelimit;

void freeMem(){
grb_udef * tmp;

  /** GLPK: free user defined parameters **/
  while (grb_user_def_params != NULL){
    tmp = grb_user_def_params;
    grb_user_def_params = grb_user_def_params->next;
    free(tmp);
  }

  /** GLPK: free structures **/
  if (tran) glp_mpl_free_wksp(tran);
  if (mip) glp_delete_prob(mip);

  if (retGRB) printf("ERROR: %s\n", GRBgeterrormsg(env));

  /** GUROBI: free structures **/
  if (model) GRBfreemodel(model);
  if (env) GRBfreeenv(env);

  printf("ERRORS OCCURRED.\nGLPK -> GUROBI -> GLPK wrapper v0.2.1 (2011)\n");

  exit(1);
}

void usage(){
    fprintf(stderr, "GLPK -> GUROBI -> GLPK wrapper v0.2.1 (2011)\nby Matteo Salani (matteo.sal...@idsia.ch)\nupdate Josep Ll. Berral (jlberral@lsi-upc-edu)\n\n"
                    "Usage: \n"
                    "wrapper_GLPK_GUROBI [OPTIONS] -m <model.mod> -d <data.dat>\n"
                    "with OPTIONS:\n"
                    "--glpk_out (enable GLPK output, default disabled)\n"
                    "--grb_out (enable GRB output, default disabled)\n"
                    "--glpk_mip_gap <value> GLPK mip gap (default 10E-4)\n"
                    "--glpk_tol_int <value> GLPK integer tolerance (default 10E-4)\n"
                    "--glpk_tol_obj <value> GLPK objective tolerance (default 10E-4)\n"
                    "--grb_feas_tol <value> GUROBI feasibility tolerance (default 10E-6)\n"
                    "--grb_int_tol <value> GUROBI integer tolerance (default 10E-5)\n"
                    "--grb_set_db_para <parameter name> <value> Set GUROBI double paramenter (example --grb_set_db_para IntFeasTol 10E-5)\n"
                    "--grb_set_int_para <parameter name> <value> Set GUROBI integer paramenter (example --grb_set_int_para ModelSense -1)\n"
                    "--grb_nomip tells Gurobi to solve the linear relaxation only \n"
                    "--keep_tmp_files Keep the temporary mps file\n");
}

void strrep(char *str, char old, char new)  {
    char *pos;
    while (1)  {
        pos = strchr(str, old);
        if (pos == NULL)  {
            break;
        }
        *pos = new;
    }
}

int main(int argc, char *argv[]){
char * file_model;
char * file_data;
char * file_output;
grb_udef * tmp;

  if (argc < 5){
    usage();
    exit(1);
  }

  printf("GLPK -> GUROBI -> GLPK wrapper v0.2.1 (2011)\n\n");

  grb_user_def_params = NULL;
  file_model = file_data = NULL;
  GRB_out = GLPK_out = GRB_noMIP = verbose = keep_tmp_mps = 0;

  for(int i=1 ; i<=argc-1 ; i++){
    if (strcmp(argv[i],"-m")==0) file_model = argv[i+1];
    if (strcmp(argv[i],"-d")==0) file_data = argv[i+1];
    if (strcmp(argv[i],"-y")==0) file_output = argv[i+1];
    if (strcmp(argv[i],"-v")==0) verbose = 1;
    if (strcmp(argv[i],"--tmlim")==0) timelimit = atof(argv[i+1]);
    if (strcmp(argv[i],"--keep_tmp_files")==0) keep_tmp_mps = 1;
    if (strcmp(argv[i],"--glpk_out")==0) GLPK_out = 1;
    if (strcmp(argv[i],"--grb_out")==0) GRB_out = 1;
    if (strcmp(argv[i],"--glpk_mip_gap")==0) glpk_iparm_mip_gap = atof(argv[i+1]);
    if (strcmp(argv[i],"--glpk_tol_int")==0) glpk_iparm_tol_int = atof(argv[i+1]);
    if (strcmp(argv[i],"--glpk_tol_obj")==0) glpk_iparm_tol_obj = atof(argv[i+1]);
    if (strcmp(argv[i],"--grb_feas_tol")==0) grb_feasibilityTol =  atof(argv[i+1]);
    if (strcmp(argv[i],"--grb_int_tol")==0) grb_intFeasTol =  atof(argv[i+1]);
    if (strcmp(argv[i],"--grb_mip_gap")==0) grb_mipGap = atof(argv[i+1]) ;
    if (strcmp(argv[i],"--grb_set_db_para")==0) {
      grb_udef * tmp = (grb_udef *) malloc(sizeof(grb_udef));
      tmp->name = argv[i+1];
      tmp->type = GRB_DB;
      tmp->db_value = atof(argv[i+2]);
      tmp->next = grb_user_def_params; 
      grb_user_def_params = tmp;
    }
    if (strcmp(argv[i],"--grb_set_int_para")==0) {
      grb_udef * tmp = (grb_udef *) malloc(sizeof(grb_udef));
      tmp->name = argv[i+1];
      tmp->type = GRB_INT;
      tmp->int_value = atoi(argv[i+2]);
      tmp->next = grb_user_def_params; 
      grb_user_def_params = tmp;
    }
    if (strcmp(argv[i],"--grb_nomip")==0) GRB_noMIP = 1;
  } 

  if ((file_model==NULL) || (file_data == NULL)){ 
    usage();
    fprintf(stderr, "Error no model or data files provided\n");
    freeMem();
  }

  printf("Wrapper parameters:\n");
  printf("  model: %s\n", file_model);
  printf("  data: %s\n", file_data);
  printf("  verbosity level: %s\n", verbose ? "quiet" : "verbose");
  printf("  keep temporary files: %s\n", keep_tmp_mps ? "on" : "off");
  printf("  GLPK parameters:\n");
  printf("    GLPK output: %s\n", GLPK_out ? "on" : "off");
  printf("    GLPK Mip Gap: %lf\n", glpk_iparm_mip_gap);
  printf("    GLPK Int Tolerance: %lf\n", glpk_iparm_tol_int);
  printf("    GLPK Objective Tolerance: %lf\n", glpk_iparm_tol_obj);
  printf("  GUROBI parameters:\n");
  printf("    GUROBI output: %s\n", GRB_out ? "on" : "off");
  printf("    GUROBI Feasibility tolerance: %lf\n", grb_feasibilityTol);
  printf("    GUROBI Integer Feasibility tolerance: %lf\n", grb_intFeasTol);
  if (grb_user_def_params != NULL){
    printf("  USER DEF GUROBI parameters:\n");
    tmp = grb_user_def_params;
    while (tmp != NULL){
      if (tmp->type == GRB_INT){
        printf("    GUROBI %s : %d\n", tmp->name, tmp->int_value);
      }
      else if (tmp->type == GRB_DB){
        printf("    GUROBI %s : %lf\n", tmp->name, tmp->db_value);
      }
      tmp = tmp->next;
    }
  }
  printf("\n\n");

  /** GLPK: Open environment **/
  mip = glp_create_prob();
  tran = glp_mpl_alloc_wksp();

  glp_term_out(GLPK_out?GLP_ON:GLP_OFF);

  /** GLPK: Read model written in MathProg **/
  ret = glp_mpl_read_model(tran, file_model, 1);

  if (ret){ 
    fprintf(stderr, "Error on translating model\n");
    freeMem();
  }
  
  /** GLPK: Read data for MathProg **/
  ret = glp_mpl_read_data(tran, file_data);
  if (ret){ 
    fprintf(stderr, "Error on translating data\n");
    freeMem();
  }

  /** GLPK: Generate model (merge data an model) **/
  ret = glp_mpl_generate(tran, NULL);
  if (ret){ 
    fprintf(stderr, "Error on generating model\n");
    freeMem();
  }

  /** GLPK: Generate Build Model **/
  glp_mpl_build_prob(tran, mip);
          
  /** GLPK: Generate Variable indexing **/
  glp_create_index(mip);

  /** GLPK: Generate LP **/
 // glp_write_mps(mip, GLP_MPS_FILE, NULL, "model.mps");
 glp_write_lp(mip, NULL, "model.lp");

  /************/
  /** GUROBI **/
  /************/

  retGRB = GRBloadenv(&env, NULL);
  if (retGRB || env == NULL)
  {
    fprintf(stderr, "Error: could not create environment\n");
    exit(1);
  }

  retGRB = GRBsetintparam(env, "OutputFlag", GRB_out?1:0);
  if (retGRB) freeMem();

  /** GUROBI: Read model **/
  retGRB = GRBreadmodel(env, "model.lp", &model);
  if (retGRB) freeMem();

  /** Remove utility files from disk **/
  if (!keep_tmp_mps) remove("model.lp");

  /** GUROBI: Get environment **/
  mipenv = GRBgetenv(model);
  if (!mipenv) freeMem();

  /** GUROBI: Set parameters **/

  /** GUROBI: Ask for more precision **/
  retGRB = GRBsetdblparam(mipenv, "FeasibilityTol", grb_feasibilityTol);
  if (retGRB) freeMem();
  retGRB = GRBsetdblparam(mipenv, "IntFeasTol", grb_intFeasTol);
  if (retGRB) freeMem();
  retGRB = GRBsetdblparam(mipenv, "MIPgap", grb_mipGap);
  if (retGRB) freeMem();

  /** GUROBI: Time Limit **/
  retGRB = GRBsetdblparam(GRBgetenv(model), "TimeLimit", timelimit);
  if (retGRB) freeMem();

  /** Set other user-defined parameters for Gurobi **/
  tmp = grb_user_def_params;
  while (tmp != NULL){
    if (tmp->type == GRB_INT){
      retGRB = GRBsetintparam(mipenv, tmp->name, tmp->int_value);
      if (retGRB) freeMem();
    }
    else if (tmp->type == GRB_DB){
      retGRB = GRBsetdblparam(mipenv, tmp->name, tmp->db_value);
      if (retGRB) freeMem();
    }
    tmp = tmp->next;
  }

  /** GUROBI: get numvars and numrows **/
  retGRB = GRBgetintattr(model, "NumVars", &numvars);
  if (retGRB) freeMem();

  /** GUROBI: get model type **/
  retGRB = GRBgetintattr(model, "IsMIP", &GRB_IsMIP);
  if (retGRB) freeMem();

  /** GUROBI: Solve the linear relaxation **/
  if (GRB_noMIP){
    int numintvars; int j; char vtype;
    /* Collect integer variables and relax them */
    retGRB = GRBgetintattr(model, "NumIntVars", &numintvars);
    if (retGRB) freeMem();
    for (j = 0; j < numintvars; ++j)
    {
      retGRB = GRBgetcharattrelement(model, "VType", j, &vtype);
      if (retGRB) freeMem();
      if (vtype != GRB_CONTINUOUS)
      {
        retGRB = GRBsetcharattrelement(model, "VType", j, GRB_CONTINUOUS);
        if (retGRB) freeMem();
      }
    }
  }

  /** GUROBI: Optimize model **/
  retGRB = GRBoptimize(model);
  //if (retGRB) freeMem();
  int timeout = 0;

  /** GUROBI: Retreive the optimization status **/
  GRBgetintattr(model, "Status", &retGRB);
  switch(retGRB){
    case GRB_OPTIMAL:
        break;
    case GRB_TIME_LIMIT :
        fprintf(stderr, "Warning GRB optimization exited with code GRB_TIME_LIMIT \n");
	timeout = 1;
        break;
    case GRB_INFEASIBLE :
        fprintf(stderr, "Error GRB optimization failed with code GRB_INFEASIBLE\n");
    case GRB_INF_OR_UNBD :
        fprintf(stderr, "Error GRB optimization failed with code GRB_INF_OR_UNBD \n");
    case GRB_UNBOUNDED :
        fprintf(stderr, "Error GRB optimization failed with code GRB_UNBOUNDED \n");
    case GRB_CUTOFF :
        fprintf(stderr, "Error GRB optimization failed with code GRB_CUTOFF \n");
    case GRB_ITERATION_LIMIT :
        fprintf(stderr, "Error GRB optimization failed with code GRB_ITERATION_LIMIT \n");
    case GRB_NODE_LIMIT :
        fprintf(stderr, "Error GRB optimization failed with code GRB_NODE_LIMIT \n");
    case GRB_SOLUTION_LIMIT :
        fprintf(stderr, "Error GRB optimization failed with code GRB_SOLUTION_LIMIT \n");
    case GRB_INTERRUPTED :
        fprintf(stderr, "Error GRB optimization failed with code GRB_INTERRUPTED \n");
    case GRB_SUBOPTIMAL :
        fprintf(stderr, "Error GRB optimization failed with code GRB_SUBOPTIMAL \n");
    case GRB_NUMERIC :
        fprintf(stderr, "Error GRB optimization failed with code GRB_NUMERIC \n");

    /** GUROBI: Quit in any case non optimal **/
    freeMem();
  }

  /** GUROBI: Get obj function value **/
  if (!GRB_noMIP){
    retGRB = GRBgetdblattr(model, "IntVio", &db_tmp);
    if (retGRB) freeMem();

    retGRB = GRBgetdblattr(model, "ObjBound", &bound);
    if (retGRB) freeMem();
  }

  retGRB = GRBgetdblattr(model, "ObjVal", &db_tmp);
  if (retGRB) freeMem();
  
  if (verbose) printf ("Objective %lf\n", db_tmp);
  if (verbose) printf ("Best bound %lf\n", bound);
  if (verbose) printf ("Absolute gap %lf\n", fabs(db_tmp - bound));

  /** GUROBI: Get variable values **/

  for (j = 0; j < numvars; ++j){

    retGRB = GRBgetdblattrelement(model, "X", j, &db_tmp);
    if (retGRB) freeMem();

    retGRB = GRBgetstrattrelement(model, "VarName", j, &nameGRB);
    if (retGRB) freeMem();

    retGRB = GRBgetcharattrelement(model, "VType", j, &type);
    if (retGRB) freeMem();

    /** GLPK search variable index by name **/
    strrep(nameGRB, '(', '[');
    strrep(nameGRB, ')', ']');
    col_index = glp_find_col(mip, nameGRB);

    if (col_index != 0){ 
      /** GLPK set variable bounds **/
      if ((type == 'B') || (type == 'I')){
        if (verbose) printf ("Variable %s is of type %c value %lf fixed to %lf\n", nameGRB, type, db_tmp, round(db_tmp));
        glp_set_col_bnds(mip, col_index, GLP_FX, round(db_tmp), round(db_tmp));
      }
      else{
        if (verbose) printf ("Variable %s is of type %c value %lf fixed to %lf\n", nameGRB, type, db_tmp, db_tmp);
        glp_set_col_bnds(mip, col_index, GLP_FX, db_tmp, db_tmp);
      }
    }
  }

/************************************************/
/* Update: In case of sub-optimal solutions,    */
/* (through TimeLimit) prevents to start GLPK,  */
/* as it may take forever...                    */
/************************************************/

  /** GLPK: free structures **/
  if (tran) glp_mpl_free_wksp(tran);
  if (mip) glp_delete_prob(mip);

  if (retGRB) printf("ERROR: %s\n", GRBgeterrormsg(env));

  /** GUROBI: free structures **/
  if (model) GRBfreemodel(model);
  if (env) GRBfreeenv(env);

  printf("Done.\nGLPK -> GUROBI  wrapper v0.2.1 (2011)\n");

  exit(0);

/************************************************/
/* End of Update                                */
/************************************************/

  if ((GRB_IsMIP) && (!GRB_noMIP)){
	  
    /** GLPK initialize parameters **/
    iparm = (glp_iocp*) malloc(sizeof(glp_iocp));
    glp_init_iocp(iparm);
    iparm->presolve = GLP_ON;
    iparm->mip_gap = glpk_iparm_mip_gap;
    iparm->tol_int = glpk_iparm_tol_int;
    iparm->tol_obj = glpk_iparm_tol_obj;

    /** GLPK get the optimal integer solution **/
    ret = glp_intopt(mip, iparm);
    if (ret){ 
      fprintf(stderr, "glp_intopt, Error on optimizing the model : %d \n", ret);
      freeMem();
    }

    ret = glp_mip_status(mip);
    switch (ret){
	    case GLP_OPT:
	    break;
	    case GLP_FEAS:
	      fprintf(stderr, "Error GLPK simplex is not optimal, GLP_FEAS, code %d\n", ret);
	      freeMem();
	    case GLP_NOFEAS:
	      fprintf(stderr, "Error GLPK simplex is not optimal, GLP_NOFEAS, code %d\n", ret);
	      freeMem();
	    case GLP_UNDEF:
	      fprintf(stderr, "Error GLPK simplex is not optimal, GLP_UNDEF, code %d\n", ret);
	      freeMem();
    }
  }
  else{

    /** GLPK initialize parameters **/
    parm = (glp_smcp*) malloc(sizeof(glp_smcp));
    glp_init_smcp(parm);
    parm->meth = GLP_DUALP;
    parm->tol_bnd = 10E-4;
    parm->tol_dj = 10E-4;
  
    /** GLPK get the optimal basis **/
    //ret = glp_simplex(mip, parm);
    if (ret){ 
     fprintf(stderr, "glp_simplex, Error on optimizing the model : %d \n", ret);
     freeMem();
    }
    ret = glp_get_status(mip);
    switch (ret){
	    case GLP_OPT:
	    break;
	    case GLP_FEAS:
	      fprintf(stderr, "Error GLPK simplex is not optimal, GLP_FEAS, code %d\n", ret);
	      freeMem();
	    case GLP_INFEAS:
	      fprintf(stderr, "Error GLPK simplex is not optimal, GLP_INFEAS, code %d\n", ret);
	      freeMem();
	    case GLP_NOFEAS:
	      fprintf(stderr, "Error GLPK simplex is not optimal, GLP_NOFEAS, code %d\n", ret);
	      freeMem();
	    case GLP_UNBND:
	      fprintf(stderr, "Error GLPK simplex is not optimal, GLP_UNBND, code %d\n", ret);
	      freeMem();
	    case GLP_UNDEF:
	      fprintf(stderr, "Warning GLPK simplex is not optimal, GLP_UNDEF, code %d\n", ret);
	      freeMem();
    }
  }

  /** GLPK: Perform postprocessing **/
  ret = glp_mpl_postsolve(tran, mip, GLP_MIP);
  if (ret != 0) fprintf(stderr, "Error on postsolving model\n");

  /** GLPK: free structures **/
  if (tran) glp_mpl_free_wksp(tran);
  if (mip) glp_delete_prob(mip);

  if (retGRB) printf("ERROR: %s\n", GRBgeterrormsg(env));

  /** GUROBI: free structures **/
  if (model) GRBfreemodel(model);
  if (env) GRBfreeenv(env);

  printf("Done.\nGLPK -> GUROBI -> GLPK wrapper v0.2.1 (2011)\n");

  exit(0);
}

