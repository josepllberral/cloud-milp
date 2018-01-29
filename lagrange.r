
################################################################################
# Inicialització i Workloads
################################################################################

library(glpk);

################################################################################
# Funcions i tal
################################################################################

crearModel <- function(l) {

	content <- paste("

	set jobs;
	set hosts;

	param mcpu {h in hosts};
	param jcpu {j in jobs};
	param lam {h in hosts};

	var x {h in hosts, j in jobs}, integer, binary;
	var run1;
	var usage {h in hosts};

	maximize benf : (sum{h in hosts, j in jobs} x[h,j])  + sum{h in hosts} (lam[h]) * (mcpu[h] * 100 - sum{j in jobs} x[h,j]*jcpu[j]);

	s.t. job1  : run1  = sum{h in hosts, j in jobs} x[h,j];
	s.t. usg1 {h in hosts}  : usage[h] = sum{j in jobs} x[h,j]*jcpu[j];

	s.t. unicitat {j in jobs} : sum{h in hosts} x[h,j] = 1;

data;

set hosts := Host1 Host2 Host3 Host4 Host5 Host6 Host7 Host8 Host9 Host10;
set jobs := Job145 Job146 Job147 Job148 Job149 Job150 Job151 Job152 Job153 Job154 Job155 Job156 Job157 Job158 Job159 Job160 Job161 Job162 Job163 Job164 Job165 Job166 Job167 Job168;
param mcpu :=
Host1 4
Host2 4
Host3 4
Host4 4
Host5 4;

param jcpu :=
Job145 103
Job146 103
Job147 103
Job148 13
Job149 9
Job150 7
Job151 94
Job152 92
Job153 101
Job154 18
Job155 18
Job156 20
Job157 30
Job158 30
Job159 30
Job160 9
Job161 9
Job162 9
Job163 8
Job164 8
Job165 8
Job166 7
Job167 7
Job168 7;

param lam :=
Host1 ",l[1],"
Host2 ",l[2],"
Host3 ",l[3],"
Host4 ",l[4],"
Host5 ",l[5],"
Host6 ",l[6],"
Host7 ",l[7],"
Host8 ",l[8],"
Host9 ",l[9],"
Host10 ",l[10],";

end;"
,sep="");

	write(content, "mlg1.mod", sep="", append=FALSE);
}

################################################################################
# Inicialització i Workloads
################################################################################

l <- rep(1.1,10);
s <- 0.2;

soll <- rep(0,10);

nits <- 0;
niits <- 0;
oldb <- -99999999;
oldbi <- 99999999;

trobat <- FALSE;

for (it in 1:5000) {

	crearModel(l);

	lp <- lpx_read_model(model="mlg1.mod");

	lpx_simplex(lp);
	ub <- lpx_get_obj_val(lp);

	lpx_integer(lp);

	b <- lpx_mip_row_val(lp,1);
	for (i in 1:10) u[i] <- lpx_mip_col_val(lp,i+241)

	print(paste("+",it," Benef: ",b," U: ",u, " lambda: ", l, " step: ", s));

	if ((sum(u <= 400) == 10) && (sum(u)==843)&& (oldb < b) ) {
		oldb <- b;
		soll <- l;
		trobat <- TRUE;
		nits <- 0;
	} 
	if (oldbi > b) { oldbi <- b; nits <- 0; }

	if (oldb >= b || b >= oldbi) {
		nits <- nits + 1;
		if (nits > 10) {
			s <- s/2;
			nits <- 0;
		}
	}

#	for (i in 1:10) {
#		l[i] <- l[i] + (s*(u[i] - 400)/((400-u[i])^2));
#	}

	for (i in 1:10) {
		oli <- l[i];
		if (u[i] < 400) { l[i] <- l[i] - s; }
		else if (u[i] > 400) { l[i] <- l[i] + s; }
		#if ((u[i] < 400 || u[i] > 400) && l[i]==oli) print("Precision ERROR");
	}

	l[l<0] <- 0;
}

if (trobat) {
	crearModel(soll);

	lp <- lpx_read_model(model="mlg1.mod");

	lpx_simplex(lp);
	lpx_integer(lp);
	##lpx_intopt(lp); # <- NOT IMPLEMENTED YET

	lpx_mip_row_val(lp,1);	# Z(lambda)
	for (i in 1:24) {
			print (paste(i," ",lpx_mip_col_val(lp,i)," ",lpx_mip_col_val(lp,i+1*24)," ",lpx_mip_col_val(lp,i+2*24)," ",lpx_mip_col_val(lp,i+3*24)," ",lpx_mip_col_val(lp,i+4*24)," ",lpx_mip_col_val(lp,i+5*24)," ",lpx_mip_col_val(lp,i+6*24)," ",lpx_mip_col_val(lp,i+7*24)," ",lpx_mip_col_val(lp,i+8*24)," ",lpx_mip_col_val(lp,i+9*24)));
	}


	b <- lpx_mip_row_val(lp,1);
	for (i in 1:10) u[i] <- lpx_mip_col_val(lp,i+241)

	power <- 0;
	for (i in 1:10) {
		if (u[i]<=100) power <- power + 267.8;
		if (u[i]>100 && u[i]<=200) power <- power + 285.5;
		if (u[i]>200 && u[i]<=300) power <- power + 302.5;
		if (u[i]>300 && u[i]<=400) power <- power + 317.9;
	}

	benef <- (b * 0.17) - (power * 9e-05);

	print (paste("benefici: ",benef));
	print (paste("usage   : ",u));
} else {
	print("Solució No Trobada");
}





















