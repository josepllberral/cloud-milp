workload <- NULL; hosts <- NULL; RT <- NULL;
mon <- NULL; tue <- NULL; wed <- NULL; thu <- NULL; fri <- NULL; sat <- NULL;
sun <- NULL; lab <- NULL; wnd <- NULL;

################################################################################
# Inicialització i Workloads
################################################################################

# Directori Base
base <- "/home/jlberral/Desktop/sim-lip/";
workprefix <- paste(base,"/workload/http-",sep="");

# Workload i Hosts
hosts <- read.table(paste(base,"datacenter-inv1.data",sep=""),header=T);
#wkld <- read.table(paste(base,"workload-tomcatGRID2010.data",sep=""),header=T);
wkld <- read.table(paste(base,"workload-wservice1.data",sep=""),header=T);

# Muntatge de la Timetable
aux <- NULL;
for (workset in 1:length(wkld[,1]))
{
	itime <- wkld[workset,"time"];
	file <- paste(workprefix,wkld[workset,"load.type"],".load",sep="");
	daya <- read.table(file,header=T);

	for (entry in 1:length(daya[,1])) {
		atime <- daya[entry,"Time"] + itime;
		clients <- daya[entry,"req.s"];
		aux <- c(aux,workset,atime,clients);
	}
}
dim(aux) <- c(3,(length(aux)/3));
aux <- t(aux);
timetable <- aux[order(aux[,2]),];

# Variables de Response Time
s1Rm <- 1.45; s1RM <- 1.87; desA <- 0.025; desB <- 0; MaxDesCPU <- 110;
A <- 162.48; B <- 395.41; C <- 18.41; D <- -18.94; E <- 1.87;
RA <- 32.13; RB <- -72.55; RC <- 44.32;

################################################################################
# Funcions
################################################################################

RT <- function (hostCPU,sumCPUs,reqs)
{
	respTime <- 0;
	if (reqs > 0) {
		desired <- reqs * desA + desB;
		availableCPU <- hostCPU - sumCPUs;
		maxReqs <- (availableCPU/desA) + (desB/desA);
		factor <- maxReqs/reqs;
	
		if ( factor > 1.2 ) {
			respTime <- s1Rm;
		}
		if ( factor > 1 ) {
			respTime <- ((desired/availableCPU) *
				((s1RM-s1Rm)/0.7)) + s1Rm;
	
		}
		if ( factor <= 1 ) {
			respTime <- RA + RB * factor + RC * factor * factor;
		}
	}
	respTime;
}

desCPU <- function (reqs)
{
	desired <- 1; # +1 per evitar divs by 0
	if (reqs > 0) {
		desired <- reqs * desA + desB;
#		availableCPU <- hostCPU - sumCPUs;
#		maxReqs <- (availableCPU/desA) + (desB/desA);
#		factor <- maxReqs/reqs;

#		if ( factor <= 1 ) {
#			desired <- availableCPU;
#		}
	}
	if (desired > MaxDesCPU) desired <- MaxDesCPU;
	desired;
}



crearFitxer <- function(hostList,mcpu,jobList,jcpu,obm) {

	content = "/* Informacio */ data;";

	hd1 <- "set hosts :=";
	for (i in 1:length(hostList)) { hd1 <- paste(hd1,hostList[i]); }
	hd1 <- paste(hd1,";",sep="");
	content <- paste(content,hd1,sep="\n");

	jd1 <- "set jobs :=";
	for (i in 1:length(jobList)) { jd1 <- paste(jd1,jobList[i]); }
	jd1 <- paste(jd1,";",sep="");
	content <- paste(content,jd1,sep="\n");

	hp1 <- "param mcpu :=";
	for (i in 1:length(hostList)) {
		hp1 <- paste(hp1,hostList[i],sep="\n");
		hp1 <- paste(hp1,mcpu[i]);
	}
	hp1 <- paste(hp1,";",sep="");
	content <- paste(content,hp1,sep="\n");

	jp1 <- "param jcpu :=";
	for (i in 1:length(jobList)) {
		jp1 <- paste(jp1,jobList[i],sep="\n");
		jp1 <- paste(jp1,jcpu[i]);
	}
	jp1 <- paste(jp1,";",sep="");
	content <- paste(content,jp1,sep="\n");

	jo1 <- "param jold :=";
	news <- 0;
	for (i in 1:length(jobList)) {
		x <- 0;
		if (length(obm[2,obm[1,]==jobList[i]]) > 0) {
			x <- as.numeric(obm[2,obm[1,]==jobList[i]]);
		}
		if (x==0) news <- news + 1;
		for (j in 1:length(hostList)) {

			jo1 <- paste(jo1,hostList[j],sep="\n");
			jo1 <- paste(jo1,jobList[i]);

			if (x == j) { jo1 <- paste(jo1,1); }
			else { jo1 <- paste(jo1,0); }
		}
	}
	jo1 <- paste(jo1,";",sep="");
	content <- paste(content,jo1,sep="\n");

	content <- paste(content,"param news := ",news,";",sep="\n");

	content <- paste(content,"end;",sep="\n");

	write(content, paste(base,"temp123.dat",sep=""), sep="", append=FALSE);
}

crearModel <- function(lam,eps,thres) {

	content = "
/* Sets */
	set hosts;
	set jobs;

/* parametres */
	param mcpu {h in hosts};
	param jcpu {j in jobs};
	param jold {h in hosts,j in jobs}, integer, binary;
	param news, integer;

/* Variables */
	var x {h in hosts, j in jobs}, integer, binary;
	var y1 {h in hosts}, >=0, binary;
	var y2 {h in hosts}, >=0, binary;
	var y3 {h in hosts}, >=0, binary;
	var y4 {h in hosts}, >=0, binary;
	var s1 {h in hosts}, >=0;

	var k {h in hosts, j in jobs}, >= 0;
	var jcpuvar {j in jobs};

/* Objectiu */
	maximize benf :
	";

	content <- paste(content,"sum{h in hosts, j in jobs} x[h,j] * 0.17
			-
			sum{j in jobs} (1 - (jcpuvar[j] / jcpu[j])) * 0.17 * ",lam,sep="");

	content <- paste(content,"
			-
			((sum{h in hosts} (y1[h]*267.8 + y2[h]*17.7 + y3[h]*17.0 + y4[h]*15.4)) * 0.00009)
			-			
			((((0.5 * sum{j in jobs, h in hosts} ( if (jold[h,j]==1) then (1-x[h,j]) else x[h,j] ) ) - news ) * 0.24 * ",eps,"));",sep="");

	content <- paste(content,"

/* Constraints */
	s.t. capacity {h in hosts} : sum{j in jobs} k[h,j] + s1[h] = (y1[h]+y2[h]+y3[h]+y4[h])*100;

	s.t. maxcpu {h in hosts} : (y1[h]+y2[h]+y3[h]+y4[h]) <= mcpu[h];
	s.t. unicitat {j in jobs} : sum{h in hosts} x[h,j] = 1;
	s.t. power1 {h in hosts} : y1[h] >= y2[h];
	s.t. power2 {h in hosts} : y2[h] >= y3[h];
	s.t. power3 {h in hosts} : y3[h] >= y4[h];

	s.t. qos1 {h in hosts, j in jobs} : k[h,j] >= x[h,j];
	s.t. qos2 {h in hosts, j in jobs} : k[h,j] <= x[h,j] * 100000;
	s.t. qos3 {h in hosts, j in jobs} : k[h,j] - jcpuvar[j] <= (1 - x[h,j]);
	s.t. qos4 {h in hosts, j in jobs} : jcpuvar[j] - k[h,j] <= (1 - x[h,j]) * 100000;

	s.t. qos5 {j in jobs} : jcpuvar[j] <= jcpu[j];
	s.t. qos6 {j in jobs} : jcpu[j] * ",thres," <= jcpuvar[j]; ",sep="");

	write(content, paste(base,"temp123.mod",sep=""), sep="", append=FALSE);
}

################################################################################
# Execució de la Simulació
################################################################################

numHosts <- length(hosts[,"name"]);
hostList <- NULL;
mcpu <- NULL;
for (h in 1:numHosts) {
	hostList <- c(hostList,paste("Host",h,sep=""));
	mcpu <- c(mcpu,hosts[h,2]);
}

library(glpk);

of <- paste(base,"trset.arff",sep="");
#write("",file=of,append=FALSE);


## Inicialitzar l'Historial

experiment <- 3; # 0 = power; 1 = eco-cons; 2 = eco-tol; 3 = aux

costlphist <- NULL;		# LP Lower Bound

# Power
if (experiment == 0) {
	powpwrhist <- NULL;		# PowCost-powr
	powmighist <- NULL;		# PowCost-migr
	powcosthist <- NULL;		# Pow Euro hist

	powqoshist <- NULL;
	powqosminhist <- NULL;
	powtspenthist <- NULL;
	powerrhist <- NULL;
}

# Eco-cons
if (experiment == 1) {
	ecopwrhist <- NULL;		# Eco Pwr hist
	ecomighist <- NULL;		# Eco Mig hist
	ecocosthist <- NULL;		# Eco Euro hist

	ecoqoshist <- NULL;
	ecoqosminhist <- NULL;
	ecotspenthist <- NULL;
	ecoerrhist <- NULL;
}

# Eco-cons-tol
if (experiment == 2) {
	europwrhist <- NULL;		# Euro Pwr hist
	euromighist <- NULL;		# Euro Mig hist
	eurocosthist <- NULL;		# Euro Euro hist

	euroqoshist <- NULL;
	euroqosminhist <- NULL;
	eurotspenthist <- NULL;
	euroerrhist <- NULL;
}

if (experiment == 3) {
	auxpwrhist <- NULL;
	auxmighist <- NULL;
	auxcosthist <- NULL;

	auxqoshist <- NULL;
	auxqosminhist <- NULL;
	auxtspenthist <- NULL;
	auxerrhist <- NULL;
}

oldbinmat <- NULL;		# Old BinMatrix

auxlam <- 1;
auxeps <- 1;
auxthres <- 0.8;

# Executar TimeTable
for(time in 0:max((timetable[,2]/3600)))
{
	print(paste("Time: ",time, sep=""));
	########################################################################
	# Per cada hora, agafa scheduling
	########################################################################

	listJobs <- timetable[timetable[,2]==(time*3600),];
	numJobs <- length(listJobs[,1]);	

	jobs <- NULL;
	jcpu <- NULL;
	for (j in 1:numJobs) {
		jobs <- c(jobs,paste("Job",listJobs[j,1],sep=""));
		#jcpu <- c(jcpu,desCPU(listJobs[j,3]));
		jcpu <- c(jcpu,ceiling(desCPU(listJobs[j,3])));
	}
	crearFitxer(hostList,mcpu,jobs,jcpu,oldbinmat);

	crearModel(auxlam,auxeps,auxthres);

	########################################################################
	# Executa LIP
	########################################################################

#	lp <- lpx_read_model(	model=(paste(base,"temp124.mod",sep="")),
#				data=(paste(base,"temp123.dat",sep="")));
#
#	lpx_simplex(lp);

	########################################################################
	# Resultats Linear Programming
	########################################################################

#	capacity <- NULL;
#	maxCPU <- NULL;
#	linres <- NULL;
#
#	numrows <- lpx_get_num_rows(lp);
#	numcols <- lpx_get_num_cols(lp);
#
#	cost <- lpx_get_row_prim(lp, 1);
#
#	for (i in 2:(numHosts+1)) {
#		capacity <- c(capacity,lpx_get_row_prim(lp, i));
#		maxCPU <- c(maxCPU,lpx_get_row_prim(lp, i+numHosts));
#	}
#
#	for (j in 1:(numHosts*numJobs)) {
#		linres <- c(linres,lpx_get_col_prim(lp, j));
#	}
#	dim(linres) <- c(numJobs,numHosts);

	########################################################################
	# Resultats Integer Programming EuroCost
	########################################################################

	newcapacity <- NULL;
	usedcpu <- NULL;
	binmatrix <- NULL;
	benefit <- 0;

	tmlim <- 120; # if(time==0) tmlim <- 20; if(time==72) tmlim <- 30;

	while (benefit == 0)
	{
		print(paste("[",time,"]: ",experiment, "; ",tmlim,sep=""));

		system(paste("glpsol -m temp123.mod -d temp123.dat -o temp123.sol --tmlim ",tmlim," --log temp123.log",sep=""));
		system("echo `grep \"Problem\" temp123.sol` `grep \"Objective:  benf =\" temp123.sol` > solution.tmp");
		x <- read.table("solution.tmp");
		benefit <- x[,6];

		system("grep \"*\" temp123.sol | grep -v \"\\[\"> solution.tmp");
		x <- read.table("solution.tmp");
		binmatrix <- x[,2];
		dim(binmatrix) <- c(numHosts,numJobs);	# Ojo! inverteix la solució, el putes
		binmatrix <- t(binmatrix);

		system("cat temp123.sol | grep -A 2 \"capacity\" | grep -v \"capacity\" | grep -v \"maxcpu\" | tr -s \" \" > solution.tmp");
		x <- read.table("solution.tmp");
		newcapacity <- x[,1];

		system("cat temp123.sol | grep -A 2 \"maxcpu\" | grep -v \"maxcpu\" | grep -v \"unicitat\" | tr -s \" \" > solution.tmp");
		x <- read.table("solution.tmp");
		usedcpu <- x[,1];

		# Variables de rendiment
		system("cat temp123.sol | grep -A 2 \"jcpuvar\" | grep -v \"jcpuvar\" | tr -s \" \" > solution.tmp");
		x <- read.table("solution.tmp");
		qos <- x[,1]/jcpu;

		system("cat temp123.log | grep \"Time used\" | tail -n 1 > solution.tmp");
		x <- read.table("solution.tmp");
		tspent <- x[,3];

		system("cat temp123.log | grep \"mip\" | tail -n 1 > solution.tmp");
		x <- read.table("solution.tmp");
		err <- 1 - (x[,5]/x[,7]);

		if (benefit == 0) {
			#if (tmlim==20) tmlim <- 0;

			if (tmlim < 600) { tmlim <- tmlim + 120; }
			else { tmlim <- tmlim + 180;}
		}
		if (tmlim > 1800) {
			print(paste("Error in time ",time," -> Solution Not Found",sep=""));
			#stop();
		}
	}

## BEGIN LPSOLVER
##
	if (benefit==0)
	{
		system("glpsol -m temp124.mod -d temp123.dat --check --wfreemps temp123.mps");
	 	system("lp_solve -fmps temp123.mps -S4 -timeout 60 > temp123.sol");
	 
	 	system("head -n 10 temp123.sol | grep 'Value of objective function' > solution.tmp");
	 	x <- read.table("solution.tmp");
	 	benefit <- x[,5];
	 
	 	system(paste("cat temp123.sol | grep \"x\" | head -n ", numHosts * numJobs," > solution.tmp",sep=""));
	 	x <- read.table("solution.tmp");
		binmatrix <- x[,2];
		dim(binmatrix) <- c(numHosts,numJobs);	# Ojo! inverteix la solució, el putes
	 	binmatrix <- t(binmatrix);

	 	system("cat temp123.sol | grep  \"jcpuvar\" > solution.tmp");
	 	x <- read.table("solution.tmp");
	 	qos <- x[,2]/jcpu;
	 	usedcpu <- ceiling((colSums(binmatrix * x[,2]))/100); # lpsol only
	 
	 	newcapacity <- -1;
	 	tspent <- -1;
	 	err <- -1;
	}
##
## END LPSOLVER

	sumpower <- 0;
	for (k in 1:(length(usedcpu))) {
		if (usedcpu[k] == 1) sumpower <- sumpower + 267.8;
		if (usedcpu[k] == 2) sumpower <- sumpower + 285.5;
		if (usedcpu[k] == 3) sumpower <- sumpower + 302.5;
		if (usedcpu[k] == 4) sumpower <- sumpower + 317.9;
	}

	migrations <- 0;
	for (i in 1:length(jobs)) {
		if (length(oldbinmat[2,oldbinmat[1,]==jobs[i]])!=0) {
			x <- oldbinmat[2,oldbinmat[1,]==jobs[i]];
			y <- 0;
			for(k in 1:numHosts) if (binmatrix[i,k]==1) y <- k;
		
			if (x != y) migrations <- migrations + 1;
		}
	}

	########################################################################
	# Desar Historial
	########################################################################

	costlphist <- c(costlphist,cost);

	if (experiment == 0)
	{
		powpwrhist <- c(powpwrhist,sumpower);			# PowCost-powr
		powmighist <- c(powmighist,migrations);			# PowCost-migr
		powcosthist <- c(powcosthist,benefit);			# PowCost-euros

		powqoshist <- c(powqoshist,mean(qos));
		powqosminhist <- c(powqosminhist,min(qos));
		powtspenthist <- c(powtspenthist,tspent);
		powerrhist <- c(powerrhist,err);
	}
	else if (experiment == 1)
	{
		ecopwrhist <- c(ecopwrhist,sumpower);			# EcoCons-powr
		ecomighist <- c(ecomighist,migrations);			# EcoCons-migr
		ecocosthist <- c(ecocosthist,benefit);			# EcoCons-euros

		ecoqoshist <- c(ecoqoshist,mean(qos));
		ecoqosminhist <- c(ecoqosminhist,min(qos));
		ecotspenthist <- c(ecotspenthist,tspent);
		ecoerrhist <- c(ecoerrhist,err);
	}
	else if (experiment == 3)
	{
		auxpwrhist <- c(auxpwrhist,sumpower);			
		auxmighist <- c(auxmighist,migrations);		
		auxcosthist <- c(auxcosthist, benefit);		

		auxqoshist <- c(auxqoshist,mean(qos));
		auxqosminhist <- c(auxqosminhist,min(qos));
		auxtspenthist <- c(auxtspenthist,tspent);
		auxerrhist <- c(auxerrhist,err);
	}
	else
	{
		europwrhist <- c(europwrhist,sumpower);			# EcoTol-powr
		euromighist <- c(euromighist,migrations);		# EcoTol-migr
		eurocosthist <- c(eurocosthist, benefit);		# EcoTol-euros

		euroqoshist <- c(euroqoshist,mean(qos));
		euroqosminhist <- c(euroqosminhist,min(qos));
		eurotspenthist <- c(eurotspenthist,tspent);
		euroerrhist <- c(euroerrhist,err);
	}

	########################################################################
	# Desar Configuració anterior
	########################################################################

	oldbinmat <- NULL;
	for (j in 1:numJobs) {
		oldbinmat <- c(oldbinmat,paste("Job",listJobs[j,1],sep=""));
		for (k in 1:numHosts) {
			if (binmatrix[j,k]==1) oldbinmat <- c(oldbinmat,k);
		}
	}
	dim(oldbinmat) <- c(2,numJobs);


	########################################################################
	# Mostrar Resultats
	########################################################################
##	sink(of, append=TRUE, split=FALSE);
##	cat("\n\n------------------------------------------------------------");
##	cat("\nSchedule at: ",time, "\n");
##
##	cat("\nBenef: ", cost);
##
##	#cat("\nLinear regression Matrix\n");
##	#print(linres);
##	#cat("\nLinear Capacity: ", capacity);
##	#cat("\nLinear max CPU: ", maxCPU);
##
##	cat("\n\nIntMigCost: ", intmigcost, " [migracions: ", migmig, "]\n");
##
##	cat("\nBinary migration regression Matrix\n");
##	print(binmigmat);
##	cat("\nBinary Capacity: ", intmigcap);
##	cat("\nBinary max CPU: ", intmigmat);
##
##	sink()

#	sink(of, append=TRUE, split=FALSE);
#	cat("\n\n",time,lam,eps,sumpower,migrations,benefit,mean(qos),min(qos),err,tspent);
#	sink()

}

print("Final EURO-SCHED");
stop();

png("21-pow-eco-qos.png", width=600, height=500);
par(mfcol= c(3, 1));
plot(0,0,xlim=c(0,length(powmighist)),ylim=c(0,max(powmighist,ecomighist,euromighist)+5),xlab="Time (Hours)",ylab="Migrations (#)",main="Power Only Model");
lines(powmighist);
plot(0,0,xlim=c(0,length(powmighist)),ylim=c(0,max(powmighist,ecomighist,euromighist)+5),xlab="Time (Hours)",ylab="Migrations (#)",main="Economic Only Model");
lines(ecomighist);
plot(0,0,xlim=c(0,length(powmighist)),ylim=c(0,max(powmighist,ecomighist,euromighist)+5),xlab="Time (Hours)",ylab="Migrations (#)",main="Full Model");
lines(euromighist);
dev.off();

png("22-pow-eco-qos.png", width=600, height=500);
par(mfcol= c(3, 1));
plot(0,0,xlim=c(0,length(powpwrhist)),ylim=c(0,max(powpwrhist,ecopwrhist,europwrhist)+0.5),xlab="Time (Hours)",ylab="Power (Watts)",main="Power Only Model");
lines(powpwrhist);
plot(0,0,xlim=c(0,length(powpwrhist)),ylim=c(0,max(powpwrhist,ecopwrhist,europwrhist)+0.5),xlab="Time (Hours)",ylab="Power (Watts)",main="Economic Only Model");
lines(ecopwrhist);
plot(0,0,xlim=c(0,length(powpwrhist)),ylim=c(0,max(powpwrhist,ecopwrhist,europwrhist)+0.5),xlab="Time (Hours)",ylab="Power (Watts)",main="Full Model");
lines(europwrhist);
dev.off();

png("24-pow-eco-qos.png", width=600, height=500);
par(mfcol= c(3, 1));
plot(0,0,xlim=c(0,length(ecocosthist)),ylim=c(3.5,max(ecocosthist,eurocosthist)+0.5),xlab="Time (Hours)",ylab="Euros (benefit)",main="Power Only Model");
lines(powcosthist);
plot(0,0,xlim=c(0,length(ecocosthist)),ylim=c(3.5,max(ecocosthist,eurocosthist)+0.5),xlab="Time (Hours)",ylab="Euros (benefit)",main="Economic Only Model");
lines(ecocosthist);
plot(0,0,xlim=c(0,length(ecocosthist)),ylim=c(3.5,max(ecocosthist,eurocosthist)+0.5),xlab="Time (Hours)",ylab="Euros (benefit)",main="Full Model");
lines(eurocosthist);
dev.off();

png("25-pow-eco-qos.png", width=1000, height=500);
par(mfcol= c(3, 1));
plot(0,0,xlim=c(0,length(ecomighist)),ylim=c(0.7,1.1),xlab="Time (Hours)",ylab="Health (Assigned CPU / Requested CPU)",main="Power Only Model");
lines(powqoshist);
plot(0,0,xlim=c(0,length(ecomighist)),ylim=c(0.7,1.1),xlab="Time (Hours)",ylab="Health (Assigned CPU / Requested CPU)",main="Economic Only Model");
lines(ecoqoshist);
plot(0,0,xlim=c(0,length(ecomighist)),ylim=c(0.7,1.1),xlab="Time (Hours)",ylab="Health (Assigned CPU / Requested CPU)",main="Full Model");
lines(euroqoshist);
dev.off();

sum(powcosthist);	## Euros Pwr model
sum(ecocosthist);	## Euros Eco model
sum(eurocosthist);	## Euros Euro model

sum(powpwrhist);	## Power Pow model
sum(ecopwrhist);	## Power Eco model
sum(europwrhist);	## Power Euro model

sum(powmighist);	## Migracions Pow model
sum(ecomighist);	## Migracions Eco model
sum(euromighist);	## Migracions Euro model

mean(pwrqoshist);	## Pwr QoS on scheduling
mean(ecoqoshist);	## Eco QoS on scheduling
mean(euroqoshist);	## Euros QoS on scheduling


png("41-qos-level.png", width=1000, height=500);
plot(0,0,xlim=c(0,length(ecomighist)),ylim=c(0.7,1.1),xlab="Time (Hours)",ylab="Health (Assigned CPU / Requested CPU)");
lines(euroqoshist,col="black");
lines(euroqosminhist,col="black", pch=22, lty=2);
legend("topleft", c("Average Health","Minimum Health"), lty=1:2);
dev.off();

mean(euroqosminhist)	## Euros minimum QoS on scheduling
mean(eurotspenthist);	## Euros time spent on scheduling
euroerrhist[is.na(euroerrhist)] <- 0;
mean(euroerrhist);	## Euros difference with lower bound

mean(ecoqosminhist)	## Eco minimum QoS on scheduling
mean(ecotspenthist);	## Eco time spent on scheduling
ecoerrhist[is.na(ecoerrhist)] <- 0;
mean(ecoerrhist);	## Eco difference with lower bound

mean(pwrqosminhist)	## Pwr minimum QoS on scheduling
mean(pwrtspenthist);	## Pwr time spent on scheduling
pwrerrhist[is.na(pwrerrhist)] <- 0;
mean(pwrerrhist);	## Pwr difference with lower bound

png("61-time-spent.png", width=600, height=500);
	histogram(eurotspenthist,xlab="Spent time (seconds)",nint=100, main="ILP Execution Time");
dev.off();

png("62-total-error.png", width=600, height=500);
	histogram(euroerrhist,xlab="Difference ratio respect Lower bound",nint=100, main="Relative error of ILP");
dev.off();


stop()


## Sumari de tot (AUX)
sum(auxcosthist);
sum(auxpwrhist);
sum(auxmighist);
mean(auxqoshist);
mean(auxqosminhist);

##########
rescombined <- NULL;

# lambda = epsilon
rescombined <- c(rescombined,0.001,0.001,811.2031,45612.2,1845,0.1821383,0.0);
rescombined <- c(rescombined,0.1,0.1,788.0320,170547.6,882,0.8605422,0.0);
rescombined <- c(rescombined,0.5,0.5,784.2298,298989.2,231,0.9916590,0.0);
rescombined <- c(rescombined,1.0,1.0,780.5729,369637.2,47,0.9975926,0.0);
rescombined <- c(rescombined,2.0,2.0,774.5946,442715.8,34,0.9990015,0.0);

# Migs only (epsilon = inf -> cond = 1.0)
rescombined <- c(rescombined,0.1,999.0,793.4657,238814.8,1081,1,0.0);
rescombined <- c(rescombined,0.2,999.0,791.5995,249432.3,737,1,0.0);
rescombined <- c(rescombined,0.5,999.0,790.0148,308679.3,415,1,0.0);
rescombined <- c(rescombined,0.8,999.0,786.2236,362625.5,185,1,0.0);
rescombined <- c(rescombined,1.0,999.0,804.2952,422692.6,167,1,0.0);
rescombined <- c(rescombined,1.25,999.0,776.053,424711.0,125,1,0.0);
#rescombined <- c(rescombined,1.5,999.0,778.2305,412661.6,70,1,0.0);
rescombined <- c(rescombined,2.0,999.0,766.9915,538627.6,59,1,0.0);

# Compr only (lambda = inf -> 0.24)
rescombined <- c(rescombined,999.0,0.0,814.7509,044990.4,0,0.06134519,0.0);
rescombined <- c(rescombined,999.0,0.1,784.3013,200890.9,0,0.8031927,0.0);
rescombined <- c(rescombined,999.0,0.2,784.7243,282177.1,0,0.9036630,0.0); # 0.497 min QoS
rescombined <- c(rescombined,999.0,0.3,786.1255,361673.2,0,0.9695588,0.0); # 0.658 min QoS
rescombined <- c(rescombined,999.0,0.4,782.9367,438688.0,0,0.9883536,0.0); # 0.826 min QoS
rescombined <- c(rescombined,999.0,0.5,771.8667,518558.6,0,0.9815150,0.0); # 0.753 min QoS
rescombined <- c(rescombined,999.0,0.8,768.6706,531754.4,0,0.9855688,0.0); # 0.797 min QoS
rescombined <- c(rescombined,999.0,0.9,754.2801,586357.6,0,0.9755915,0.0); # 0.640 min QoS
rescombined <- c(rescombined,999.0,1.0,756.9373,561131.3,0,0.9706820,0.0); # 0.640 min QoS
#rescombined <- c(rescombined,999.0,1.1,757.8428,588021.9,0,0.9839333,0.0); # 0.647 min QoS
#rescombined <- c(rescombined,999.0,1.2,776.5046,451460.6,0,0.9909603,0.0); # 0.850 min QoS
#rescombined <- c(rescombined,999.0,1.3,782.1409,357984.7,0,0.9891606,0.0); # 0.850 min QoS
#rescombined <- c(rescombined,999.0,1.4,780.8874,377017.1,0,0.9903157,0.0); # 0.850 min QoS
#rescombined <- c(rescombined,999.0,1.5,779.7940,376862.7,0,0.9900978,0.0); # 0.850 min QoS
#rescombined <- c(rescombined,999.0,2.0,781.5213,370441.3,0,0.9895019,0.0); # 0.850 min QoS

dim(rescombined) <- c(7,length(rescombined)/7);
rescombined <- t(rescombined);
colnames(rescombined) <- c('lambda','epsilon','eq-benef','power','migr','qos','benef');
rescombined[,'benef'] <- ((rescombined[,'eq-benef'] + rescombined[,'migr']*0.0014*rescombined[,'lambda'] + rescombined[,'power']*0.00009)/(1 - (1 - rescombined[,'qos'])*rescombined[,'epsilon']))*rescombined[,'qos'] - rescombined[,'migr']*0.0014 - rescombined[,'power']*0.00009;

reslambda <- rescombined[rescombined[,'epsilon']==999.0,];
resepsilon <- rescombined[rescombined[,'lambda']==999.0,];
resequal <- rescombined[rescombined[,'lambda']==rescombined[,'epsilon'],];
resequal <- resequal[resequal[,'lambda']>999.0,];

# epsilon = inf (migracions only)
plot(reslambda[,'lambda'],reslambda[,'migr'],xlab="enforcement of Migr (var lambda, qos=1)",ylab="Number Migrations");
lines(reslambda[,'lambda'],reslambda[,'migr']);

plot(reslambda[,'lambda'],reslambda[,'power'],xlab="enforcement of Migr (var lambda, qos=1)",ylab="Power Cons (kW)");
lines(reslambda[,'lambda'],reslambda[,'power']);

plot(reslambda[,'lambda'],reslambda[,'benef'],xlab="enforcement of Migr (var lambda; qos=1)",ylab="Real Benefit (Euro)");
lines(reslambda[,'lambda'],reslambda[,'benef']);

# lambda = inf (compresió only)
plot(resepsilon[,'epsilon'],resepsilon[,'qos'],xlab="enforcement of QoS (var epsilon; migs=0)",ylab="Level of QoS");
lines(resepsilon[,'epsilon'],resepsilon[,'qos']);

plot(resepsilon[,'epsilon'],resepsilon[,'power'],xlab="enforcement of QoS (var epsilon; migs=0)",ylab="Power Cons (kW)");
lines(resepsilon[,'epsilon'],resepsilon[,'power']);

plot(resepsilon[,'epsilon'],resepsilon[,'benef'],xlab="enforcement of QoS (var epsilon; migs=0)",ylab="Real Benefit (Euro)");
lines(resepsilon[,'epsilon'],resepsilon[,'benef']);

# lambda = epsilon (igualtat polítiques)
plot(resequal[,'epsilon'],resequal[,'qos'],xlab="enforcement of const (var epsilon, lambda)",ylab="Level of QoS");
lines(resequal[,'epsilon'],resequal[,'qos']);

plot(resequal[,'epsilon'],resequal[,'power'],xlab="enforcement of const (var epsilon, lambda)",ylab="Power Cons (kW)");
lines(resequal[,'epsilon'],resequal[,'power']);

plot(resequal[,'epsilon'],resequal[,'benef'],xlab="enforcement of const (var epsilon, lambda)",ylab="Real Benefit (Euro)");
lines(resequal[,'epsilon'],resequal[,'benef']);



# regressions Lambda only
png("51-lambdavspower.png", width=500, height=500);
	reg11 <- lm(reslambda[,'power'] ~ reslambda[,'lambda']);
	plot(reslambda[,'lambda'],reslambda[,'power'],xlab="Migration Restriction policy (lambda=[0.0,2.0], qos=1)",ylab="Consumed Power (W)");
	lines(reslambda[,'lambda'],reslambda[,'power']);
	#aux <- seq(min(reslambda[,'lambda']),max(reslambda[,'lambda']),by=0.1);
	#aux1 <- 228824 + aux*161336;
	#lines(aux,aux1,col='red');
dev.off();

png("52-powvsmig.png", width=500, height=500);
	aux12 <- max(reslambda[,'migr'])-reslambda[,'migr'];
	reg12 <- lm(reslambda[,'power'] ~ aux12);
	plot(reslambda[,'migr'],reslambda[,'power'],ylab="Consumed Power (W)",xlab="Number Migrations (#)");
	lines(reslambda[,'migr'],reslambda[,'power']);
	#aux <- seq(min(reslambda[,'migr']),max(reslambda[,'migr']),by=10);
	#aux1 <- 194257.3 + (max(reslambda[,'migr'])-aux)*248.7;
	#lines(aux,aux1,col='red');
	#legend("topleft", c("Observed Results","Fitted Linear Regression"), fill=c("black","red"));
	#abline(v=200);
	#abline(v=750);
dev.off();


aux13 <- 1/reslambda[,'lambda'];
reg13 <- lm(reslambda[,'migr'] ~ aux13);
plot(reslambda[,'lambda'],reslambda[,'migr'],xlab="enforcement of Migr (var lambda, qos=1)",ylab="Number Migrations");
lines(reslambda[,'lambda'],reslambda[,'migr']);
aux <- seq(min(reslambda[,'lambda']),max(reslambda[,'lambda']),by=0.1);
aux1 <- 76.58 + (1/aux)*102.38;
lines(aux,aux1,col='red');


# regressions Epsilon only
png("53-epsilonvspower.png", width=500, height=500);
	reg21 <- lm(resepsilon[,'power'] ~ resepsilon[,'epsilon']);
	plot(resepsilon[,'epsilon'],resepsilon[,'power'],xlab="QoS Loss Restriction policy (epsilon=[0,1],migrations=0)",ylab="Consumed Power (W)");
	lines(resepsilon[,'epsilon'],resepsilon[,'power']);
dev.off();

png("54-powervsqos.png", width=500, height=500);
	reg22 <- lm(resepsilon[,'power'] ~ resepsilon[,'qos']);
	plot(resepsilon[,'qos'],resepsilon[,'power'],ylab="Consumed Power (W)",xlab="Average Quality of Service (Health)");
	aux <- resepsilon[order(resepsilon[,'qos']),c('qos','power')];
	lines(aux[,1],aux[,2]);
dev.off();


# regressions Lambda=Epsilon only

reg3 <- lm(rescombined[,'power'] ~ (rescombined[,'lambda'] + rescombined[,'epsilon'])^2);

aux31 <- seq(min(rescombined[,'lambda']),max(rescombined[,'lambda']),by=0.1);
aux32 <- seq(min(rescombined[,'epsilon']),max(rescombined[,'epsilon']),by=0.1);
aux <- 265167.17 + aux31*12.82  + aux32*-25.14  + aux31*aux32*148.17;

g <- expand.grid(x = aux31, y = aux32);
g$z <- (148.17*g$x*g$y + 12.82*g$x + -25.14*g$y + 265167.17);
wireframe(z ~ x * y, data = g, drape = TRUE, colorkey = TRUE, scales = list(arrows = FALSE),screen = list(z = 40, x = -65));

aux40 <- max(rescombined[,'migr'])-rescombined[,'migr'];
reg4 <- lm(rescombined[,'power'] ~ (aux40 + rescombined[,'qos'])^2);

aux41 <- seq(min(rescombined[,'migr']),max(rescombined[,'migr']),by=50);
aux42 <- seq(min(rescombined[,'qos']),max(rescombined[,'qos']),by=0.1);
aux <- 60189.39 + (max(rescombined[,'migr'])-aux41)*-36.82  + aux42*-92306.53  + (max(rescombined[,'migr'])-aux41)*aux42*303.00;

g <- expand.grid(x = (max(rescombined[,'migr'])-aux41), y = aux42);
g$z <- (303.00*g$x*g$y + -36.82*g$x + -92306.53*g$y + 60189.39);
wireframe(z ~ x * y, data = g, drape = TRUE, colorkey = TRUE, scales = list(arrows = FALSE),screen = list(z = 20, x = -75));


################################################################################
# Estrategia Round Robin
################################################################################

numHosts <- length(hosts[,"name"]);
hostList <- NULL;
mcpu <- NULL;
for (h in 1:numHosts) {
	hostList <- c(hostList,paste("Host",h,sep=""));
	mcpu <- c(mcpu,hosts[h,2]);
}

recalculateHosts <- function(lambda1, numJobs, jcpu, mcpu) {
	retval <- 0;
	aux <- 0;
	while (aux < sum(jcpu)*(1 + lambda1/100)) {
		retval <- retval + 1;
		aux <- aux + mcpu[retval]*100;
	}
	retval;
}

mod <- function(a,b){
	a-(trunc(a/b)*b);
}

ofrr <- paste(base,"outputrr.sched",sep="");
write("",file=ofrr,append=FALSE);

costrrhist <- NULL;
counter <- 0;
numAvHosts <- 0;
lambda1 <- 30;
time <- 0;

oldrrmat <- NULL;
migrrhist <- NULL;
eurorr <- NULL;

for(time in 0:max((timetable[,2]/3600)))
{
	########################################################################
	# Per cada hora, agafa scheduling
	########################################################################

	listJobs <- timetable[timetable[,2]==(time*3600),];
	numJobs <- length(listJobs[,1]);	

	jobs <- NULL;
	jcpu <- NULL;
	for (j in 1:numJobs) {
		jobs <- c(jobs,paste("Job",listJobs[j,1],sep=""));
		jcpu <- c(jcpu,desCPU(listJobs[j,3]));
	}

	########################################################################
	# Recalcul NumHosts
	########################################################################
	
	numAvHosts <- recalculateHosts(lambda1,numJobs,jcpu,mcpu);
	counter <- mod(counter,numAvHosts);

	########################################################################
	# Scheduling RR
	########################################################################
	
	binrr <- rep(0,numAvHosts*numJobs);
	dim(binrr) <- c(numJobs,numAvHosts);

	ocup <- rep(0,numAvHosts);

	for (j in 1:numJobs) {
		while ((ocup[(counter+1)]+jcpu[j])>(mcpu[(counter+1)]*100)) {
			counter <- mod((counter + 1),numAvHosts);
		}
		binrr[j,(counter+1)] <- 1;
		ocup[(counter+1)] <- ocup[(counter+1)] + jcpu[j];
		counter <- mod((counter + 1),numAvHosts);
	}	

	########################################################################
	# Resultats de l'Scheduler
	########################################################################

	capacity <- NULL;
	maxCPU <- NULL;
	linres <- NULL;
	cost <- 0;

	capacity <- ocup - ceiling(ocup/100)*100;
	maxCPU <- ceiling(ocup/100);
	linres <- binrr;
	cost <- sum(maxCPU);

	sumcost <- 0;
	for (k in 1:(length(maxCPU))) {
		if (maxCPU[k] == 0) sumcost <- sumcost + 238.9;
		if (maxCPU[k] == 1) sumcost <- sumcost + 267.8;
		if (maxCPU[k] == 2) sumcost <- sumcost + 285.5;
		if (maxCPU[k] == 3) sumcost <- sumcost + 302.5;
		if (maxCPU[k] == 4) sumcost <- sumcost + 317.9;
	}

	migrr <- 0;
	for (i in 1:length(jobs)) {
		if (length(oldrrmat[2,oldrrmat[1,]==jobs[i]])!=0) {
			x <- oldrrmat[2,oldrrmat[1,]==jobs[i]];
			y <- 0;
			for(k in 1:numAvHosts) if (binrr[i,k]==1) y <- k;
		
			if (x != y) migrr <- migrr + 1;
		}
	}

	eurorraux <- numJobs * 0.17 - (sumcost * 0.00009 + migrr * 0.014);

	########################################################################
	# Mostrar Resultats
	########################################################################

	sink(ofrr, append=TRUE, split=FALSE);
	cat("\n\n------------------------------------------------------------");
	cat("\nSchedule at: ",time, "\n");

	cat("\nCost: ", sumcost, " [cpu: ", cost, "]");

	cat("\nScheduling RR Matrix\n");
	print(linres);
	cat("\nRR Capacity: ", capacity);
	cat("\nRR max CPU: ", maxCPU);

	sink()

	########################################################################
	# Desar Historial
	########################################################################

	costrrhist <- c(costrrhist,sumcost);
	migrrhist <- c(migrrhist,migrr);
	eurorr <- c(eurorr, eurorraux);


	oldrrmat <- NULL;
	for (j in 1:numJobs) {
		oldrrmat <- c(oldrrmat,paste("Job",listJobs[j,1],sep=""));
		for (k in 1:numAvHosts) {
			if (binrr[j,k]==1) oldrrmat <- c(oldrrmat,k);
		}
	}
	dim(oldrrmat) <- c(2,numJobs);


}

################################################################################
# Estrategia First Fit
################################################################################

numHosts <- length(hosts[,"name"]);
hostList <- NULL;
mcpu <- NULL;
for (h in 1:numHosts) {
	hostList <- c(hostList,paste("Host",h,sep=""));
	mcpu <- c(mcpu,hosts[h,2]);
}

offf <- paste(base,"outputff.sched",sep="");
write("",file=offf,append=FALSE);


time <- 0;
oldffmat <- NULL;

powffhist <- NULL;
migffhist <- NULL;
euroffhist <- NULL;

for(time in 0:max((timetable[,2]/3600)))
{
	########################################################################
	# Per cada hora, agafa scheduling
	########################################################################

	listJobs <- timetable[timetable[,2]==(time*3600),];
	numJobs <- length(listJobs[,1]);	

	jobs <- NULL;
	jcpu <- NULL;
	for (j in 1:numJobs) {
		jobs <- c(jobs,paste("Job",listJobs[j,1],sep=""));
		jcpu <- c(jcpu,desCPU(listJobs[j,3]));
	}

	########################################################################
	# Recalcul NumHosts
	########################################################################
	
	counter <- mod(counter,numHosts);

	########################################################################
	# Scheduling FF
	########################################################################
	
	binff <- rep(0,numHosts*numJobs);
	dim(binff) <- c(numJobs,numHosts);

	ocup <- rep(0,numHosts);

	for (j in 1:numJobs) {
		while ((ocup[(counter+1)]+jcpu[j])>(mcpu[(counter+1)]*100)) {
			counter <- mod((counter + 1),numAvHosts);
		}
		binff[j,(counter+1)] <- 1;
		ocup[(counter+1)] <- ocup[(counter+1)] + jcpu[j];
		counter <- mod((counter + 1),numAvHosts);
	}	

	########################################################################
	# Resultats de l'Scheduler
	########################################################################

	capacity <- NULL;
	maxCPU <- NULL;
	linres <- NULL;
	cost <- 0;

	capacity <- ocup - ceiling(ocup/100)*100;
	maxCPU <- ceiling(ocup/100);
	linres <- binff;
	cost <- sum(maxCPU);

	sumffcost <- 0;
	for (k in 1:(length(maxCPU))) {
		if (maxCPU[k] == 0) sumffcost <- sumffcost + 238.9;
		if (maxCPU[k] == 1) sumffcost <- sumffcost + 267.8;
		if (maxCPU[k] == 2) sumffcost <- sumffcost + 285.5;
		if (maxCPU[k] == 3) sumffcost <- sumffcost + 302.5;
		if (maxCPU[k] == 4) sumffcost <- sumffcost + 317.9;
	}

	migff <- 0;
	for (i in 1:length(jobs)) {
		if (length(oldffmat[2,oldffmat[1,]==jobs[i]])!=0) {
			x <- oldffmat[2,oldffmat[1,]==jobs[i]];
			y <- 0;
			for(k in 1:numHosts) if (binff[i,k]==1) y <- k;
		
			if (x != y) migff <- migff + 1;
		}
	}

	euroffaux <- numJobs * 0.17 - (sumffcost * 0.00009 + migff * 0.014);

	########################################################################
	# Mostrar Resultats
	########################################################################

#	sink(ofrr, append=TRUE, split=FALSE);
#	cat("\n\n------------------------------------------------------------");
#	cat("\nSchedule at: ",time, "\n");
#
#	cat("\nCost: ", sumcost, " [cpu: ", cost, "]");
#
#	cat("\nScheduling RR Matrix\n");
#	print(linres);
#	cat("\nRR Capacity: ", capacity);
#	cat("\nRR max CPU: ", maxCPU);
#
#	sink()

	########################################################################
	# Desar Historial
	########################################################################

	powffhist <- c(powffhist,sumffcost);
	migffhist <- c(migffhist,migff);
	euroffhist <- c(euroffhist, euroffaux);


	oldffmat <- NULL;
	for (j in 1:numJobs) {
		oldffmat <- c(oldffmat,paste("Job",listJobs[j,1],sep=""));
		for (k in 1:numHosts) {
			if (binff[j,k]==1) oldffmat <- c(oldffmat,k);
		}
	}
	dim(oldffmat) <- c(2,numJobs);


}

################################################################################
################################################################################

png("31-qos.rr.png", width=1000, height=750);
plot(0,0,xlim=c(0,length(euromigcost)),ylim=c(3,max(euromigcost,eurorr)+0.5),xlab="Time (Hours)",ylab="Euro");
lines(ecocosthist,col="blue");
lines(eurocosthist,col="green");
lines(eurorr,col="orange");
legend("topleft", c("Economic Model","QoS Model","Lambda-RR"), fill=c("blue","green","orange"));
dev.off();

png("32-qos.rr.png", width=1000, height=750);
plot(0,0,xlim=c(0,length(ecopwrhist)),ylim=c(0,max(ecopwrhist,costmiginthist,costrrhist)+0.5),xlab="Time (Hours)",ylab="Power (Watts)");
lines(ecopwrhist,col="blue");
lines(costmiginthist,col="green");
lines(costrrhist,col="orange");
legend("topleft", c("Economic Model","QoS Model","Lambda-RR"), fill=c("blue","green","orange"));
dev.off();

png("33-qos.rr.png", width=1000, height=750);
plot(0,0,xlim=c(0,length(ecomighist)),ylim=c(0,max(ecomighist,migmighist,migrrhist)+5),xlab="Time (Hours)",ylab="Migrations (number of)");
lines(ecomighist,col="blue");
lines(migmighist,col="green");
lines(migrrhist,col="orange");
legend("topleft", c("Economic Model","QoS Model","Lambda-RR"), fill=c("blue","green","orange"));
dev.off();

sum(euroffhist);	## Euros Rand model
sum(eurorr);		## Euros l-RR

sum(powffhist);		## Power Rand model
sum(costrrhist);	## Power l-RR

sum(migffhist);		## Migracions Rand model
sum(migrrhist);		## Migracions l-RR



#pvec <- c(230.0,267.8,285.5,302.5,317.9)
#paux <- c(0,1,2,3,4)
#png("power.png",width=1000, height=750)
#plot(0,0,xlim=c(0,4),ylim=c(min(pvec)-5,max(pvec)+5),xlab="Running Processors",ylab="Consumed Watts per Hour",main="MultiCore Power Consumption Model")
#points(paux,pvec)
#lines(paux,pvec)
#dev.off()

