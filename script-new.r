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
#hosts <- read.table(paste(base,"datacenter-inv1.data",sep=""),header=T);
hosts <- read.table(paste(base,"datacenter-inv1-mini.data",sep=""),header=T);
#wkld <- read.table(paste(base,"workload-tomcatGRID2010.data",sep=""),header=T);
wkld <- read.table(paste(base,"workload-wservice1-mini.data",sep=""),header=T);
#wkld <- read.table(paste(base,"workload-wservice1-mini2.data",sep=""),header=T);

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
		bytes <- daya[entry,"bytes.req"];
		rt0 <- daya[entry,"RT0"];
		aux <- c(aux,workset,atime,clients,bytes,rt0);
	}
}
dim(aux) <- c(5,(length(aux)/5));
aux <- t(aux);
timetable <- aux[order(aux[,2]),];

numTotalHosts <- length(hosts[,"name"]);

## Variables de Response Time
#s1Rm <- 1.45; s1RM <- 1.87; desA <- 0.025; desB <- 0; MaxDesCPU <- 110;
#A <- 162.48; B <- 395.41; C <- 18.41; D <- -18.94; E <- 1.87;
#RA <- 32.13; RB <- -72.55; RC <- 44.32;

source("functions.r");

################################################################################
# Funcions
################################################################################

crearFitxer <- function(hostList,mcpu,mmem,jobList,jcpu,jmem,jrto,jreq,jbyt,obm,mbpq,tpq,jcpumax) {

	hd1 <- NULL; # set hosts
	for (i in 1:length(hostList)) { hd1 <- paste(hd1,hostList[i]); }
	jd1 <- NULL; # set jobs
	for (i in 1:length(jobList)) { jd1 <- paste(jd1,jobList[i]); }
	hp1 <- NULL # param cpu
	for (i in 1:length(hostList)) {	hp1 <- paste(hp1,hostList[i]," ",mcpu[i],"\n",sep=""); }
	hm1 <- NULL; # param mem
	for (i in 1:length(hostList)) {	hm1 <- paste(hm1,hostList[i]," ",mmem[i],"\n",sep="");	}
	jp1 <- NULL; # param jcpu
	for (i in 1:length(jobList)) { jp1 <- paste(jp1,jobList[i]," ",jcpu[i],"\n",sep=""); }
	jcm1 <- NULL # param jcpumax
	for (i in 1:length(jobList)) { jcm1 <- paste(jcm1,jobList[i]," ",jcpumax[i],"\n",sep=""); }
	jm1 <- NULL; # param jmem
	for (i in 1:length(jobList)) { jm1 <- paste(jm1,jobList[i]," ",jmem[i],"\n",sep=""); }
	jt1 <- NULL # param rto
	for (i in 1:length(jobList)) { jt1 <- paste(jt1,jobList[i]," ",jrto[i],"\n",sep=""); }
	jr1 <- NULL; # param req
	for (i in 1:length(jobList)) { jr1 <- paste(jr1,jobList[i]," ",jreq[i],"\n",sep=""); }
	jb1 <- NULL; # param byt
	for (i in 1:length(jobList)) { jb1 <- paste(jb1,jobList[i]," ",jbyt[i],"\n",sep=""); }
	mbpq1 <- NULL; # param mbps
	for (i in 1:length(jobList)) { mbpq1 <- paste(mbpq1,jobList[i]," ",mbpq[i],"\n",sep=""); }
	tpq1 <- NULL; # param tpq
	for (i in 1:length(jobList)) { tpq1 <- paste(tpq1,jobList[i]," ",tpq[i],"\n",sep=""); }

	jo1 <- NULL; # param jold
	news <- 0;
	for (i in 1:length(jobList)) {
		x <- 0;
		if (length(obm[2,obm[1,]==jobList[i]]) > 0) {
			x <- as.numeric(obm[2,obm[1,]==jobList[i]]);
		}
		if (x==0) news <- news + 1;
		for (j in 1:length(hostList)) {
			jo1 <- paste(jo1,hostList[j]," ",jobList[i]," ",sep="");
			if (x == j) { jo1 <- paste(jo1,1,"\n",sep=""); }
			else { jo1 <- paste(jo1,0,"\n",sep=""); }
		}
	}

	content <- paste("
/* Informacio */

data;
set hosts := ",hd1,";
set jobs  := ",jd1,";
param mcpu :=
",hp1,";
param mmem :=
",hm1,";
param jcpu :=
",jp1,";
param jcpumax :=
",jcm1,";
param jmem :=
",jm1,";
param rto :=
",jt1,";
param req :=
",jr1,";
param byt :=
",jb1,";
param mbps :=
",mbpq1,";
param tpq :=
",tpq1,";
param jold :=
",jo1,";
param news := ",news,";

end;
",sep="");

	write(content, paste(base,"temp125.dat",sep=""), sep="", append=FALSE);
}

crearModel <- function(rtolevel,ppj,ppp,slack1,slack2) {

	aux1 <- (rtolevel-1);

	content <- paste("
/* Sets */
	set hosts;
	set jobs;

/* parametres */
	param mcpu {h in hosts};
	param mmem {h in hosts};
	param jcpu {j in jobs};
	param jmem {j in jobs};
	param jcpumax {j in jobs}, integer;

	param rto {j in jobs};
	param req {j in jobs};
	param byt {j in jobs};
	param mbps {j in jobs};
	param tpq {j in jobs};

	param jold {h in hosts,j in jobs}, integer, binary;
	param news, integer;


/* Variables */
	var x {h in hosts, j in jobs}, integer, binary;
	var y1 {h in hosts}, binary;
	var y2 {h in hosts}, binary;
	var y3 {h in hosts}, binary;
	var y4 {h in hosts}, binary;

	var k {h in hosts, j in jobs}, >= 0, integer;
	var km {h in hosts, j in jobs}, >= 0, integer;
	var jcpuvar {j in jobs}, integer;
	var jmemvar {j in jobs}, integer;

	var hcpu {j in jobs} >= 0;
	var kcpu {h in hosts, j in jobs} >= 0;
	var cocup {h in hosts} >= 0;

	var hmem {j in jobs} >= 0;
	var kmem {h in hosts, j in jobs} >= 0;
	var mocup {h in hosts} >= 0;

	var v1 {j in jobs};
	var s1 {j in jobs} binary;
	var s2 {j in jobs} binary;
	var s3 {j in jobs} integer >= 0;
	var sy {j in jobs} >= 0;
	
	var sla {j in jobs} >= 0;
	var rt {j in jobs};

	var maquines; 				/* Variable dummy output */
	var power; 				/* Variable dummy output */
	var benefici; 				/* Benefici Resultant */
	var usedcpu {h in hosts};		/* CPU usada per host */

/* Objectiu */
	maximize benf : benefici;

/* Constraints */

	s.t. out0: benefici = ((sum{j in jobs} sla[j]) * ",ppj,") - (power * ",ppp,");/* -	(((0.5 * sum{j in jobs, h in hosts} (if (jold[h,j]==1) then (1-x[h,j]) else x[h,j])) - 0.5*news) * 0.014);*/
	s.t. out1: maquines = sum{h in hosts} y1[h];
	s.t. out2: power = (sum{h in hosts} (y1[h]*267.8 + y2[h]*17.7 + y3[h]*17.0 + y4[h]*15.4));
	s.t. out3 {h in hosts}: usedcpu[h] = y1[h] + y2[h] + y3[h] + y4[h];

	/* Preu Job: 0.17 ; Preu Power: 0.00009: Penalty Migr: 0.014 */

	s.t. capacity {h in hosts} : cocup[h] <= (y1[h]+y2[h]+y3[h]+y4[h])*100;
	s.t. cpmemory {h in hosts} : mocup[h] <= mmem[h]*1000;

	s.t. maxcpu {h in hosts} : (y1[h]+y2[h]+y3[h]+y4[h]) <= mcpu[h];
	s.t. unicitat {j in jobs} : sum{h in hosts} x[h,j] = 1;
	s.t. power1 {h in hosts} : y1[h] >= y2[h];
	s.t. power2 {h in hosts} : y2[h] >= y3[h];
	s.t. power3 {h in hosts} : y3[h] >= y4[h];

	s.t. qos1 {h in hosts, j in jobs} : k[h,j] >= x[h,j];
	s.t. qos2 {h in hosts, j in jobs} : k[h,j] <= x[h,j] * 100000;
	s.t. qos3 {h in hosts, j in jobs} : k[h,j] - jcpuvar[j] <= (1 - x[h,j]);
	s.t. qos4 {h in hosts, j in jobs} : jcpuvar[j] - k[h,j] <= (1 - x[h,j]) * 100000;

	s.t. qos5 {j in jobs} : jcpuvar[j] <= jcpumax[j];
	s.t. qos6 {j in jobs} : jcpu[j] <= jcpuvar[j];

	s.t. qos7 {h in hosts, j in jobs} : km[h,j] >= x[h,j];
	s.t. qos8 {h in hosts, j in jobs} : km[h,j] <= x[h,j] * 100000;
	s.t. qos9 {h in hosts, j in jobs} : km[h,j] - jmemvar[j] <= (1 - x[h,j]);
	s.t. qos10 {h in hosts, j in jobs} : jmemvar[j] - km[h,j] <= (1 - x[h,j]) * 100000;

	s.t. qos11 {j in jobs} : jmemvar[j] <= jmem[j];
	s.t. qos12 {j in jobs} : jmem[j] * 1.0 <= jmemvar[j];

	s.t. sla0 {j in jobs} : v1[j] = (1 - ((rt[j] - rto[j])/ (",aux1,"*rto[j])));

	s.t. sla1 {j in jobs} : rt[j] - rto[j] <= 100000 * (1 - s1[j]);
	s.t. sla2 {j in jobs} : rto[j] - rt[j] <= 100000 * s1[j];
	s.t. sla3 {j in jobs} : rt[j] - ",rtolevel," * rto[j] <= 100000 * (1 - s2[j]);
	s.t. sla4 {j in jobs} : ",rtolevel," * rto[j] - rt[j] <= 100000 * s2[j];

	s.t. sla5 {j in jobs} : s3[j] = s2[j] - s1[j];

	s.t. sla6 {j in jobs} : sy[j] >= -1 * s3[j];
	s.t. sla7 {j in jobs} : sy[j] <= 100000 * s3[j];
	s.t. sla8 {j in jobs} : sy[j] - v1[j] <= 100000 * (1 - s3[j]);
	s.t. sla9 {j in jobs} : v1[j] - sy[j] <= 100000 * (1 - s3[j]);

	s.t. sla10 {j in jobs} : sla[j] = sy[j] + s1[j];

	s.t. rt01 {j in jobs} : rt[j] =  3.950e-03
					+ ( 1.087e-05 * req[j])
					+ (-4.147e-08 * byt[j])
					+ ( 2.954e-05 * hcpu[j])
					+ (-1.919e-04 * mbps[j])
					+ ( 1.906e-08 * tpq[j])
					+ (-3.633e-05 * jcpuvar[j])
					+ ( 4.762e-06 * jcpu[j]);
	
	s.t. cpu1 {h in hosts} :		cocup[h] = sum{i in jobs}  k[h,i];
	s.t. cpu2 {h in hosts, j in jobs} : 	kcpu[h,j] >= x[h,j];
	s.t. cpu3 {h in hosts, j in jobs} :	kcpu[h,j] <= x[h,j] * 100000;
	s.t. cpu4 {h in hosts, j in jobs} :	kcpu[h,j] - cocup[h] <= (1 - x[h,j]);
	s.t. cpu5 {h in hosts, j in jobs} :	cocup[h] - kcpu[h,j] <= (1 - x[h,j]) * 100000;
	s.t. cpu6 {j in jobs} :			hcpu[j] = sum{h in hosts}  kcpu[h,j];

	s.t. mem1 {h in hosts} :		mocup[h] = sum{i in jobs}  km[h,i];
	s.t. mem2 {h in hosts, j in jobs} : 	kmem[h,j] >= x[h,j];
	s.t. mem3 {h in hosts, j in jobs} :	kmem[h,j] <= x[h,j] * 100000;
	s.t. mem4 {h in hosts, j in jobs} :	kmem[h,j] - mocup[h] <= (1 - x[h,j]);
	s.t. mem5 {h in hosts, j in jobs} :	mocup[h] - kmem[h,j] <= (1 - x[h,j]) * 100000;
	s.t. mem6 {j in jobs} :			hmem[j] = sum{h in hosts}  kmem[h,j];
 "
,sep="");

	content <- paste(content,"end;",sep="\n");

	write(content, paste(base,"temp125.mod",sep=""), sep="", append=FALSE);
}

################################################################################
# Execució de la Simulació
################################################################################

# Preu Job: 0.17 ; Preu Power: 0.00009: Penalty Migr: 0.014 #

texps <- matrix(0,28,6);
texps[1,] <- c(2,0.17,0.00009,10,1,240);	# OK
texps[2,] <- c(2,0.17,0.00014,10,1,240);
texps[3,] <- c(2,0.17,0.00045,10,1,240);	# OK
texps[4,] <- c(2,0.17,0.00090,10,1,240);
texps[5,] <- c(2,0.17,0.00180,10,1,240);
texps[6,] <- c(2,0.17,0.00360,10,1,240);
texps[7,] <- c(2,0.17,0.00720,10,1,240);
texps[8,] <- c(2,0.17,0.01440,10,1,240);
texps[9,] <- c(2,0.17,0.02880,10,1,240);
texps[10,] <- c(2,0.17,0.05760,10,1,240);
texps[11,] <- c(2,0.17,0.10000,10,1,240);
texps[12,] <- c(2,0.17,0.20000,10,1,240);

texps[13,] <- c(1.5,0.17,0.00009,10,1,240);
texps[14,] <- c(10 ,0.17,0.00009,10,1,240);	# OK

texps[15,] <- c(2,0.17,0.00001,10,1,240);	# OK
texps[16,] <- c(2,0.17,0.000001,10,1,240);	# OK
texps[17,] <- c(2,0.17,0.0,10,1,240);		# OK
texps[18,] <- c(2,0.17,0.0,10,1,240);		# OK EXTRA 24 màquines

texps[19,] <- c(2,0.17,0.00009,10,1,240);
texps[20,] <- c(2,0.17,0.00009,10,1,240);
texps[21,] <- c(2,0.17,0.00009,10,1,240);
texps[22,] <- c(2,0.17,0.00009,10,1,240);
texps[23,] <- c(2,0.17,0.00009,10,1,240);
texps[24,] <- c(2,0.17,0.00009,10,1,240);
texps[25,] <- c(2,0.17,0.00009,10,1,240);
texps[26,] <- c(2,0.17,0.00009,10,1,240);
texps[27,] <- c(2,0.17,0.00009,10,1,240);
texps[28,] <- c(2,0.17,0.00009,10,1,240);


#results <- list();
#statresults <- list();		
#auxresults <- list();

#auxresults2 <- list();		# Arreglar migs del auxresults
#auxrepeticions <- list();	# <- ACTUAL (Arreglar migs del auxresults)

auxini <- 24;
auxfin <- 28;

start <- 0;

for (exps in auxini:auxfin) {
	
	paramrt <- texps[exps,1];
	ppj <- texps[exps,2];
	ppp <- texps[exps,3];
	slack1 <- texps[exps,4];
	slack2 <- texps[exps,5];
	tlim1 <- texps[exps,6];

	## Inicialitzar l'Historial

	auxpwrhist <- NULL;
	auxmighist <- NULL;
	auxcosthist <- NULL;

	auxqoshist <- NULL;
	auxqosmaxhist <- NULL;	
	auxqosminhist <- NULL;
	auxtspenthist <- NULL;
	auxerrhist <- NULL;

	auxcpuhist <- NULL;
	auxhostshist <- NULL;

	oldbinmat <- NULL;		# Old BinMatrix


	# Executar TimeTable
	for(time in start:max((timetable[,2]/3600)))
	{
		print(paste("Time: ",time,"       -- Exp: ",exps, sep=""));

		########################################################################
		# Per cada hora, agafa scheduling
		########################################################################

		listJobs <- timetable[timetable[,2]==(time*3600),];
		numJobs <- length(listJobs[,1]);	

		jobs <- NULL;
		jcpu <- NULL;
		jmem <- NULL;
		jrto <- NULL;
		jreq <- NULL;
		jbyt <- NULL;
		jmbps<- NULL;
		jtpq <- NULL;
		for (j in 1:numJobs) {

			p0 <- randTPQ();
			p1 <- randMBPS();
			p2 <- ceiling(listJobs[j,3]*10/500); # input-> reqs per second, required-> reqs per 10 seconds #
			p3 <- ceiling(desMEM(p2,listJobs[j,4],listJobs[j,5]));
			p4 <- ceiling(desCPU(p2,listJobs[j,4],p0));
			jobs <- c(jobs,paste("Job",listJobs[j,1],sep=""));
			jreq <- c(jreq,p2); 
			jbyt <- c(jbyt,max(listJobs[j,4],2048));
			jrto <- c(jrto,(listJobs[j,5]/10));
			jmem <- c(jmem,p3);
			jcpu <- c(jcpu,p4);
			jmbps <- c(jmbps,p1);
			jtpq <- c(jtpq,p0);
		}

		cpumax <- rep(0,numJobs);
		for (i in 1:numJobs) {
			cpumax[i] <- INVRT (jreq[i],jbyt[i],jmbps[i],jtpq[i],400,jcpu[i],jrto[i]);
			if (cpumax[i]<jcpu[i]) cpumax[i] <- jcpu[i];
		}

		########################################################################
		# Minitzar el nombre de hosts disponibles
		########################################################################

		numHosts <- numTotalHosts;
		hostList <- NULL;
		mcpu <- NULL;
		mmem <- NULL;
		for (h in 1:numHosts) {
			hostList <- c(hostList,paste("Host",h,sep=""));
			mcpu <- c(mcpu,hosts[h,2]);
			mmem <- c(mmem,(hosts[h,4]));
		}
		lambda1 <- 30;
		numHosts <- recalculateHosts(lambda1, sum(cpumax), mcpu);

		########################################################################
		# Crear fitxers de Model
		########################################################################

		crearFitxer (hostList[1:numHosts],mcpu,mmem,jobs,jcpu,jmem,jrto,jreq,jbyt,oldbinmat,jmbps,jtpq,cpumax);
		crearModel(paramrt,ppj,ppp,slack1,slack2);

		########################################################################
		# Resultats Integer Programming EuroCost
		########################################################################

		newcapacity <- NULL;
		usedcpu <- NULL;
		binmatrix <- NULL;
		jcpuvar <- NULL;
		benefit <- 0;
		resptime <- NULL;

		tmlim <- tlim1;

		gurobi <- TRUE;

		if (gurobi == FALSE) {

			cerca1 <- TRUE;

			while (benefit == 0)
			{
				print(paste("[",time,"]: ",tmlim,sep=""));

				if (cerca1) { system(paste("glpsol -m temp125.mod -d temp125.dat -o temp125.sol --tmlim ",tmlim," --log temp125.log",sep="")); }
				else	    { system(paste("glpsol -m temp125.mod -d temp125.dat -o temp125.sol --mipgap 100 --log temp125.log",sep="")); }
				system("echo `grep \"Problem\" temp125.sol` `grep \"benefici\" temp125.sol` > solution.tmp");
				x <- read.table("solution.tmp");
				benefit <- x[,5];

				system(paste("grep \"*\" temp125.sol | grep -v \"\\[\" | head -n ",numHosts*numJobs,"> solution.tmp"));
				x <- read.table("solution.tmp");
				binmatrix <- x[,2];
				dim(binmatrix) <- c(numHosts,numJobs);	# Ojo! de vegades inverteix les dimensions, el putes
				binmatrix <- t(binmatrix);

				system("cat temp125.sol | grep -A 2 \"maxcpu\" | grep -v \"maxcpu\" | grep -v \"unicitat\" | tr -s \" \" > solution.tmp");
				x <- read.table("solution.tmp");
				usedcpu <- x[,1];

				system("cat temp125.sol | grep -A 2 \"jcpuvar\" | grep -v \"jcpuvar\" | grep -v \"jmemvar\" | tr -s \" \" > solution.tmp");
				x <- read.table("solution.tmp");
				jcpuvar <- x[,2];

				system("cat temp125.sol | grep \"rt\\[\" | tr -s \" \" > solution.tmp");
				x <- read.table("solution.tmp");
				resptime <- x[,3];
				resptime[resptime<0] <- 0;

				system("cat temp125.log | grep \"Time used\" | tail -n 1 > solution.tmp");
				x <- read.table("solution.tmp");
				tspent <- x[,3];

				system("cat temp125.log | grep \"mip\" | tail -n 1 > solution.tmp");
				x <- read.table("solution.tmp");
				err <- x[,7];

				if (benefit == 0) cerca1 <- FALSE;
			}
		}

		if (gurobi == TRUE)
		{
			print(paste("[",time,"]: ",tmlim,sep=""));

			system(paste("export GRB_LICENSE_FILE=/opt/gurobi400/linux64/gurobi.lic; wrapper_GLPK_GUROBI -m temp125.mod -d temp125.dat --glpk_out --grb_out -v --tmlim ",tmlim,"> temp125.sol"));

			system("echo `grep \"benefici\" temp125.sol` > solution.tmp");
			x <- read.table("solution.tmp");
			benefit <- x[,8];

			system("grep \"x\\[\" temp125.sol > solution.tmp");
			x <- read.table("solution.tmp");
			binmatrix <- x[,8];
			dim(binmatrix) <- c(numHosts,numJobs);
			binmatrix <- t(binmatrix);

			system("grep \"usedcpu\" temp125.sol > solution.tmp");
			x <- read.table("solution.tmp");
			usedcpu <- x[,8];

			system("grep \"jcpuvar\" temp125.sol > solution.tmp");
			x <- read.table("solution.tmp");
			jcpuvar <- x[,8];

			system("grep \"rt\\[\" temp125.sol > solution.tmp");
			x <- read.table("solution.tmp");
			resptime <- x[,8];
			resptime[resptime<0] <- 0;

			system("grep \"Explored\" temp125.sol > solution.tmp");
			x <- read.table("solution.tmp");
			tspent <- x[,8];

			system("grep \"Absolute gap\" temp125.sol > solution.tmp");
			x <- read.table("solution.tmp");
			err <- x[,3];
		}

		########################################################################
		# Resultats de l'Scheduler
		########################################################################

		sla <- (1 - ((resptime - jrto) / (paramrt * jrto)));
		sla[sla < 0] <- 0;
		sla[sla > 1] <- 1;

		sumpower <- powercons(usedcpu);
		sumcpu <- sum(usedcpu);
		sumhosts <- sum(usedcpu>0);

		schedule <- NULL;
		for (j in 1:numJobs) {
			schedule <- c(schedule,jobs[j]);
			for (k in 1:numHosts) {
				if (binmatrix[j,k]==1) schedule <- c(schedule,k);
			}
		}
		dim(schedule) <- c(2,numJobs);

		migrations <- migs(jobs,numHosts,binmatrix,oldbinmat);

		migsaux <- 0;
		for (i in 1:length(jobs)) {
			if (length(oldbinmat[2,oldbinmat[1,]==jobs[i]])!=0) {
				x <- oldbinmat[2,oldbinmat[1,]==jobs[i]];
				y <- 0;
				for(k in 1:numHosts) if (binmatrix[i,k]==1) y <- k;
		
				if (x != y) migsaux <- migsaux + 1;
			}
		}

		print(paste("Benefit: ",benefit," Power: ",sumpower," Migrations: ",migrations,"//",migsaux," SumCPU: ",sumcpu," SumHosts: ",sumhosts," Tspent: ",tspent,"\n"));

		########################################################################
		# Desar Historial
		########################################################################

		auxpwrhist <- c(auxpwrhist,sumpower);			
		auxmighist <- c(auxmighist,migrations);		
		auxcosthist <- c(auxcosthist, benefit);		

		auxqoshist <- c(auxqoshist,mean(sla));
		auxqosmaxhist <- c(auxqosmaxhist,max(sla));
		auxqosminhist <- c(auxqosminhist,min(sla));
		auxtspenthist <- c(auxtspenthist,tspent);
		auxerrhist <- c(auxerrhist,err);

		auxcpuhist <- c(auxcpuhist,sumcpu);
		auxhostshist <- c(auxhostshist,sumhosts);

		oldbinmat <- schedule;
	}

	aux <- list(auxpwrhist,auxmighist,auxcosthist,auxqoshist,auxqosmaxhist,auxqosminhist,auxtspenthist,auxerrhist,auxcpuhist,auxhostshist);

	#results[[exps]] <- aux;
	#statresults[[exps]] <- aux;
	#auxresults[[exps]] <- aux;
	#auxresults2[[exps]] <- aux;
	auxrepeticions[[exps]] <- aux;

	start <- 0;

# FINAL DE LA RONDA
}

print("Final EURO-SCHED");
stop();

# auxresults <- No Migs
# results <- Migs

print("Power  Migs  Benefit  QoS  MaxQoS  MinQoS  TimeSpent  Error  UsedCPU UsedHosts");
for (i in 5:9) {
	a <- auxresults2[[i]];
	aux <- NULL;
	print(
		paste(sum(a[[1]]),"  ",sum(a[[2]]),"  ",sum(a[[3]]),"  ",mean(a[[4]]),"  ",
		mean(a[[5]]),"  ",mean(a[[6]]),"  ",mean(a[[7]]),"  ",mean(a[[8]]),"  ",sum(a[[9]])," ",sum(a[[10]]), sep="")
	);
}

a1 <- auxresults[[1]]
a2 <- auxresults[[2]]
a3 <- auxresults[[3]]
a4 <- auxresults[[4]]
a5 <- auxresults[[5]]
a6 <- auxresults[[6]]
a7 <- auxresults[[7]]
a8 <- auxresults[[8]]
a9 <- auxresults[[9]]
a10 <- auxresults[[10]]
a11 <- auxresults[[11]]
a12 <- auxresults[[12]]
a13 <- auxresults[[13]]
a14 <- auxresults[[14]]
a15 <- auxresults[[15]]
a16 <- auxresults[[16]]
a17 <- auxresults[[17]]
a18 <- auxresults[[18]]

png(filename = "23-experiments1.png",width = 1000, height = 500, units = "px",pointsize = 12, bg = "white");
par(mfcol= c(1, 2));
	boxplot(a18[[4]],a15[[4]],a1[[4]],a2[[4]],a3[[4]],a4[[4]],a5[[4]],a6[[4]],a7[[4]],a8[[4]],a9[[4]],
		xlab="Cost of Power (euro/Kwh)",ylab="Average SLA Fulfillment",main="SLA Fulfillment depending on Power Cost",
		names=c("0.00","0.01","0.09", "0.14", "0.45", "0.90","1.80","3.60","7.20","14.40","28.80")); # RT
	boxplot(a18[[1]],a15[[1]],a1[[1]],a2[[1]],a3[[1]],a4[[1]],a5[[1]],a6[[1]],a7[[1]],a8[[1]],a9[[1]],
		xlab="Cost of Power (euro/Kwh)",ylab="Average Power Consumption",main="Power Consumption depending on Power Cost",
		names=c("0.00","0.01","0.09", "0.14", "0.45", "0.90","1.80","3.60","7.20","14.40","28.80")); # Power
dev.off();

# Resultats [0.17, 9e-05, 2RT]
ffp <- ffresults[[2]]
rrp <- rrresults[[2]]
bfp <- bfresults[[7]]
ilpp <- auxresults[[1]]

#png(filename = "13-powercons.png",width = 750, height = 750, units = "px",pointsize = 12, bg = "white");
par(mfcol= c(2, 1));
	plot(-20,0,xlim=c(0,length(ffp[[1]])),ylim=c(0,max(ffp[[1]],rrp[[1]],bfp[[1]],ilpp[[1]])+500),xlab="Time (Hours)",ylab="Power Consumption");
	lines(ffp[[1]],lty=4);
	lines(rrp[[1]],lty=3);
	lines(bfp[[1]],lty=2);
	lines(ilpp[[1]],lty=1);
	legend("topleft", c("First Fit","Lambda-RR","Best Fit","ILP"), lty=c(4,3,2,1), horiz=TRUE);

	plot(-20,0,xlim=c(0,length(ffp[[4]])),ylim=c(min(ffp[[4]],rrp[[4]],bfp[[4]],ilpp[[4]]),max(ffp[[4]],rrp[[4]],bfp[[4]],ilpp[[4]])+0.1),xlab="Time (Hours)",ylab="SLA fulfillment level");
	lines(ffp[[4]],lty=4);
	lines(rrp[[4]],lty=3);
	lines(bfp[[4]],lty=2);
	lines(ilpp[[4]],lty=1);
	legend("topleft", c("First Fit","Lambda-RR","Best Fit","ILP"), lty=c(4,3,2,1), horiz=TRUE);
#dev.off();

#############################


ar1 <- auxrepeticions[[19]]
ar2 <- auxrepeticions[[20]]
ar3 <- auxrepeticions[[21]]
ar4 <- auxrepeticions[[22]]
ar5 <- auxrepeticions[[23]]
ar6 <- auxrepeticions[[24]]
ar7 <- auxrepeticions[[25]]
ar8 <- auxrepeticions[[26]]
ar9 <- auxrepeticions[[27]]
ar10 <- auxrepeticions[[28]]

#boxplot(b1[[4]],b2[[4]],b3[[4]],b4[[4]],b5[[4]],b6[[4]],b7[[4]],b8[[4]],b9[[4]],b10[[4]],xlab="Cost of Power (euro/Kwh)",ylab="Average SLA Fulfillment",main="SLA Fulfillment depending on Power Cost",); # RT
#hist(c(mean(b1[[4]]),mean(b2[[4]]),mean(b3[[4]]),mean(b4[[4]]),mean(b5[[4]]),mean(b6[[4]]),mean(b7[[4]]),mean(b8[[4]]),mean(b9[[4]]),mean(b10[[4]])))

index <- 4

mean(c(mean(ar1[[index]]),mean(ar2[[index]]),mean(ar3[[index]]),mean(ar4[[index]]),mean(ar5[[index]]),mean(ar6[[index]]),mean(ar7[[index]]),mean(ar8[[index]]),mean(ar9[[index]]),mean(ar10[[index]])))
aux <- var(c(mean(ar1[[index]]),mean(ar2[[index]]),mean(ar3[[index]]),mean(ar4[[index]]),mean(ar5[[index]]),mean(ar6[[index]]),mean(ar7[[index]]),mean(ar8[[index]]),mean(ar9[[index]]),mean(ar10[[index]])))
aux
sqrt(aux)
max(c(mean(ar1[[index]]),mean(ar2[[index]]),mean(ar3[[index]]),mean(ar4[[index]]),mean(ar5[[index]]),mean(ar6[[index]]),mean(ar7[[index]]),mean(ar8[[index]]),mean(ar9[[index]]),mean(ar10[[index]])))
min(c(mean(ar1[[index]]),mean(ar2[[index]]),mean(ar3[[index]]),mean(ar4[[index]]),mean(ar5[[index]]),mean(ar6[[index]]),mean(ar7[[index]]),mean(ar8[[index]]),mean(ar9[[index]]),mean(ar10[[index]])))


index <- 10

mean(c(sum(ar1[[index]]),sum(ar2[[index]]),sum(ar3[[index]]),sum(ar4[[index]]),sum(ar5[[index]]),sum(ar6[[index]]),sum(ar7[[index]]),sum(ar8[[index]]),sum(ar9[[index]]),sum(ar10[[index]])))
aux <- var(c(sum(ar1[[index]]),sum(ar2[[index]]),sum(ar3[[index]]),sum(ar4[[index]]),sum(ar5[[index]]),sum(ar6[[index]]),sum(ar7[[index]]),sum(ar8[[index]]),sum(ar9[[index]]),sum(ar10[[index]])))
aux
sqrt(aux)
max(c(sum(ar1[[index]]),sum(ar2[[index]]),sum(ar3[[index]]),sum(ar4[[index]]),sum(ar5[[index]]),sum(ar6[[index]]),sum(ar7[[index]]),sum(ar8[[index]]),sum(ar9[[index]]),sum(ar10[[index]])))
min(c(sum(ar1[[index]]),sum(ar2[[index]]),sum(ar3[[index]]),sum(ar4[[index]]),sum(ar5[[index]]),sum(ar6[[index]]),sum(ar7[[index]]),sum(ar8[[index]]),sum(ar9[[index]]),sum(ar10[[index]])))


