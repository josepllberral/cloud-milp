
################################################################################
# Inicialització i Workloads
################################################################################

# Directori Base
base <- "/home/jlberral/Desktop/sim-lip/";
workprefix <- paste(base,"/workload/http-",sep="");

# Workload i Hosts
hosts <- read.table(paste(base,"datacenter-inv1-mini.data",sep=""),header=T);
#hosts <- read.table(paste(base,"datacenter-inv1.data",sep=""),header=T);
wkld <- read.table(paste(base,"workload-wservice1-mini.data",sep=""),header=T);
#wkld <- read.table(paste(base,"workload-wservice1.data",sep=""),header=T);

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

source("functions.r");

################################################################################
# Estrategia Best Fit
################################################################################

numHosts <- length(hosts[,"name"]);
hostList <- NULL;
mcpu <- NULL;
for (h in 1:numHosts) {
	hostList <- c(hostList,paste("Host",h,sep=""));
	mcpu <- c(mcpu,hosts[h,2]);
}


#bfresults <- list();


auxini <- 1;
auxfin <- 10;

for (exps in auxini:auxfin)
{
	print(paste("Iteració ",exps));
	time <- 0;
	oldbfmat <- NULL;
	counter <- 1;

	powbfhist <- NULL;
	migbfhist <- NULL;
	eurobfhist <- NULL;
	qosbfhist <- NULL;
	qosbfmaxhist <- NULL;
	qosbfminhist <- NULL;
	cpubfhist <- NULL;
	hostbfhist <- NULL;

	for(time in 0:max((timetable[,2]/3600)))
	{
		print(paste("Time: ", time));
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
		# Scheduling Best Fit
		########################################################################
	
		binmatrix <- rep(0,numHosts*numJobs);
		dim(binmatrix) <- c(numJobs,numHosts);

		ocup <- rep(0,numHosts);
		mocup <- rep(0,numHosts);
		jcpuvar <- rep(0,numJobs);

		# REORDENACIO
		dec <- TRUE;
		ord <- order(cpumax,decreasing=dec);

		jobs <- jobs[ord];
		jreq <- jreq[ord]; 
		jbyt <- jbyt[ord];
		jrto <- jrto[ord];
		jmem <- jmem[ord];
		jcpu <- jcpu[ord];
		jmbps <- jmbps[ord];
		jtpq <- jtpq[ord];
		cpumax <- cpumax[ord];
		# FI DE LA REORDENACIO

		for (j in 1:numJobs)
		{
			benefaux1 <- 0;
			candidate <- 1;
			jcpuvar[j] <- 1;
			for (k in 1:numHosts)
			{
				if ((ocup[k]+jcpu[j]) <= 400 && (mocup[k]+jmem[j]) <= 4000) 
				{
					binaux <- binmatrix;				
					binaux[j,k] <- 1;
					jcpuaux <- jcpuvar;

					auxfree <- 400 - ocup[k];
					ovar <-	jcpu[j];
					if ( auxfree > cpumax[j] ) {
						qvar <- cpumax[j];
					} else {
						qvar <- auxfree;
					}
					pvar <- ovar + floor((qvar-ovar)/1.6180339887);
					rvar <- ovar + (qvar - pvar);

					jcpuaux[j] <- pvar;
					pben <- benefici_global(binaux,jcpuaux,0.17,9e-05,1);
					#pben <- benefici_global_migs(binaux,jcpuaux,0.17,9e-05,1,oldbfmat,jobs,numHosts,0.014);
					jcpuaux[j] <- rvar;
					rben <- benefici_global(binaux,jcpuaux,0.17,9e-05,1);
					#rben <- benefici_global_migs(binaux,jcpuaux,0.17,9e-05,1,oldbfmat,jobs,numHosts,0.014);

					#for(m in jcpu[j]:cpumax[j])
					seguir <- TRUE;
					while(seguir)
					{
						if (rben > benefaux1 || benefaux1 == 0) {
							benefaux1 <- rben;
							candidate <- k;
							jcpuvar[j] <- rvar;
						}

						if (rvar < pvar) {
							if (rben > pben) {
								qvar <- pvar;
								pvar <- rvar;
								qben <- pben;
								pben <- rben;
							} else {
								ovar <- rvar;
								oben <- rben;
							}
						} else {
							if (pben > rben) {
								qvar <- rvar;
								qben <- rben;
							} else {
								ovar <- pvar;
								pvar <- rvar;
								oben <- pben;
								pben <- rben;
							}
						}
						rvar <- ovar + (qvar - pvar);

						jcpuaux[j] <- rvar;
						rben <- benefici_global(binaux,jcpuaux,0.17,9e-05,1);
						#rben <- benefici_global_migs(binaux,jcpuaux,0.17,9e-05,1,oldbfmat,jobs,numHosts,0.014);

						if (pvar == qvar || ovar == qvar || rvar == qvar) seguir <- FALSE;
					}
				}
			}
			binmatrix[j,candidate] <- 1;
			ocup[candidate] <- ocup[candidate] + jcpuvar[j];
			mocup[candidate] <- mocup[candidate] + jmem[j];
		}	

		########################################################################
		# Resultats de l'Scheduler Inicial
		########################################################################

		kmat <- binmatrix*jcpuvar;
		ocupacio <- colSums(kmat);
		usedcpu <- ceiling(ocupacio/100);

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

		migrations <- migs(jobs,numHosts,binmatrix,oldbfmat);

		resptime <- NULL;
		minresptime <- NULL;
		for (i in 1:length(jobs))
		{
			pos <- 0;
			for (k in 1:numHosts) { if (binmatrix[i,k]==1) pos <- k; }

			memsum <- 0;
			for (k in 1:length(jobs)) { if (binmatrix[k,pos]==1) memsum <- memsum + jmem[k]; }

			if (pos == 0) { rtaux <- 999999; }
			else { rtaux <- RT(jreq[i],jbyt[i],jmbps[i],jtpq[i],ocupacio[pos],mocup[pos],jcpuvar[i],jmem[i],jcpu[i],jmem[i]); }
			resptime <- c(resptime,rtaux);
			minresptime <- c(minresptime,jrto[i]);

		}

		sla <- (1 - ((resptime - minresptime) / (1 * minresptime)));
		sla[sla < 0] <- 0;
		sla[sla > 1] <- 1;

		benefit <- sum(sla) * 0.17 - sumpower * 9e-05;# - migrations * 0.014;

		print(paste("Benefit: ",benefit," Power: ",sumpower," Migrations: ",migrations," SumCPU: ",sumcpu," SumHosts: ",sumhosts,"\n"));

		########################################################################
		# Desar Historial
		########################################################################

		powbfhist <- c(powbfhist,sumpower);
		migbfhist <- c(migbfhist,migrations);
		eurobfhist <- c(eurobfhist,benefit);
		qosbfhist <- c(qosbfhist,mean(sla));
		qosbfmaxhist <- c(qosbfmaxhist,max(sla));
		qosbfminhist <- c(qosbfminhist,min(sla));
		cpubfhist <- c(cpubfhist,sumcpu);
		hostbfhist <- c(hostbfhist,sumhosts);

		oldbfmat <- schedule;
	}

	## Sumari de tot (BF)
	sum(eurobfhist);
	sum(powbfhist);
	sum(migbfhist);
	mean(qosbfhist);
	mean(qosbfmaxhist);
	mean(qosbfminhist);
	sum(cpubfhist);
	sum(hostbfhist);

	aux <- list(powbfhist,migbfhist,eurobfhist,qosbfhist,qosbfmaxhist,qosbfminhist,cpubfhist,hostbfhist);
	bfresults[[exps]] <- aux;
}


stop();


b1 <- bfresults[[1]]
b2 <- bfresults[[2]]
b3 <- bfresults[[3]]
b4 <- bfresults[[4]]
b5 <- bfresults[[5]]
b6 <- bfresults[[6]]
b7 <- bfresults[[7]]
b8 <- bfresults[[8]]
b9 <- bfresults[[9]]
b10 <- bfresults[[10]]

#boxplot(b1[[4]],b2[[4]],b3[[4]],b4[[4]],b5[[4]],b6[[4]],b7[[4]],b8[[4]],b9[[4]],b10[[4]],xlab="Cost of Power (euro/Kwh)",ylab="Average SLA Fulfillment",main="SLA Fulfillment depending on Power Cost",); # RT
#hist(c(mean(b1[[4]]),mean(b2[[4]]),mean(b3[[4]]),mean(b4[[4]]),mean(b5[[4]]),mean(b6[[4]]),mean(b7[[4]]),mean(b8[[4]]),mean(b9[[4]]),mean(b10[[4]])))

index <- 4

mean(c(mean(b1[[index]]),mean(b2[[index]]),mean(b3[[index]]),mean(b4[[index]]),mean(b5[[index]]),mean(b6[[index]]),mean(b7[[index]]),mean(b8[[index]]),mean(b9[[index]]),mean(b10[[index]])))
aux <- var(c(mean(b1[[index]]),mean(b2[[index]]),mean(b3[[index]]),mean(b4[[index]]),mean(b5[[index]]),mean(b6[[index]]),mean(b7[[index]]),mean(b8[[index]]),mean(b9[[index]]),mean(b10[[index]])))
aux
sqrt(aux)
max(c(mean(b1[[index]]),mean(b2[[index]]),mean(b3[[index]]),mean(b4[[index]]),mean(b5[[index]]),mean(b6[[index]]),mean(b7[[index]]),mean(b8[[index]]),mean(b9[[index]]),mean(b10[[index]])))
min(c(mean(b1[[index]]),mean(b2[[index]]),mean(b3[[index]]),mean(b4[[index]]),mean(b5[[index]]),mean(b6[[index]]),mean(b7[[index]]),mean(b8[[index]]),mean(b9[[index]]),mean(b10[[index]])))


index <- 8

mean(c(sum(b1[[index]]),sum(b2[[index]]),sum(b3[[index]]),sum(b4[[index]]),sum(b5[[index]]),sum(b6[[index]]),sum(b7[[index]]),sum(b8[[index]]),sum(b9[[index]]),sum(b10[[index]])))
aux <- var(c(sum(b1[[index]]),sum(b2[[index]]),sum(b3[[index]]),sum(b4[[index]]),sum(b5[[index]]),sum(b6[[index]]),sum(b7[[index]]),sum(b8[[index]]),sum(b9[[index]]),sum(b10[[index]])))
aux
sqrt(aux)
max(c(sum(b1[[index]]),sum(b2[[index]]),sum(b3[[index]]),sum(b4[[index]]),sum(b5[[index]]),sum(b6[[index]]),sum(b7[[index]]),sum(b8[[index]]),sum(b9[[index]]),sum(b10[[index]])))
min(c(sum(b1[[index]]),sum(b2[[index]]),sum(b3[[index]]),sum(b4[[index]]),sum(b5[[index]]),sum(b6[[index]]),sum(b7[[index]]),sum(b8[[index]]),sum(b9[[index]]),sum(b10[[index]])))

##1 <- 0.17,45e-05,1
##2 <- 0.17,09e-05,1

##4 <- 0.17,09e-05,1 DESC
##5 <- 0.17,45e-05,1 DESC
##6 <- 0.17,09e-05,9  DESC

##7 <- 0.17,09e-05,1 DESC, NEW
##8 <- 0.17,09e-05,9  DESC, NEW

##9 <- 0.17,09e-05,1 DESC, MIGS
##10 <- 0.17,09e-05,1 ASC, MIGS

#bfresults <- list();
aux <- list(powbfhist,migbfhist,eurobfhist,qosbfhist,qosbfmaxhist,qosbfminhist,cpubfhist,hostbfhist);
bfresults[[10]] <- aux;



