
################################################################################
# Inicialització i Workloads
################################################################################

# Directori Base
base <- "/home/jlberral/Desktop/sim-lip/";
workprefix <- paste(base,"/workload/http-",sep="");

# Workload i Hosts
hosts <- read.table(paste(base,"datacenter-inv1.data",sep=""),header=T);
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

source("functions.r");

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

#rrresults <- list();

auxini <- 1;
auxfin <- 10;

for (exps in auxini:auxfin)
{
	counter <- 1;
	numAvHosts <- 0;
	lambda1 <- 30;
	time <- 0;

	oldrrmat <- NULL;

	pwrrrhist <- NULL;
	migrrhist <- NULL;
	eurorrhist <- NULL;
	qosrrhist <- NULL;
	qosrrmaxhist <- NULL;
	qosrrminhist <- NULL;
	cpurrhist <- NULL;
	hostrrhist <- NULL;

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
		# Recalcul NumHosts
		########################################################################
	
		numAvHosts <- recalculateHosts(lambda1,sum(cpumax),mcpu);
		if (counter > numAvHosts) counter <- 1;

		########################################################################
		# Scheduling RR
		########################################################################
	
		binmatrix <- rep(0,(numAvHosts+1)*numJobs);
		dim(binmatrix) <- c(numJobs,(numAvHosts+1));

		ocup <- rep(0,(numAvHosts+1));
		mocup <- rep(0,(numAvHosts+1));

		for (j in 1:numJobs) {

			visited <- 0;
			while (
				((ocup[counter]+cpumax[j]) > (mcpu[counter]*100)
				|
				(mocup[counter]+jmem[j]) > (4*1000))
				&
				(visited <= numAvHosts)
			) {
				counter <- counter +1;
				if (counter > numAvHosts) counter <- 1;
				visited <- visited + 1;
			}
			if (visited <= numAvHosts)
			{
				binmatrix[j,counter] <- 1;
				ocup[counter] <- ocup[counter] + cpumax[j];
				mocup[counter] <- mocup[counter] + jmem[j];
				counter <- counter +1;
				if (counter > numAvHosts) counter <- 1;
			}
			else
			{
				binmatrix[j,(numAvHosts+1)] <- 1;
				ocup[(numAvHosts+1)] <- ocup[(numAvHosts+1)] + cpumax[j];
				mocup[(numAvHosts+1)] <- mocup[(numAvHosts+1)] + jmem[j];
			}
		}	

		########################################################################
		# Resultats de l'Scheduler
		########################################################################

		kmat <- binmatrix*cpumax;
		ocupacio <- colSums(kmat);
		usedcpu <- ceiling(ocupacio/100);

		sumpower <- powercons(usedcpu);
		sumcpu <- sum(usedcpu);
		sumhosts <- sum(usedcpu>0);

		schedule <- NULL;
		for (j in 1:numJobs) {
			schedule <- c(schedule,jobs[j]);
			for (k in 1:(numAvHosts+1)) {
				if (binmatrix[j,k]==1) schedule <- c(schedule,k);
			}
		}
		dim(schedule) <- c(2,numJobs);

		migrations <- migs(jobs,(numAvHosts+1),binmatrix,oldffmat);

		resptime <- NULL;
		minresptime <- NULL;
		for (i in 1:length(jobs))
		{
			for (k in 1:(numAvHosts+1)) { if (binmatrix[i,k]==1) pos <- k; }

			memsum <- 0;
			for (k in 1:length(jobs)) { if (binmatrix[k,pos]==1) memsum <- memsum + jmem[k]; }

			rtaux <- RT(jreq[i],jbyt[i],jmbps[i],jtpq[i],ocupacio[pos],mocup[pos],jcpuvar[i],jmem[i],jcpu[i],jmem[i]);
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

		pwrrrhist <- c(pwrrrhist,sumpower);
		migrrhist <- c(migrrhist,migrations);
		eurorrhist <- c(eurorrhist,benefit);
		qosrrhist <- c(qosrrhist,mean(sla));
		qosrrmaxhist <- c(qosrrmaxhist,max(sla));
		qosrrminhist <- c(qosrrminhist,min(sla));
		cpurrhist <- c(cpurrhist,sumcpu);
		hostrrhist <- c(hostrrhist,sumhosts);

		oldrrmat <- schedule;
	}

	sum(eurorrhist);
	sum(pwrrrhist);
	sum(migrrhist);
	mean(qosrrhist);
	mean(qosrrmaxhist);
	mean(qosrrminhist);
	sum(cpurrhist);
	sum(hostrrhist);

	aux <- list(pwrrrhist,migrrhist,eurorrhist,qosrrhist,qosrrmaxhist,qosrrminhist,cpurrhist,hostrrhist);
	rrresults[[exps]] <- aux;
}

stop();


r1 <- rrresults[[1]]
r2 <- rrresults[[2]]
r3 <- rrresults[[3]]
r4 <- rrresults[[4]]
r5 <- rrresults[[5]]
r6 <- rrresults[[6]]
r7 <- rrresults[[7]]
r8 <- rrresults[[8]]
r9 <- rrresults[[9]]
r10 <- rrresults[[10]]

#boxplot(r1[[4]],r2[[4]],r3[[4]],r4[[4]],r5[[4]],r6[[4]],r7[[4]],r8[[4]],r9[[4]],r10[[4]]); # RT
#hist(c(mean(r1[[4]]),mean(r2[[4]]),mean(r3[[4]]),mean(r4[[4]]),mean(r5[[4]]),mean(r6[[4]]),mean(r7[[4]]),mean(r8[[4]]),mean(r9[[4]]),mean(r10[[4]])))

index <- 4

mean(c(mean(r1[[index]]),mean(r2[[index]]),mean(r3[[index]]),mean(r4[[index]]),mean(r5[[index]]),mean(r6[[index]]),mean(r7[[index]]),mean(r8[[index]]),mean(r9[[index]]),mean(r10[[index]])))
aux <- var(c(mean(r1[[index]]),mean(r2[[index]]),mean(r3[[index]]),mean(r4[[index]]),mean(r5[[index]]),mean(r6[[index]]),mean(r7[[index]]),mean(r8[[index]]),mean(r9[[index]]),mean(r10[[index]])))
aux
sqrt(aux)
max(c(mean(r1[[index]]),mean(r2[[index]]),mean(r3[[index]]),mean(r4[[index]]),mean(r5[[index]]),mean(r6[[index]]),mean(r7[[index]]),mean(r8[[index]]),mean(r9[[index]]),mean(r10[[index]])))
min(c(mean(r1[[index]]),mean(r2[[index]]),mean(r3[[index]]),mean(r4[[index]]),mean(r5[[index]]),mean(r6[[index]]),mean(r7[[index]]),mean(r8[[index]]),mean(r9[[index]]),mean(r10[[index]])))


index <- 8

mean(c(sum(r1[[index]]),sum(r2[[index]]),sum(r3[[index]]),sum(r4[[index]]),sum(r5[[index]]),sum(r6[[index]]),sum(r7[[index]]),sum(r8[[index]]),sum(r9[[index]]),sum(r10[[index]])))
aux <- var(c(sum(r1[[index]]),sum(r2[[index]]),sum(r3[[index]]),sum(r4[[index]]),sum(r5[[index]]),sum(r6[[index]]),sum(r7[[index]]),sum(r8[[index]]),sum(r9[[index]]),sum(r10[[index]])))
aux
sqrt(aux)
max(c(sum(r1[[index]]),sum(r2[[index]]),sum(r3[[index]]),sum(r4[[index]]),sum(r5[[index]]),sum(r6[[index]]),sum(r7[[index]]),sum(r8[[index]]),sum(r9[[index]]),sum(r10[[index]])))
min(c(sum(r1[[index]]),sum(r2[[index]]),sum(r3[[index]]),sum(r4[[index]]),sum(r5[[index]]),sum(r6[[index]]),sum(r7[[index]]),sum(r8[[index]]),sum(r9[[index]]),sum(r10[[index]])))

##1 <- 0.17,45e-05,1
##2 <- 0.17,09e-05,1
##3 <- 0.17,09e-05,9

#rrresults <- list();
aux <- list(pwrrrhist,migrrhist,eurorrhist,qosrrhist,qosrrmaxhist,qosrrminhist,cpurrhist,hostrrhist);
rrresults[[3]] <- aux;


