
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
# Estrategia First Fit
################################################################################

numHosts <- length(hosts[,"name"]);
hostList <- NULL;
mcpu <- NULL;
for (h in 1:numHosts) {
	hostList <- c(hostList,paste("Host",h,sep=""));
	mcpu <- c(mcpu,hosts[h,2]);
}

#ffresults <- list();

auxini <- 1;
auxfin <- 10;

for (exps in auxini:auxfin)
{
	print(paste("Iteració ",exps));
	time <- 0;
	oldffmat <- NULL;
	counter <- 1;

	powffhist <- NULL;
	migffhist <- NULL;
	euroffhist <- NULL;
	qosffhist <- NULL;
	qosffmaxhist <- NULL;
	qosffminhist <- NULL;
	cpuffhist <- NULL;
	hostffhist <- NULL;

	for(time in 0:max((timetable[,2]/3600)))
	{
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
		# Scheduling FF
		########################################################################
	
		binmatrix <- rep(0,numHosts*numJobs);
		dim(binmatrix) <- c(numJobs,numHosts);

		ocup <- rep(0,numHosts);
		mocup <- rep(0,numHosts);

		for (j in 1:numJobs) {
			brk <- 0;
			while (((ocup[counter]+cpumax[j])>(mcpu[counter]*100)
				|
				(mocup[counter]+jmem[j]) > (4*1000))
				&
				brk<numHosts)
			{
				counter <- counter +1;
				if (counter > numHosts) counter <- 1;	
				brk <- brk+1;
			}
			binmatrix[j,counter] <- 1;
			ocup[counter] <- ocup[counter] + cpumax[j];
			mocup[counter] <- mocup[counter] + jmem[j];
			counter <- 1;
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
			for (k in 1:numHosts) {
				if (binmatrix[j,k]==1) schedule <- c(schedule,k);
			}
		}
		dim(schedule) <- c(2,numJobs);

		migrations <- migs(jobs,numHosts,binmatrix,oldffmat);

		resptime <- NULL;
		minresptime <- NULL;
		for (i in 1:length(jobs))
		{
			for (k in 1:numHosts) { if (binmatrix[i,k]==1) pos <- k; }

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

		powffhist <- c(powffhist,sumpower);
		migffhist <- c(migffhist,migrations);
		euroffhist <- c(euroffhist,benefit);
		qosffhist <- c(qosffhist,mean(sla));
		qosffmaxhist <- c(qosffhist,max(sla));
		qosffminhist <- c(qosffhist,min(sla));
		cpuffhist <- c(cpuffhist,sumcpu);
		hostffhist <- c(hostffhist,sumhosts);

		oldffmat <- schedule;
	}

	## Sumari de tot (FF)
	sum(euroffhist);
	sum(powffhist);
	sum(migffhist);
	mean(qosffhist);
	mean(qosffmaxhist);
	mean(qosffminhist);
	sum(cpuffhist);
	sum(hostffhist);

	aux <- list(powffhist,migffhist,euroffhist,qosffhist,qosffmaxhist,qosffminhist,cpuffhist,hostffhist);
	ffresults[[exps]] <- aux;
}
stop();


f1 <- ffresults[[1]]
f2 <- ffresults[[2]]
f3 <- ffresults[[3]]
f4 <- ffresults[[4]]
f5 <- ffresults[[5]]
f6 <- ffresults[[6]]
f7 <- ffresults[[7]]
f8 <- ffresults[[8]]
f9 <- ffresults[[9]]
f10 <- ffresults[[10]]

#boxplot(f1[[4]],f2[[4]],f3[[4]],f4[[4]],f5[[4]],f6[[4]],f7[[4]],f8[[4]],f9[[4]],f10[[4]]); # RT
#hist(c(mean(f1[[4]]),mean(f2[[4]]),mean(f3[[4]]),mean(f4[[4]]),mean(f5[[4]]),mean(f6[[4]]),mean(f7[[4]]),mean(f8[[4]]),mean(f9[[4]]),mean(f10[[4]])))

index <- 4

mean(c(mean(f1[[index]]),mean(f2[[index]]),mean(f3[[index]]),mean(f4[[index]]),mean(f5[[index]]),mean(f6[[index]]),mean(f7[[index]]),mean(f8[[index]]),mean(f9[[index]]),mean(f10[[index]])))
aux <- var(c(mean(f1[[index]]),mean(f2[[index]]),mean(f3[[index]]),mean(f4[[index]]),mean(f5[[index]]),mean(f6[[index]]),mean(f7[[index]]),mean(f8[[index]]),mean(f9[[index]]),mean(f10[[index]])))
aux
sqrt(aux)
max(c(mean(f1[[index]]),mean(f2[[index]]),mean(f3[[index]]),mean(f4[[index]]),mean(f5[[index]]),mean(f6[[index]]),mean(f7[[index]]),mean(f8[[index]]),mean(f9[[index]]),mean(f10[[index]])))
min(c(mean(f1[[index]]),mean(f2[[index]]),mean(f3[[index]]),mean(f4[[index]]),mean(f5[[index]]),mean(f6[[index]]),mean(f7[[index]]),mean(f8[[index]]),mean(f9[[index]]),mean(f10[[index]])))


index <- 8

mean(c(sum(f1[[index]]),sum(f2[[index]]),sum(f3[[index]]),sum(f4[[index]]),sum(f5[[index]]),sum(f6[[index]]),sum(f7[[index]]),sum(f8[[index]]),sum(f9[[index]]),sum(f10[[index]])))
aux <- var(c(sum(f1[[index]]),sum(f2[[index]]),sum(f3[[index]]),sum(f4[[index]]),sum(f5[[index]]),sum(f6[[index]]),sum(f7[[index]]),sum(f8[[index]]),sum(f9[[index]]),sum(f10[[index]])))
aux
sqrt(aux)
max(c(sum(f1[[index]]),sum(f2[[index]]),sum(f3[[index]]),sum(f4[[index]]),sum(f5[[index]]),sum(f6[[index]]),sum(f7[[index]]),sum(f8[[index]]),sum(f9[[index]]),sum(f10[[index]])))
min(c(sum(f1[[index]]),sum(f2[[index]]),sum(f3[[index]]),sum(f4[[index]]),sum(f5[[index]]),sum(f6[[index]]),sum(f7[[index]]),sum(f8[[index]]),sum(f9[[index]]),sum(f10[[index]])))

##1 <- 0.17,45e-05,1
##2 <- 0.17,09e-05,1
##3 <- 0.17,09e-05,9

#ffresults <- list();
aux <- list(powffhist,migffhist,euroffhist,qosffhist,qosffmaxhist,qosffminhist,cpuffhist,hostffhist);
ffresults[[3]] <- aux;


