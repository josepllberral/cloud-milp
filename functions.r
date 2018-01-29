################################################################################
# Funcions
################################################################################


desCPU <- function (reqs,bytespq,timepq)
{
	## CELERON M
#	reqcpu <- 17.2214277 + 0.2911892 * reqs - 0.0001129 * bytespq;

	## XEON 4 CORE
	reqcpu <- 1;
	if (reqs <= 576.5) {
		if (timepq <= 3404.019) {
			if (reqs <= 411.5) { reqcpu <- 0.04620767 * reqs + 0.00361214 * timepq -  5.926363; }
			else {
				if (bytespq <= 41927.799) { reqcpu <- 0.02645041 * reqs - 0.0001369089 * bytespq - 0.0007096089 * timepq + 35.6245; }
				else { reqcpu <- 0.02645041 * reqs - 0.0002000944 * bytespq - 0.001266081 * timepq + 35.5955; }
			}
		}
		else { reqcpu <- 0.01823293 * reqs + 0.00051134 * bytespq + 0.0001556182 * timepq -  19.9455; }
	}
	else { reqcpu <- 0.07544295 * reqs + 0.002768338 * timepq -  0.4847; }
	
	if (reqcpu > 400 ) reqcpu <- 400;
	if (reqcpu < 1) reqcpu <- 1;
	reqcpu;
}


desCPU2 <- function (reqs,bytes,timepq,mbpspq,memvm)
{
	## CELERON M
#	reqcpu <- 17.2214277 + 0.2911892 * reqs - 0.0001129 * bytes;

	## XEON 4 CORE
	reqcpu <- 1;
	if (reqs <= 993) {
		if (reqs <= 255) {
			reqcpu <- 0.0093 * reqs - 0.0038 * mbpspq + 100.204 * timepq - 0.0264 * memvm + 7.9243;
		} else {
			reqcpu <- 0.0009 * reqs - 0.0046 * mbpspq + 1948.7123 * timepq - 0.0046 * memvm + 3.7874;
		}
	} else {
		reqcpu <- 0.0006 * reqs - 0.0017 * mbpspq + 3722.1517 * timepq + 3.4816;
	}

	if (reqcpu < 1) reqcpu <- 1;
	reqcpu;
}

desCPU3 <- function (reqs,bytes,rto)
{
	## XEON 4 CORE
	reqcpu <- 1;
	if (reqs <= 993) {
		if (reqs <= 255) {
			reqcpu <- 0.008813322*reqs + 0.00002089*bytes + 2.6521;
		} else {
			reqcpu <- 0.000982871*reqs + 0.000068024*bytes + 5.1828;
		}
	} else {
		reqcpu <- 0.0005582153*reqs + 0.0001749918*bytes + 6.2151;
	}


	if (reqcpu < 1) reqcpu <- 1;
	reqcpu;
}

desMEM <- function (reqs,bytes,rto)
{
	##reqmem <- (rto - 8.578e-03 - 3.535e-05 * reqs - 1.218e-07 * bytes) / -4.849e-05;
	#reqmem <- (rto - 5.039e-03 - 3.555e-05 * reqs - 1.231e-07 * bytes) / -2.854e-05;
	#if (reqmem < 1) reqmem <- 1;
	reqmem <- 128;
	reqmem;

}

vecmbps <- c(5,13,303,347,268,228,189,116,29,17,10,20,30,40,50,60,70,80,90,100);
dim(vecmbps) <- c(10,2);
vecmbps[,1] <- vecmbps[,1] / sum(vecmbps[,1]);
for (i in 2:length(vecmbps[,1])) vecmbps[i,1] <- vecmbps[i,1] + vecmbps[(i-1),1];

randMBPS <- function ()
{
	rnd1 <- runif(1,min=0.0,max=1.0);
	aux_rnd1 <- vecmbps[vecmbps[,1]>=rnd1,];
	if ( length(dim(aux_rnd1)) < 2) aux_rnd1 <- t(as.matrix(aux_rnd1));
	aux_rnd1[1,2];
}

vectpq <- c(40,85,115,75,30,20,15,5,10,7,200,1000,2000,3000,4000,5000,6000,7000,8000,9000);
dim(vectpq) <- c(10,2);
vectpq[,1] <- vectpq[,1] / sum(vectpq[,1]);
for (i in 2:length(vectpq[,1])) vectpq[i,1] <- vectpq[i,1] + vectpq[(i-1),1];

randTPQ <- function ()
{
	rnd2 <- runif(1,min=0.0,max=1.0);
	aux_rnd2 <- vectpq[vectpq[,1]>=rnd2,];
	if ( length(dim(aux_rnd2)) < 2) aux_rnd2 <- t(as.matrix(aux_rnd2));
	aux_rnd2[1,2];
}

RT <- function (reqs,bytes,mbps,tpq,cpupm,mempm,rcpuvm,rmemvm,cpuvm,memvm)
{
	## CELERON M
#	resp <- - 5.047e+01 - 1.556e-06 * reqs + 5.05e-01 * cpupm + 7.189e-05 * mempm - 3.245e-04 * rcpuvm - 2.448e-04 * rmemvm	+ 7.184e-04 * cpuvm + 8.162e-05 * memvm;

	## XEON 4 CORE
#	resp <- + 5.595e-03 + 8.005e-08 * reqs - 1.813e-08 * bytes -7.078e-05 * mbps + 1.024e-02 * tpq + 7.805e-06 * cpupm - 1.130e-05 * rcpuvm + 1.082e-05 * cpuvm;

	## XEON 4 CORE NEW
	resp <- 3.950e-03 + 1.087e-05 * reqs - 4.147e-08 * bytes -1.919e-04 * mbps + 1.906e-08 * tpq + 2.954e-05 * cpupm - 3.633e-05 * rcpuvm + 4.762e-06 * cpuvm;
	if (resp <= 0) resp <- 0.0000001;
	resp;
}

INVRT <- function (reqs,bytes,mbps,tpq,cpupm,cpuvm,rto)
{
	rcpuvm <- ceiling((rto - 3.950e-03 - 1.087e-05 * reqs + 4.147e-08 * bytes + 1.919e-04 * mbps - 1.906e-08 * tpq - 2.954e-05 * cpupm - 4.762e-06 * cpuvm) / -3.633e-05);
	if (rcpuvm > 400)  { rcpuvm <- 400; }
	if (rcpuvm < 1) { rcpuvm <- 1; }
	rcpuvm;
}

powercons <- function(usedcpu) {
	sumpower <- 0;
	for (k in 1:(length(usedcpu)))
	{
		if (usedcpu[k] == 1) sumpower <- sumpower + 267.8;
		if (usedcpu[k] == 2) sumpower <- sumpower + 285.5;
		if (usedcpu[k] == 3) sumpower <- sumpower + 302.5;
		if (usedcpu[k] == 4) sumpower <- sumpower + 317.9;
		if (usedcpu[k] > 4) { print(paste("Warning ",usedcpu[k], " in machine ", k)); sumpower <- sumpower + 317.9;}
	}
	sumpower;
}

migs <- function(jobs,numHosts,binmatrix,oldbinmat) {
	migrations <- 0;
	for (i in 1:length(jobs)) {
		if (length(oldbinmat[2,oldbinmat[1,]==jobs[i]])!=0) {
			x <- oldbinmat[2,oldbinmat[1,]==jobs[i]];
			y <- 0;
			for(k in 1:numHosts) if (binmatrix[i,k]==1) y <- k;
	
			if (x != y) migrations <- migrations + 1;
		}
	}
	migrations;
}

benefici_global <- function(binmatrix,jcpuvar,ppj,ppp,alpha)
{
	kmat <- binmatrix*jcpuvar;
	ocupacio <- colSums(kmat);
	usedcpu <- ceiling(ocupacio/100);

	sumpower <- powercons(usedcpu);
	sumcpu <- sum(usedcpu);
	sumhosts <- sum(usedcpu>0);

	resptime <- NULL;
	minresptime <- NULL;
	for (i in 1:length(jobs))
	{
		if (sum(binmatrix[i,])==1) {
			for (k in 1:numHosts) { if (binmatrix[i,k]==1) pos <- k; }

			memsum <- 0;
			for (k in 1:length(jobs)) { if (binmatrix[k,pos]==1) memsum <- memsum + jmem[k]; }

			rtaux <- RT(jreq[i],jbyt[i],jmbps[i],jtpq[i],ocupacio[pos],mocup[pos],jcpuvar[i],jmem[i],jcpu[i],jmem[i]);
			resptime <- c(resptime,rtaux);
			minresptime <- c(minresptime,jrto[i]);
		}
	}

	slaaux <-  (1 - ((resptime - minresptime) / (alpha * minresptime)));
	slaaux[slaaux < 0] <- 0;
	slaaux[slaaux > 1] <- 1;


	benefit <- sum(slaaux) * ppj - sumpower * ppp;
	benefit;
}

benefici_global_migs <- function(binmatrix,jcpuvar,ppj,ppp,alpha,oldbinmat,jobs,nhosts,ppm)
{
	kmat <- binmatrix*jcpuvar;
	ocupacio <- colSums(kmat);
	usedcpu <- ceiling(ocupacio/100);

	sumpower <- powercons(usedcpu);
	sumcpu <- sum(usedcpu);
	sumhosts <- sum(usedcpu>0);

	migracions <- migs(jobs,nhosts,binmatrix,oldbinmat);

	resptime <- NULL;
	minresptime <- NULL;
	for (i in 1:length(jobs))
	{
		if (sum(binmatrix[i,])==1) {
			for (k in 1:nhosts) { if (binmatrix[i,k]==1) pos <- k; }

			memsum <- 0;
			for (k in 1:length(jobs)) { if (binmatrix[k,pos]==1) memsum <- memsum + jmem[k]; }

			rtaux <- RT(jreq[i],jbyt[i],jmbps[i],jtpq[i],ocupacio[pos],mocup[pos],jcpuvar[i],jmem[i],jcpu[i],jmem[i]);
			resptime <- c(resptime,rtaux);
			minresptime <- c(minresptime,jrto[i]);
		}
	}

	slaaux <-  (1 - ((resptime - minresptime) / (alpha * minresptime)));
	slaaux[slaaux < 0] <- 0;
	slaaux[slaaux > 1] <- 1;


	benefit <- sum(slaaux) * ppj - sumpower * ppp - migracions * ppm;
	benefit;
}


recalculateHosts <- function(lambda1, totalCPU, mcpu) {
	retval <- 0;
	aux <- 0;
	while ((aux < totalCPU*(1 + lambda1/100))&&(retval < length(mcpu))) {
		retval <- retval + 1;
		aux <- aux + mcpu[retval]*100;
	}
	retval;
}

