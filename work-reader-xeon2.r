
################################################################################
# GENERACIO DEL DATASET
################################################################################

dataload <- function(base)
{

# Data-Sets des del servidor
rvm <- read.table(paste(base,"record-vm.log",sep=""),header=F);
rpm <- read.table(paste(base,"record-pm.log",sep=""),header=F);

colnames(rvm) <- c("date","time","cpu","mem");
z <- strptime(paste(rvm[,"date"]," ",rvm[,"time"],sep=""), "%F %X");
secs <- (z$sec)+(z$min*60)+(z$hour*3600);
rvm <- cbind(rvm,secs);
colnames(rvm) <- c("date","time","cpu","mem","secs");
rvm <- rvm[order(rvm[,"secs"]),c("date","time","cpu","mem","secs")];

colnames(rpm) <- c("date","time","cpu","mem","rcpu","rmem");
z <- strptime(paste(rpm[,"date"]," ",rpm[,"time"],sep=""), "%F %X");
secs <- (z$sec)+(z$min*60)+(z$hour*3600);
rpm <- cbind(rpm,secs);
colnames(rpm) <- c("date","time","cpu","mem","rcpu","rmem","secs");
rpm <- rpm[order(rpm[,"secs"]),c("date","time","cpu","mem","rcpu","rmem","secs")];

paux <- rpm[,3:7];
paux[,5] <- trunc(paux[,5]/10);
a1 <- tapply(paux[,1],list(paux[,5]),FUN=mean);
a2 <- tapply(paux[,2],list(paux[,5]),FUN=mean);
a3 <- tapply(paux[,3],list(paux[,5]),FUN=mean);
a4 <- tapply(paux[,4],list(paux[,5]),FUN=mean);
a5 <- unique(paux[,5]);
pnew <- cbind(a1,a2,a3,a4,a5);
colnames(pnew) <- c("cpu","mem","rcpu","rmem","secs");

vaux <- rvm[,3:5];
vaux[,3] <- trunc(vaux[,3]/10);
a1 <- tapply(vaux[,1],list(vaux[,3]),FUN=mean);
a2 <- tapply(vaux[,2],list(vaux[,3]),FUN=mean);
a3 <- unique(vaux[,3]);
vnew <- cbind(a1,a2,a3);
colnames(vnew) <- c("cpu","mem","secs");

ds1 <- merge(vnew[,c("secs","cpu","mem")],pnew[,c("secs","cpu","mem","rcpu","rmem")],by="secs");
colnames(ds1) <- c("time","cpuvm","memvm","cpupm","mempm","rcpuvm","rmemvm");

# Data-Sets des del client

ds2 <- NULL;

for(i in 10:509) {
	aux1 <- read.table(paste(base,"testFile-",i,".txt",sep=""),header=F);
	ds2 <- rbind(ds2,aux1);
}
colnames(ds2) <- c("date","time","reqs","rtpq","bytespq","mbpspq");

z <- strptime(paste(ds2[,"date"]," ",ds2[,"time"],sep=""), "%F %X");
secs <- (z$sec)+(z$min*60)+(z$hour*3600);
ds2 <- cbind(ds2,secs);
colnames(ds2) <- c("date","time","reqs","rtpq","bytespq","mbpspq","secs");
ds2 <- ds2[,c("reqs","rtpq","bytespq","mbpspq","secs")];
colnames(ds2) <- c("reqs","rtpq","bytespq","mbpspq","time");

ds2 <- ds2[order(ds2[,"time"]),c("reqs","rtpq","bytespq","mbpspq","time")];

ds2[,5] <- trunc(ds2[,5]/10);
a1 <- tapply(ds2[,1],list(ds2[,5]),FUN=sum);
a2 <- tapply(ds2[,2],list(ds2[,5]),FUN=mean);
a3 <- tapply(ds2[,3],list(ds2[,5]),FUN=mean);
a4 <- tapply(ds2[,4],list(ds2[,5]),FUN=mean);
a5 <- unique(ds2[,5]);
ds2 <- cbind(a1,a2,a3,a4,a5);
colnames(ds2) <- c("reqs","rtpq","bytespq","mbpspq","time");


# Treball del Apache

acc <- read.table(paste(base,"access.log",sep=""),header=F);
aux <- acc[,c(5,6,13)];

z <- strptime(paste(aux[,1]," ",aux[,2],sep=""), "%F %X");
secs <- (z$sec)+(z$min*60)+(z$hour*3600);
aux <- cbind(aux,secs);
colnames(aux) <- c("date","hour","timepq","time");
acc <- aux;

acc[,4] <- trunc(acc[,4]/10);
a1 <- tapply(acc[,3],list(acc[,4]),FUN=mean);
a2 <- unique(acc[,4]);
acc <- cbind(a1,a2);
colnames(acc) <- c("timepq","time");


# Reunió de datasets
ds3 <- merge(ds1[,c("time","cpuvm","memvm","cpupm","mempm","rcpuvm","rmemvm")],ds2[,c("time","reqs","rtpq","bytespq","mbpspq")],by="time");
ds3 <- merge(ds3[,c("time","cpuvm","memvm","cpupm","mempm","rcpuvm","rmemvm","reqs","rtpq","bytespq","mbpspq")],acc[,c("time","timepq")],by="time");

ds3[,"cpuvm"] <- ds3[,"cpuvm"] * 4;
ds3[,"cpupm"] <- ds3[,"cpupm"] * 4;

ds3;

}

################################################################################
# PROVES DE LEARNING RT
################################################################################

library(RWeka);
library(e1071);


# Training Round
dl1 <- dataload("/home/jlberral/Desktop/parsers/records-28B-stress11/");
dl2 <- dataload("/home/jlberral/Desktop/parsers/records-27-stress11/");
dl3 <- dataload("/home/jlberral/Desktop/parsers/records-28B-lowint-full/");
dl4 <- dataload("/home/jlberral/Desktop/parsers/records-27-medint/");


aux <- dl1[sample(1:length(dl1[,1])),];
dataset1 <- aux[1:(length(aux[,1])/2),];
dataset2 <- aux[((length(aux[,1])/2)+1):length(aux[,1]),];

dataset1 <- dl1;
dataset2 <- dl2;

#write.table(dataset1,file="RT_dataset.data");
#write.arff(data.frame(dataset1), file = "RT_dataset.arff")


aux <- dataset1[sample(1:length(dataset1[,1])),];
cutpoint <- length(aux[,1])*0.60;
RT_tr <- aux[1:cutpoint,];
RT_ts <- aux[(cutpoint+1):length(aux[,1]),];


##  time         reqs         rtpq      bytespq     mbpspq       timepq 
##  cpuvm        memvm        cpupm     mempm       rcpuvm       rmemvm

daux <- RT_tr[,c("reqs","bytespq","mbpspq","cpuvm","cpupm","rcpuvm","timepq")];
raux <- RT_tr[,"rtpq"];

daux2 <- RT_ts[,c("reqs","bytespq","mbpspq","cpuvm","cpupm","rcpuvm","timepq")];
raux2 <- RT_ts[,"rtpq"];

df <- data.frame(y=raux, daux);
lr1 <- lm(y~.,data=df);
#lr1 <- lm(y~(.)^2,data=df);
#lr1 <- svm(y~.,data=df);
#lr1 <- M5P(y~.,data=df,control=Weka_control(M = 10.0, U = TRUE));

test0 <- predict(lr1,data.frame(daux));
test0 <- as.vector(test0);

	plot(test0,raux,xlab="Predicted RT",ylab="Original RT",main="Response Time Learnt Function");
	abline(a=c(0,0), b=c(1,1));

test1 <- predict(lr1,data.frame(daux2));
test1 <- as.vector(test1);

#png(filename = "RT-TR1.png",width = 1000, height = 1000, units = "px",pointsize = 12, bg = "white");
	plot(test1,raux2,xlab="Predicted RT",ylab="Original RT",main="Response Time Learnt Function");
	abline(a=c(0,0), b=c(1,1));
#dev.off();

error <- sum((raux2-test1)^2)/length(test1);
stdev <- (sum((error-abs(raux2-test1))^2)/length(test1))^(1/2);
error;stdev;

#write.table(tr,file="RT_tr.data");
#write.table(ts,file="RT_ts.data");
#RT_tr <- read.table(file="RT_tr.data",header=TRUE);
#RT_ts <- read.table(file="RT_ts.data",header=TRUE);

#png(filename = "10-RT-prediction.png",width = 1000, height = 1000, units = "px",pointsize = 12, bg = "white");
par(mfcol= c(2, 1));
	plot(test0,raux,xlab="Predicted RT",ylab="Original RT",main="Response Time Learnt Function",ylim=c(0,0.04));
	abline(a=c(0,0), b=c(1,1));
	legend("topleft", c("y = x"), lty=1);
	plot(test1,raux2,xlab="Predicted RT",ylab="Original RT",main="Response Time (Test Run)",ylim=c(0,0.04));
	abline(a=c(0,0), b=c(1,1));
	legend("topleft", c("y = x"), lty=1);
#dev.off();


# Testing Round

fdaux <- dataset2[,c("reqs","bytespq","mbpspq","cpuvm","cpupm","rcpuvm","timepq")];
fraux <- dataset2[,"rtpq"];

test2 <- predict(lr1,data.frame(fdaux));
test2 <- as.vector(test2);
test2[test2<0] <- 0;

#png(filename = "RT-TS1.png",width = 1000, height = 500, units = "px",pointsize = 12, bg = "white");
	plot(test2,fraux,xlab="Predicted RT",ylab="Original RT",main="Response Time (Test Run)",ylim=c(0,0.1));
	abline(a=c(0,0), b=c(1,1));
#dev.off();

error <- sum((fraux-test2)^2)/length(test2);
stdev <- (sum((error-abs(fraux-test2))^2)/length(test2))^(1/2);
error;stdev;

#write.table(dataset2,file="RT_fts.data");
#dataset2 <- read.table(file="RT_fts.data",header=TRUE);

# Test Individual

daux3 <- as.matrix(c(500,34000,50,100,50,50,3000)); daux3 <- t(daux3);
colnames(daux3) <- c("reqs","bytespq","mbpspq","cpuvm","cpupm","rcpuvm","timepq");
predict(lr1,data.frame(daux3));



## dataset1 <<-- dl1
## dataset2 <<-- dl2

## Error 4.514474e-05
## StDev 0.006694002

## TS Error 0.002932135
## TS StDev 0.05332852

## lm(formula = y ~ ., data = df)
##
## Coefficients:
## (Intercept)         reqs      bytespq       mbpspq        cpuvm        cpupm  
##   3.950e-03    1.087e-05   -4.147e-08   -1.919e-04    4.762e-06    2.954e-05  
##      rcpuvm       timepq  
##  -3.633e-05    1.906e-08  


#png(filename = "11-RT-prediction.png",width = 1000, height = 1000, units = "px",pointsize = 12, bg = "white");
par(mfcol= c(2, 1));
	plot(test1,raux2,xlab="Predicted RT",ylab="Original RT",main="Response Time Learnt Function");
	abline(a=c(0,0), b=c(1,1));
	legend("topleft", c("y = x"), lty=1);
	plot(test2,fraux,xlab="Predicted RT",ylab="Original RT",main="Response Time (Test Run)",ylim=c(0,0.1));
	abline(a=c(0,0), b=c(1,1));
	legend("topleft", c("y = x"), lty=1);
#dev.off();



################################################################################
# PROVES DE LEARNING CPU
################################################################################


dataset3 <- dl4;
dataset4 <- dl4;

aux <- dataset3[sample(1:length(dataset3[,1])),];
cutpoint <- length(aux[,1])*0.30;
CPU_tr <- aux[1:cutpoint,];
CPU_ts <- aux[(cutpoint+1):length(aux[,1]),];


##  time         reqs         rtpq      bytespq     mbpspq       timepq 
##  cpuvm        memvm        cpupm     mempm       rcpuvm       rmemvm

CPU_daux <- CPU_tr[,c("reqs","bytespq","timepq")];
CPU_raux <- CPU_tr[,"rcpuvm"];

CPU_df <- data.frame(y=CPU_raux, CPU_daux);

CPU_daux2 <- CPU_ts[,c("reqs","bytespq","timepq")];
CPU_raux2 <- CPU_ts[,"rcpuvm"];

#CPU_lr1 <- lm(y~.,data=CPU_df);
#cpu_lr1 <- lm(y~(.)^2,data=cpu_df);
#CPU_lr1 <- svm(y~.,data=cpu_df);
CPU_lr1 <- M5P(y~.,data=CPU_df,control=Weka_control(M = 5.0, U = FALSE));

CPU_test0 <- predict(CPU_lr1,data.frame(CPU_daux));
CPU_test0 <- as.vector(CPU_test0);


	plot(CPU_test0,CPU_raux,xlab="Predicted CPU_VM",ylab="Original CPU_VM",main="Required CPU (Training Run 0.66/0.33)");
	abline(a=c(0,0), b=c(1,1));


CPU_test1 <- predict(CPU_lr1,data.frame(CPU_daux2));
CPU_test1 <- as.vector(CPU_test1);


#png(filename = "CPUVM-TR1.png",width = 1000, height = 1000, units = "px",pointsize = 12, bg = "white");
	plot(CPU_test1,CPU_raux2,xlab="Predicted CPU_VM",ylab="Original CPU_VM",main="Required CPU (Training Run 0.66/0.33)");
	abline(a=c(0,0), b=c(1,1));
#dev.off();

error <- sum((CPU_raux2-CPU_test1)^2)/length(CPU_test1);
stdev <- (sum((error-abs(CPU_raux2-CPU_test1))^2)/length(CPU_test1))^(1/2);
error;stdev;

#png(filename = "12-CPU-prediction.png",width = 1000, height = 1000, units = "px",pointsize = 12, bg = "white");
par(mfcol= c(2, 1));
	plot(CPU_test0,CPU_raux,xlab="Predicted CPU_VM",ylab="Original CPU_VM",main="Response Time Learnt Function",xlim=c(0,150));
	abline(a=c(0,0), b=c(1,1));
	legend("topleft", c("y = x"), lty=1);
	plot(CPU_test1,CPU_raux2,xlab="Predicted CPU_VM",ylab="Original CPU_VM",main="Response Time (Test Run)",xlim=c(0,150));
	abline(a=c(0,0), b=c(1,1));
	legend("topleft", c("y = x"), lty=1);
#dev.off();

#write.table(CPU_tr,file="CPU_tr.data");
#write.table(CPU_ts,file="CPU_ts.data");
#CPU_tr <- read.table(file="CPU_tr.data",header=TRUE);
#CPU_ts <- read.table(file="CPU_ts.data",header=TRUE);



# Testing Round

CPU_fdaux <- dataset4[,c("reqs","bytespq","timepq")];
CPU_fraux <- dataset4[,"rcpuvm"];

CPU_test2 <- predict(CPU_lr1,data.frame(CPU_fdaux));
CPU_test2 <- as.vector(CPU_test2);

#png(filename = "CPUVM-TS1.png",width = 1000, height = 1000, units = "px",pointsize = 12, bg = "white");
	plot(CPU_test2,CPU_fraux,xlab="Predicted CPU_VM",ylab="Original CPU_VM",main="Required CPU (Test Run)");
	abline(a=c(0,0), b=c(1,1));
#dev.off();

error <- sum((CPU_fraux-CPU_test2)^2)/length(CPU_test2);
stdev <- (sum((error-abs(CPU_fraux-CPU_test2))^2)/length(CPU_test2))^(1/2);
error;stdev;


## dataset3 <<-- dl4
## dataset4 <<-- NO
## 
## reqs <= 576.5 : 
## |   timepq <= 3404.019 : 
## |   |   reqs <= 411.5 : LM1 (109/28.725%)
## |   |   reqs >  411.5 : 
## |   |   |   bytespq <= 41927.799 : LM2 (23/47.72%)
## |   |   |   bytespq >  41927.799 : LM3 (11/48.871%)
## |   timepq >  3404.019 : LM4 (38/101.397%)
## reqs >  576.5 : LM5 (52/90.042%)
## 
## LM num: 1
## y = 
## 	0.0462 * reqs 				0.04620767
## 	+ 0.0036 * timepq 			0.00361214
## 	+ 5.9264				5.926363
## 
## LM num: 2
## y = 
## 	0.0265 * reqs 				0.02645041
## 	- 0.0001 * bytespq 			-0.0001369089
## 	- 0.0007 * timepq 			-0.0007096089
## 	+ 35.6244				35.6245
## 
## LM num: 3
## y = 
## 	0.0265 * reqs 				0.02645041
## 	- 0.0002 * bytespq 			-0.0002000944
## 	- 0.0013 * timepq 			-0.001266081
## 	+ 35.5954				35.5955
## 
## LM num: 4
## y = 
## 	0.0182 * reqs 				0.01823293
## 	+ 0.0005 * bytespq 			0.00051134
## 	+ 0.0002 * timepq 			0.0001556182
## 	+ 19.9454				19.9455
## 
## LM num: 5
## y = 
## 	0.0754 * reqs 				0.07544295
## 	+ 0.0028 * timepq 			0.002768338
## 	- 0.4846				-0.4847

# Test Individual

CPU_daux3 <- as.matrix(c(412,41928,1000)); CPU_daux3 <- t(CPU_daux3);
colnames(CPU_daux3) <- c("reqs","bytespq","timepq");
predict(CPU_lr1,data.frame(CPU_daux3));


#png(filename = "12-CPU-prediction.png",width = 1000, height = 1000, units = "px",pointsize = 12, bg = "white");
par(mfcol= c(2, 1));
	plot(CPU_test1,CPU_raux2,xlab="Predicted CPU_VM",ylab="Original CPU_VM",main="Response Time Learnt Function",xlim=c(0,150));
	abline(a=c(0,0), b=c(1,1));
	legend("topleft", c("y = x"), lty=1);
	plot(CPU_test2,CPU_fraux,xlab="Predicted CPU_VM",ylab="Original CPU_VM",main="Response Time (Test Run)",xlim=c(0,150));
	abline(a=c(0,0), b=c(1,1));
	legend("topleft", c("y = x"), lty=1);
#dev.off();

