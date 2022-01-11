methane<-read.csv("methane.csv")
data_inc<-read.csv("nutrient.csv")

s205<- methane[methane$sample==205,]
ns<- methane[methane$sample=="NS",]
nsa<- methane[methane$sample=="NSA",]
s211<- methane[methane$sample==211,]
s<- methane[methane$sample=="S",]
sa<- methane[methane$sample=="SA",]
csw<- methane[methane$sample=="CT",]


par(mfrow=c(2,2))

plot(ns$time,ns$methane,type = "b",cex=1.5,pch=22,
     xlim = c(0,100), ylim=c(2.5,5),
     xlab="Time (hour)",ylab=expression("Methane (nM)"))
points(s205$time,s205$methane,cex=1.5,pch=25)
arrows(ns$time,ns$methane-ns$msd,ns$time,
       ns$methane+ns$msd, 
       length = 0.02,angle=90, code=3)
legend("topright", legend = c("NS-bottom", "NS- inc."), pch=c(25,22), 
       lty=c(NA,1),cex = 1.2)


plot(s$time,s$methane,type = "b",cex=1.5,pch=22,
     xlim = c(0,100), ylim=c(0,80),
     xlab="Time (hour)",ylab=expression("Methane (nM)"))
points(s211$time,s211$methane,cex=1.5,pch=25)
arrows(s$time,s$methane-s$msd,s$time,
       s$methane+s$msd,length = 0.02,angle=90, code=3)
legend("topright", legend = c("S-bottom", "S- inc."), pch=c(25,22), 
       lty=c(NA,1),cex = 1.2)

plot(nsa$time,nsa$methane,type = "b",cex=1.5,pch=22,
     xlim = c(0,100), ylim=c(300,1100),
     xlab="Time (hour)",ylab=expression("Methane (nM)"))
arrows(nsa$time,nsa$methane-nsa$msd,nsa$time,
       nsa$methane+nsa$msd, 
       length = 0.02,angle=90, code=3)
points(csw$time,csw$methane,type = "b",lty=2, col="blue")
arrows(csw$time,csw$methane-csw$msd,csw$time,lty=2,
       csw$methane+csw$msd, col="blue",
       length = 0.02,angle=90, code=3)
legend("topright", legend = c("Control", "Nonseep added"), pch=c(21,22),
       lty=c(2,1),col=c("blue","black"),cex = 1.2)

plot(sa$time,sa$methane,type = "b",pch=22,
     xlim = c(0,100), ylim=c(300,1100),cex=2,
     xlab="Time (hour)",ylab=expression("Methane (nM)"))
arrows(sa$time,sa$methane-sa$msd,sa$time,
       sa$methane+sa$msd, 
       length = 0.02,angle=90, code=3)
points(csw$time,csw$methane,type = "b",lty=2, col="blue")
arrows(csw$time,csw$methane-csw$msd,csw$time,lty=2,
       csw$methane+csw$msd, col="blue",
       length = 0.02,angle=90, code=3)
legend("topright", legend = c("Control", "Seep added"), pch=c(21,22),
       lty=c(2,1),col=c("blue","black"),cex = 1.2)






data_inc$seep<-c(rep("nonseep",3),rep("seep",3))
data_inc$sample<-c( rep(c("t0","tf","tf_a"),2))
dos<-tapply(data_inc$do, list(data_inc$sample, data_inc$seep),mean)
flow<-tapply(data_inc$flow, list(data_inc$sample, data_inc$seep),mean)
kmox<-tapply(data_inc$kmox, list(data_inc$sample, data_inc$seep),mean)
mox<-tapply(data_inc$mox, list(data_inc$sample, data_inc$seep),mean)
no3<-tapply(data_inc$no3, list(data_inc$sample, data_inc$seep),mean)
po4<-tapply(data_inc$po4, list(data_inc$sample, data_inc$seep),mean)
sio4<-tapply(data_inc$sio4, list(data_inc$sample, data_inc$seep),mean)
doc<-tapply(data_inc$doc, list(data_inc$sample, data_inc$seep),mean)
tn<-tapply(data_inc$tn, list(data_inc$sample, data_inc$seep),mean)


par(mfrow=c(2,3))

bp<-barplot(kmox, ylim = c(0,0.015),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "kMOx (1/day)",cex.lab = 1.5, cex.axis = 1.5)
#text(bp,0, round(kmox, 3),pos = 1) 
arrows(bp,data_inc$kmox-data_inc$kmoxsd,bp,data_inc$kmox+data_inc$kmoxsd,
       length = 0.06,angle=90, code=3)

bp<-barplot(mox, ylim = c(0,4),beside = T,space= c(0.2,0.75),
            col =c("grey90","grey50","grey30"),
            ylab = "MOx rate (nM/day)",cex.lab = 1.5, cex.axis = 1.5)
#text(bp,0, round(mox,2),pos=1) 
arrows(bp,data_inc$mox-data_inc$moxsd,bp,data_inc$mox+data_inc$moxsd,
       length = 0.06,angle=90, code=3)

bp<-barplot(dos,ylim = c(0,10), space= c(0.2,0.75), beside = T,
            col =c("grey90","grey50","grey30"), ylab = "DO (mg/L)",
            cex.lab = 1.5, cex.axis = 1.5)
#text(bp,0, round(dos, 2),pos=1) 
arrows(bp,data_inc$do-data_inc$dosd,bp,data_inc$do+data_inc$dosd,
       length = 0.06,angle=90, code=3)
#legend("topright", legend= c("initial","final","final \nmethane \nadded"), 
#       fill = c("grey90","grey50","grey30"),cex = 2)

bp<-barplot(flow, ylim = c(0,8000),beside = T,space= c(0.2,0.75),
            col =c( "grey90","grey50","grey30"),ylab = "# of cells",
            cex.lab = 1.5, cex.axis = 1.5)
#text(bp,0, round(flow, 0),pos=1,) 
arrows(bp,data_inc$flow-data_inc$flowsd,bp, data_inc$flow+data_inc$flowsd,
       length = 0.06,angle=90,code=3)

bp<-barplot(no3, ylim = c(0,15), beside = T,space= c(0.2,0.75),
            col =c("grey90","grey50","grey30"),ylab = "Nitrate (uM)",
            cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5)
#text(bp,0, round(no3, 1),pos=1)

bp<-barplot(po4,ylim = c(0,1), beside = T,space= c(0.2,0.75),
            col =c("grey90","grey50","grey30"),ylab = "Phosphate (uM)",
            cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5)
#text(bp,0, round(po4, 2),pos=1)

bp<-barplot(sio4, ylim = c(0,6), beside = T,space= c(0.2,0.75),
            col =c("grey90","grey50","grey30"),ylab = "Silicate (uM)",
            cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5)
#text(bp,0, round(sio4, 2),pos=1)

bp<-barplot(doc,ylim = c(0,70), beside = T,space= c(0.2,0.75),
            col =c("grey90","grey50","grey30"),ylab = "DOC (uM)",
            cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5)
#text(bp,0, round(doc, 1),pos=1)

bp<-barplot(tn, ylim = c(0,20), beside = T,space= c(0.2,0.75),
            col =c("grey90","grey50","grey30"),ylab = "TN (uM)",
            cex.lab = 1.5, cex.axis = 1.5, cex.names = 1.5)
#text(bp,0, round(tn, 1),pos=1)
