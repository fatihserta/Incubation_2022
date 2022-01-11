library(oce)

# incubation stations profiles----

ctd205<-read.ctd.sbe("Sta0205.cnv")
ctd211<-read.ctd.sbe("Sta0211.cnv")

par(mfrow=c(1,3))

plotProfile(ctd205,ytype= "depth",xtype = "temperature",type = "l", Tlim=c(3.5,5.2))
plotProfile(ctd211,ytype= "depth",xtype = "temperature",type = "l", col="red", add=T)
legend("bottomleft",legend=c("non-seep (205)","seep (211)"), lty=c(1,1),col=c("black","red"))

plotProfile(ctd205,ytype= "depth",xtype = "salinity",type = "l", Slim=c(34.5,35))
plotProfile(ctd211,ytype= "depth",xtype = "salinity",type = "l", col="red", add=T)

plotProfile(ctd205,ytype= "depth",xtype = "oxygen",type = "l", Slim=c(34.5,35))
plotProfile(ctd211,ytype= "depth",xtype = "oxygen",type = "l", col="red", add=T)