library(data.table)

load("inc_neg.RData")
load("inc_pos.RData")

nsi<- c(inc_neg[c(1:3)],inc_pos[1])
nsf<- c(inc_neg[c(4:6)],inc_pos[2])
nsa<- c(inc_neg[c(7:9)],inc_pos[3])
sei<- c(inc_neg[c(10:12)],inc_pos[4])
sef<- c(inc_neg[c(13:15)],inc_pos[5])
sea<- c(inc_neg[c(16:18)],inc_pos[6])

# ---

nsi<-as.data.table(do.call("rbind",nsi))
nsi <- nsi[!duplicated(nsi$index),]



nsf<-as.data.table(do.call("rbind",nsf))
nsf <- nsf[!duplicated(nsf$index),]


nsa<-as.data.table(do.call("rbind",nsa))
nsa <- nsa[!duplicated(nsa$index),]


#  ---

sei<-as.data.table(do.call("rbind",sei))
sei <- sei[!duplicated(sei$index),]


sef<-as.data.table(do.call("rbind",sef))
sef <- sef[!duplicated(sef$index),]


sea<-as.data.table(do.call("rbind",sea))
sea <- sea[!duplicated(sea$index),]


par(mfrow=c(2,3))


plot(nsi$O.C, nsi$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))
plot(nsf$O.C, nsf$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))
plot(nsa$O.C, nsa$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))

plot(sei$O.C, sei$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))
plot(sef$O.C, sef$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))
plot(sea$O.C, sea$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))




## ortak - fark functions ----


ortak <- function(x,y,...){
  df<-as.data.table(rbind(x,y,...))
  df[, replicate := 1:.N, by="index"]
  df<- subset(df, replicate==length(c(x,y,...))/ncol(x))
  df
  
}

fark <- function(x,y){
  x<-x[!duplicated(x$index),]
  y<-y[!duplicated(y$index),]
  x<-x[!(x$index %in% y$index),]
  x
}

#incubation addeds and gones-----


all_o<-ortak(nsi,nsf,nsa,sei,sef,sea)
ns_o<-ortak(nsi,nsf,nsa)
se_o<-ortak(sei,sef,sea)

ns_gone<-fark(nsi,nsf)
ns_add<-fark(nsf,nsi)
ns_gone2<-fark(nsi,nsa)
ns_add2<-fark(nsa,nsi)

se_gone<-fark(sei,sef)
se_add<-fark(sef,sei)
se_gone2<-fark(sei,sea)
se_add2<-fark(sea,sei)

par(mfrow=c(2,3))

plot(nsi$O.C, nsi$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))

plot(nsf$O.C, nsf$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))
points(ns_gone$O.C, ns_gone$H.C, pch=20,col= "red")
points(ns_add$O.C, ns_add$H.C, pch=20,col= "blue")

plot(nsa$O.C, nsa$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))
points(ns_add2$O.C, ns_add2$H.C, pch=20,col= "blue")
points(ns_gone2$O.C, ns_gone2$H.C, pch=20,col= "red")

plot(sei$O.C, sei$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))

plot(sef$O.C, sef$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))
points(se_gone$O.C, se_gone$H.C, pch=20,col= "red")
points(se_add$O.C, se_add$H.C, pch=20,col= "blue")

plot(sea$O.C, sea$H.C, xlab="O:C", ylab= "H:C",las =1, 
     cex.lab=1, font.lab=2, pch=20,col= "grey",ylim = c(0,2.5),xlim = c(0,1.2))
points(se_gone2$O.C, se_gone2$H.C, pch=20,col= "red")
points(se_add2$O.C, se_add2$H.C, pch=20,col= "blue")






# PCoA analysis---------


inc_group<-list(nsi,nsf,nsa,sei,sef,sea)
inc_names<-list("nsi","nsf","nsa","sei","sef","sea")

# community table (cmt) - rows: samples, columns; forumulas  

cmt<- inc_group

for (i in 1:length(cmt)) cmt[[i]]<-cmt[[i]][c(14,2)] 

cmt<- Reduce(function(x, y) merge(x,y, by="index", all=TRUE), cmt) 


cmt[is.na(cmt)] <- 0
cmt[1:5,1:5]

# comm. table /w relative abundance
BC <- as.data.frame(t(cmt[,-1]))
rownames(BC)<-inc_names
colnames(BC)<-cmt[,1]
BC[1:6,1:5]

# comm. table presence/absence
JD<- as.data.frame(ifelse(BC>0,1,0))
JD[1:6,1:5]


dist_m<-vegdist(JD,method="jaccard")

pcoa <- cmdscale (dist_m, eig = TRUE)

ordiplot (pcoa, display = 'sites', type = 'text')



# DOM barplots ---------


mlb<-data.frame(matrix(ncol = 13,nrow=6))
colnames(mlb)<-c("sample", 
                 "nform", "mlb",
                 "lpd","car","uhc","lgn", 
                 "cho","chon","chos","chons", "Av.RA","Av.MW")
mlb[,12]<- c(1.43,1.34,3.95,2.64,2.23,2.23) # Relative abundances before removing duplicates
mlb[,13]<- c(458,439,470,464,451,486) # molecular weight after removing nacl adjuncts
for(i in 1:6) {
  mlb[i,1]<- inc_names[[i]]
  mlb[i,2]<- nrow(inc_group[[i]])
  mlb[i,3]<- nrow(inc_group[[i]][inc_group[[i]]$H.C>=1.5,])
  mlb[i,4]<- nrow(inc_group[[i]][inc_group[[i]]$H.C>=1.5 & inc_group[[i]]$O.C<=0.7,]) 
  mlb[i,5]<- nrow(inc_group[[i]][inc_group[[i]]$H.C>=1.5 & inc_group[[i]]$O.C>0.7,])
  mlb[i,6]<- nrow(inc_group[[i]][inc_group[[i]]$H.C<=0.7 & inc_group[[i]]$O.C<=0.7,])
  mlb[i,7]<- nrow(inc_group[[i]])-(mlb[i,6]+mlb[i,5]+mlb[i,4])
  mlb[i,8]<- nrow(inc_group[[i]][inc_group[[i]]$CHO=="CHO",])
  mlb[i,9]<- nrow(inc_group[[i]][inc_group[[i]]$CHO=="CHON",])
  mlb[i,10]<- nrow(inc_group[[i]][inc_group[[i]]$CHO=="CHOS",])
  mlb[i,11]<- nrow(inc_group[[i]][inc_group[[i]]$CHO=="CHONS",])
}



mlb$seep<-c(rep("nonseep",3),rep("seep",3))
mlb$sample<-c( rep(c("t0","tf","tf_a"),2))
nform<-tapply(mlb$nform, list(mlb$sample, mlb$seep),mean)
lpd<-tapply(mlb$lpd, list(mlb$sample, mlb$seep),mean)
car<-tapply(mlb$car, list(mlb$sample, mlb$seep),mean)
lgn<-tapply(mlb$lgn, list(mlb$sample, mlb$seep),mean)
uhc<-tapply(mlb$uhc, list(mlb$sample, mlb$seep),mean)
cho<-tapply(mlb$cho, list(mlb$sample, mlb$seep),mean)
chon<-tapply(mlb$chon, list(mlb$sample, mlb$seep),mean)
chos<-tapply(mlb$chos, list(mlb$sample, mlb$seep),mean)
avra<-tapply(mlb$Av.RA, list(mlb$sample, mlb$seep),mean)
avmw<-tapply(mlb$Av.MW, list(mlb$sample, mlb$seep),mean)



par(mfrow=c(2,3))

bp<-barplot(nform, ylim = c(0,6000),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "# formula",cex.lab = 1.5, cex.axis = 1.5)
bp<-barplot(avmw, ylim = c(0,500),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "Ave. MW",cex.lab = 1.5, cex.axis = 1.5)
bp<-barplot(lpd, ylim = c(0,1000),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "# LPD",cex.lab = 1.5, cex.axis = 1.5)
bp<-barplot(car, ylim = c(0,80),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "# CAR",cex.lab = 1.5, cex.axis = 1.5)
bp<-barplot(lgn, ylim = c(0,5000),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "# LGN",cex.lab = 1.5, cex.axis = 1.5)
bp<-barplot(uhc, ylim = c(0,200),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "# UHC",cex.lab = 1.5, cex.axis = 1.5)
bp<-barplot(cho, ylim = c(0,2500),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "# CHO",cex.lab = 1.5, cex.axis = 1.5)
bp<-barplot(chon, ylim = c(0,3000),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "# CHON",cex.lab = 1.5, cex.axis = 1.5)
bp<-barplot(chos, ylim = c(0,800),beside = T,
            col =c("grey90","grey50","grey30"),space= c(0.2,0.75),
            ylab = "# CHOS",cex.lab = 1.5, cex.axis = 1.5)
