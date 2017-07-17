#####################################################
# WASH Benefits Kenya
# Primary analysis

# subset distance matrix to distance from control compounds
# to each treatment arm

# obtain distribution of distances between
# compound in C and each T arm

# input: washb-dist.RData, washb-kenya-tr.csv, msP_household_tracking_20160909.csv
# output: washb-dist-sub.RData, washb-dist-km.csv, washb-dist-quant.csv

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
#####################################################

source("~/Documents/CRG/wash-benefits/kenya/src/primary/analysis/10-btw-clus-spill/10a-distance-functions.R")

# load distance matrix objects
load("~/Dropbox/WBK-primary-analysis/Results/Jade/washb-dist.RData")

# load cluster treatment assignments
tx=read.csv("~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/0-Treatment-assignments/washb-kenya-tr.csv")
tx=tx[,c("clusterid","tr")]

# load list of compound ids
washb=read.csv("~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/1-Main-survey/msP_household_tracking_20160909.csv")

# merge tx with compound id
comptx=merge(washb,tx,by="clusterid",all.x=TRUE)
comptx=comptx[,c("clusterid","hhid","GPS_lat","GPS_long","tr")]

# drop compounds with missing latitude/longitude
comptx=comptx[!is.na(comptx$GPS_lat),]

# drop the four compounds that relocated to Nairobi
# Nairobi is in southern hemisphere, other 
# areas are in norther hemisphere
comptx=comptx[comptx$GPS_lat>0,]

# sort comptx by hhid
comptx=comptx[order(comptx$hhid),]

# ----------------------------------------------
# for each tx arm, create matrices subset to rows 
# with Active control compounds and columns with tx compounds
# ----------------------------------------------
# subset matrix to rows with control compounds
cont.rows=which(comptx$tr=="Control")
C.mat=dm[cont.rows,]

# subset matrix to rows with control compounds
# and columns are treated compounds only
T.cols=which(comptx$tr!="Control")
CT.mat=C.mat[,T.cols]

# subset matrix to columns with compounds in water arm
W.cols=which(comptx$tr=="Water")
W.mat=C.mat[,W.cols]

# subset matrix to columns with compounds in sanitation arm
S.cols=which(comptx$tr=="Sanitation")
S.mat=C.mat[,S.cols]

# subset matrix to columns with compounds in hygiene arm
H.cols=which(comptx$tr=="Handwashing")
H.mat=C.mat[,H.cols]

# subset matrix to columns with compounds in hygiene arm
WSH.cols=which(comptx$tr=="WSH")
WSH.mat=C.mat[,WSH.cols]

# subset matrix to columns with compounds in hygiene arm
N.cols=which(comptx$tr=="Nutrition")
N.mat=C.mat[,N.cols]

# subset matrix to columns with compounds in hygiene arm
WSHN.cols=which(comptx$tr=="Nutrition + WSH")
WSHN.mat=C.mat[,WSHN.cols]

# subset matrix to columns with compounds in any tr arm or 
# passive control arm
C.cols=which(comptx$tr=="Control")
anyTr.mat=C.mat[,-C.cols]


# ----------------------------------------------
# for each set of tx arms, create matrices subset to rows 
# with Active control compounds and columns with tx compounds
# ----------------------------------------------
# subset matrix to columns with compounds in ANY water arm
anyW.cols=which(comptx$tr=="Water" | comptx$tr=="WSH" | comptx$tr=="Nutrition + WSH")
anyW.mat=C.mat[,anyW.cols]

anyS.cols=which(comptx$tr=="Sanitation" | comptx$tr=="WSH" | comptx$tr=="Nutrition + WSH")
anyS.mat=C.mat[,anyS.cols]

anyH.cols=which(comptx$tr=="Handwashing" | comptx$tr=="WSH" | comptx$tr=="Nutrition + WSH")
anyH.mat=C.mat[,anyH.cols]

anyN.cols=which(comptx$tr=="Nutrition" | comptx$tr=="Nutrition + WSH")
anyN.mat=C.mat[,anyN.cols]

save(C.mat, CT.mat, W.mat, S.mat, H.mat, WSH.mat, N.mat, WSHN.mat, 
     anyW.mat, anyS.mat, anyH.mat, anyN.mat, anyTr.mat,
     file="~/Dropbox/WBK-primary-analysis/Results/Jade/washb-dist-sub.RData")


# ----------------------------------------------
# distribution of distance from control compound
# to nearest tx compound
# ----------------------------------------------
W.near=apply(W.mat,1,min)
S.near=apply(S.mat,1,min)
H.near=apply(H.mat,1,min)
WSH.near=apply(WSH.mat,1,min)
N.near=apply(N.mat,1,min)
WSHN.near=apply(WSHN.mat,1,min)

pdf(file="~/Dropbox/WBK-primary-analysis/Results/Figures/compdist_w.pdf",
    onefile=TRUE,width=6,height=4)
hist(W.near,main="Distance from Control to nearest Water compound",
     xlab="Kilometers",breaks=20)
dev.off()
pdf(file="~/Dropbox/WBK-primary-analysis/Results/Figures/compdist_s.pdf",
    onefile=TRUE,width=6,height=4)
hist(S.near,main="Distance from Control to nearest Sanitation compounds",
     xlab="Kilometers",breaks=20)
dev.off()
pdf(file="~/Dropbox/WBK-primary-analysis/Results/Figures/compdist_h.pdf",
    onefile=TRUE,width=6,height=4)
hist(H.near,main="Distance from Control to nearest Handwashing compounds",
     xlab="Kilometers",breaks=20)
dev.off()
pdf(file="~/Dropbox/WBK-primary-analysis/Results/Figures/compdist_wsh.pdf",
    onefile=TRUE,width=6,height=4)
hist(WSH.near,main="Distance from Control to nearest WSH compounds",
     xlab="Kilometers",breaks=20)
dev.off()
pdf(file="~/Dropbox/WBK-primary-analysis/Results/Figures/compdist_n.pdf",
    onefile=TRUE,width=6,height=4)
hist(N.near,main="Distance from Control to nearest Nutrition compounds",
     xlab="Kilometers",breaks=20)
dev.off()
pdf(file="~/Dropbox/WBK-primary-analysis/Results/Figures/compdist_wshn.pdf",
    onefile=TRUE,width=6,height=4)
hist(WSHN.near,main="Distance from Control to nearest Nutrition+WSH compounds",
     xlab="Kilometers",breaks=20)
dev.off()


# ----------------------------------------------
# mean number of treated compounds within 
# X kilometers of each control compound
# ----------------------------------------------
W.dists=dm.dist(data=W.mat,dists=seq(0,3,.2))
S.dists=dm.dist(data=S.mat,dists=seq(0,3,.2))
H.dists=dm.dist(data=H.mat,dists=seq(0,3,.2))
WSH.dists=dm.dist(data=WSH.mat,dists=seq(0,3,.2))
N.dists=dm.dist(data=N.mat,dists=seq(0,3,.2))
WSHN.dists=dm.dist(data=WSHN.mat,dists=seq(0,3,.2))

all.dists=cbind(W.dists,S.dists[,2],H.dists[,2],
                WSH.dists[,2],N.dists[,2],WSHN.dists[,2])
colnames(all.dists)=c("km","Water","Sanitation","Handwashing",
                      "WSH","Nutrition","Nutrition+WSH")

for(i in 2:7){
  all.dists[,i]=as.numeric(sprintf("%0.2f",all.dists[,i]))
}

write.csv(all.dists,"~/Dropbox/WBK-primary-analysis/Results/Jade/washb-dist-km.csv")

# ----------------------------------------------
# quantiles of distance to nearest tx compound
# ----------------------------------------------
W.q=quantile(W.near,probs=c(0.25,0.5,0.75))
S.q=quantile(S.near,probs=c(0.25,0.5,0.75))
H.q=quantile(H.near,probs=c(0.25,0.5,0.75))
WSH.q=quantile(WSH.near,probs=c(0.25,0.5,0.75))
N.q=quantile(N.near,probs=c(0.25,0.5,0.75))
WSHN.q=quantile(WSHN.near,probs=c(0.25,0.5,0.75))

all.q=rbind(W.q,S.q,H.q,WSH.q,N.q,WSHN.q)
rownames(all.q)=c("Water","Sanitation","Handwashing",
                  "WSH","Nutrition","Nutrition+WSH")

for(i in 1:3){
  all.q[,i]=as.numeric(sprintf("%0.2f",all.q[,i]))
}

write.csv(all.q,"~/Dropbox/WBK-primary-analysis/Results/Jade/washb-dist-quant.csv")




