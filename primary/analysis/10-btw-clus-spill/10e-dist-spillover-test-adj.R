#####################################################
# WASH Benefits Kenya
# Primary analysis

# between-cluster spillovers

# Adjusted permutation test - primary outcomes
# HAZ: endline data
# diarrhea: midline and endline data

# input: washb-dist-sub.RData, endline-anthro.csv, diarrhea.csv
# output: spill-dist-adj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
#####################################################
library(SuperLearner)

rm(list=ls())

source("~/Documents/CRG/wash-benefits/kenya/src/primary/analysis/10-btw-clus-spill/10a-distance-functions.R")
source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")


#----------------------------------------------------
# Read in distance matrices
#----------------------------------------------------
load("~/Dropbox/WBK-primary-analysis/Results/Jade/washb-dist-sub.RData")

#----------------------------------------------------
# Read in outcome data - HAZ
#----------------------------------------------------
# load child length endline dataset
e=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.csv",stringsAsFactors=TRUE)
e=preprocess.anthro(e, "haz")
e=preprocess.adj(e, "haz")

# subset to control compounds only
e = subset(e,e$tr=="Control")

#----------------------------------------------------
# Read in outcome data - diarrhea
#----------------------------------------------------
# load child length endline dataset
data=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/diarrhea.csv")
d=preprocess.diarr(data)
d=preprocess.adj(d, "diarr7")

# subset to control compounds only
d = subset(d,d$tr=="Control")

# --------------------------------------
# select covariates that are associated with the 
# outcome
# --------------------------------------
# HAZ
Wcols.haz=colnames(e)[-which(colnames(e)%in%c("block","hhid","childid","clusterid","tr","haz"))]
Ws.haz=Wprescreen(Y=e$haz, Ws=e[,Wcols.haz],family="gaussian")

# subset data frame to Ws selected in prescreening
ed=e[,c("block","hhid","childid","clusterid","haz",Ws.haz)]

# subset to complete cases
ed=ed[complete.cases(ed),]

# diarrhea
Wcols.diarr=colnames(d)[-which(colnames(d)%in%c("block","hhid","childid","clusterid","tr","diarr7"))]
Ws.diarr=Wprescreen(Y=d$diarr7, Ws=d[,Wcols.diarr],family="gaussian")

# subset data frame to Ws selected in prescreening
dd=d[,c("block","hhid","childid","clusterid","diarr7",Ws.diarr)]

# subset to complete cases
dd=dd[complete.cases(dd),]

# sort
dd=dd[order(dd$block, dd$clusterid, dd$hhid, dd$childid),]



# --------------------------------------
# fit model
# --------------------------------------
# define SuperLearner libraries
SL.library<-list("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
# for adjusted permutation test, fit model of outcome
# as a function of covariates excluding treatment assignment

sl.haz=sl.prep(ed,y="haz")
colnames(sl.haz$A)

sl.diarr=sl.prep(dd,y="diarr7") 
colnames(sl.diarr$A)

set.seed(67890)
sl.haz.fit=SuperLearner(Y=sl.haz$Y, X=sl.haz$A, SL.library=SL.library,
    id=sl.haz$clusterid,family="gaussian",method="method.NNLS")
sl.haz.fit

set.seed(67890)
sl.diarr.fit=SuperLearner(Y=sl.diarr$Y, X=sl.diarr$A, SL.library=SL.library,
     id=sl.diarr$clusterid,family="binomial",method="method.NNLS")
sl.diarr.fit

# generate residuals
resid.haz=sl.haz$Y - sl.haz.fit$SL.predict
resid.diarr=sl.diarr$Y - sl.diarr.fit$SL.predict

resid.df.haz=data.frame(hhid=ed$hhid,haz=resid.haz)
resid.df.diarr=data.frame(hhid=dd$hhid,diarr7=resid.diarr)

#----------------------------------------------------
# Count the number of treated compounds within a control
# compound for a given set of treatments
#----------------------------------------------------
W.comp2km=comp2km(anyW.mat)
S.comp2km=comp2km(anyS.mat)
H.comp2km=comp2km(anyH.mat)
N.comp2km=comp2km(anyN.mat)

# get the number of compounds within 2 km  - any tr
C.comp2km=comp2km(C.mat)

W.comp2km$prop=W.comp2km$comp2km / C.comp2km$comp2km
S.comp2km$prop=S.comp2km$comp2km / C.comp2km$comp2km
H.comp2km$prop=H.comp2km$comp2km / C.comp2km$comp2km
N.comp2km$prop=N.comp2km$comp2km / C.comp2km$comp2km

#----------------------------------------------------
# Run permutation test - HAZ - unadjusted
#----------------------------------------------------
B=100000

set.seed(67890)
W.perm.haz=myperm.test(W.comp2km,y="haz",ydata=resid.df.haz,B)
set.seed(67890)
S.perm.haz=myperm.test(S.comp2km,y="haz",ydata=resid.df.haz,B)
set.seed(67890)
H.perm.haz=myperm.test(H.comp2km,y="haz",ydata=resid.df.haz,B)
set.seed(67890)
N.perm.haz=myperm.test(N.comp2km,y="haz",ydata=resid.df.haz,B)

# combine results
out.list=list(W.perm.haz,S.perm.haz,H.perm.haz,N.perm.haz)
perm.haz.dist.adj_j=t(sapply(out.list,format.perm))


#----------------------------------------------------
# Run permutation test - HAZ - unadjusted
#----------------------------------------------------
B=100000

set.seed(67890)
W.perm.diar=myperm.test(W.comp2km,y="diarr7",ydata=resid.df.diarr,B)
set.seed(67890)
S.perm.diar=myperm.test(S.comp2km,y="diarr7",ydata=resid.df.diarr,B)
set.seed(67890)
H.perm.diar=myperm.test(H.comp2km,y="diarr7",ydata=resid.df.diarr,B)
set.seed(67890)
N.perm.diar=myperm.test(N.comp2km,y="diarr7",ydata=resid.df.diarr,B)

# combine results
out.list=list(W.perm.diar,S.perm.diar,H.perm.diar,
              N.perm.diar)
perm.diarr.dist.adj_j=t(sapply(out.list,format.perm))



coln=colnames(perm.haz.dist.adj_j)
newlaz=matrix(NA,4,6)
newdiar=matrix(NA,4,6)
for(i in 1:ncol(perm.haz.dist.adj_j)){
  newlaz[,i]=unlist(perm.haz.dist.adj_j[,i])
  newdiar[,i]=unlist(perm.diarr.dist.adj_j[,i])
}
perm.haz.dist.adj_j=as.data.frame(newlaz)
perm.diarr.dist.adj_j=as.data.frame(newdiar)
colnames(perm.haz.dist.adj_j)=coln
colnames(perm.diarr.dist.adj_j)=coln

rownames(perm.haz.dist.adj_j)=c("Water","Sanitation","Handwashing","Nutrition")
rownames(perm.diarr.dist.adj_j)=c("Water","Sanitation","Handwashing","Nutrition")

save(perm.haz.dist.adj_j, perm.diarr.dist.adj_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/spill-dist-adj.RData")

