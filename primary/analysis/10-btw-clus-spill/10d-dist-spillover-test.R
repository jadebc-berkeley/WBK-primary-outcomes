#####################################################
# WASH Benefits Kenya
# Primary analysis

# between-cluster spillovers

# Permutation test - primary outcomes

# by Jade (jadebc@berkeley.edu)
#####################################################

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

# subset to control compounds only
e = subset(e,e$tr=="Control")
e=e[order(e$hhid),]

#----------------------------------------------------
# Read in outcome data - diarrhea
#----------------------------------------------------
# load child length endline dataset
data=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/diarrhea.csv")
d=preprocess.diarr(data)

# subset to control compounds only
d = subset(d,d$tr=="Control")

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
W.perm.haz=myperm.test(W.comp2km,y="haz",ydata=e,B)
set.seed(67890)
S.perm.haz=myperm.test(S.comp2km,y="haz",ydata=e,B)
set.seed(67890)
H.perm.haz=myperm.test(H.comp2km,y="haz",ydata=e,B)
set.seed(67890)
N.perm.haz=myperm.test(N.comp2km,y="haz",ydata=e,B)

# combine results
out.list=list(W.perm.haz,S.perm.haz,H.perm.haz,N.perm.haz)
perm.haz.dist_j=t(sapply(out.list,format.perm))


#----------------------------------------------------
# Run permutation test - diarrhea - unadjusted
#----------------------------------------------------
B=100000

set.seed(67890)
W.perm.diar=myperm.test(W.comp2km,y="diarr7",ydata=d,B)
set.seed(67890)
S.perm.diar=myperm.test(S.comp2km,y="diarr7",ydata=d,B)
set.seed(67890)
H.perm.diar=myperm.test(H.comp2km,y="diarr7",ydata=d,B)
set.seed(67890)
N.perm.diar=myperm.test(N.comp2km,y="diarr7",ydata=d,B)

# combine results
out.list=list(W.perm.diar,S.perm.diar,H.perm.diar,
              N.perm.diar)
perm.diarr.dist_j=t(sapply(out.list,format.perm))



coln=colnames(perm.haz.dist_j)
newlaz=matrix(NA,4,6)
newdiar=matrix(NA,4,6)
for(i in 1:ncol(perm.haz.dist_j)){
  newlaz[,i]=unlist(perm.haz.dist_j[,i])
  newdiar[,i]=unlist(perm.diarr.dist_j[,i])
}
perm.haz.dist_j=as.data.frame(newlaz)
perm.diarr.dist_j=as.data.frame(newdiar)
colnames(perm.haz.dist_j)=coln
colnames(perm.diarr.dist_j)=coln
rownames(perm.haz.dist_j)=c("Water","Sanitation","Handwashing","Nutrition")
rownames(perm.diarr.dist_j)=c("Water","Sanitation","Handwashing","Nutrition")


save(perm.haz.dist_j, perm.diarr.dist_j,
     file="~/Dropbox/WBK-primary-analysis/Results/jade/spill-dist-unadj.RData")

