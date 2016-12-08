##############################################
# WASH Benefits Kenya
# Primary outcome analysis

# Generate plots of each primray outcome by 
# the proportion of treated households within
# 2km of each control compound for sets of treatments
# 
# by Jade (jadebc@berkeley.edu)
##############################################
rm(list=ls())
library(ggplot2)

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
e=read.csv("~/Dropbox/WBK-primary-analysis/Data/final/jade/endline-anthro.csv",stringsAsFactors=TRUE)
e=preprocess.anthro(e, "haz")

# subset to control compounds only
e = subset(e,e$tr=="Control")
e=e[order(e$hhid),]

#----------------------------------------------------
# Read in outcome data - diarrhea
#----------------------------------------------------
# load child length endline dataset
data=read.csv("~/Dropbox/WBK-primary-analysis/Data/final/jade/diarrhea.csv")
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
# Merge datasets
#----------------------------------------------------
mymerge=function(trNdata,ydata,y){
  y.ncomp=merge(trNdata,ydata[,c("hhid",y)],by="hhid",all.x=TRUE,all.y=TRUE)
  y.ncomp=y.ncomp[!is.na(y.ncomp[[y]]),]
  y.ncomp=y.ncomp[!is.na(y.ncomp$prop),]
  return(y.ncomp)
}

W.dat.haz=mymerge(W.comp2km,e,"haz")
S.dat.haz=mymerge(S.comp2km,e,"haz")
H.dat.haz=mymerge(H.comp2km,e,"haz")
N.dat.haz=mymerge(N.comp2km,e,"haz")

ggplot(W.dat.haz,aes(x=prop,y=haz))+geom_point()
ggplot(S.dat.haz,aes(x=prop,y=haz))+geom_point()
ggplot(H.dat.haz,aes(x=prop,y=haz))+geom_point()
ggplot(N.dat.haz,aes(x=prop,y=haz))+geom_point()



