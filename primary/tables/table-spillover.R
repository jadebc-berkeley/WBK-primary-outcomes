##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Table of between cluster spillover p-values
# from permutation test

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################

rm(list=ls())

source("~/Dropbox/WBK-primary-analysis/Results/table/0-table-base-functions.R")

load("~/Dropbox/WBK-primary-analysis/Results/jade/spill-dist-unadj.RData")
load("~/Dropbox/WBK-primary-analysis/Results/jade/spill-dist-adj.RData")

#-----------------------------------------
# unadjusted
#-----------------------------------------
# distance
perm.haz.dist.df=data.frame(perm.haz.dist_j)
perm.haz.dist.df$Mean20=apply(matrix(unlist(perm.haz.dist.df$Mean20)),1,pt.est.f,3,1)
perm.haz.dist.df$Mean80=apply(matrix(unlist(perm.haz.dist.df$Mean80)),1,pt.est.f,3,1)
perm.haz.dist.df$Diff=apply(matrix(unlist(perm.haz.dist.df$Diff)),1,pt.est.f,3,1)
perm.haz.dist.df$P.value=apply(matrix(unlist(perm.haz.dist.df$P.value)),1,pt.est.f,3,1)

perm.diarr.dist.df=data.frame(perm.diarr.dist_j)
perm.diarr.dist.df$Mean20=apply(matrix(unlist(perm.diarr.dist.df$Mean20)),1,pt.est.f,3,1)
perm.diarr.dist.df$Mean80=apply(matrix(unlist(perm.diarr.dist.df$Mean80)),1,pt.est.f,3,1)
perm.diarr.dist.df$Diff=apply(matrix(unlist(perm.diarr.dist.df$Diff)),1,pt.est.f,3,1)
perm.diarr.dist.df$P.value=apply(matrix(unlist(perm.diarr.dist.df$P.value)),1,pt.est.f,3,1)

perm.haz.dist.df$lab=c("Water, WSH, Nutrition + WSH",
                       "Sanitation, WSH, Nutrition + WSH",
                       "Handwashing, WSH, Nutrition + WSH",
                       "Nutrition, Nutrition + WSH")
perm.haz.dist.df=perm.haz.dist.df[,c(7,1:6)]

perm.diarr.dist.df$lab=c("Water, WSH, Nutrition + WSH",
                         "Sanitation, WSH, Nutrition + WSH",
                         "Handwashing, WSH, Nutrition + WSH",
                         "Nutrition, Nutrition + WSH")
perm.diarr.dist.df=perm.diarr.dist.df[,c(7,1:6)]

#-----------------------------------------
# adjusted
#-----------------------------------------
perm.haz.dist.adj.df=data.frame(perm.haz.dist.adj_j)
perm.haz.dist.adj.df$Diff=apply(matrix(unlist(perm.haz.dist.adj.df$Diff)),1,pt.est.f,3,1)
perm.haz.dist.adj.df$P.value=apply(matrix(unlist(perm.haz.dist.adj.df$P.value)),1,pt.est.f,3,1)

perm.diarr.dist.adj.df=data.frame(perm.diarr.dist.adj_j)
perm.diarr.dist.adj.df$Diff=apply(matrix(unlist(perm.diarr.dist.adj.df$Diff)),1,pt.est.f,3,1)
perm.diarr.dist.adj.df$P.value=apply(matrix(unlist(perm.diarr.dist.adj.df$P.value)),1,pt.est.f,3,1)

sp.haz = cbind(perm.haz.dist.df,perm.haz.dist.adj.df$P.value)
sp.diarr = cbind(perm.diarr.dist.df,perm.diarr.dist.adj.df$P.value)

colnames(sp.haz)=c(colnames(perm.haz.dist.df),"Adj-P.value")
colnames(sp.diarr)=c(colnames(perm.diarr.dist.df),"Adj-P.value")

save(sp.haz,sp.diarr,
     file="~/Dropbox/WBK-primary-analysis/Results/jade/table-btw-spill.RData")

