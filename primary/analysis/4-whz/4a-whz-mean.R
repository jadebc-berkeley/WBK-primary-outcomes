##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Weight-for-height 
# N, mean, SD by arm

# input: midline-anthro.csv, endline-anthro.csv
# output: whz_mean.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################

rm(list=ls())
m=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/midline-anthro.csv")
e=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.csv")

source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")

m=preprocess.anthro(m, "whz")
e=preprocess.anthro(e, "whz")

#----------------------------------------------
# N by arm
#----------------------------------------------
N.1=table(m$tr[!is.na(m$whz)])
N.2=table(e$tr[!is.na(e$whz)])

#----------------------------------------------
# mean whz by arm
#----------------------------------------------
mn.whz.1=as.numeric(t(aggregate(m$whz,list(m$tr),mean,na.rm=TRUE))[2,])
mn.whz.2=as.numeric(t(aggregate(e$whz,list(e$tr),mean,na.rm=TRUE))[2,])


#----------------------------------------------
# sd of whz by arm
#----------------------------------------------
sd.whz.1=as.numeric(t(aggregate(m$whz,list(m$tr),sd,na.rm=TRUE))[2,])
sd.whz.2=as.numeric(t(aggregate(e$whz,list(e$tr),sd,na.rm=TRUE))[2,])

whz_t1_n_j=as.data.frame(matrix(cbind(N.1,mn.whz.1,sd.whz.1),8,3))
rownames(whz_t1_n_j)=levels(m$tr)
colnames(whz_t1_n_j)=c("N","mean","sd")

whz_t2_n_j=as.data.frame(matrix(cbind(N.2,mn.whz.2,sd.whz.2),8,3))
rownames(whz_t2_n_j)=levels(e$tr)
colnames(whz_t2_n_j)=c("N","mean","sd")

whz_t1_n_j

whz_t2_n_j

save(whz_t1_n_j,whz_t2_n_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/whz_mean.RData")




