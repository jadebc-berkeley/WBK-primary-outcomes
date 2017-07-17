##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Bruising (negative control) 
# n, N, prevalence, and 95% CI by arm at
# baseline and follow-up

# input: bruise.csv
# output: bruise_prev.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())
source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")

d=read.csv("~/Dropbox/WBK-primary-analysis/Data/final/jade/bruise.csv")

# keep diarrhea cohort members
d=subset(d, d$dcohort==1)

# reorder tr labels
reord=function(x){
  x$tr=factor(x$tr,levels(x$tr)[c(1,5,7,6,2,8,3,4)])
  return(x)
}

d=reord(d)

#----------------------------------------------
# n and N by arm and follow-up time point
#----------------------------------------------
N.1=table(d$tr[!is.na(d$bruise7) & d$time==1])
n.1=table(d$bruise7[d$time==1],d$tr[d$time==1])[2,]

bruise_t1_n_j=data.frame(cbind(N.1=N.1,n.1=n.1))

N.2=table(d$tr[!is.na(d$bruise7) & d$time==2])
n.2=table(d$bruise7[d$time==2],d$tr[d$time==2])[2,]

bruise_t2_n_j=data.frame(cbind(N.2=N.2,n.2=n.2))

N.12=table(d$tr[!is.na(d$bruise7) & d$time>0])
n.12=table(d$bruise7[d$time>0],d$tr[d$time>0])[2,]

bruise_t12_n_j=data.frame(cbind(N.12=N.12,n.12=n.12))

names(N.1)=NULL
names(N.2)=NULL
names(N.12)=NULL

#----------------------------------------------
# prevalence by arm and time point
#----------------------------------------------
bruise_t1_prev_j=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$bruise7[d$tr==x & d$time==1],id=d$clusterid[d$tr==x & d$time==1],print=FALSE)))

bruise_t2_prev_j=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$bruise7[d$tr==x & d$time==2],id=d$clusterid[d$tr==x & d$time==2],print=FALSE)))

bruise_t12_prev_j=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$bruise7[d$tr==x & d$time>=1],id=d$clusterid[d$tr==x & d$time>=1],print=FALSE)))

colnames(bruise_t1_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(bruise_t2_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(bruise_t12_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")

# drop columns that aren't needed
bruise_t1_prev_j=bruise_t1_prev_j[,c("Prev","lb","ub")]
bruise_t2_prev_j=bruise_t2_prev_j[,c("Prev","lb","ub")]
bruise_t12_prev_j=bruise_t12_prev_j[,c("Prev","lb","ub")]

bruise_t1_n_j
bruise_t2_n_j
bruise_t12_n_j

bruise_t1_prev_j
bruise_t2_prev_j
bruise_t12_prev_j

save(
  bruise_t1_n_j, bruise_t2_n_j, bruise_t12_n_j, 
  bruise_t1_prev_j, bruise_t2_prev_j, bruise_t12_prev_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/bruise_prev.RData")

