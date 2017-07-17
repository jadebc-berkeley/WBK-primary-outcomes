##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Underweight
# n, N, prevalence, and 95% CI by arm at
# baseline and follow-up

# input: midline-anthro.csv, endline-anthro.csv
# output: underwt_prev.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())
source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")

m=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/midline-anthro.csv")
e=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.csv")

m=preprocess.anthro(m, "underwt")
e=preprocess.anthro(e, "underwt")

#----------------------------------------------
# n and N by arm and follow-up time point
#----------------------------------------------
N.1=table(m$tr[!is.na(m$underwt)])
n.1=table(m$underwt,m$tr)[2,]

underwt_t1_n_j=data.frame(cbind(N.1=N.1,n.1=n.1))

N.2=table(e$tr[!is.na(e$underwt)])
n.2=table(e$underwt,e$tr)[2,]

underwt_t2_n_j=data.frame(cbind(N.2=N.2,n.2=n.2))

names(N.1)=NULL
names(N.2)=NULL

#----------------------------------------------
# prevalence by arm and time point
#----------------------------------------------
underwt_t1_prev_j=t(sapply(levels(m$tr), function(x) washb_mean(
  Y=m$underwt[m$tr==x],id=m$clusterid[m$tr==x],print=FALSE)))

underwt_t2_prev_j=t(sapply(levels(e$tr), function(x) washb_mean(
  Y=e$underwt[e$tr==x],id=e$clusterid[e$tr==x],print=FALSE)))

colnames(underwt_t1_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(underwt_t2_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")

# drop columns that aren't needed
underwt_t1_prev_j=underwt_t1_prev_j[,c("Prev","lb","ub")]
underwt_t2_prev_j=underwt_t2_prev_j[,c("Prev","lb","ub")]

underwt_t1_n_j
underwt_t2_n_j

underwt_t1_prev_j
underwt_t2_prev_j

save(underwt_t1_n_j, underwt_t2_n_j, underwt_t1_prev_j, underwt_t2_prev_j, 
     file="~/Dropbox/WBK-primary-analysis/results/jade/underwt_prev.RData")

