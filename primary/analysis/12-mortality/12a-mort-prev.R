##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# All-cause mortality

# by Jade (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())
source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")

d=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/mortality.csv")

# reorder tr labels
reord=function(x){
  x$tr=factor(x$tr,levels(x$tr)[c(1,5,7,6,2,8,3,4)])
  return(x)
}

d=reord(d)

#----------------------------------------------
# n and N by arm and follow-up time point
#----------------------------------------------
N_mort_j=table(d$tr,d$childdeath)


#----------------------------------------------
# prevalence by arm and time point
#----------------------------------------------
mort_prev_j=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$childdeath[d$tr==x],id=d$clusterid[d$tr==x],print=FALSE)))

colnames(mort_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")

N_mort_j
mort_prev_j

save(N_mort_j, mort_prev_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/mort_prev.RData")

