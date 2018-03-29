##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# All-cause mortality

# input: washb-kenya-mortality-public.csv
# output: mort_prev.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())

# define directories
source.dir="~/documents/crg/wash-benefits/kenya/src/primary/analysis/"
data.dir="~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/Public/"
res.dir="~/Dropbox/WBK-primary-analysis/results/jade/"

source(paste0(source.dir,"0-base-programs.R"))

d=read.csv(paste0(data.dir,"washb-kenya-mortality-public.csv"))

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
     file=paste0(res.dir,"mort_prev.RData"))

