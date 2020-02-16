##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# All-cause mortality

# input: washb-kenya-mortality-public.csv
# output: mort_prev.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

d=read.csv(here("primary/data/washb-kenya-mortality-public.csv"))

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
     file=here("primary/res_data/mort_prev.RData"))

