##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Diarrhea 
# n, N, prevalence, and 95% CI by arm at
# baseline and follow-up

# input: washb-kenya-diar-public.csv
# output: diar_prev.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))


source(here("primary/analysis/0-base-programs.R"))

d=read.csv(here("primary/data/washb-kenya-diar-public.csv"))

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
N.0=table(d$tr[!is.na(d$diarr7)& d$time==0])
n.0=table(d$diarr7[d$time==0],d$tr[d$time==0])[2,]

diar_t0_n_j=data.frame(cbind(N.0=N.0,n.0=n.0))

N.1=table(d$tr[!is.na(d$diarr7) & d$time==1])
n.1=table(d$diarr7[d$time==1],d$tr[d$time==1])[2,]

diar_t1_n_j=data.frame(cbind(N.1=N.1,n.1=n.1))

N.2=table(d$tr[!is.na(d$diarr7) & d$time==2])
n.2=table(d$diarr7[d$time==2],d$tr[d$time==2])[2,]

diar_t2_n_j=data.frame(cbind(N.2=N.2,n.2=n.2))

N.12=table(d$tr[!is.na(d$diarr7) & d$time>0])
n.12=table(d$diarr7[d$time>0],d$tr[d$time>0])[2,]

diar_t12_n_j=data.frame(cbind(N.12=N.12,n.12=n.12))

names(N.0)=NULL
names(N.1)=NULL
names(N.2)=NULL
names(N.12)=NULL

#----------------------------------------------
# prevalence by arm and time point
#----------------------------------------------
diar_t0_prev_j=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$diarr7[d$tr==x & d$time==0],id=d$clusterid[d$tr==x & d$time==0],print=FALSE)))

diar_t1_prev_j=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$diarr7[d$tr==x & d$time==1],id=d$clusterid[d$tr==x & d$time==1],print=FALSE)))

diar_t2_prev_j=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$diarr7[d$tr==x & d$time==2],id=d$clusterid[d$tr==x & d$time==2],print=FALSE)))

diar_t12_prev_j=t(sapply(levels(d$tr), function(x) washb_mean(
  Y=d$diarr7[d$tr==x & d$time>=1],id=d$clusterid[d$tr==x & d$time>=1],print=FALSE)))

colnames(diar_t0_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(diar_t1_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(diar_t2_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")
colnames(diar_t12_prev_j)=c("N","Prev","SD","Robust SE","lb","ub")

# drop columns that aren't needed
diar_t0_prev_j=diar_t0_prev_j[,c("Prev","lb","ub")]
diar_t1_prev_j=diar_t1_prev_j[,c("Prev","lb","ub")]
diar_t2_prev_j=diar_t2_prev_j[,c("Prev","lb","ub")]
diar_t12_prev_j=diar_t12_prev_j[,c("Prev","lb","ub")]

diar_t0_n_j
diar_t1_n_j
diar_t2_n_j
diar_t12_n_j

diar_t0_prev_j
diar_t1_prev_j
diar_t2_prev_j
diar_t12_prev_j

save(diar_t0_n_j, diar_t1_n_j, diar_t2_n_j, diar_t12_n_j, 
     diar_t0_prev_j, diar_t1_prev_j, diar_t2_prev_j, diar_t12_prev_j,
     file=here("primary/res_data/diar_prev.RData"))

