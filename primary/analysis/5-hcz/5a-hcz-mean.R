##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Head circumference z-score 
# N, mean, SD by arm

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: hcz_mean.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################

rm(list=ls())

# define directories
source.dir="~/documents/crg/wash-benefits/kenya/src/primary/analysis/"
data.dir="~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/Public/"
res.dir="~/Dropbox/WBK-primary-analysis/results/jade/"

m=read.csv(paste0(data.dir,"washb-kenya-midline-anthro-public.csv"))
e=read.csv(paste0(data.dir,"washb-kenya-endline-anthro-public.csv"))

source(paste0(source.dir,"0-base-programs.R"))

m=preprocess.anthro(m, "hcz")
e=preprocess.anthro(e, "hcz")

#----------------------------------------------
# N by arm
#----------------------------------------------
N.1=table(m$tr[!is.na(m$hcz)])
N.2=table(e$tr[!is.na(e$hcz)])

#----------------------------------------------
# mean hcz by arm
#----------------------------------------------
mn.hcz.1=as.numeric(t(aggregate(m$hcz,list(m$tr),mean,na.rm=TRUE))[2,])
mn.hcz.2=as.numeric(t(aggregate(e$hcz,list(e$tr),mean,na.rm=TRUE))[2,])


#----------------------------------------------
# sd of hcz by arm
#----------------------------------------------
sd.hcz.1=as.numeric(t(aggregate(m$hcz,list(m$tr),sd,na.rm=TRUE))[2,])
sd.hcz.2=as.numeric(t(aggregate(e$hcz,list(e$tr),sd,na.rm=TRUE))[2,])

hcz_t1_n_j=as.data.frame(matrix(cbind(N.1,mn.hcz.1,sd.hcz.1),8,3))
rownames(hcz_t1_n_j)=levels(m$tr)
colnames(hcz_t1_n_j)=c("N","mean","sd")

hcz_t2_n_j=as.data.frame(matrix(cbind(N.2,mn.hcz.2,sd.hcz.2),8,3))
rownames(hcz_t2_n_j)=levels(e$tr)
colnames(hcz_t2_n_j)=c("N","mean","sd")

hcz_t1_n_j

hcz_t2_n_j

save(hcz_t1_n_j,hcz_t2_n_j,
     file=paste0(res.dir,"hcz_mean.RData"))




