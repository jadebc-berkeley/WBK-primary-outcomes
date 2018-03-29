##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Weight-for-age 
# N, mean, SD by arm

# input: midline-anthro.csv, endline-anthro.csv
# output: waz_mean.RData

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

m=preprocess.anthro(m, "waz")
e=preprocess.anthro(e, "waz")

#----------------------------------------------
# N by arm
#----------------------------------------------
N.1=table(m$tr[!is.na(m$waz)])
N.2=table(e$tr[!is.na(e$waz)])

#----------------------------------------------
# mean waz by arm
#----------------------------------------------
mn.waz.1=as.numeric(t(aggregate(m$waz,list(m$tr),mean,na.rm=TRUE))[2,])
mn.waz.2=as.numeric(t(aggregate(e$waz,list(e$tr),mean,na.rm=TRUE))[2,])


#----------------------------------------------
# sd of waz by arm
#----------------------------------------------
sd.waz.1=as.numeric(t(aggregate(m$waz,list(m$tr),sd,na.rm=TRUE))[2,])
sd.waz.2=as.numeric(t(aggregate(e$waz,list(e$tr),sd,na.rm=TRUE))[2,])

waz_t1_n_j=as.data.frame(matrix(cbind(N.1,mn.waz.1,sd.waz.1),8,3))
rownames(waz_t1_n_j)=levels(m$tr)
colnames(waz_t1_n_j)=c("N","mean","sd")

waz_t2_n_j=as.data.frame(matrix(cbind(N.2,mn.waz.2,sd.waz.2),8,3))
rownames(waz_t2_n_j)=levels(e$tr)
colnames(waz_t2_n_j)=c("N","mean","sd")

waz_t1_n_j

waz_t2_n_j

save(waz_t1_n_j,waz_t2_n_j,
     file=paste0(res.dir,"waz_mean.RData"))




