##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Table with diarrhea results

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################

rm(list=ls())

source("~/Documents/CRG/wash-benefits/kenya/src/primary/tables/0-table-base-functions.R")

load("~/Dropbox/WBK-primary-analysis/results/jade/laz_mean.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/laz_mean.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/whz_mean.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/hcz_mean.RData")

load("~/Dropbox/WBK-primary-analysis/results/jade/laz_rd_unadj.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/laz_rd_unadj.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/whz_rd_unadj.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/hcz_rd_unadj.RData")

load("~/Dropbox/WBK-primary-analysis/results/jade/laz_t2_pval_unadj.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/laz_t2_pval_unadj.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/whz_t2_pval_unadj.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/hcz_t2_pval_unadj.RData")


#----------------- control prevalence ----------------- 
laz_t2_prev=sprintf("%0.02f",laz_t2_n_j[,2])
laz_t2_prev=sprintf("%0.02f",laz_t2_n_j[,2])
whz_t2_prev=sprintf("%0.02f",whz_t2_n_j[,2])
hcz_t2_prev=sprintf("%0.02f",hcz_t2_n_j[,2])

#----------------- risk differences ----------------- 
rd.laz.h1.pt.est=c("",apply(as.matrix(laz_t2_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.laz.h3.pt.est=rbind(matrix("",7,2),apply(as.matrix(laz_t2_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.laz.h1.pt.est=c("",apply(as.matrix(laz_t2_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.laz.h3.pt.est=rbind(matrix("",7,2),apply(as.matrix(laz_t2_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.whz.h1.pt.est=c("",apply(as.matrix(whz_t2_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.whz.h3.pt.est=rbind(matrix("",7,2),apply(as.matrix(whz_t2_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

rd.hcz.h1.pt.est=c("",apply(as.matrix(hcz_t2_h1_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))
rd.hcz.h3.pt.est=rbind(matrix("",7,2),apply(as.matrix(hcz_t2_h3_rd_unadj_j),1,pt.est.ci.f,decimals=2,scale=1))

  
#----------------- rr table ----------------- 
blank=rep("",7)
table=data.frame(rbind(cbind(laz_t2_prev,rd.laz.h1.pt.est,rd.laz.h3.pt.est),
		cbind(laz_t2_prev,rd.laz.h1.pt.est,rd.laz.h3.pt.est),
      cbind(whz_t2_prev,rd.whz.h1.pt.est,rd.whz.h3.pt.est),
      cbind(hcz_t2_prev,rd.hcz.h1.pt.est,rd.hcz.h3.pt.est)))

lab=rep(c("Control","Passive Control","Water","Sanitation","Handwashing",
                           "Water + Sanitation + Handwashing (WSH)","Nutrition",
                           "Nutrition + WSH"),4)


anthroZ.table=data.frame(cbind(lab,table))

save(anthroZ.table,file="~/Dropbox/WBK-primary-analysis/Results/jade/table-anthroZ-reformat.RData")

save(anthroZ.table,file="~/Dropbox/WBK-primary-analysis/Results/tables/table-anthroZ-reformat.RData")



