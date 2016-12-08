##############################################
# WASH Benefits Kenya
# Primary outcome analysis  

# Wasting unadjusted analysis

# by Jade (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())
source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")

m=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/midline-anthro.csv")
e=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.csv")

m=preprocess.anthro(m, "wasted")
e=preprocess.anthro(e, "wasted")

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
# Midline
#----------------------------------------------
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

wast_t1_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$wasted,tr=m$tr,
        strat=m$block,contrast=c("Control",x),measure="RD")))

wast_t1_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$wasted,tr=m$tr,
        strat=m$block,contrast=c("Control",x),measure="RR")))

rownames(wast_t1_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(wast_t1_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")


#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
# Endline
#----------------------------------------------
wast_t2_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$wasted,tr=e$tr,
        strat=e$block,contrast=c("Control",x),measure="RD")))

wast_t2_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$wasted,tr=e$tr,
        strat=e$block,contrast=c("Control",x),measure="RR")))

rownames(wast_t2_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(wast_t2_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

  
#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSHN vs. 
# single arms.  PR, CI, MH P-value
# Midline
#----------------------------------------------
trlist=c("Nutrition","WSH")

wast_t1_h3_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$wasted,tr=m$tr,
        strat=m$block,contrast=c(x,"Nutrition + WSH"),measure="RD")))

wast_t1_h3_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$wasted,tr=m$tr,
        strat=m$block,contrast=c(x,"Nutrition + WSH"),measure="RR")))

rownames(wast_t1_h3_pr_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")
rownames(wast_t1_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")

  
#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSHN vs. 
# single arms.  PR, CI, MH P-value
# Endline
#----------------------------------------------
trlist=c("Nutrition","WSH")

wast_t2_h3_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$wasted,tr=e$tr,
        strat=e$block,contrast=c(x,"Nutrition + WSH"),measure="RD")))

wast_t2_h3_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$wasted,tr=e$tr,
        strat=e$block,contrast=c(x,"Nutrition + WSH"),measure="RR")))

rownames(wast_t2_h3_pr_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")
rownames(wast_t2_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")

  

#----------------------------------------------
# save objects
#----------------------------------------------
save(wast_t1_h1_pr_unadj_j, wast_t1_h3_pr_unadj_j,
     wast_t2_h1_pr_unadj_j, wast_t2_h3_pr_unadj_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/wast_pr_unadj.RData")

  save(wast_t1_h1_rd_unadj_j, wast_t1_h3_rd_unadj_j,
       wast_t2_h1_rd_unadj_j, wast_t2_h3_rd_unadj_j,
       file="~/Dropbox/WBK-primary-analysis/results/jade/wast_rd_unadj.RData")
