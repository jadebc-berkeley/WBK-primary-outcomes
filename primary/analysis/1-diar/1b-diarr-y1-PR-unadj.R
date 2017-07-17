##############################################
# WASH Benefits Kenya
# Primary outcome analysis  

# Diarrhea unadjusted analysis
# Midline data

# by Jade (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())
data=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/diarrhea.csv")
source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")

d=preprocess.diarr(data)

# subset to year 1 and 2
df = d[d$time==1,]
  
# subset to columns needed for unadjusted PR
df = df[,c("block","clusterid","tr","diarr7")]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
#----------------------------------------------
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

diar_y1_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$diarr7,tr=df$tr,
        strat=df$block,contrast=c("Control",x),measure="RD")))

diar_y1_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$diarr7,tr=df$tr,
        strat=df$block,contrast=c("Control",x),measure="RR")))

rownames(diar_y1_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(diar_y1_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")
  
#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, MH P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing")

diar_y1_h2_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$diarr7,tr=df$tr,
        strat=df$block,contrast=c(x,"WSH"),measure="RD")))

diar_y1_h2_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$diarr7,tr=df$tr,
        strat=df$block,contrast=c(x,"WSH"),measure="RR")))

rownames(diar_y1_h2_pr_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(diar_y1_h2_rd_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")


save(diar_y1_h1_pr_unadj_j, diar_y1_h2_pr_unadj_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/diar_y1_pr_unadj.RData")

  save(diar_y1_h1_rd_unadj_j, diar_y1_h2_rd_unadj_j,
       file="~/Dropbox/WBK-primary-analysis/results/jade/diar_y1_rd_unadj.RData")
  