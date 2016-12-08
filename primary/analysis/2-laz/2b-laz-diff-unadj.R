##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# LAZ unadjusted analysis

# by Jade (jadebc@berkeley.edu)
##############################################

library(washb)

rm(list=ls())
m=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/midline-anthro.csv")
e=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.csv")

source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")

m=preprocess.anthro(m, "haz")
e=preprocess.anthro(e, "haz")

# #----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
#----------------------------------------------
# Midline
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

laz_t1_h1_rd_unadj_j=t(sapply(trlist,function(x) washb_ttest(Y=m$haz,tr=m$tr,
     strat=m$block, contrast=c("Control",x))))

rownames(laz_t1_h1_rd_unadj_j)=c("Passive Control vs C","Water vs C",
	"Sanitation vs C","Handwashing vs C","WSH vs C","Nutrition vs C",
	"Nutrition + WSH vs C")

# Endline
laz_t2_h1_rd_unadj_j=t(sapply(trlist,function(x) washb_ttest(Y=e$haz,tr=e$tr,
     strat=e$block, contrast=c("Control",x))))

rownames(laz_t2_h1_rd_unadj_j)=c("Passive Control vs C","Water vs C",
	"Sanitation vs C","Handwashing vs C","WSH vs C","Nutrition vs C",
	"Nutrition + WSH vs C")

#----------------------------------------------
# H3: H3: Unadjusted difference and 95% CI and 
# t-test P-value; WSHN vs. N and WSHN vs. WSH
#----------------------------------------------

# Midline
trlist=c("Nutrition","WSH")

laz_t1_h3_rd_unadj_j=t(sapply(trlist,function(x) washb_ttest(Y=m$haz,tr=m$tr,
     strat=m$block, contrast=c(x,"Nutrition + WSH"))))

rownames(laz_t1_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition",
                                   "Nutrition + WSH vs WSH")

# Endline
laz_t2_h3_rd_unadj_j=t(sapply(trlist,function(x) washb_ttest(Y=e$haz,tr=e$tr,
     strat=e$block, contrast=c(x,"Nutrition + WSH"))))

rownames(laz_t2_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition",
                                   "Nutrition + WSH vs WSH")


save(laz_t1_h1_rd_unadj_j, laz_t1_h3_rd_unadj_j,
     laz_t2_h1_rd_unadj_j, laz_t2_h3_rd_unadj_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/laz_rd_unadj.RData")

