##############################################
# WASH Benefits Kenya
# Primary outcome analysis  

# Mortality unadjusted analysis
# calculate unadjusted differences
# between treatment arms for H1 and H2

# input: washb-kenya-mortality-public.csv
# output: mort_pr_unadj.RData, mort_rd_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())

# define directories
source.dir="~/documents/crg/wash-benefits/kenya/src/primary/analysis/"
data.dir="~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/Public/"
res.dir="~/Dropbox/WBK-primary-analysis/results/jade/"

d=read.csv(paste0(data.dir,"washb-kenya-mortality-public.csv"))
source(paste0(source.dir,"0-base-programs.R"))

# subset to columns needed for unadjusted PR
df = d[,c("block","clusterid","tr","childdeath")]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
#----------------------------------------------
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

mort_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$childdeath,tr=df$tr,
     strat=df$block,contrast=c("Control",x),measure="RD")))

mort_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$childdeath,tr=df$tr,
    strat=df$block,contrast=c("Control",x),measure="RR")))

rownames(mort_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
                               "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(mort_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
                               "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, MH P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing")

mort_h2_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$childdeath,tr=df$tr,
       strat=df$block,contrast=c(x,"WSH"),measure="RD")))

mort_h2_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$childdeath,tr=df$tr,
       strat=df$block,contrast=c(x,"WSH"),measure="RR")))

rownames(mort_h2_pr_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(mort_h2_rd_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")


save(mort_h1_pr_unadj_j, mort_h2_pr_unadj_j,
     file=paste0(res.dir,"mort_pr_unadj.RData"))

save(mort_h1_rd_unadj_j, mort_h2_rd_unadj_j,
     file=paste0(res.dir,"mort_rd_unadj.RData"))
