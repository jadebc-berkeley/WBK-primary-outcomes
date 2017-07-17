##############################################
# WASH Benefits Kenya
# Primary outcome analysis  

# Negative control analysis 
# Bruising unadjusted analysis
# calculate unadjusted differences
# between treatment arms for H1 and H2

# input: bruise.csv
# output: bruise_pr_unadj.RData, bruise_rd_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())
data=read.csv("~/Dropbox/WBK-primary-analysis/Data/final/jade/bruise.csv")
source("~/documents/crg/wash-benefits/kenya/src/primary/analysis/0-base-programs.R")

# keep diarrhea cohort members
d=subset(data, data$dcohort==1)

# replace diarr7 as missing if diarr7==999
d$bruise7[d$bruise7==999]=NA

# drop if diarrhea is missing
d=d[!is.na(d$bruise7),]

# reorder tr labels
d$tr=factor(d$tr,levels(d$tr)[c(1,5,7,6,2,8,3,4)])

# keep midline and endline measurements
d=subset(d,d$time>0)

# subset to columns needed for unadjusted PR
df = d[,c("block","clusterid","tr","bruise7")]

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
#----------------------------------------------
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

bruise_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$bruise7,tr=df$tr,
                                                         strat=df$block,contrast=c("Control",x),measure="RD")))

bruise_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$bruise7,tr=df$tr,
                                                         strat=df$block,contrast=c("Control",x),measure="RR")))

rownames(bruise_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
                               "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(bruise_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
                               "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

#----------------------------------------------
# H2: Unadjusted prevalence ratios; combined WSH vs. 
# single arms.  PR, CI, MH P-value
#----------------------------------------------
trlist=c("Water","Sanitation","Handwashing")

bruise_h2_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$bruise7,tr=df$tr,
                                                         strat=df$block,contrast=c(x,"WSH"),measure="RD")))

bruise_h2_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=df$bruise7,tr=df$tr,
                                                         strat=df$block,contrast=c(x,"WSH"),measure="RR")))

rownames(bruise_h2_pr_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")
rownames(bruise_h2_rd_unadj_j)=c("WSH vs Water","WSH vs Sanitation","WSH vs Handwashing")


save(bruise_h1_pr_unadj_j, bruise_h2_pr_unadj_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/bruise_pr_unadj.RData")

save(bruise_h1_rd_unadj_j, bruise_h2_rd_unadj_j,
     file="~/Dropbox/WBK-primary-analysis/results/jade/bruise_rd_unadj.RData")
