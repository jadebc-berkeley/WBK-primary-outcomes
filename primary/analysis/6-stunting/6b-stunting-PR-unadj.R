##############################################
# WASH Benefits Kenya
# Primary outcome analysis  

# Stunting unadjusted analysis
# calculate unadjusted differences
# between treatment arms for H1 and H3

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: stunt_pr_unadj.RData, stunt_rd_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
library(washb)

rm(list=ls())

# define directories
source.dir="~/documents/crg/wash-benefits/kenya/src/primary/analysis/"
data.dir="~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/Public/"
res.dir="~/Dropbox/WBK-primary-analysis/results/jade/"

m=read.csv(paste0(data.dir,"washb-kenya-midline-anthro-public.csv"))
e=read.csv(paste0(data.dir,"washb-kenya-endline-anthro-public.csv"))

source(paste0(source.dir,"0-base-programs.R"))

m=preprocess.anthro(m, "stunted")
e=preprocess.anthro(e, "stunted")

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
# Midline
#----------------------------------------------
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

stunt_t1_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$stunted,tr=m$tr,
        strat=m$block,contrast=c("Control",x),measure="RD")))

stunt_t1_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$stunted,tr=m$tr,
        strat=m$block,contrast=c("Control",x),measure="RR")))

rownames(stunt_t1_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(stunt_t1_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")


#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
# Endline
#----------------------------------------------
stunt_t2_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$stunted,tr=e$tr,
        strat=e$block,contrast=c("Control",x),measure="RD")))

stunt_t2_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$stunted,tr=e$tr,
        strat=e$block,contrast=c("Control",x),measure="RR")))

rownames(stunt_t2_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(stunt_t2_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

  
#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSHN vs. 
# single arms.  PR, CI, MH P-value
# Midline
#----------------------------------------------
trlist=c("Nutrition","WSH")

stunt_t1_h3_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$stunted,tr=m$tr,
        strat=m$block,contrast=c(x,"Nutrition + WSH"),measure="RD")))

stunt_t1_h3_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$stunted,tr=m$tr,
        strat=m$block,contrast=c(x,"Nutrition + WSH"),measure="RR")))

rownames(stunt_t1_h3_pr_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")
rownames(stunt_t1_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")

  
#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSHN vs. 
# single arms.  PR, CI, MH P-value
# Endline
#----------------------------------------------
trlist=c("Nutrition","WSH")

stunt_t2_h3_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$stunted,tr=e$tr,
        strat=e$block,contrast=c(x,"Nutrition + WSH"),measure="RD")))

stunt_t2_h3_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$stunted,tr=e$tr,
        strat=e$block,contrast=c(x,"Nutrition + WSH"),measure="RR")))

rownames(stunt_t2_h3_pr_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")
rownames(stunt_t2_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")

  

#----------------------------------------------
# save objects
#----------------------------------------------
save(stunt_t1_h1_pr_unadj_j, stunt_t1_h3_pr_unadj_j,
     stunt_t2_h1_pr_unadj_j, stunt_t2_h3_pr_unadj_j,
     file=paste0(res.dir,"stunt_pr_unadj.RData"))

  save(stunt_t1_h1_rd_unadj_j, stunt_t1_h3_rd_unadj_j,
       stunt_t2_h1_rd_unadj_j, stunt_t2_h3_rd_unadj_j,
       file=paste0(res.dir,"stunt_rd_unadj.RData"))
