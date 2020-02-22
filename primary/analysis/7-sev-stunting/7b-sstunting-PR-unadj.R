##############################################
# WASH Benefits Kenya
# Primary outcome analysis  

# Severe stunting unadjusted analysis
# calculate unadjusted differences
# between treatment arms for H1 and H3

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: sstunt_pr_unadj.RData, sstunt_rd_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

m=read.csv(here("primary/data/washb-kenya-midline-anthro-public.csv"))
e=read.csv(here("primary/data/washb-kenya-endline-anthro-public.csv"))

m=preprocess.anthro(m, "sstunted")
e=preprocess.anthro(e, "sstunted")

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
# Midline
#----------------------------------------------
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

sstunt_t1_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$sstunted,tr=m$tr,
        strat=m$block,contrast=c("Control",x),measure="RD")))

sstunt_t1_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$sstunted,tr=m$tr,
        strat=m$block,contrast=c("Control",x),measure="RR")))

rownames(sstunt_t1_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(sstunt_t1_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")


#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
# Endline
#----------------------------------------------
sstunt_t2_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$sstunted,tr=e$tr,
        strat=e$block,contrast=c("Control",x),measure="RD")))

sstunt_t2_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$sstunted,tr=e$tr,
        strat=e$block,contrast=c("Control",x),measure="RR")))

rownames(sstunt_t2_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(sstunt_t2_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

  
#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSHN vs. 
# single arms.  PR, CI, MH P-value
# Midline
#----------------------------------------------
trlist=c("Nutrition","WSH")

sstunt_t1_h3_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$sstunted,tr=m$tr,
        strat=m$block,contrast=c(x,"Nutrition + WSH"),measure="RD")))

sstunt_t1_h3_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$sstunted,tr=m$tr,
        strat=m$block,contrast=c(x,"Nutrition + WSH"),measure="RR")))

rownames(sstunt_t1_h3_pr_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")
rownames(sstunt_t1_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")

  
#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSHN vs. 
# single arms.  PR, CI, MH P-value
# Endline
#----------------------------------------------
trlist=c("Nutrition","WSH")

sstunt_t2_h3_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$sstunted,tr=e$tr,
        strat=e$block,contrast=c(x,"Nutrition + WSH"),measure="RD")))

sstunt_t2_h3_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$sstunted,tr=e$tr,
        strat=e$block,contrast=c(x,"Nutrition + WSH"),measure="RR")))

rownames(sstunt_t2_h3_pr_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")
rownames(sstunt_t2_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")

sstunt_t1_h1_pr_unadj_j
sstunt_t1_h3_pr_unadj_j
sstunt_t2_h1_pr_unadj_j
sstunt_t2_h3_pr_unadj_j

sstunt_t1_h1_rd_unadj_j
sstunt_t1_h3_rd_unadj_j
sstunt_t2_h1_rd_unadj_j
sstunt_t2_h3_rd_unadj_j

#----------------------------------------------
# save objects
#----------------------------------------------
save(sstunt_t1_h1_pr_unadj_j, sstunt_t1_h3_pr_unadj_j,
     sstunt_t2_h1_pr_unadj_j, sstunt_t2_h3_pr_unadj_j,
     file=here("primary/res_data/sstunt_pr_unadj.RData"))

  save(sstunt_t1_h1_rd_unadj_j, sstunt_t1_h3_rd_unadj_j,
       sstunt_t2_h1_rd_unadj_j, sstunt_t2_h3_rd_unadj_j,
       file=here("primary/res_data/sstunt_rd_unadj.RData"))
