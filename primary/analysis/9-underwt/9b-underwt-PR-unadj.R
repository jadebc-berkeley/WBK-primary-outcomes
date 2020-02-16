##############################################
# WASH Benefits Kenya
# Primary outcome analysis  

# Stunting unadjusted analysis
# calculate unadjusted differences
# between treatment arms for H1 and H3

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: underwt_pr_unadj.RData, underwt_rd_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

m=read.csv(here("primary/data/washb-kenya-midline-anthro-public.csv"))
e=read.csv(here("primary/data/washb-kenya-endline-anthro-public.csv"))

m=preprocess.anthro(m, "underwt")
e=preprocess.anthro(e, "underwt")

#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
# Midline
#----------------------------------------------
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

underwt_t1_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$underwt,tr=m$tr,
        strat=m$block,contrast=c("Control",x),measure="RD")))

underwt_t1_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$underwt,tr=m$tr,
        strat=m$block,contrast=c("Control",x),measure="RR")))

rownames(underwt_t1_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(underwt_t1_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")


#----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
# Endline
#----------------------------------------------
underwt_t2_h1_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$underwt,tr=e$tr,
        strat=e$block,contrast=c("Control",x),measure="RD")))

underwt_t2_h1_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$underwt,tr=e$tr,
        strat=e$block,contrast=c("Control",x),measure="RR")))

rownames(underwt_t2_h1_pr_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

rownames(underwt_t2_h1_rd_unadj_j)=c("Passive vs Active C","Water vs C","Sanitation vs C",
  "Handwashing vs C", "WSH vs C","Nutrition vs C","Nutrition + WSH vs C")

  
#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSHN vs. 
# single arms.  PR, CI, MH P-value
# Midline
#----------------------------------------------
trlist=c("Nutrition","WSH")

underwt_t1_h3_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$underwt,tr=m$tr,
        strat=m$block,contrast=c(x,"Nutrition + WSH"),measure="RD")))

underwt_t1_h3_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=m$underwt,tr=m$tr,
        strat=m$block,contrast=c(x,"Nutrition + WSH"),measure="RR")))

rownames(underwt_t1_h3_pr_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")
rownames(underwt_t1_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")

  
#----------------------------------------------
# H3: Unadjusted prevalence ratios; combined WSHN vs. 
# single arms.  PR, CI, MH P-value
# Endline
#----------------------------------------------
trlist=c("Nutrition","WSH")

underwt_t2_h3_rd_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$underwt,tr=e$tr,
        strat=e$block,contrast=c(x,"Nutrition + WSH"),measure="RD")))

underwt_t2_h3_pr_unadj_j=t(sapply(trlist, function(x) washb_mh(Y=e$underwt,tr=e$tr,
        strat=e$block,contrast=c(x,"Nutrition + WSH"),measure="RR")))

rownames(underwt_t2_h3_pr_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")
rownames(underwt_t2_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition","Nutrition + WSH vs WSH")

  
underwt_t1_h1_pr_unadj_j
underwt_t1_h3_pr_unadj_j
underwt_t2_h1_pr_unadj_j
underwt_t2_h3_pr_unadj_j

underwt_t1_h1_rd_unadj_j
underwt_t1_h3_rd_unadj_j
underwt_t2_h1_rd_unadj_j
underwt_t2_h3_rd_unadj_j

#----------------------------------------------
# save objects
#----------------------------------------------
save(underwt_t1_h1_pr_unadj_j, underwt_t1_h3_pr_unadj_j,
     underwt_t2_h1_pr_unadj_j, underwt_t2_h3_pr_unadj_j,
     file=here("primary/res_data/underwt_pr_unadj.RData"))

  save(underwt_t1_h1_rd_unadj_j, underwt_t1_h3_rd_unadj_j,
       underwt_t2_h1_rd_unadj_j, underwt_t2_h3_rd_unadj_j,
       file=here("primary/res_data/underwt_rd_unadj.RData"))
