##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# WAZ unadjusted analysis
# calculate unadjusted differences
# between treatment arms for H1 and H3

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: hcz_rd_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

m=read.csv(here("primary/data/washb-kenya-midline-anthro-public.csv"))
e=read.csv(here("primary/data/washb-kenya-endline-anthro-public.csv"))

m=preprocess.anthro(m, "hcz")
e=preprocess.anthro(e, "hcz")

# #----------------------------------------------
# H1: Unadjusted prevalence ratios; each arm vs. 
# control. PR, CI, MH P-value
#----------------------------------------------
# Midline
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

hcz_t1_h1_rd_unadj_j=t(sapply(trlist,function(x) washb_ttest(Y=m$hcz,tr=m$tr,
     strat=m$block, contrast=c("Control",x))))

rownames(hcz_t1_h1_rd_unadj_j)=c("Passive Control vs C","Water vs C",
  "Sanitation vs C","Handwashing vs C","WSH vs C","Nutrition vs C",
  "Nutrition + WSH vs C")

# Endline
hcz_t2_h1_rd_unadj_j=t(sapply(trlist,function(x) washb_ttest(Y=e$hcz,tr=e$tr,
     strat=e$block, contrast=c("Control",x))))

rownames(hcz_t2_h1_rd_unadj_j)=c("Passive Control vs C","Water vs C",
  "Sanitation vs C","Handwashing vs C","WSH vs C","Nutrition vs C",
  "Nutrition + WSH vs C")

#----------------------------------------------
# H3: H3: Unadjusted difference and 95% CI and 
# t-test P-value; WSHN vs. N and WSHN vs. WSH
#----------------------------------------------

# Midline
trlist=c("Nutrition","WSH")

hcz_t1_h3_rd_unadj_j=t(sapply(trlist,function(x) washb_ttest(Y=m$hcz,tr=m$tr,
     strat=m$block, contrast=c(x,"Nutrition + WSH"))))

rownames(hcz_t1_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition",
                                   "Nutrition + WSH vs WSH")

# Endline
hcz_t2_h3_rd_unadj_j=t(sapply(trlist,function(x) washb_ttest(Y=e$hcz,tr=e$tr,
     strat=e$block, contrast=c(x,"Nutrition + WSH"))))

rownames(hcz_t2_h3_rd_unadj_j)=c("Nutrition + WSH vs Nutrition",
                                   "Nutrition + WSH vs WSH")

hcz_t1_h1_rd_unadj_j
hcz_t1_h3_rd_unadj_j
hcz_t2_h1_rd_unadj_j
hcz_t2_h3_rd_unadj_j

save(hcz_t1_h1_rd_unadj_j, hcz_t1_h3_rd_unadj_j,
     hcz_t2_h1_rd_unadj_j, hcz_t2_h3_rd_unadj_j,
     file=here("primary/res_data/hcz_rd_unadj.RData"))

