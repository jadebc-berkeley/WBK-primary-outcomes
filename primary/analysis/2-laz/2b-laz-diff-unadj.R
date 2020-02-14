##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# LAZ unadjusted analysis
# calculate unadjusted differences
# between treatment arms for H1 and H3

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: laz_rd_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

m=read.csv(here("primary/data/washb-kenya-midline-anthro-public.csv"))
e=read.csv(here("primary/data/washb-kenya-endline-anthro-public.csv"))

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

laz_t1_h1_rd_unadj_j
laz_t2_h1_rd_unadj_j
laz_t1_h3_rd_unadj_j
laz_t2_h3_rd_unadj_j


save(laz_t1_h1_rd_unadj_j, laz_t1_h3_rd_unadj_j,
     laz_t2_h1_rd_unadj_j, laz_t2_h3_rd_unadj_j,
     file=here("primary/res_data/laz_rd_unadj.RData"))

