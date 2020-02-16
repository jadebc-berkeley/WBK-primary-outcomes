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
source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

d=read.csv(here("primary/data/washb-kenya-mortality-public.csv"))

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

mort_h1_pr_unadj_j
mort_h2_pr_unadj_j
mort_h1_rd_unadj_j
mort_h2_rd_unadj_j

save(mort_h1_pr_unadj_j, mort_h2_pr_unadj_j,
     file=here("primary/res_data/mort_pr_unadj.RData"))

save(mort_h1_rd_unadj_j, mort_h2_rd_unadj_j,
     file=here("primary/res_data/mort_rd_unadj.RData"))
