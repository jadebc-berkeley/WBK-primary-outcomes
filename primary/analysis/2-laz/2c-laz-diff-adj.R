##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# LAZ adjusted analysis
# calculate unadjusted differences
# between treatment arms for H1 and H3

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: laz_PR_adj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

m=read.csv(here("primary/data/washb-kenya-midline-anthro-public.csv"))
e=read.csv(here("primary/data/washb-kenya-endline-anthro-public.csv"))

m=preprocess.adj(m,y="haz",time=1)
e=preprocess.adj(e,y="haz",time=2)

W=c("month","HHS","aged","sex","mother_age","motherht","mother_edu",
    "u18","Ncomp","water_time","floor","roof","cow",
    "goat","chicken","dog","elec","radio","tv","mobilephone",
    "clock","bicycle","motorcycle","stove","staffid")

mW=m[,c("haz","block","tr","clusterid",W)]
mW$block=as.factor(mW$block)

eW=e[,c("haz","block","tr","clusterid",W)]
eW$block=as.factor(eW$block)

######################################################
# H1: Adjusted prevalence ratios; each arm vs. control
######################################################

#---------------------------------------------
# Midline
#---------------------------------------------
# Midline
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.h1.t1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=mW$haz,tr=mW$tr,pair=mW$block,
     id=mW$block,W=mW[,W],family="gaussian",contrast=c("Control",x),
     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, 
     seed=67890, print=TRUE))

laz_t1_h1_rd_adj_j=format.tmle(est.h1.t1,family="gaussian")$rd

rownames(laz_t1_h1_rd_adj_j)=c("Passive Control vs C","Water vs C",
  "Sanitation vs C","Handwashing vs C","WSH vs C","Nutrition vs C",
  "Nutrition + WSH vs C")

# Endline
est.h1.t2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=eW$haz,tr=eW$tr,pair=eW$block,
     id=eW$block,W=eW[,W],family="gaussian",contrast=c("Control",x),
     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, 
     seed=67890, print=TRUE))

laz_t2_h1_rd_adj_j=format.tmle(est.h1.t2,family="gaussian")$rd

rownames(laz_t2_h1_rd_adj_j)=c("Passive Control vs C","Water vs C",
  "Sanitation vs C","Handwashing vs C","WSH vs C","Nutrition vs C",
  "Nutrition + WSH vs C")



######################################################
# H3: Adjusted prevalence ratios;
# WSHN vs. N and WSHN vs. WSH
######################################################

# Midline
trlist=c("Nutrition","WSH")
est.h3.t1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=mW$haz,tr=mW$tr,pair=mW$block,
     id=mW$block,W=mW[,W],family="gaussian",contrast=c(x,"Nutrition + WSH"),
     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, 
     seed=67890, print=TRUE))

laz_t1_h3_rd_adj_j=format.tmle(est.h3.t1,family="gaussian")$rd

rownames(laz_t1_h3_rd_adj_j)=c("Nutrition + WSH vs Nutrition", 
                               "Nutrition + WSH vs WSH")

# Endline
est.h3.t2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=eW$haz,tr=eW$tr,pair=eW$block,
     id=eW$block,W=eW[,W],family="gaussian",contrast=c(x,"Nutrition + WSH"),
     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, 
     seed=67890, print=TRUE))

laz_t2_h3_rd_adj_j=format.tmle(est.h3.t2,family="gaussian")$rd

rownames(laz_t2_h3_rd_adj_j)=c("Nutrition + WSH vs Nutrition", 
                               "Nutrition + WSH vs WSH")



######################################################
# Save results
######################################################
laz_t1_h1_rd_adj_j
laz_t1_h3_rd_adj_j
laz_t2_h1_rd_adj_j
laz_t2_h3_rd_adj_j

save(laz_t1_h1_rd_adj_j,laz_t1_h3_rd_adj_j,
  laz_t2_h1_rd_adj_j,laz_t2_h3_rd_adj_j,
  file=here("primary/res_data/laz-PR-adj.RData"))


