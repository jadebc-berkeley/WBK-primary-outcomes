##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Diarrhea adjusted analysis
# Pooled midline and endline 

# H1: Adjusted prevalence ratios; each arm vs. control
# H2: Adjusted prevalence ratios; combined WSH vs. single arms

# input: diarrhea.csv
# output: diarr-PR-adj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
library(devtools)
library(washb)

rm(list=ls())

# define directories
source.dir="~/documents/crg/wash-benefits/kenya/src/primary/analysis/"
data.dir="~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/Public/"
res.dir="~/Dropbox/WBK-primary-analysis/results/jade/"

data=read.csv(paste0(data.dir,"washb-kenya-diar-public.csv"),stringsAsFactors=TRUE)

source(paste0(source.dir,"0-base-programs.R"))

d=preprocess.diarr(data)

d=preprocess.adj(d,y="diarr7")

W=c("month","HHS","aged","sex","mother_age","motherht","mother_edu",
    "u18","Ncomp","water_time","floor","roof","cow",
    "goat","chicken","dog","elec","radio","tv","mobilephone",
    "clock","bicycle","motorcycle","stove","staffid")

dW=d[,c("diarr7","block","tr","clusterid",W)]
dW$block=as.factor(dW$block)

######################################################
# H1: Adjusted prevalence ratios; each arm vs. control
# fit with Poisson if log binomial fails to converge
######################################################
trlist=c("Passive Control","Water","Sanitation","Handwashing",
         "WSH","Nutrition","Nutrition + WSH")

SL.library=c("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

est.h1=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$diarr7,tr=dW$tr,
    pair=dW$block, id=dW$block,W=dW[,W],
    family="binomial",contrast=c("Control",x),Q.SL.library=SL.library,
    g.SL.library=SL.library, pval=0.2, seed=67890, print=TRUE))
  
diar_h1_pr_adj_j=format.tmle(est.h1,family="binomial")$rr
diar_h1_rd_adj_j=format.tmle(est.h1,family="binomial")$rd

rownames(diar_h1_pr_adj_j)=c("Passive Control vs. C", "Water vs. C",
                             "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
                             "Nutrition vs. C", "Nutrition + WSH vs. C")
rownames(diar_h1_rd_adj_j)=c("Passive Control vs. C", "Water vs. C",
                             "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
                             "Nutrition vs. C", "Nutrition + WSH vs. C")

######################################################
# H2: Adjusted prevalence ratios;
# Combined vs single
######################################################
trlist=c("Water","Sanitation","Handwashing")

est.h2=apply(matrix(trlist), 1,function(x) washb_tmle(Y=dW$diarr7,tr=dW$tr,
     pair=dW$block, id=dW$block,W=dW[,W],family="binomial",contrast=c(x,"WSH"),
     Q.SL.library=SL.library,g.SL.library=SL.library, pval=0.2, seed=67890, 
     print=TRUE))

diar_h2_pr_adj_j=format.tmle(est.h2,family="binomial")$rr
diar_h2_rd_adj_j=format.tmle(est.h2,family="binomial")$rd

rownames(diar_h2_pr_adj_j)=c("WSH vs. Water","WSH vs. Sanitation","WSH vs. Handwashing")
rownames(diar_h2_rd_adj_j)=c("WSH vs. Water","WSH vs. Sanitation","WSH vs. Handwashing")

######################################################
# Save results
######################################################
diar_h1_pr_adj_j
diar_h2_pr_adj_j
diar_h1_rd_adj_j
diar_h2_rd_adj_j

save(diar_h1_pr_adj_j,diar_h2_pr_adj_j, diar_h1_rd_adj_j, diar_h2_rd_adj_j,
     file=paste0(res.dir,"diarr-PR-adj.RData"))


