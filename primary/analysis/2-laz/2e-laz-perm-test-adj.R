##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# LAZ adjusted analysis
# Permutation tests: non-parametric Wilcoxon signed rank 
# test statistic
# 2-sided p-values

# Note: this code does not use the washb R package
# However, the results are replicated with the washb_permute function

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: laz_t1_pval_adj.RData, laz_t2_pval_adj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################

library(SuperLearner)
library(reshape2)
library(coin)
library(washb)

rm(list=ls())

# define directories
source.dir="~/documents/crg/wash-benefits/kenya/src/primary/analysis/"
data.dir="~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/Public/"
res.dir="~/Dropbox/WBK-primary-analysis/results/jade/"

m=read.csv(paste0(data.dir,"washb-kenya-midline-anthro-public.csv"))
e=read.csv(paste0(data.dir,"washb-kenya-endline-anthro-public.csv"))

source(paste0(source.dir,"0-base-programs.R"))

m1=preprocess.anthro(m, y="haz")
e1=preprocess.anthro(e, y="haz")

m2=preprocess.adj(m1,y="haz",time=1)
e2=preprocess.adj(e1,y="haz",time=2)

# --------------------------------------
# select covariates that are associated with the 
# outcome
# --------------------------------------
W=c("month","HHS","aged","sex","mother_age","motherht","mother_edu",
    "u18","Ncomp","water_time","floor","roof","cow",
    "goat","chicken","dog","elec","radio","tv","mobilephone",
    "clock","bicycle","motorcycle","stove","staffid")

W_screen_m=washb_prescreen(Y=m2$haz, Ws=m2[,W], family="gaussian")
W_screen_e=washb_prescreen(Y=e2$haz, Ws=e2[,W], family="gaussian")

# subset data frame to Ws selected in prescreening
mW=m2[,c("block","haz","clusterid","tr",W_screen_m)]
eW=e2[,c("block","haz","clusterid","tr",W_screen_e)]

# ensure relevant covariates are defined as factors, create indicators
mW$block=as.factor(mW$block)
eW$block=as.factor(eW$block)
mW$month=as.factor(mW$month)
eW$month=as.factor(eW$month)

# subset to complete cases
mW=mW[complete.cases(mW),]
eW=eW[complete.cases(eW),]

#-------------------------------------------
# run SuperLearner
#-------------------------------------------
# define SuperLearner libraries
SL.library<-list("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")
# for adjusted permutation test, fit model of outcome
# as a function of covariates excluding treatment assignment

sl.m=sl.prep(mW,y="haz")
sl.e=sl.prep(eW,y="haz")

colnames(sl.m$A)
colnames(sl.e$A)


# fit model
set.seed(67890)
sl.m.fit=SuperLearner(Y=sl.m$Y, X=sl.m$A, SL.library=SL.library,
     id=sl.m$clusterid,family="gaussian",method="method.NNLS")
sl.m.fit

set.seed(67890)
sl.e.fit=SuperLearner(Y=sl.e$Y, X=sl.e$A, SL.library=SL.library,
     id=sl.e$clusterid,family="gaussian",method="method.NNLS")
sl.e.fit

# generate residuals
resid.m=sl.m$Y - sl.m.fit$SL.predict
resid.e=sl.e$Y - sl.e.fit$SL.predict

# save predicted values and treatment assignments in data frame
resid.df.m=data.frame(block=mW$block,tr=mW$tr,haz=resid.m)
resid.df.e=data.frame(block=eW$block,tr=eW$tr,haz=resid.e)

#----------------------------------------------
# H1: P-value from permutation test for adjusted 
# differences; each arm vs. control
#----------------------------------------------
# midline
P.t1.df=coin.prep(resid.df.m,tx="Passive Control",cont="Control",y="haz")
nrow(P.t1.df)
set.seed(67890)
P.t1.permtest=wilcoxsign_test(haz~tr | block, data=P.t1.df,
    distribution=approximate(B=100000))
P.t1.perm.p=pvalue(P.t1.permtest)

W.t1.df=coin.prep(resid.df.m,tx="Water",cont="Control",y="haz")
nrow(W.t1.df)
set.seed(67890)
W.t1.permtest=wilcoxsign_test(haz~tr | block, data=W.t1.df,
      distribution=approximate(B=100000))
W.t1.perm.p=pvalue(W.t1.permtest)

S.t1.df=coin.prep(resid.df.m,tx="Sanitation",cont="Control",y="haz")
nrow(S.t1.df)
set.seed(67890)
S.t1.permtest=wilcoxsign_test(haz~tr | block, data=S.t1.df,
      distribution=approximate(B=100000))
S.t1.perm.p=pvalue(S.t1.permtest)

H.t1.df=coin.prep(resid.df.m,tx="Handwashing",cont="Control",y="haz")
nrow(H.t1.df)
set.seed(67890)
H.t1.permtest=wilcoxsign_test(haz~tr | block, data=H.t1.df,
      distribution=approximate(B=100000))
H.t1.perm.p=pvalue(H.t1.permtest)

WSH.t1.df=coin.prep(resid.df.m,tx="WSH",cont="Control",y="haz")
nrow(WSH.t1.df)
set.seed(67890)
WSH.t1.permtest=wilcoxsign_test(haz~tr | block, data=WSH.t1.df,
      distribution=approximate(B=100000))
WSH.t1.perm.p=pvalue(WSH.t1.permtest)

N.t1.df=coin.prep(resid.df.m,tx="Nutrition",cont="Control",y="haz")
nrow(N.t1.df)
set.seed(67890)
N.permtest=wilcoxsign_test(haz~tr | block, data=N.t1.df,
      distribution=approximate(B=100000))
N.t1.perm.p=pvalue(N.permtest)

WSHN.t1.df=coin.prep(resid.df.m,tx="Nutrition + WSH",cont="Control",y="haz")
nrow(WSHN.t1.df)
set.seed(67890)
WSHN.t1.permtest=wilcoxsign_test(haz~tr | block, data=WSHN.t1.df,
      distribution=approximate(B=100000))
WSHN.t1.perm.p=pvalue(WSHN.t1.permtest)

laz_t1_h1_pval_adj_j=data.frame(perm.pvalue=c(P.t1.perm.p,W.t1.perm.p,S.t1.perm.p,
  H.t1.perm.p,WSH.t1.perm.p,N.t1.perm.p,WSHN.t1.perm.p))
rownames(laz_t1_h1_pval_adj_j)=c("Passive Control vs. C","Water vs. C",
    "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
    "Nutrition vs. C", "Nutrition + WSH vs. C")


# endline
P.t2.df=coin.prep(resid.df.e,tx="Passive Control",cont="Control",y="haz")
nrow(P.t2.df)
set.seed(67890)
P.t2.permtest=wilcoxsign_test(haz~tr | block, data=P.t2.df,
      distribution=approximate(B=100000))
P.t2.perm.p=pvalue(P.t2.permtest)

W.t2.df=coin.prep(resid.df.e,tx="Water",cont="Control",y="haz")
nrow(W.t2.df)
set.seed(67890)
W.t2.permtest=wilcoxsign_test(haz~tr | block, data=W.t2.df,
      distribution=approximate(B=100000))
W.t2.perm.p=pvalue(W.t2.permtest)

S.t2.df=coin.prep(resid.df.e,tx="Sanitation",cont="Control",y="haz")
nrow(S.t2.df)
set.seed(67890)
S.t2.permtest=wilcoxsign_test(haz~tr | block, data=S.t2.df,
      distribution=approximate(B=100000))
S.t2.perm.p=pvalue(S.t2.permtest)

H.t2.df=coin.prep(resid.df.e,tx="Handwashing",cont="Control",y="haz")
nrow(H.t2.df)
set.seed(67890)
H.t2.permtest=wilcoxsign_test(haz~tr | block, data=H.t2.df,
      distribution=approximate(B=100000))
H.t2.perm.p=pvalue(H.t2.permtest)

WSH.t2.df=coin.prep(resid.df.e,tx="WSH",cont="Control",y="haz")
nrow(WSH.t2.df)
set.seed(67890)
WSH.t2.permtest=wilcoxsign_test(haz~tr | block, data=WSH.t2.df,
      distribution=approximate(B=100000))
WSH.t2.perm.p=pvalue(WSH.t2.permtest)

N.t2.df=coin.prep(resid.df.e,tx="Nutrition",cont="Control",y="haz")
nrow(N.t2.df)
set.seed(67890)
N.t2.permtest=wilcoxsign_test(haz~tr | block, data=N.t2.df,
      distribution=approximate(B=100000))
N.t2.perm.p=pvalue(N.t2.permtest)

WSHN.t2.df=coin.prep(resid.df.e,tx="Nutrition + WSH",cont="Control",y="haz")
nrow(WSHN.t2.df)
set.seed(67890)
WSHN.t2.permtest=wilcoxsign_test(haz~tr | block, data=WSHN.t2.df,
      distribution=approximate(B=100000))
WSHN.t2.perm.p=pvalue(WSHN.t2.permtest)

laz_t2_h1_pval_adj_j=data.frame(perm.pvalue=c(P.t2.perm.p,W.t2.perm.p,S.t2.perm.p,H.t2.perm.p,
        WSH.t2.perm.p,N.t2.perm.p,WSHN.t2.perm.p))
rownames(laz_t2_h1_pval_adj_j)=c("Passive Control vs. C","Water vs. C",
     "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
     "Nutrition vs. C", "Nutrition + WSH vs. C")

#----------------------------------------------
# H3: P-value from permutation test for adjusted 
# differences; WSHN vs. N and WSHN vs. WSH
#----------------------------------------------
# midline
WSHN.N.t1.df=coin.prep(resid.df.m,tx="Nutrition + WSH",cont="Nutrition",y="haz")
nrow(WSHN.N.t1.df)
set.seed(67890)
WSHN.N.t1.permtest=wilcoxsign_test(haz~tr | block, data=WSHN.N.t1.df,
      distribution=approximate(B=100000))
WSHN.N.t1.perm.p=pvalue(WSHN.N.t1.permtest)

WSHN.WSH.t1.df=coin.prep(resid.df.m,tx="Nutrition + WSH",cont="WSH",y="haz")
nrow(WSHN.WSH.t1.df)
set.seed(67890)
WSHN.WSH.t1.permtest=wilcoxsign_test(haz~tr | block, data=WSHN.WSH.t1.df,
      distribution=approximate(B=100000))
WSHN.WSH.t1.perm.p=pvalue(WSHN.WSH.t1.permtest)

laz_t1_h3_pval_adj_j=data.frame(perm.pvalue=c(WSHN.N.t1.perm.p,WSHN.WSH.t1.perm.p))
rownames(laz_t1_h3_pval_adj_j)=c("Nutrition + WSH vs. Nutrition","Nutrition + WSH vs. WSH")


# endline
WSHN.N.t2.df=coin.prep(resid.df.e,tx="Nutrition + WSH",cont="Nutrition",y="haz")
nrow(WSHN.N.t2.df)
set.seed(67890)
WSHN.N.t2.permtest=wilcoxsign_test(haz~tr | block, data=WSHN.N.t2.df,
      distribution=approximate(B=100000))
WSHN.N.t2.perm.p=pvalue(WSHN.N.t2.permtest)

WSHN.WSH.t2.df=coin.prep(resid.df.e,tx="Nutrition + WSH",cont="WSH",y="haz")
nrow(WSHN.WSH.t2.df)
set.seed(67890)
WSHN.WSH.t2.permtest=wilcoxsign_test(haz~tr | block, data=WSHN.WSH.t2.df,
      distribution=approximate(B=100000))
WSHN.WSH.t2.perm.p=pvalue(WSHN.WSH.t2.permtest)

laz_t2_h3_pval_adj_j=data.frame(perm.pvalue=c(WSHN.N.t2.perm.p,WSHN.WSH.t2.perm.p))
rownames(laz_t2_h3_pval_adj_j)=c("Nutrition + WSH vs. Nutrition","Nutrition + WSH vs. WSH")

save(laz_t1_h1_pval_adj_j,laz_t1_h3_pval_adj_j,
     file=paste0(res.dir,"laz_t1_pval_adj.RData"))

save(laz_t2_h1_pval_adj_j,laz_t2_h3_pval_adj_j,
     file=paste0(res.dir,"laz_t2_pval_adj.RData"))

laz_t1_h1_pval_adj_j
laz_t1_h3_pval_adj_j
laz_t2_h1_pval_adj_j
laz_t2_h3_pval_adj_j



