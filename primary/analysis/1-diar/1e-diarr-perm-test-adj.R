##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Diarrhea adjusted analysis
# Pooled midline and endline 

# Permutation tests: non-parametric Wilcoxon signed rank test statistic
# 2-sided p-values

# Note: this code does not use the washb R package
# However, the results are replicated with the washb_permute function

# input: washb-kenya-diar-public.csv
# output: diarr_pval_adj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################












data=read.csv(here("primary/data/washb-kenya-diar-public.csv"),stringsAsFactors=TRUE)
source(here("primary/analysis/0-base-programs.R"))

d=preprocess.diarr(data)

d=preprocess.adj(d,y="diarr7")

# --------------------------------------
# select covariates that are associated with the 
# outcome
# --------------------------------------
W=c("month","HHS","aged","sex","mother_age","motherht","mother_edu",
    "u18","Ncomp","water_time","floor","roof","cow",
    "goat","chicken","dog","elec","radio","tv","mobilephone",
    "clock","bicycle","motorcycle","stove","staffid")

W_screen=washb_prescreen(Y=d$diarr7, Ws=d[,W], family="binomial")

# subset data frame to Ws selected in prescreening
dW=d[,c("block","diarr7","clusterid","tr",W_screen)]

# ensure relevant covariates are defined as factors, create indicators
dW$block=as.factor(dW$block)

# subset to complete cases
dW=dW[complete.cases(dW),]

# --------------------------------------
# run SuperLearner
# --------------------------------------

# define SuperLearner libraries
SL.library<-list("SL.mean","SL.glm","SL.bayesglm","SL.gam","SL.glmnet")

# for adjusted permutation test, fit model of outcome
# as a function of covariates excluding treatment assignment
# define outcome for SuperLearner

sl.d=sl.prep(dW,y="diarr7")
colnames(sl.d$A)

set.seed(67890)
# fit model
sl.fit=SuperLearner(Y=sl.d$Y, X=sl.d$A, SL.library=SL.library,id=sl.d$clusterid,
    family="binomial",method="method.NNLS")

sl.fit

# generate residuals
resid=sl.d$Y - sl.fit$SL.predict

# save predicted values and treatment assignments in data frame
resid.df=data.frame(block=dW$block,tr=dW$tr,diarr7=resid)

#----------------------------------------------
# H1: P-value from permutation test for adjusted 
# differences; each arm vs. control
#----------------------------------------------
P.df=coin.prep(resid.df,tx="Passive Control",cont="Control",y="diarr7")
set.seed(67890)
P.permtest=wilcoxsign_test(diarr7~tr | block, data=P.df,
    distribution=approximate(B=100000))
P.perm.p=coin::pvalue(P.permtest)

W.df=coin.prep(resid.df,tx="Water",cont="Control",y="diarr7")
set.seed(67890)
W.permtest=wilcoxsign_test(diarr7~tr | block, data=W.df,
   distribution=approximate(B=100000))
W.perm.p=coin::pvalue(W.permtest)

washb_permute(Y=resid.df$diarr7,tr=resid.df$tr,contrast=c("Control","Water"),
              pair=resid.df$block,nreps=100000,seed=67890)

S.df=coin.prep(resid.df,tx="Sanitation",cont="Control",y="diarr7")
set.seed(67890)
S.permtest=wilcoxsign_test(diarr7~tr | block, data=S.df,
   distribution=approximate(B=100000))
S.perm.p=coin::pvalue(S.permtest)

H.df=coin.prep(resid.df,tx="Handwashing",cont="Control",y="diarr7")
set.seed(67890)
H.permtest=wilcoxsign_test(diarr7~tr | block, data=H.df,
   distribution=approximate(B=100000))
H.perm.p=coin::pvalue(H.permtest)

WSH.df=coin.prep(resid.df,tx="WSH",cont="Control",y="diarr7")
set.seed(67890)
WSH.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.df,
   distribution=approximate(B=100000))
WSH.perm.p=coin::pvalue(WSH.permtest)

N.df=coin.prep(resid.df,tx="Nutrition",cont="Control",y="diarr7")
set.seed(67890)
N.permtest=wilcoxsign_test(diarr7~tr | block, data=N.df,
   distribution=approximate(B=100000))
N.perm.p=coin::pvalue(N.permtest)

WSHN.df=coin.prep(resid.df,tx="Nutrition + WSH",cont="Control",y="diarr7")
set.seed(67890)
WSHN.permtest=wilcoxsign_test(diarr7~tr | block, data=WSHN.df,
   distribution=approximate(B=100000))
WSHN.perm.p=coin::pvalue(WSHN.permtest)

diar_h1_pval_adj_j=data.frame(perm.pvalue=c(P.perm.p,W.perm.p,S.perm.p,H.perm.p,
                         WSH.perm.p,N.perm.p,WSHN.perm.p))
rownames(diar_h1_pval_adj_j)=c("Passive Control vs. C","Water vs. C",
   "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
   "Nutrition vs. C", "Nutrition + WSH vs. C")

#----------------------------------------------
# H2: P-value from permutation test for adjusted 
# differences; WSHN vs. single arms
#----------------------------------------------
WSH.W.df=coin.prep(resid.df,tx="WSH",cont="Water",y="diarr7")
set.seed(67890)
WSH.W.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.W.df,
   distribution=approximate(B=100000))
WSH.W.perm.p=coin::pvalue(WSH.W.permtest)

WSH.S.df=coin.prep(resid.df,tx="WSH",cont="Sanitation",y="diarr7")
set.seed(67890)
WSH.S.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.S.df,
   distribution=approximate(B=100000))
WSH.S.perm.p=coin::pvalue(WSH.S.permtest)

WSH.H.df=coin.prep(resid.df,tx="WSH",cont="Handwashing",y="diarr7")
set.seed(67890)
WSH.H.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.H.df,
    distribution=approximate(B=100000))
WSH.H.perm.p=coin::pvalue(WSH.H.permtest)

diar_h2_pval_adj_j=data.frame(perm.pvalue=c(WSH.W.perm.p,WSH.S.perm.p,WSH.H.perm.p))
rownames(diar_h2_pval_adj_j)=c("WSH vs. Water",
                                 "WSH vs. Sanitation", "WSH vs. Handwashing")

save(diar_h1_pval_adj_j,diar_h2_pval_adj_j,
     file=here("primary/res_data/diarr_pval_adj.RData"))


diar_h1_pval_adj_j
diar_h2_pval_adj_j



