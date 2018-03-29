##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Diarrhea unadjusted analysis
# Pooled midline and endline 

# Permutation tests: non-parametric Wilcoxon signed rank test statistic
# 2-sided p-values

# Note: this code does not use the washb R package
# However, the results are replicated with the washb_permute function

# input: diarrhea.csv
# output: diarr_pval_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################

rm(list=ls())
library(reshape2)
library(coin)

# define directories
source.dir="~/documents/crg/wash-benefits/kenya/src/primary/analysis/"
data.dir="~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/Public/"
res.dir="~/Dropbox/WBK-primary-analysis/results/jade/"

data=read.csv(paste0(data.dir,"washb-kenya-diar-public.csv"),stringsAsFactors=TRUE)
source(paste0(source.dir,"0-base-programs.R"))

d=preprocess.diarr(data)

#----------------------------------------------
# H1: P-value from permutation test for unadjusted 
# differences; each arm vs. control
#----------------------------------------------
P.df=coin.prep(d,tx="Passive Control",cont="Control",y="diarr7")
set.seed(67890)
P.permtest=wilcoxsign_test(diarr7~tr | block, data=P.df,
       distribution=approximate(B=100000))
P.perm.p=pvalue(P.permtest)

W.df=coin.prep(d,tx="Water",cont="Control",y="diarr7")
set.seed(67890)
W.permtest=wilcoxsign_test(diarr7~tr | block, data=W.df,
   distribution=approximate(B=100000))
W.perm.p=pvalue(W.permtest)

WSH.S.df=coin.prep(d,tx="Sanitation",cont="Control",y="diarr7")
set.seed(67890)
WSH.S.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.S.df,
   distribution=approximate(B=100000))
WSH.S.perm.p=pvalue(WSH.S.permtest)

H.df=coin.prep(d,tx="Handwashing",cont="Control",y="diarr7")
set.seed(67890)
H.permtest=wilcoxsign_test(diarr7~tr | block, data=H.df,
   distribution=approximate(B=100000))
H.perm.p=pvalue(H.permtest)

WSH.df=coin.prep(d,tx="WSH",cont="Control",y="diarr7")
set.seed(67890)
WSH.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.df,
   distribution=approximate(B=100000))
WSH.perm.p=pvalue(WSH.permtest)

N.df=coin.prep(d,tx="Nutrition",cont="Control",y="diarr7")
set.seed(67890)
N.permtest=wilcoxsign_test(diarr7~tr | block, data=N.df,
   distribution=approximate(B=100000))
N.perm.p=pvalue(N.permtest)

WSHN.df=coin.prep(d,tx="Nutrition + WSH",cont="Control",y="diarr7")
set.seed(67890)
WSHN.permtest=wilcoxsign_test(diarr7~tr | block, data=WSHN.df,
   distribution=approximate(B=100000))
WSHN.perm.p=pvalue(WSHN.permtest)

diar_h1_pval_unadj_j=data.frame(perm.pvalue=c(P.perm.p,W.perm.p,WSH.S.perm.p,H.perm.p,
                         WSH.perm.p,N.perm.p,WSHN.perm.p))
rownames(diar_h1_pval_unadj_j)=c("Passive Control vs. C","Water vs. C",
   "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
   "Nutrition vs. C", "Nutrition + WSH vs. C")

#----------------------------------------------
# H2: P-value from permutation test for unadjusted 
# differences; WSHN vs. single arms
#----------------------------------------------
WSH.W.df=coin.prep(d,tx="WSH",cont="Water",y="diarr7")
set.seed(67890)
WSH.W.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.W.df,
   distribution=approximate(B=100000))
WSH.W.perm.p=pvalue(WSH.W.permtest)

WSH.S.df=coin.prep(d,tx="WSH",cont="Sanitation",y="diarr7")
set.seed(67890)
WSH.S.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.S.df,
   distribution=approximate(B=100000))
WSH.S.perm.p=pvalue(WSH.S.permtest)

WSH.H.df=coin.prep(d,tx="WSH",cont="Handwashing",y="diarr7")
set.seed(67890)
WSH.H.permtest=wilcoxsign_test(diarr7~tr | block, data=WSH.H.df,
    distribution=approximate(B=100000))
WSH.H.perm.p=pvalue(WSH.H.permtest)

diar_h2_pval_unadj_j=data.frame(perm.pvalue=c(WSH.W.perm.p,WSH.S.perm.p,WSH.H.perm.p))
rownames(diar_h2_pval_unadj_j)=c("WSH vs. Water",
                                 "WSH vs. Sanitation", "WSH vs. Handwashing")

save(diar_h1_pval_unadj_j,diar_h2_pval_unadj_j,
     file=paste0(res.dir,"diarr_pval_unadj.RData"))


diar_h1_pval_unadj_j
diar_h2_pval_unadj_j


