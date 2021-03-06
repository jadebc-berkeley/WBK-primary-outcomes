##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# whz unadjusted analysis
# Permutation tests: non-parametric Wilcoxon signed rank test statistic
# 2-sided p-values

# Note: this code does not use the washb R package
# However, the results are replicated with the washb_permute function

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: whz_t1_pval_unadj.RData, whz_t2_pval_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################

library(coin)
library(reshape2)

rm(list=ls())

# define directories
source.dir="~/documents/crg/wash-benefits/kenya/src/primary/analysis/"
data.dir="~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/Public/"
res.dir="~/Dropbox/WBK-primary-analysis/results/jade/"

m=read.csv(paste0(data.dir,"washb-kenya-midline-anthro-public.csv"))
e=read.csv(paste0(data.dir,"washb-kenya-endline-anthro-public.csv"))

source(paste0(source.dir,"0-base-programs.R"))

m=preprocess.anthro(m, "whz")
e=preprocess.anthro(e, "whz")

#----------------------------------------------
# H1: P-value from permutation test for unadjusted 
# differences; each arm vs. control
#----------------------------------------------
# midline
P.t1.df=coin.prep(m,tx="Passive Control",cont="Control",y="whz")
set.seed(67890)
P.t1.permtest=wilcoxsign_test(whz~tr | block, data=P.t1.df,
      distribution=approximate(B=100000))
P.t1.perm.p=pvalue(P.t1.permtest)

W.t1.df=coin.prep(m,tx="Water",cont="Control",y="whz")
set.seed(67890)
W.t1.permtest=wilcoxsign_test(whz~tr | block, data=W.t1.df,
      distribution=approximate(B=100000))
W.t1.perm.p=pvalue(W.t1.permtest)

WSH.S.t1.df=coin.prep(m,tx="Sanitation",cont="Control",y="whz")
set.seed(67890)
WSH.S.t1.permtest=wilcoxsign_test(whz~tr | block, data=WSH.S.t1.df,
      distribution=approximate(B=100000))
WSH.S.t1.perm.p=pvalue(WSH.S.t1.permtest)

H.t1.df=coin.prep(m,tx="Handwashing",cont="Control",y="whz")
set.seed(67890)
H.t1.permtest=wilcoxsign_test(whz~tr | block, data=H.t1.df,
      distribution=approximate(B=100000))
H.t1.perm.p=pvalue(H.t1.permtest)

WSH.t1.df=coin.prep(m,tx="WSH",cont="Control",y="whz")
set.seed(67890)
WSH.t1.permtest=wilcoxsign_test(whz~tr | block, data=WSH.t1.df,
      distribution=approximate(B=100000))
WSH.t1.perm.p=pvalue(WSH.t1.permtest)

N.t1.df=coin.prep(m,tx="Nutrition",cont="Control",y="whz")
set.seed(67890)
N.permtest=wilcoxsign_test(whz~tr | block, data=N.t1.df,
      distribution=approximate(B=100000))
N.t1.perm.p=pvalue(N.permtest)

WSHN.t1.df=coin.prep(m,tx="Nutrition + WSH",cont="Control",y="whz")
set.seed(67890)
WSHN.t1.permtest=wilcoxsign_test(whz~tr | block, data=WSHN.t1.df,
      distribution=approximate(B=100000))
WSHN.t1.perm.p=pvalue(WSHN.t1.permtest)

whz_t1_h1_pval_unadj_j=data.frame(perm.pvalue=c(P.t1.perm.p,W.t1.perm.p,WSH.S.t1.perm.p,
        H.t1.perm.p,WSH.t1.perm.p,N.t1.perm.p,WSHN.t1.perm.p))
rownames(whz_t1_h1_pval_unadj_j)=c("Passive Control vs. C","Water vs. C",
    "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
    "Nutrition vs. C", "Nutrition + WSH vs. C")

# endline
P.t2.df=coin.prep(e,tx="Passive Control",cont="Control",y="whz")
set.seed(67890)
P.t2.permtest=wilcoxsign_test(whz~tr | block, data=P.t2.df,
       distribution=approximate(B=100000))
P.t2.perm.p=pvalue(P.t2.permtest)

W.t2.df=coin.prep(e,tx="Water",cont="Control",y="whz")
set.seed(67890)
W.t2.permtest=wilcoxsign_test(whz~tr | block, data=W.t2.df,
      distribution=approximate(B=100000))
W.t2.perm.p=pvalue(W.t2.permtest)

WSH.S.t2.df=coin.prep(e,tx="Sanitation",cont="Control",y="whz")
set.seed(67890)
WSH.S.t2.permtest=wilcoxsign_test(whz~tr | block, data=WSH.S.t2.df,
      distribution=approximate(B=100000))
WSH.S.t2.perm.p=pvalue(WSH.S.t2.permtest)

H.t2.df=coin.prep(e,tx="Handwashing",cont="Control",y="whz")
set.seed(67890)
H.t2.permtest=wilcoxsign_test(whz~tr | block, data=H.t2.df,
      distribution=approximate(B=100000))
H.t2.perm.p=pvalue(H.t2.permtest)

WSH.t2.df=coin.prep(e,tx="WSH",cont="Control",y="whz")
set.seed(67890)
WSH.t2.permtest=wilcoxsign_test(whz~tr | block, data=WSH.t2.df,
      distribution=approximate(B=100000))
WSH.t2.perm.p=pvalue(WSH.t2.permtest)

N.t2.df=coin.prep(e,tx="Nutrition",cont="Control",y="whz")
set.seed(67890)
N.t2.permtest=wilcoxsign_test(whz~tr | block, data=N.t2.df,
      distribution=approximate(B=100000))
N.t2.perm.p=pvalue(N.t2.permtest)

WSHN.t2.df=coin.prep(e,tx="Nutrition + WSH",cont="Control",y="whz")
set.seed(67890)
WSHN.t2.permtest=wilcoxsign_test(whz~tr | block, data=WSHN.t2.df,
      distribution=approximate(B=100000))
WSHN.t2.perm.p=pvalue(WSHN.t2.permtest)

whz_t2_h1_pval_unadj_j=data.frame(perm.pvalue=c(P.t2.perm.p,W.t2.perm.p,WSH.S.t2.perm.p,H.t2.perm.p,
        WSH.t2.perm.p,N.t2.perm.p,WSHN.t2.perm.p))
rownames(whz_t2_h1_pval_unadj_j)=c("Passive Control vs. C","Water vs. C",
     "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
     "Nutrition vs. C", "Nutrition + WSH vs. C")

#----------------------------------------------
# H3: P-value from permutation test for unadjusted 
# differences; WSHN vs. N and WSHN vs. WSH
#----------------------------------------------
# midline
WSHN.N.t1.df=coin.prep(m,tx="Nutrition + WSH",cont="Nutrition",y="whz")
set.seed(67890)
WSHN.N.t1.permtest=wilcoxsign_test(whz~tr | block, data=WSHN.N.t1.df,
      distribution=approximate(B=100000))
WSHN.N.t1.perm.p=pvalue(WSHN.N.t1.permtest)

WSHN.WSH.t1.df=coin.prep(m,tx="Nutrition + WSH",cont="WSH",y="whz")
set.seed(67890)
WSHN.WSH.t1.permtest=wilcoxsign_test(whz~tr | block, data=WSHN.WSH.t1.df,
      distribution=approximate(B=100000))
WSHN.WSH.t1.perm.p=pvalue(WSHN.WSH.t1.permtest)

whz_t1_h3_pval_unadj_j=data.frame(perm.pvalue=c(WSHN.N.t1.perm.p,WSHN.WSH.t1.perm.p))
rownames(whz_t1_h3_pval_unadj_j)=c("Nutrition + WSH vs. Nutrition",
                                   "Nutrition + WSH vs. WSH")


# endline
WSHN.N.t2.df=coin.prep(e,tx="Nutrition + WSH",cont="Nutrition",y="whz")
set.seed(67890)
WSHN.N.t2.permtest=wilcoxsign_test(whz~tr | block, data=WSHN.N.t2.df,
      distribution=approximate(B=100000))
WSHN.N.t2.perm.p=pvalue(WSHN.N.t2.permtest)

WSHN.WSH.t2.df=coin.prep(e,tx="Nutrition + WSH",cont="WSH",y="whz")
set.seed(67890)
WSHN.WSH.t2.permtest=wilcoxsign_test(whz~tr | block, data=WSHN.WSH.t2.df,
      distribution=approximate(B=100000))
WSHN.WSH.t2.perm.p=pvalue(WSHN.WSH.t2.permtest)

whz_t2_h3_pval_unadj_j=data.frame(perm.pvalue=c(WSHN.N.t2.perm.p,WSHN.WSH.t2.perm.p))
rownames(whz_t2_h3_pval_unadj_j)=c("Nutrition + WSH vs. Nutrition","Nutrition + WSH vs. WSH")


save(whz_t1_h1_pval_unadj_j,whz_t1_h3_pval_unadj_j,
     file=paste0(res.dir,"whz_t1_pval_unadj.RData"))

save(whz_t2_h1_pval_unadj_j,whz_t2_h3_pval_unadj_j,
     file=paste0(res.dir,"whz_t2_pval_unadj.RData"))

whz_t1_h1_pval_unadj_j
whz_t1_h3_pval_unadj_j
whz_t2_h1_pval_unadj_j
whz_t2_h3_pval_unadj_j


