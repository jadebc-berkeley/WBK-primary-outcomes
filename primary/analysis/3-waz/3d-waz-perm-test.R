##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# waz unadjusted analysis
# Permutation tests: non-parametric Wilcoxon signed rank test statistic
# 2-sided p-values

# Note: this code does not use the washb R package
# However, the results are replicated with the washb_permute function

# input: washb-kenya-midline-anthro-public.csv,
# washb-kenya-endline-anthro-public.csv
# output: waz_t1_pval_unadj.RData, waz_t2_pval_unadj.RData

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

m=read.csv(here("primary/data/washb-kenya-midline-anthro-public.csv"))
e=read.csv(here("primary/data/washb-kenya-endline-anthro-public.csv"))

m=preprocess.anthro(m, "waz")
e=preprocess.anthro(e, "waz")

#----------------------------------------------
# H1: P-value from permutation test for unadjusted 
# differences; each arm vs. control
#----------------------------------------------
# midline
P.t1.df=coin.prep(m,tx="Passive Control",cont="Control",y="waz")
set.seed(67890)
P.t1.permtest=wilcoxsign_test(waz~tr | block, data=P.t1.df,
      distribution=approximate(nresample=100000))
P.t1.perm.p=coin::pvalue(P.t1.permtest)

W.t1.df=coin.prep(m,tx="Water",cont="Control",y="waz")
set.seed(67890)
W.t1.permtest=wilcoxsign_test(waz~tr | block, data=W.t1.df,
      distribution=approximate(nresample=100000))
W.t1.perm.p=coin::pvalue(W.t1.permtest)

WSH.S.t1.df=coin.prep(m,tx="Sanitation",cont="Control",y="waz")
set.seed(67890)
WSH.S.t1.permtest=wilcoxsign_test(waz~tr | block, data=WSH.S.t1.df,
      distribution=approximate(nresample=100000))
WSH.S.t1.perm.p=coin::pvalue(WSH.S.t1.permtest)

H.t1.df=coin.prep(m,tx="Handwashing",cont="Control",y="waz")
set.seed(67890)
H.t1.permtest=wilcoxsign_test(waz~tr | block, data=H.t1.df,
      distribution=approximate(nresample=100000))
H.t1.perm.p=coin::pvalue(H.t1.permtest)

WSH.t1.df=coin.prep(m,tx="WSH",cont="Control",y="waz")
set.seed(67890)
WSH.t1.permtest=wilcoxsign_test(waz~tr | block, data=WSH.t1.df,
      distribution=approximate(nresample=100000))
WSH.t1.perm.p=coin::pvalue(WSH.t1.permtest)

N.t1.df=coin.prep(m,tx="Nutrition",cont="Control",y="waz")
set.seed(67890)
N.permtest=wilcoxsign_test(waz~tr | block, data=N.t1.df,
      distribution=approximate(nresample=100000))
N.t1.perm.p=coin::pvalue(N.permtest)

WSHN.t1.df=coin.prep(m,tx="Nutrition + WSH",cont="Control",y="waz")
set.seed(67890)
WSHN.t1.permtest=wilcoxsign_test(waz~tr | block, data=WSHN.t1.df,
      distribution=approximate(nresample=100000))
WSHN.t1.perm.p=coin::pvalue(WSHN.t1.permtest)

waz_t1_h1_pval_unadj_j=data.frame(perm.pvalue=c(P.t1.perm.p,W.t1.perm.p,WSH.S.t1.perm.p,
        H.t1.perm.p,WSH.t1.perm.p,N.t1.perm.p,WSHN.t1.perm.p))
rownames(waz_t1_h1_pval_unadj_j)=c("Passive Control vs. C","Water vs. C",
    "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
    "Nutrition vs. C", "Nutrition + WSH vs. C")

# endline
P.t2.df=coin.prep(e,tx="Passive Control",cont="Control",y="waz")
set.seed(67890)
P.t2.permtest=wilcoxsign_test(waz~tr | block, data=P.t2.df,
       distribution=approximate(nresample=100000))
P.t2.perm.p=coin::pvalue(P.t2.permtest)

W.t2.df=coin.prep(e,tx="Water",cont="Control",y="waz")
set.seed(67890)
W.t2.permtest=wilcoxsign_test(waz~tr | block, data=W.t2.df,
      distribution=approximate(nresample=100000))
W.t2.perm.p=coin::pvalue(W.t2.permtest)

WSH.S.t2.df=coin.prep(e,tx="Sanitation",cont="Control",y="waz")
set.seed(67890)
WSH.S.t2.permtest=wilcoxsign_test(waz~tr | block, data=WSH.S.t2.df,
      distribution=approximate(nresample=100000))
WSH.S.t2.perm.p=coin::pvalue(WSH.S.t2.permtest)

H.t2.df=coin.prep(e,tx="Handwashing",cont="Control",y="waz")
set.seed(67890)
H.t2.permtest=wilcoxsign_test(waz~tr | block, data=H.t2.df,
      distribution=approximate(nresample=100000))
H.t2.perm.p=coin::pvalue(H.t2.permtest)

WSH.t2.df=coin.prep(e,tx="WSH",cont="Control",y="waz")
set.seed(67890)
WSH.t2.permtest=wilcoxsign_test(waz~tr | block, data=WSH.t2.df,
      distribution=approximate(nresample=100000))
WSH.t2.perm.p=coin::pvalue(WSH.t2.permtest)

N.t2.df=coin.prep(e,tx="Nutrition",cont="Control",y="waz")
set.seed(67890)
N.t2.permtest=wilcoxsign_test(waz~tr | block, data=N.t2.df,
      distribution=approximate(nresample=100000))
N.t2.perm.p=coin::pvalue(N.t2.permtest)

WSHN.t2.df=coin.prep(e,tx="Nutrition + WSH",cont="Control",y="waz")
set.seed(67890)
WSHN.t2.permtest=wilcoxsign_test(waz~tr | block, data=WSHN.t2.df,
      distribution=approximate(nresample=100000))
WSHN.t2.perm.p=coin::pvalue(WSHN.t2.permtest)

waz_t2_h1_pval_unadj_j=data.frame(perm.pvalue=c(P.t2.perm.p,W.t2.perm.p,WSH.S.t2.perm.p,H.t2.perm.p,
        WSH.t2.perm.p,N.t2.perm.p,WSHN.t2.perm.p))
rownames(waz_t2_h1_pval_unadj_j)=c("Passive Control vs. C","Water vs. C",
     "Sanitation vs. C", "Handwashing vs. C", "WSH vs. C",
     "Nutrition vs. C", "Nutrition + WSH vs. C")

#----------------------------------------------
# H3: P-value from permutation test for unadjusted 
# differences; WSHN vs. N and WSHN vs. WSH
#----------------------------------------------
# midline
WSHN.N.t1.df=coin.prep(m,tx="Nutrition + WSH",cont="Nutrition",y="waz")
set.seed(67890)
WSHN.N.t1.permtest=wilcoxsign_test(waz~tr | block, data=WSHN.N.t1.df,
      distribution=approximate(nresample=100000))
WSHN.N.t1.perm.p=coin::pvalue(WSHN.N.t1.permtest)

WSHN.WSH.t1.df=coin.prep(m,tx="Nutrition + WSH",cont="WSH",y="waz")
set.seed(67890)
WSHN.WSH.t1.permtest=wilcoxsign_test(waz~tr | block, data=WSHN.WSH.t1.df,
      distribution=approximate(nresample=100000))
WSHN.WSH.t1.perm.p=coin::pvalue(WSHN.WSH.t1.permtest)

waz_t1_h3_pval_unadj_j=data.frame(perm.pvalue=c(WSHN.N.t1.perm.p,WSHN.WSH.t1.perm.p))
rownames(waz_t1_h3_pval_unadj_j)=c("Nutrition + WSH vs. Nutrition",
                                   "Nutrition + WSH vs. WSH")


# endline
WSHN.N.t2.df=coin.prep(e,tx="Nutrition + WSH",cont="Nutrition",y="waz")
set.seed(67890)
WSHN.N.t2.permtest=wilcoxsign_test(waz~tr | block, data=WSHN.N.t2.df,
      distribution=approximate(nresample=100000))
WSHN.N.t2.perm.p=coin::pvalue(WSHN.N.t2.permtest)

WSHN.WSH.t2.df=coin.prep(e,tx="Nutrition + WSH",cont="WSH",y="waz")
set.seed(67890)
WSHN.WSH.t2.permtest=wilcoxsign_test(waz~tr | block, data=WSHN.WSH.t2.df,
      distribution=approximate(nresample=100000))
WSHN.WSH.t2.perm.p=coin::pvalue(WSHN.WSH.t2.permtest)

waz_t2_h3_pval_unadj_j=data.frame(perm.pvalue=c(WSHN.N.t2.perm.p,WSHN.WSH.t2.perm.p))
rownames(waz_t2_h3_pval_unadj_j)=c("Nutrition + WSH vs. Nutrition","Nutrition + WSH vs. WSH")


save(waz_t1_h1_pval_unadj_j,waz_t1_h3_pval_unadj_j,
     file=here("primary/res_data/waz_t1_pval_unadj.RData"))

save(waz_t2_h1_pval_unadj_j,waz_t2_h3_pval_unadj_j,
     file=here("primary/res_data/waz_t2_pval_unadj.RData"))

waz_t1_h1_pval_unadj_j
waz_t1_h3_pval_unadj_j
waz_t2_h1_pval_unadj_j
waz_t2_h3_pval_unadj_j


