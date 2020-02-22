##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Table with diarrhea results

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/tables/0-table-base-functions.R"))

load(here("primary/res_data/diar_prev.RData"))
load(here("primary/res_data/diar_pr_unadj.RData"))
load(here("primary/res_data/diar_rd_unadj.RData"))
load(here("primary/res_data/diarr_pval_unadj.RData"))
load(here("primary/res_data/diarr-PR-adj.RData"))
load(here("primary/res_data/diarr_pval_adj.RData"))

#----------------- prevalence ----------------- 
prevn12=paste(sprintf("%0.01f",diar_t12_prev_j[,1]*100),"$\\%$",sep="")

#----------------- risk difference - unadjusted ----------------- 
diar_h1_rd_unadj_j=diar_h1_rd_unadj_j[,-2]
diar_h2_rd_unadj_j=diar_h2_rd_unadj_j[,-2]
rd.h1.unadj.pt.est=c("",apply(as.matrix(diar_h1_rd_unadj_j),1,pt.est.ci.f,decimals=1,scale=100))
rd.h2.unadj.pt.est=c("",apply(as.matrix(diar_h2_rd_unadj_j),1,pt.est.ci.f,decimals=1,scale=100))

#----------------- risk difference - adjusted ----------------- 
rd.h1.adj.pt.est=c("",apply(as.matrix(diar_h1_rd_adj_j),1,pt.est.ci.f,decimals=1,scale=100))
rd.h2.adj.pt.est=c("",apply(as.matrix(diar_h2_rd_adj_j),1,pt.est.ci.f,decimals=1,scale=100))

#----------------- rr table ----------------- 
h1.tab=cbind(prevn12,rd.h1.unadj.pt.est,rd.h1.adj.pt.est)
h2.tab=cbind(prevn12[c(6,3:5)],
  rd.h2.unadj.pt.est,rd.h2.adj.pt.est)

diarr.table=rbind(h1.tab,h2.tab)
lab=c(rownames(diar_t12_prev_j),"WSH","Water","Sanitation","Handwashing")
diarr.table=cbind(lab,diarr.table)

rownames(diarr.table)=NULL

save(diarr.table,file=here("primary/res_tables/table-diarrhea.RData"))



