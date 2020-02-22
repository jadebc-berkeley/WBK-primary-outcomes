##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Table with binary anthropometry results

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/tables/0-table-base-functions.R"))

load(here("primary/res_data/stunt_prev.RData"))
load(here("primary/res_data/sstunt_prev.RData"))
load(here("primary/res_data/wast_prev.RData"))
load(here("primary/res_data/underwt_prev.RData"))

load(here("primary/res_data/stunt_rd_unadj.RData"))
load(here("primary/res_data/sstunt_rd_unadj.RData"))
load(here("primary/res_data/wast_rd_unadj.RData"))
load(here("primary/res_data/underwt_rd_unadj.RData"))

load(here("primary/res_data/stunted-PR-adj.RData"))
load(here("primary/res_data/sstunted-PR-adj.RData"))
load(here("primary/res_data/wasted-PR-adj.RData"))
load(here("primary/res_data/underwted-PR-adj.RData"))

#-----------------  prevalence ----------------- 
stunt_t2_prev=paste(sprintf("%0.01f",stunt_t2_prev_j[,1]*100),"$\\%$",sep="")
sstunt_t2_prev=paste(sprintf("%0.01f",sstunt_t2_prev_j[,1]*100),"$\\%$",sep="")
wast_t2_prev=paste(sprintf("%0.01f",wast_t2_prev_j[,1]*100),"$\\%$",sep="")
underwt_t2_prev=paste(sprintf("%0.01f",underwt_t2_prev_j[,1]*100),"$\\%$",sep="")

#----------------- risk differences ----------------- 
# unadjusted 
rd.stunt.h1.pt.est.unadj=c("",apply(stunt_t2_h1_rd_unadj_j[,-2],1,pt.est.ci.f,1,100))
rd.stunt.h3.pt.est.unadj=c("",apply(stunt_t2_h3_rd_unadj_j[,-2],1,pt.est.ci.f,1,100))

rd.sstunt.h1.pt.est.unadj=c("",apply(sstunt_t2_h1_rd_unadj_j[,-2],1,pt.est.ci.f,1,100))
rd.sstunt.h3.pt.est.unadj=c("",apply(sstunt_t2_h3_rd_unadj_j[,-2],1,pt.est.ci.f,1,100))

rd.wast.h1.pt.est.unadj=c("",apply(wast_t2_h1_rd_unadj_j[,-2],1,pt.est.ci.f,1,100))
rd.wast.h3.pt.est.unadj=c("",apply(wast_t2_h3_rd_unadj_j[,-2],1,pt.est.ci.f,1,100))

rd.underwt.h1.pt.est.unadj=c("",apply(underwt_t2_h1_rd_unadj_j[,-2],1,pt.est.ci.f,1,100))
rd.underwt.h3.pt.est.unadj=c("",apply(underwt_t2_h3_rd_unadj_j[,-2],1,pt.est.ci.f,1,100))

# adjusted 
rd.stunt.h1.pt.est.adj=c("",apply(stunt_t2_h1_rd_adj_j,1,pt.est.ci.f,1,100))
rd.stunt.h3.pt.est.adj=c("",apply(stunt_t2_h3_rd_adj_j,1,pt.est.ci.f,1,100))

rd.sstunt.h1.pt.est.adj=c("",apply(sstunt_t2_h1_rd_adj_j,1,pt.est.ci.f,1,100))
rd.sstunt.h3.pt.est.adj=c("",apply(sstunt_t2_h3_rd_adj_j,1,pt.est.ci.f,1,100))

rd.wast.h1.pt.est.adj=c("",apply(wast_t2_h1_rd_adj_j,1,pt.est.ci.f,1,100))
rd.wast.h3.pt.est.adj=c("",apply(wast_t2_h3_rd_adj_j,1,pt.est.ci.f,1,100))

rd.underwt.h1.pt.est.adj=c("",apply(underwt_t2_h1_rd_adj_j,1,pt.est.ci.f,1,100))
rd.underwt.h3.pt.est.adj=c("",apply(underwt_t2_h3_rd_adj_j,1,pt.est.ci.f,1,100))

#----------------- rr table ----------------- 
table.h1=rbind(cbind(stunt_t2_prev,rd.stunt.h1.pt.est.unadj,rd.stunt.h1.pt.est.adj),
               cbind(sstunt_t2_prev,rd.sstunt.h1.pt.est.unadj,rd.sstunt.h1.pt.est.adj),
               cbind(wast_t2_prev,rd.wast.h1.pt.est.unadj,rd.wast.h1.pt.est.adj),
               cbind(underwt_t2_prev,rd.underwt.h1.pt.est.unadj,rd.underwt.h1.pt.est.adj))

table.h3=rbind(cbind(stunt_t2_prev[c(8,7,6)],rd.stunt.h3.pt.est.unadj,rd.stunt.h3.pt.est.adj),
               cbind(sstunt_t2_prev[c(8,7,6)],rd.sstunt.h3.pt.est.unadj,rd.sstunt.h3.pt.est.adj),
               cbind(wast_t2_prev[c(8,7,6)],rd.wast.h3.pt.est.unadj,rd.wast.h3.pt.est.adj),
               cbind(underwt_t2_prev[c(8,7,6)],rd.underwt.h3.pt.est.unadj,rd.underwt.h3.pt.est.adj))

lab.h1=rep(c("Control","Passive Control","Water","Sanitation","Handwashing",
             "Water + Sanitation + Handwashing (WSH)","Nutrition",
             "Nutrition + WSH"),4)

lab.h3=rep(c("Nutrition + WSH","Nutrition","WSH"),4)

anthro.bin.table.adj.h1=cbind(lab.h1, table.h1)
anthro.bin.table.adj.h3=cbind(lab.h3, table.h3)

save(anthro.bin.table.adj.h1,anthro.bin.table.adj.h3,
  file=here("primary/res_tables/table-anthro-bin-adj.RData"))

