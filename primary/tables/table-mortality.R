##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Table with mortality results

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here::here("primary/analysis/0-config.R"))
source(here("primary/tables/0-table-base-functions.R"))

load(here("primary/res_data/mort_prev.RData"))
load(here("primary/res_data/mort_rd_unadj.RData"))

pt.est.ci.f=function(obj,decimals,scale){
  a=sprintf(paste("%0.0",decimals,"f",sep=""),obj[1]*scale)
  b=sprintf(paste("%0.0",decimals,"f",sep=""),obj[2]*scale)
  c=sprintf(paste("%0.0",decimals,"f",sep=""),obj[3]*scale)
  return(paste(a," (",b,", ",c,")",sep=""))
}


mort.prev=apply(as.matrix(mort_prev_j[,2]),1,function(x) pt.est.f(x,3,1))
mort.est=c("",apply(as.matrix(mort_h1_rd_unadj_j[,c(1,3:4)]),1,function(x) pt.est.ci.f(x,3,1)))

N=N_mort_j[,1]+N_mort_j[,2]
n=N_mort_j[,2]

mort.tab=cbind(rownames(N_mort_j),N,n,mort.prev,mort.est)

save(mort.tab,file=here("primary/res_tables/table-mort.RData"))
