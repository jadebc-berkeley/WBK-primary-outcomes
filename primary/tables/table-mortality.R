rm(list=ls())
load("~/Dropbox/WBK-primary-analysis/results/jade/mort_prev.RData")
load("~/Dropbox/WBK-primary-analysis/results/jade/mort_rd_unadj.RData")


source("~/dropbox/wbk-primary-analysis/results/table/0-table-base-functions.R")

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

save(mort.tab,file="~/Dropbox/WBK-primary-analysis/Results/jade/table-mort.RData")
