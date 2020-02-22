##############################################
# WASH Benefits Kenya
# Primary outcome analysis 
# Uptake figure

# prepare data for plotting in the uptake figure

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################
source(here("primary/tables/0-table-base-functions.R"))




b=read.csv(here("primary/data/washb-kenya-uptake-baseline-public.csv"))
m=read.csv(here("primary/data/washb-kenya-uptake-midline-public.csv"))
e=read.csv(here("primary/data/washb-kenya-uptake-endline-public.csv"))

b$svy=0
m$svy=1
e$svy=2

d=rbind(b,m,e)

# reorder tr labels
d$tr=factor(d$tr,levels(d$tr)[c(1,5,7,6,2,8,3,4)])

#wrapper function to call washb_mean
mean.est <- function(Y,tr,svy,id,group="Control",s=0,print=FALSE) {
  # Y : outcome variable
  # tr: treatment indicator variable
  # svy  : measurment round variable
  # id: cluster ID variable
  # group : string. treatment factor level to compute mean
  # s     : survey round to compute mean. 0, 1, or 2
  require(washb)
  if(group=="Passive Control" & s>0){
      res=rep(NA,3)
  }else{
    dat <- data.frame(id=id[tr==group & svy==s],
                      svy=svy[tr==group & svy==s],
                      Y=Y[tr==group & svy==s],
                      tr=tr[tr==group & svy==s])
    
    dat <- dat[complete.cases(dat),]
    fit <- washb_mean(Y=dat$Y,
                      id=dat$id,
                      print=print
    )
    if(print==TRUE) print(fit)
    res<-fit[c(2,5,6)]
  }

    names(res) <- c("mean","ci.lb","ci.ub")
  return(res)
}


#---------------------------------------
# Calculate means and 95% CIs by survey round
#---------------------------------------
arms <- levels(d$tr)
fu.arms = levels(m$tr)
  
promoter_vis0 <- matrix(NA,nrow=3,ncol=8)
promoter_vis1 <- sapply(arms,mean.est,Y=d$promoter,tr=d$tr,svy=d$svy,id=d$clusterid,s=1, print=FALSE)
promoter_vis1[,2] = NA
promoter_vis2 <- sapply(arms,mean.est,Y=d$promoter,tr=d$tr,svy=d$svy,id=d$clusterid,s=2, print=F)
promoter_vis2[,2] = NA

# store water with detectable chlorine
freechl0 <- sapply(arms,mean.est,Y=d$chlorine,tr=d$tr,svy=d$svy,id=d$clusterid,s=0)
freechl1 <- sapply(arms,mean.est,Y=d$chlorine,tr=d$tr,svy=d$svy,id=d$clusterid,s=1)
freechl2 <- sapply(arms,mean.est,Y=d$chlorine,tr=d$tr,svy=d$svy,id=d$clusterid,s=2)

# Access to improved latrine"
latfeces0 <- sapply(arms,mean.est,Y=d$imp_lat,tr=d$tr,svy=d$svy,id=d$clusterid,s=0)
latfeces1 <- sapply(arms,mean.est,Y=d$imp_lat,tr=d$tr,svy=d$svy,id=d$clusterid,s=1)
latfeces2 <- sapply(arms,mean.est,Y=d$imp_lat,tr=d$tr,svy=d$svy,id=d$clusterid,s=2)

#Child feces safely disposed
humfeces0 <- sapply(arms,mean.est,Y=d$feces_disp,tr=d$tr,svy=d$svy,id=d$clusterid,s=0)
humfeces1 <- sapply(arms,mean.est,Y=d$feces_disp,tr=d$tr,svy=d$svy,id=d$clusterid,s=1)
humfeces2 <- sapply(arms,mean.est,Y=d$feces_disp,tr=d$tr,svy=d$svy,id=d$clusterid,s=2)

# Primary handwashing station has water and soap
hwsw0 <- sapply(arms,mean.est,Y=d$hws,tr=d$tr,svy=d$svy,id=d$clusterid,s=0)
hwsw1 <- sapply(arms,mean.est,Y=d$hws,tr=d$tr,svy=d$svy,id=d$clusterid,s=1)
hwsw2 <- sapply(arms,mean.est,Y=d$hws,tr=d$tr,svy=d$svy,id=d$clusterid,s=2)

# Mean sachets of LNS fed in prior week to index child 6-24 mos (not measured at enrollment, only measured in nutrition arms)
narms <- arms[grep("Nutrition",arms)]
# pad with missings for other treatment arms
rlnsp1 <- cbind(matrix(NA,nrow=3,ncol=6),sapply(narms,mean.est,Y=d$lns,tr=d$tr,svy=d$svy,id=d$clusterid,s=1))
rlnsp2 <- cbind(matrix(NA,nrow=3,ncol=6),sapply(narms,mean.est,Y=d$lns,tr=d$tr,svy=d$svy,id=d$clusterid,s=2))


#---------------------------------------
# save the objects
#---------------------------------------
rm(d)
save.image(file=here("primary/res_data/kenya-uptake.RData"))



