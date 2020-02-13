#----------------------------------------
# 0-base-functions.R
#
# Base functions for the WASH Benefits Kenya
# intention to treat (ITT) analyses
#
# by Jade Benjamin-Chung (jadebc@berkeley.edu)
#----------------------------------------

#----------------------------------------------------
# function to prepare data for diarrhea analysis in R
#----------------------------------------------------
preprocess.diarr=function(d){
  
  # keep diarrhea cohort members
  d=subset(d, d$dcohort==1)
    
  # replace diarr7 as missing if diarr7==999
  d$diarr7[d$diarr7==999]=NA

  # drop if diarrhea is missing
  d=d[!is.na(d$diarr7),]
  
  # reorder tr labels
  d$tr=factor(d$tr,levels(d$tr)[c(1,5,7,6,2,8,3,4)])
  
  # keep midline and endline measurements
  d=subset(d,d$time>0)

  return(d)
}

#----------------------------------------------------
# function to prepare data for anthro analysis in R
#----------------------------------------------------
preprocess.anthro=function(d, y){

  # reorder tr labels
  d$tr=factor(d$tr,levels(d$tr)[c(1,5,7,6,2,8,3,4)])
  
  # drop if y is missing
  d=d[!is.na(d[[y]]),]
  
  # drop if not target child
  d=d[d$targetchild==1,]
  
  return(d)
}

#----------------------------------------------------
#function to prepare data for adjusted analysis 
#----------------------------------------------------
preprocess.adj=function(d,y,time=NA){
  
  # sort data 
  if(y=="diarr7"){
    d <- d[order(d$block,d$clusterid,d$hhid,d$childid, d$time),]
  }else{
    d <- d[order(d$block,d$clusterid,d$hhid,d$childid),]
  }

  # drop columns that aren't needed in SL
  keeps = c(y,"clusterid","tr","month",
  "HHS","aged","sex","mother_age","motherht","mother_edu",
  "u18","Ncomp","water_time","floor","roof","cow",
  "goat","chicken","dog","elec","radio","tv","mobilephone",
  "clock","bicycle","motorcycle","stove",
  "block","hhid","childid","staffid")

  df=d[,names(d) %in% keeps,]
  
  # make sure month is a factor
  df$month = as.factor(df$month)

  # reorder factor values to so that the reference is 
  # not having something instead of the first thing alphabetically
  df$clock=relevel(df$clock,ref="No clock")
  df$elec=relevel(df$elec,ref="No electricity")
  df$mother_edu=relevel(df$mother_edu,ref="Incomplete primary")
  df$roof=relevel(df$roof,ref="Thatch/leaf")
  df$floor=relevel(df$floor,ref="Earth/dung")
  df$bicycle=relevel(df$bicycle,ref="No bicycle")
  df$radio=relevel(df$radio,ref="No radio")
  df$tv=relevel(df$tv,ref="No TV")
  df$mobilephone=relevel(df$mobilephone,ref="No mobile phone")
  df$stove=relevel(df$stove,ref="No stove")

  # lump field investigators with <100 measurements into a single code 
  if(y=="diarr7"){
    df$staffid[df$staffid==327] = 0
    df$staffid[df$staffid==460] = 0
    df$staffid[df$staffid==1213] = 0
    df$staffid[df$staffid==1400] = 0
    df$staffid[df$staffid==1405] = 0
    df$staffid[df$staffid==1723] = 0
    df$staffid[df$staffid==1727] = 0
    df$staffid[df$staffid==1728] = 0
    df$staffid[df$staffid==1830] = 0
    df$staffid[df$staffid==2105] = 0
    df$staffid[df$staffid==2112] = 0
    df$staffid[df$staffid==2174] = 0
    df$staffid[df$staffid==2217] = 0
    df$staffid[df$staffid==2242] = 0
    df$staffid[df$staffid==2311] = 0
    df$staffid[df$staffid==2321] = 0
    df$staffid[df$staffid==2328] = 0
    df$staffid[df$staffid==2674] = 0
    df$staffid[df$staffid==2847] = 0
    df$staffid[df$staffid==3102] = 0
    df$staffid[df$staffid==3322] = 0
    df$staffid[df$staffid==3323] = 0
    df$staffid[df$staffid==3352] = 0
    df$staffid[df$staffid==3357] = 0
    df$staffid[df$staffid==3408] = 0
    df$staffid[df$staffid==3410] = 0
    df$staffid[df$staffid==3418] = 0
    df$staffid[df$staffid==3420] = 0
    df$staffid[df$staffid==3421] = 0
    df$staffid[df$staffid==3424] = 0
    df$staffid[df$staffid==3425] = 0
    df$staffid[df$staffid==3435] = 0
    df$staffid[df$staffid==3436] = 0
    df$staffid[df$staffid==3437] = 0
    df$staffid[df$staffid==3783] = 0
    df$staffid[df$staffid==4187] = 0
    df$staffid[df$staffid==4328] = 0
    df$staffid[df$staffid==4345] = 0
    df$staffid[df$staffid==4347] = 0
    df$staffid[df$staffid==4348] = 0
    df$staffid[df$staffid==4382] = 0
    df$staffid[df$staffid==4433] = 0
    df$staffid[df$staffid==4438] = 0
    df$staffid[df$staffid==4471] = 0
    df$staffid[df$staffid==4515] = 0
    df$staffid[df$staffid==4518] = 0
    df$staffid[df$staffid==4522] = 0
    df$staffid[df$staffid==4531] = 0
    df$staffid[df$staffid==4548] = 0
    df$staffid[df$staffid==4645] = 0
    df$staffid[df$staffid==5422] = 0
    df$staffid[df$staffid==7383] = 0
    df$staffid[df$staffid==8152] = 0
    df$staffid[df$staffid==8274] = 0
    df$staffid[df$staffid==8604] = 0
    df$staffid[df$staffid==8787] = 0
    df$staffid[df$staffid==8883] = 0
    df$staffid[df$staffid==8884] = 0
    df$staffid[df$staffid==9999] = 0
  }else{
    df$staffid[df$staffid==231] = 0
    df$staffid[df$staffid==431] = 0
    df$staffid[df$staffid==542] = 0
    df$staffid[df$staffid==551] = 0
    df$staffid[df$staffid==545] = 0
    df$staffid[df$staffid==860] = 0
    df$staffid[df$staffid==1727] = 0
    df$staffid[df$staffid==2312] = 0
    df$staffid[df$staffid==2687] = 0
    df$staffid[df$staffid==3334] = 0
    df$staffid[df$staffid==3433] = 0
    df$staffid[df$staffid==3436] = 0
    df$staffid[df$staffid==3438] = 0
    df$staffid[df$staffid==3446] = 0
    df$staffid[df$staffid==3450] = 0
    df$staffid[df$staffid==3508] = 0
    df$staffid[df$staffid==3683] = 0
    df$staffid[df$staffid==3783] = 0
    df$staffid[df$staffid==3785] = 0
    df$staffid[df$staffid==3787] = 0
    df$staffid[df$staffid==4218] = 0
    df$staffid[df$staffid==4313] = 0
    df$staffid[df$staffid==4314] = 0
    df$staffid[df$staffid==4347] = 0
    df$staffid[df$staffid==4345] = 0
    df$staffid[df$staffid==4348] = 0
    df$staffid[df$staffid==4405] = 0
    df$staffid[df$staffid==4515] = 0
    df$staffid[df$staffid==4534] = 0
    df$staffid[df$staffid==4538] = 0
    df$staffid[df$staffid==4548] = 0
    df$staffid[df$staffid==4617] = 0
    df$staffid[df$staffid==4630] = 0
    df$staffid[df$staffid==4714] = 0
    df$staffid[df$staffid==4715] = 0
    df$staffid[df$staffid==4717] = 0
    df$staffid[df$staffid==4785] = 0
    df$staffid[df$staffid==4812] = 0
    df$staffid[df$staffid==4835] = 0
    df$staffid[df$staffid==5324] = 0
    df$staffid[df$staffid==5421] = 0
    df$staffid[df$staffid==5422] = 0
    df$staffid[df$staffid==5818] = 0
    df$staffid[df$staffid==7540] = 0
    df$staffid[df$staffid==7640] = 0
    df$staffid[df$staffid==7848] = 0
    df$staffid[df$staffid==8246] = 0
    df$staffid[df$staffid==8274] = 0
    df$staffid[df$staffid==8303] = 0
    df$staffid[df$staffid==8604] = 0
    df$staffid[df$staffid==8681] = 0
    df$staffid[df$staffid==8787] = 0
    df$staffid[df$staffid==8803] = 0
    df$staffid[df$staffid==8884] = 0
  }  

  df$staffid <- factor(df$staffid)

  return(df)

}

# --------------------------------------
# function to pre-screen adjustment
# covariates -- restrict to those
# with a LR test P<0.2
# --------------------------------------
Wprescreen <- function(Y,Ws,family) {
  # Y   : outcome variable of interest
  # Ws  : data frame of candidate covariates to screen
  dat <- data.frame(Ws,Y)
  dat <- dat[complete.cases(dat),] 
  nW <- ncol(Ws)
  LRp <- rep(NA,nW)
  for(i in 1:nW) {
    dat$W <- dat[,i]
    fit1 <- glm(Y~W,data=dat,family=family)
    fit0 <- glm(Y~1,data=dat,family=family)
    LRp[i] <- lrtest(fit1,fit0)[2,5]
  }
  p20 <- ifelse(LRp<0.2,1,0)
  cat("\nLikelihood Ratio Test P-values:\n")
  print(cbind(names(Ws),paste("P =",sprintf("%1.3f",LRp))))
  cat("\n\nCovariates selected (P<0.20):\n")
  print(cbind(names(Ws)[p20==1],paste("P =",sprintf("%1.3f",LRp[p20==1]))))
  return(names(Ws)[p20==1])
}



#----------------------------------------------
# function to drop blocks for a specific contrast
#----------------------------------------------
dropblocks=function(tx,cont,data){
  block.tr=table(data$block,data$tr)
  # drop blocks that are missing tx
  drops.tr=which(block.tr[,tx]==0)
  # drop blocks that are missing cont
  drops.cont=which(block.tr[,cont]==0)

  temp=data[!data$block %in% names(c(drops.tr,drops.cont)),]
  temp$block=droplevels(temp$block)

  print("Blocks dropped:")
  print(paste(c(drops.tr,drops.cont)))
  print(paste("N before dropping blocks: ",nrow(data[data$tr==tx|data$tr==cont,]),
              sep=""))
  print(paste("N after dropping blocks: ",nrow(temp[temp$tr==tx|temp$tr==cont,]),sep=""))

  return(temp)
}

#----------------------------------------------
# function to prep for coin
#----------------------------------------------
# takes compound level data frame
# subsets to relevant arms
# aggregates by block
# shuffles treatment within blocks
#----------------------------------------------
coin.prep=function(data,tx,cont,y){
  df=subset(data,data$tr==tx|data$tr==cont)
  
  df$block=as.factor(df$block)
  
  df=dropblocks(data=df,tx=tx,cont=cont)
  
  df=df[,c(y,"tr","block")]
  bdf=aggregate(df[[y]],list(tr=df$tr,block=df$block),mean)
  bdfw=reshape(bdf, timevar="tr",idvar="block",direction="wide")
  colnames(bdfw)=c("block",tx,cont)
  
  #shuffle arms
  perm=rnorm(2,0,1)
  colnames(bdfw)=colnames(bdfw)[c(1,order(perm)+1)]
  
  # convert back to long format
  bdfl=melt(bdfw,id="block",value.name=y)
  colnames(bdfl)[grep("variable",colnames(bdfl))]="tr"
  
  # converting tr and block to factors for coin 
  bdfl$tr=as.factor(bdfl$tr)
  bdfl$block=as.factor(bdfl$block)
  
  print(paste("Number of blocks included:",length(names(table(bdfl$block)))))
  print(paste("List of blocks dropped:"))
  blocklist=which(!names(table(data$block)) %in% names(table(df$block)))
  print(paste(blocklist))
  
  
  return(bdfl)
}


#----------------------------------------------------
# Prepare for Superlearner (permutation test)
# by removing special characters from colnames
# and making factor variables into indicators

# Input: 
#  data: data frame to be read into Super Learner
#  y: string for outcome variable name
# Output: data frame ready for SuperLearner
#----------------------------------------------------
# function to prepare data for TMLE
sl.prep=function(data,y){
  
  Y=data[[y]]
  A=data[,!names(data) %in% c(y,"block","clusterid","dataid","hhid","childid","tr","prep")]
  
  # which rows are factors
  # get names of columns that are factors
  factor.cols=matrix(0,length(colnames(A)),1)
  for(i in 1:length(colnames(A))){
    if(class(A[,i])=="factor"){
      factor.cols[i,]=1 
    }
  }
  coldf=cbind(data.frame(names=colnames(A),factor=factor.cols))
  factor.names=as.character(coldf$names[coldf$factor==1])
  
  f.df=A[,colnames(A) %in% factor.names]
  
  # drop factor levels that are blank
  for(i in 1:ncol(f.df)){
    f.df[,i]=droplevels(f.df[,i])
  }
  
  # make indicators
  Aind=model.matrix(~., data=f.df)[,-1]
  otherAs=colnames(A)[(!colnames(A) %in% factor.names) & 
                        !colnames(A) %in% c(y,"clusterid","tr")]
  A.out=cbind(A[,colnames(A) %in% otherAs],Aind)
  
  # remove spaces and symbols from column names
  undcols=grep("_",colnames(A.out))
  spacecols=grep(" ",colnames(A.out))
  slashcols=grep("/",colnames(A.out))
  symbcols=grep(">",colnames(A.out))
  dashcols=grep("-",colnames(A.out))
  pcols1=grep("\\(",colnames(A.out))
  pcols2=grep("\\)",colnames(A.out))

  for(i in 1:length(colnames(A.out)[undcols])){
    colnames(A.out)[undcols][i]=  gsub("_","",colnames(A.out)[undcols][i])
  }
  for(i in 1:length(colnames(A.out)[spacecols])){
    colnames(A.out)[spacecols][i]=  gsub(" ","",colnames(A.out)[spacecols][i])
  }
  for(i in 1:length(colnames(A.out)[slashcols])){
    colnames(A.out)[slashcols][i]=  gsub("/","",colnames(A.out)[slashcols][i])
  }
  for(i in 1:length(colnames(A.out)[symbcols])){
    colnames(A.out)[symbcols][i]=  gsub(">","",colnames(A.out)[symbcols][i])
  }
  for(i in 1:length(colnames(A.out)[dashcols])){
    colnames(A.out)[dashcols][i]=  gsub("-","",colnames(A.out)[dashcols][i])
  }
  for(i in 1:length(colnames(A.out)[pcols1])){
    colnames(A.out)[pcols1][i]=  gsub("\\(","",colnames(A.out)[pcols1][i])
  }
  for(i in 1:length(colnames(A.out)[pcols2])){
    colnames(A.out)[pcols2][i]=  gsub("\\)","",colnames(A.out)[pcols2][i])
  }

  tr.clusterid=data$clusterid
  
  cat("\nNumber of obs in analysis data frame\n")
  print(nrow(data))
  return(list(Y=Y, A=A.out, clusterid=tr.clusterid))
}

#----------------------------------------------------
# Format TMLE results
# Input: output from washb_tmle run with an apply statement
# such that the estimates are produced for each comparison
# e.g. apply(matrix(trlist), 1,function(x) washb_tmle(...))
# Output: Formatted tmle results with comparisons in each row
#----------------------------------------------------
format.tmle=function(out,family){
  if(family=="binomial"){
    rr.res=matrix(NA,length(out),3)
    for(i in 1:length(out)){
      rr.res[i,1]=out[[i]]$estimates$RR$psi
      rr.res[i,2]=out[[i]]$estimates$RR$CI[1]
      rr.res[i,3]=out[[i]]$estimates$RR$CI[2]
    }
    rr.res=as.data.frame(rr.res)
    colnames(rr.res)=c("rr","lb","ub") 
  }
  rd.res=matrix(NA,length(out),3)
  for(i in 1:length(out)){
    rd.res[i,1]=out[[i]]$estimates$ATE$psi
    rd.res[i,2]=out[[i]]$estimates$ATE$CI[1]
    rd.res[i,3]=out[[i]]$estimates$ATE$CI[2]
  }
  
  rd.res=as.data.frame(rd.res)
  colnames(rd.res)=c("rd","lb","ub")  
  
  if(family=="binomial"){
    return(list(rr=rr.res,rd=rd.res))
  }
  return(list(rd=rd.res))
  
}

