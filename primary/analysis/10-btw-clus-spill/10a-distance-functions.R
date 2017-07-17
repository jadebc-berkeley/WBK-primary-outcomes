##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# functions for between-cluster spillover test

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
##############################################


#----------------------------------------------------
# Function to convert from degrees to meters using UTM projection
# Input: data frame with a column for longitude and a column for latitude
# Output: data frame with data projected on UTM for Bangladesh (in meters)
#----------------------------------------------------
utmproj=function(data,utm){
  matrix=project(as.matrix(data), paste("+proj=utm +north +zone=",utm," ellps=WGS84",sep=""))
  data$x = matrix[,1]
  data$y = matrix[,2]
  # Move the origin to the bottom-left corner
  # Leave a 1000 meters margin to avoid any zero related issues
  data$x = data$x - min(data$x) + 1000
  data$y = data$y - min(data$y) + 1000
  data$lat = NULL
  data$long = NULL
  return(data)
}


#----------------------------------------------------
# Function to calculate how many treated compounds within 
# X kilometers of each control compound
# Inputs:
#   Data: distance matrix with rows indexed by control compounds
#   and columns indexed by treated compounds 
#   Dists: a vector of distances in which to calculate the number 
#   of treated compounds
# Output: matrix with the number of treated compounds within X 
# kilometers of each control compound 
#----------------------------------------------------
dm.dist=function(dists,data){
  ids=rownames(data)
  mat=matrix(NA,length(ids),length(dists))
  for(i in 1:length(ids)){
    # data frame with one control compound and distances
    # to all tx compounds 
    sub=data.frame(dist=data[ids[i],])
    for(j in 1:length(dists)){
      # number of compounds within distance j
      mat[i,j]=length(sub$dist[sub$dist<dists[j]])
    }
  }
  
  # average number of treated compounds within X kilometers
  comp.dist=data.frame(km=dists,compounds=colMeans(mat))
  
  return(comp.dist)
}

#----------------------------------------------------
# Function to permute the outcome in the data frame
# that contains the number of tr compounds within 2km
# and the outcome 
# Inputs: 
#  data: data frame with the following columns:
#
#    hhid: household id number
#    prop: proportion of households within 2km of 
#    the control household that are treated
#
#  y: character string for outcome (e.g., "haz")
#
#  ydata: data frame containing individual level outcome
#
#  data in the column indicated in "y"
# 
# Output: data frame with permuted outcome variable
#----------------------------------------------------
permute.y=function(df,y,ydata){

  # Merge in y data
  df.y=merge(df,ydata[,c("hhid",y)],by="hhid",all.x=TRUE,all.y=TRUE)
  df.y=df.y[!is.na(df.y[[y]]),]
  df.y=df.y[!is.na(df.y$prop),]

  ydf=data.frame(df.y[,y])
  colnames(ydf)[1]=y
  ydf$rand=rnorm(nrow(ydf),0,1)
  ydf=ydf[order(ydf$rand),]
  df.perm=data.frame(cbind(df.y[,c("hhid","prop")],ydf[,y]))
  colnames(df.perm)=c("hhid","prop",y)

  return(df.perm)
}



#----------------------------------------------
# number of treated compounds within 2 km
# of a given compound
# input: matrix of distances from a single 
# control compound to each tr compound 
# 
# output: dataframe with number of tr compounds
# within 2km, childid
#----------------------------------------------
comp2km=function(data){
  out=data.frame(hhid=as.numeric(as.character(rownames(data))))
  out$comp2km=apply(data,1,function(x) sum(ifelse(x<2,1,0)))
  return(out)
}

#----------------------------------------------
# function to get the mean at <=20th and >=80th percentiles
# of the number of tr compounds within 
# 2km of each control compound 
# Inputs:
#  data: data frame with the proportion of households
#  within 2km from the control household that were treated 
#  (this column must be called "prop") and the outcome variable
# Outputs: 
#  n20: number of observations with the proportion of treated
#  households <= the 20th percentile of the distribution
#  n80: number of observations with the proportion of treated
#  households <= the 80th percentile of the distribution
#  mean20: mean proportion of treated households within 2km of the
#  control household for households <= the 20th percentile of the 
#  distribution
#  mean80: mean proportion of treated households within 2km of the
#  control household for households <= the 80th percentile of the 
#  distribution
#  diff: mean80-mean20
#----------------------------------------------
psi=function(data,y){
    
  p20=quantile(data[["prop"]],probs=0.2,type=5)
  p80=quantile(data[["prop"]],probs=0.8,type=5)
  
  mean20=mean(data[[y]][data$prop<=p20])
  mean80=mean(data[[y]][data$prop>=p80])
  
  diff=mean80-mean20
  
  n20=nrow(data[data$prop<=p20,])
  n80=nrow(data[data$prop>=p80,])
  
  return(list(n20=n20,n80=n80,mean20=mean20,mean80=mean80,diff=diff))
}




#----------------------------------------------
# function that 
# 1. permutes the outcome
# 2. estimates parameter
# Inputs: 
#  data: data frame with the following columns:
#
#    hhid: household id number
#    prop: proportion of households within 2km of 
#    the control household that are treated
#
#  y: character string for outcome (e.g., "haz")
#
#  ydata: data frame containing individual level outcome
#
#  data in the column indicated in "y"
#----------------------------------------------
perm.dist.diff=function(data,y,ydata){
  # Permute compound ids in distance matrix
  y.tx.tdist=permute.y(data,y,ydata)
  
  # estimate parameter
  perm.diff=psi(y.tx.tdist,y=y)
  
  return(perm.diff$diff)
}



#----------------------------------------------
# function that 
# 1. estimates the observed difference
# 2. repeatedly permutes the outcome 
#    and re-estimate parameters
# 3. compares the observed difference to the 
#    permutation distribution
# Inputs: 
#  data: data frame with the following columns:
#
#    hhid: household id number
#
#    prop: proportion of households within 2km of 
#    the control household that are treated
#
#  y: character string for outcome (e.g., "haz")
#
#  ydata: data frame containing individual level outcome
#  data in the column indicated in "y"
#  
#  B: the number of replicates for the permutation test
#
# Output: Permutation test estimates:
#  n20: number of observations with the proportion of treated
#  households <= the 20th percentile of the distribution
#  n80: number of observations with the proportion of treated
#  households <= the 80th percentile of the distribution
#  mean20: mean proportion of treated households within 2km of the
#  control household for households <= the 20th percentile of the 
#  distribution
#  mean80: mean proportion of treated households within 2km of the
#  control household for households <= the 80th percentile of the 
#  distribution
#  diff: mean80-mean20
#----------------------------------------------
myperm.test=function(data,y,ydata,B){
  
  # Merge in y data
  y.ncomp=merge(data,ydata[,c("hhid",y)],by="hhid",all.x=TRUE,all.y=TRUE)
  y.ncomp=y.ncomp[!is.na(y.ncomp[[y]]),]
  y.ncomp=y.ncomp[!is.na(y.ncomp$prop),]
  
  # sort by hhid
  y.ncomp=y.ncomp[order(y.ncomp$hhid),]
  
  # observed difference
  obs=psi(y.ncomp,y=y)
  obs.diff=obs$diff
  obs.n20=obs$n20
  obs.n80=obs$n80
  mean.20=obs$mean20
  mean.80=obs$mean80
  
  # permuted dists
  perm.dist=data.frame(diff=unlist(replicate(B,perm.dist.diff(data,y,ydata))))
  
  comp.dist=matrix(NA,B,1)
  for(i in 1:B){
    comp.dist[i,]=abs(perm.dist[i,]) > abs(obs.diff)
  }
  
  # two-tailed test
  p=sum(comp.dist)/B
  
  return(list(p=p,perm.dist=perm.dist,obs.diff=obs.diff,obs.n20=obs.n20,
              obs.n80=obs.n80,mean.20=mean.20,mean.80=mean.80))
}

#----------------------------------------------------
# function to format output of the permutation test
# Input: Object produced by myperm.test
# Output: Formatted permutation test output
#----------------------------------------------------
format.perm=function(obj){
  out=data.frame(matrix(c(obj$obs.n20,obj$obs.n80,obj$mean.20,obj$mean.80,
                          obj$obs.diff,obj$p),1,6))
  colnames(out)=c("N20","N80","Mean20","Mean80","Diff","P-value")
  return(out)
}
