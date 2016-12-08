##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# between-cluster spillover test
# get distance matrix for compounds

# by Jade (jadebc@berkeley.edu)
##############################################

rm(list=ls())
library(rgeos)
library(maptools)
library(rgdal)
library(reshape2)

source("~/Documents/CRG/wash-benefits/kenya/src/primary/analysis/10-btw-clus-spill/10a-distance-functions.R")

#----------------------------------------------------
# Read in data
#----------------------------------------------------
gps=read.csv("~/Dropbox/WASHB-Kenya-Data/0-Untouched-data/1-Main-survey/msP_household_tracking_20161006.csv")

# drop compounds with missing latitude/longitude
gps=gps[!is.na(gps$GPS_lat),]
gps=gps[!is.na(gps$GPS_long),]

gps.ll=data.frame(gps[,c("GPS_long","GPS_lat")])

rownames(gps.ll)=gps$hhid

# drop the four compounds that relocated to Nairobi
gps.ll=gps.ll[gps.ll$GPS_lat>0,]

#----------------------------------------------------
# Change projection to UTM
#----------------------------------------------------
gps.proj=utmproj(gps.ll,utm=36)[,c("x","y")]

#----------------------------------------------------
# Create distance matrix
#----------------------------------------------------
dm=as.matrix(dist(gps.proj,method="euclidian"))
dm=dm/1000

#----------------------------------------------------
# Quantiles of distance distribution across all clusters
#----------------------------------------------------
# create flat object with one row for each pair of compounds
dm.flat=melt(dm)[melt(upper.tri(dm))$value,]

# distribution of distances between compounds within full study
# population
quantile(dm.flat$value,probs=c(0,0.25,0.5,0.75,1))

           
save(dm,dm.flat,file="~/Dropbox/WBK-primary-analysis/Results/Jade/washb-dist.RData")

