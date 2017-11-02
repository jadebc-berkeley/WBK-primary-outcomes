##############################################
# WASH Benefits Kenya

# Table 1: enrollment characteristics by arm

# by jade benjamin-chung (jadebc@berkeley.edu)
##############################################

rm(list=ls())
library(reshape2)


data=read.csv("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/final/jade/table1.csv")


# reorder tr labels
data$tr=factor(data$tr,levels(data$tr)[c(1,5,7,6,2,8,3,4)])

comp.tab=data.frame(table(data$tr,data$compoundid))
comp.tab=comp.tab[comp.tab$Freq>0,]
n.comp=table(comp.tab$Var1)

n.hh=table(data$tr)

#-----------------------------------------------
# function to create a row of n and mean/% by treatment arm
#-----------------------------------------------
# x = variable
# lab = label
# decimals = # decimals to include in mean/percent
# scale = factor to scale result by (100 for percent)
tr.mean=function(x,lab){
  N=colSums(table(data[[x]],data$tr))
  n=(table(data[[x]],data$tr))[2,]
  mn=aggregate(data[[x]],list(data$tr),mean,na.rm=TRUE)
  mn.df=data.frame(matrix(mn[,2],nrow=1,ncol=8))
  colnames(mn.df)=mn[,1]
  out=cbind(lab,n[1],mn.df[1],n[2],mn.df[2],n[3],mn.df[3],n[4],mn.df[4],
            n[5],mn.df[5],n[6],mn.df[6],n[7],mn.df[7],n[8],mn.df[8])
  colnames(out)=c("lab","N","Active Control","N","Passive Control","N","Water","N",
                  "Sanitation","N",
                  "Handwashing","N","WSH","N","Nutrition","N","Nutrition + WSH")
  rownames(out)=NULL
  return(out)
}


#-----------------------------------------------
# calculate a row of n and mean/% by treatment arm
#-----------------------------------------------

mother_age=tr.mean("mother_age","Age (years)")
mother_edu=tr.mean("mother_edu","Completed at least primary")
mother_height=tr.mean("mother_height","Height (cm)")
firstborn=tr.mean("firstborn","Study child is firstborn")

father_edu=tr.mean("father_edu","Completed at least primary")
father_agri=tr.mean("father_agri","Works in agriculture")

Nhh=tr.mean("Nhh","Number of households per compound")
Nppl=tr.mean("Nppl","Number of people per compound")
u18=tr.mean("u18","Number of children <18 years in the household")
elec=tr.mean("elec","Has electricity")
cement=tr.mean("cement","Has a cement floor")
roof=tr.mean("roof","Has an iron roof")
mobile=tr.mean("mobilephone","Owns a mobile phone")
motorcycle=tr.mean("motorcycle","Owns a motorcycle")


prim_drink_ws=tr.mean("prim_drink_ws","Primary drinking water source is improved")
wat_time=tr.mean("wat_time","One-way walking time to primary water source (minutes)")
tr_storedwt=tr.mean("tr_storedwt","Reported treating currently stored water")

toilet_men=tr.mean("toilet_men","Adult men")
toilet_women=tr.mean("toilet_women","Adult women")
od_child38=tr.mean("od_child38","Children 3 to $<$8 years")
od_child03=tr.mean("od_child03","Children 0 to $<$3 years")

ownlat=tr.mean("ownlat","Own any latrine")
implat=tr.mean("implat","Access to improved latrine")
feces=tr.mean("feces","Human feces observed in the compound")

water=tr.mean("water2m","Has water within 2 meters of handwashing location")
soap=tr.mean("soap2m","Has soap within 2 meters of handwashing location")

hhs=tr.mean("HHS_bi","Prevalence of moderate to severe household hunger*")





table1=data.frame(rbind(mother_age, mother_edu, mother_height, firstborn,
                        father_edu, father_agri,
             Nhh, Nppl, u18, elec, cement, roof, mobile, motorcycle,
             prim_drink_ws, wat_time, tr_storedwt,
             toilet_men,toilet_women,od_child38, od_child03,
             ownlat,implat,feces,
             water,soap,hhs))

table1_j <- data.frame(lapply(table1, as.character), stringsAsFactors=FALSE)



#-----------------------------------------------
# function to create a row of SD by treatment arm
#-----------------------------------------------
# x = variable
# lab = label
# decimals = # decimals to include in mean/percent
# scale = factor to scale result by (100 for percent)
tr.SD=function(x,lab){
  n=colSums(table(data[[x]],data$tr))
  mn=aggregate(data[[x]],list(data$tr),sd,na.rm=TRUE)
  mn.df=data.frame(matrix(mn[,2],nrow=1,ncol=8))
  colnames(mn.df)=mn[,1]
  out=cbind(lab,n[1],mn.df[1],n[2],mn.df[2],n[3],mn.df[3],n[4],mn.df[4],
            n[5],mn.df[5],n[6],mn.df[6],n[7],mn.df[7],n[8],mn.df[8])
  colnames(out)=c("lab","N","Active Control","N","Passive Control","N","Water","N",
                  "Sanitation","N",
                  "Handwashing","N","WSH","N","Nutrition","N","Nutrition + WSH")
  rownames(out)=NULL
  return(out)
}


#-----------------------------------------------
# calculate a row of SD by treatment arm
#-----------------------------------------------

mother_age_SD=tr.SD("mother_age","Age (years)")
mother_edu_SD=tr.SD("mother_edu","Completed at least primary")
mother_height_SD=tr.SD("mother_height","Height (cm)")
firstborn_SD=tr.SD("firstborn","Study child is firstborn")

father_edu_SD=tr.SD("father_edu","Completed at least primary")
father_agri_SD=tr.SD("father_agri","Works in agriculture")

Nhh_SD=tr.SD("Nhh","Number of households per compound")
Nppl_SD=tr.SD("Nppl","Number of people per compound")
u18_SD=tr.SD("u18","Number of children <18 years in the household")
elec_SD=tr.SD("elec","Has electricity")
cement_SD=tr.SD("cement","Has a cement floor")
roof_SD=tr.SD("roof","Has an iron roof")
mobile_SD=tr.SD("mobilephone","Owns a mobile phone")
motorcycle_SD=tr.SD("motorcycle","Owns a motorcycle")

prim_drink_ws_SD=tr.SD("prim_drink_ws","Primary drinking water source is improved")
wat_time_SD=tr.SD("wat_time","One-way walking time to primary water source (minutes)")
tr_storedwt_SD=tr.SD("tr_storedwt","Reported treating currently stored water")

toilet_men_SD=tr.SD("toilet_men","Adult men")
toilet_women_SD=tr.SD("toilet_women","Adult women")
od_child38_SD=tr.SD("od_child38","Children 3 to $<$8 years")
od_child03_SD=tr.SD("od_child03","Children 0 to $<$3 years")

ownlat_SD=tr.SD("ownlat","Own any latrine")
implat_SD=tr.SD("implat","Access to improved latrine")
feces_SD=tr.SD("feces","Human feces observed in the compound")

water_SD=tr.SD("water2m","Has water within 2 meters of handwashing location")
soap_SD=tr.SD("soap2m","Has soap within 2 meters of handwashing location")

hhs_SD=tr.SD("HHS_bi","Prevalence of moderate to severe household hunger*")

table1_SD=data.frame(rbind(mother_age_SD, mother_edu_SD, mother_height_SD, firstborn_SD,
                           father_edu_SD, father_agri_SD,
             Nhh_SD, Nppl_SD, u18_SD, elec_SD, cement_SD, roof_SD, mobile_SD, motorcycle_SD,
             prim_drink_ws_SD, wat_time_SD, tr_storedwt_SD,
             toilet_men_SD,toilet_women_SD,od_child38_SD, od_child03_SD,
             ownlat_SD,implat_SD,feces_SD,
             water_SD,soap_SD,hhs_SD))

table1_SD_j <- data.frame(lapply(table1_SD, as.character), stringsAsFactors=FALSE)





save(n.comp,n.hh,table1_j, table1_SD_j,file="C:/Users/andre/Dropbox/WBK-primary-analysis/Results/jade/table1.RData")



