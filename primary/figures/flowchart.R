##############################################
# WASH Benefits Kenya
# Primary outcome analysis

# CONSORT flow chart
# Prepare data for the figure

# by Jade (jadebc@berkeley.edu)
##############################################
rm(list=ls())
library(plyr)

d=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/diarrhea.csv")

ma=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/midline-anthro.csv")
ea=read.csv("~/Dropbox/WASHB-Kenya-Data/1-primary-outcome-datasets/endline-anthro.csv")

idchar=read.csv("~/Dropbox/WBK-primary-analysis/Data/Untouched/msP_child_IDchar_20161006.csv")

# reorder tr labels
reord=function(x){
  x$tr=factor(x$tr,levels(x$tr)[c(1,5,7,6,2,8,3,4)])
  return(x)
}

d=reord(d)
ma=reord(ma)
ea=reord(ea)

#--------------------------------------
# Target kids with haz
#-------------------------------------- 
t1_tc_haz=table(ma$tr[!is.na(ma$haz) & ma$targetchild==1])
t2_tc_haz=table(ea$tr[!is.na(ea$haz) & ea$targetchild==1])

#--------------------------------------
# Target kids with diarrhea
#-------------------------------------- 
t1_tc_diar=table(d$tr[!is.na(d$diarr7) & d$targetchild==1 & d$time==1])
t2_tc_diar=table(d$tr[!is.na(d$diarr7) & d$targetchild==1 & d$time==2])

#--------------------------------------
# Target kids with haz OR diarrhea
#--------------------------------------
d=subset(d, d$targetchild==1)
ma=ma[ma$targetchild==1,]
ea=ea[ea$targetchild==1,]

md.data=d[d$time==1,c("childid","tr","diarr7")]
ed.data=d[d$time==2,c("childid","tr","diarr7")]
ma.data=ma[,c("childid","tr","haz")]
ea.data=ea[,c("childid","tr","haz")]

y1=merge(md.data,ma.data,by=c("childid","tr"),all.x=TRUE,all.y=TRUE)
y2=merge(ed.data,ea.data,by=c("childid","tr"),all.x=TRUE,all.y=TRUE)

y1$outcome=ifelse(!is.na(y1$diarr7) | !is.na(y1$haz),1,0)
y2$outcome=ifelse(!is.na(y2$diarr7) | !is.na(y2$haz),1,0)

t1_outcome=table(y1$outcome,y1$tr)[2,]
t2_outcome=table(y2$outcome,y2$tr)[2,]

#--------------------------------------
# Print output
#--------------------------------------
t1_tc_haz
t2_tc_haz
t1_tc_diar
t2_tc_diar

save(t1_tc_haz,t2_tc_haz, t1_tc_diar,t2_tc_diar,
     file="~/Dropbox/WBK-primary-analysis/Results/Jade/flowchart.RData")


