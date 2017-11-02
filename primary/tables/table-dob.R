##############################################
# WASH Benefits Kenya
# Primary outcome analysis 

# Table with date of birth data origins

# by Andrew Mertens (amertens@berkeley.edu)
##############################################


rm(list=ls())
library(xtable)

#Clean table function
cleantable <- function(x,digits) {
 print( xtable(x,digits=digits),
        sanitize.text.function=function(y){y},
        floating=FALSE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        only.contents=TRUE,
        hline.after=NULL
 )
}




setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Untouched")
tab<-read.csv("washk_dob_determination_20171012.csv")

#Extract subheaders
category <- as.character(tab$X)[c(1,2,5,8,14)]

#Drop empty rows
tab<-tab[!is.na(tab$Active.Control),]

#Seperate N and percents and round percents to whole numbers
d_perc <- tab[tab$X.1!="n",]
d_perc[,-c(1,2)] <- apply(d_perc[,-c(1,2)], 2, function(x) as.numeric(x))
d_perc[,-c(1,2)] <- round(d_perc[,-c(1,2)], 0)
d <- tab[tab$X.1=="n",]

#Concatenate N and percents
for(i in 3:ncol(d)){
  d[-1,i]<-paste0(d[-1,i]," (", d_perc[,i],"\\%)")
}

#Clean up formatting
d <- d[,-2]
colnames(d) <- c("", "Active Control", "Passive Control", "Water", "Sanitation", "Handwashing", "WSH", "Nutrition", "Nutrition + WSH")


#Add subheaders back in
blank <- rep("",8)

category


d <- rbind(d[1:3,],
           c(category[4], blank),
           d[4:5,],
           c(category[5], blank),
           d[6:7,])
d[,1] <- as.character(d[,1])
d[c(1:4,7),1] <- paste0("\\textbf{",d[c(1:4,7),1],"}")

d[c(5:6,8:9),1]  <- paste0("~~",d[c(5:6,8:9),1] )          

rownames(d) <- NULL


cleantable(d,0)




setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
dob.tab <- d
save(dob.tab, file="table-dob.RData")





