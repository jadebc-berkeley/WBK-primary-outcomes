#---------------------------------------
# adherence-table.R
# 
# andrew mertens (amertens@berkeley.edu)
# adapted from code by ben arnold 
#
# make a table of adherence measures
# at enrollment, year 1, and year 2
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
rm(list=ls())
library(xtable)

#---------------------------------------
# load the uptake estimates
#---------------------------------------


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")
load("uptake_table_A.Rdata")

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew/")
d <- read.csv("washb-kenya-uptake.csv")


try(setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/"))
try(setwd("~/Dropbox/WBK-primary-analysis/Results/Jade/"))
load("kenya-uptake.RData")


#---------------------------------------
# put the result objects into a list
# to make them easier to combine
#---------------------------------------

rlnsp0 <- matrix(NA,nrow=nrow(rlnsp1),ncol=ncol(rlnsp1))
# uptakelist <- list(
#                    promoter_vis0,promoter_vis1,promoter_vis2,
#                    freechl0,freechl1,freechl2,
#                    latfeces0,latfeces1,latfeces2,
#                    humfeces0,humfeces1,humfeces2,
#                    hwsw0,hwsw1,hwsw2,
#                    rlnsp0,rlnsp1,rlnsp2)




#Covert into dataframe
#d <- t(data.frame(matrix(unlist(uptakelist), nrow=54, byrow=T),stringsAsFactors=FALSE))



#---------------------------------------
# idiosyncratic estimate formatting function
# takes a matrix with cols for arms and rows for mean, lb, ub
#---------------------------------------

makeci <- function(x) {
  paste(sprintf("%1.0f",x[,1]*100), " (",
        sprintf("%1.0f",x[,2]*100),", ",
        sprintf("%1.0f",x[,3]*100),")",sep="")
}
estformat <- function(x) {
  fx <- makeci(t(x))
  fx[grep("NA",fx)] <- "-"
  names(fx) <- colnames(x)
  fx
}

#---------------------------------------
# format estimates for each indicator
#---------------------------------------


# One adherence measureswithout measurements in all arms
rlnsp0 <- matrix(NA,nrow=nrow(rlnsp1),ncol=ncol(rlnsp1))


#promoter visited
tab_promot <- sapply(list(promoter_vis0,promoter_vis1,promoter_vis2),estformat)

# free chlorine
tab_freechl <- sapply(list(freechl0,freechl1,freechl2),estformat)

# latrine w/ no visible feces
tab_latfeces <- sapply(list(latfeces0,latfeces1,latfeces2),estformat)

#Child feces safely disposed   
tab_fecdisp <- sapply(list(humfeces0,humfeces1,humfeces2),estformat)

# handwasthing location has soap
tab_hwss <- sapply(list(hwsw0,hwsw1,hwsw2),estformat)

# LNS consumption
tab_rlns <-  sapply(list(rlnsp0,rlnsp1,rlnsp2),estformat)

#---------------------------------------
# grab N compounds measured at each time
#---------------------------------------

#Add in blank rows for rounds where passive control was not visited
uptake_n <- rbind(uptake_n[1:4,],
        data.frame(tr="Passive Control", studyyear="1", n=NA),
        data.frame(tr="Passive Control", studyyear="2", n=NA),
        uptake_n[5:22,])
Ncomp <- matrix(format(uptake_n[,3],big.mark=" "),nrow=3,ncol=8)

#---------------------------------------
# collate estimates into a 
# single adherence matrix
#---------------------------------------
adtab <- rbind(
  rep(NA,8),
  Ncomp,
  rep(NA,8),
  t(tab_promot),
  rep(NA,8),
  t(tab_freechl),
  rep(NA,8),
  t(tab_latfeces),
  rep(NA,8),
  t(tab_fecdisp),
  rep(NA,8),
  t(tab_hwss),
  rep(NA,8),
  t(tab_rlns)
)


# labels for the adherence indicators
adlabs <- c(
  "Visited by\npromoter\n in past month\n(%)",
  "Stored drinking\nwater has\ndetectable\nfree chlorine\n(%)",
  "Access to\nimproved\nlatrine\n(%)",
  "Child feces\nsafely disposed\n(%)",
  "Handwashing\nlocation\nhas water\n and soap\n(%)",
  "LNS sachets\nconsumed\n(% of expected)"
)

rownames(adtab) <- c(
  "Number of compounds measured (N)",c("Enrollment","Year 1","Year 2"),
  adlabs[1],c("Enrollment","Year 1","Year 2"),
  adlabs[2],c("Enrollment","Year 1","Year 2"),
  adlabs[3],c("Enrollment","Year 1","Year 2"),
  adlabs[4],c("Enrollment","Year 1","Year 2"),
  adlabs[5],c("Enrollment","Year 1","Year 2"),
  adlabs[6],c("Enrollment","Year 1","Year 2")
)

#---------------------------------------
# write the matrix to a file
#---------------------------------------
#write.csv2(adtab,file='C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew/table-adherence.csv',quote=TRUE)



# having trouble w/ excel opening ; delimited file
# so just output to html
adtab2 <- cbind(rownames(adtab),adtab)
print(xtable(adtab2),
      file='C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Table/table-adherence.xls',
      type='html',include.rownames=FALSE
)

d <- adtab2
save(d, file = "C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew/uptake_table.Rdata")
