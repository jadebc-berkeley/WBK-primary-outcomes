#---------------------------------------
# Kenya-uptake-plot.R
#
# Andrew Mertens (amertens@berkeley.edu)
#
# plot uptake measures for the WASH
# Benefits Kenya trial
#---------------------------------------


#---------------------------------------
# preamble
#---------------------------------------
rm(list=ls())
library(scales)

#---------------------------------------
# load the uptake estimates
#---------------------------------------

try(setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade/"))
try(setwd("~/Dropbox/WBK-primary-analysis/Results/Jade/"))
load("kenya-uptake.RData")

#---------------------------------------
# general plotting function for uptake
#---------------------------------------

uplot <- function(x,varlab="",svy,cols,yaxis=F) {

  # empty plot
  plot(1:8,1:8,type="n",
       xaxt="n",xlab="",xlim=c(0.5,8.5),
       yaxt="n",ylab="",bty="n",ylim=c(0,1.3)
  )

  # Y-axis
  if(yaxis==TRUE) mtext(seq(0,100,by=20),side=2,line=0,at=seq(0,1,by=0.2),las=1,col="gray20")
  segments(x0=0,x1=8,y0=seq(0,1,by=0.2),col="gray80",lty=2)

  # X-axis labels
  mtext(c("C","P","W","S","H","WSH","N","WSHN"),side=1,line=0,at=seq(1,8,1),col=cols,cex=0.8,las=1)
  mtext(svy,side=1,line=2.5,col="gray20",cex=1)

  # plot points
  arrows(x0=1:8, y0=x[2,], y1=x[3,], col=cols,lwd=1,length=0.085,angle=90,code=3)
  points(1:8,x[1,],pch=21,cex=1.5,lwd=1,col=cols,bg="white")
  points(1:8,x[1,],pch=21,cex=1.5,lwd=0,col=cols,bg=alpha(cols,alpha=0.5))

}

# general label plot
ulabplot <- function(title) {
  plot(1,1,type="n",
       xaxt="n",xlab="",xlim=c(0,1),
       yaxt="n",ylab="",bty="n",ylim=c(0,1)
  )
  text(1,0.5,title,adj=1,cex=1.5)
}


#---------------------------------------
# put the result objects into a list
# to make them easier to plot in a loop
#---------------------------------------

rlnsp0 <- matrix(NA,nrow=nrow(rlnsp1),ncol=ncol(rlnsp1))
uptakelist <- list(
  list(promoter_vis0,promoter_vis1,promoter_vis2),
                   list(freechl0,freechl1,freechl2),
                   list(latfeces0,latfeces1,latfeces2),
                   list(humfeces0,humfeces1,humfeces2),
                   list(hwsw0,hwsw1,hwsw2),
                   list(rlnsp0,rlnsp1,rlnsp2))

# labels for the lists
uptakelabs <- c(
  "Visited by\npromoter\n in past month\n(%)",
  "Stored drinking\nwater has\ndetectable\nfree chlorine\n(%)",
  "Access to\nimproved\nlatrine\n(%)",
  "Child feces\nsafely disposed\n(%)",
  "Handwashing\nlocation\nhas water\n and soap\n(%)",
  "LNS sachets\nconsumed\n(% of expected)"
)

try(setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Figures/"))
try(setwd("~/Dropbox/WBK-primary-analysis/Results/Figures/"))
pdf("kenya-uptake.pdf",width=10.5,height=14)



black = "#000004FF"
blue = "#3366AA"
teal = "#11AA99"
green = "#66AA55"
chartr = "#CCCC55"
magent = "#992288"
red = "#EE3333"
orange = "#EEA722"
yellow = "#FFEE33"
grey = "#777777"
cols=c(black,chartr,blue,teal,green,orange,red,magent)


#Set up plot layout
lo <- layout(mat=matrix(1:24,ncol=4,nrow=6,byrow=T),widths=c(0.5,1,1,1))
op <- par(mar=c(4,2.5,3,0.5)+0.1)

#Plot the uptake measures
for(i in 1:length(uptakelist)) {
  if(i!=length(uptakelist)){
    op <- par(mar=c(3,1,1,0.5)+0.1)
    ulabplot(uptakelabs[i])
    op <- par(mar=c(3,2.5,1,0.5)+0.1)
    uplot(x=uptakelist[[i]][[1]],cols=cols,svy="Enrollment",yaxis=T)
    uplot(x=uptakelist[[i]][[2]],cols=cols,svy="Year 1")
    uplot(x=uptakelist[[i]][[3]],cols=cols,svy="Year 2")
  } else{
    op <- par(mar=c(3,1,1,0.5)+0.1)
    ulabplot(uptakelabs[i])
    op <- par(mar=c(3,2.5,1,0.5)+0.1)
    uplot(x=uptakelist[[i]][[1]],cols=cols,svy="",yaxis=T)
    uplot(x=uptakelist[[i]][[2]],cols=cols,svy="")
    uplot(x=uptakelist[[i]][[3]],cols=cols,svy="")
  }

}
par(op)
dev.off()








