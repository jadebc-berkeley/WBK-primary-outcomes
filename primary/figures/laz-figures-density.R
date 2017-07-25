# --------------------------------------
# laz-figures-density.R
# andrew mertens (amertens@berkeley.edu)
#
# description:
# plot distributions by study arm for
# LAZ at follow-up 1 and 2
# --------------------------------------

# --------------------------------------
# input files:
# laz_mean.Rdata
# laz_rd_unadj.Rdata
# laz_t1_pval_unadj.Rdata
# laz_t2_pval_unadj.Rdata
# midline-anthro.csv
# endline-anthro.csv
#
# output files:
#	kenya-laz1.pdf
# kenya-laz2.pdf
# --------------------------------------

# --------------------------------------
# preamble
# --------------------------------------

rm(list=ls())
library(RColorBrewer)
library(scales)


# --------------------------------------
# load the analysis output
# --------------------------------------

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Jade")
try(setwd("~/Dropbox/WBK-primary-analysis/Results/Jade/"))

load("laz_mean.Rdata")
load("laz_rd_unadj.Rdata")
load("laz_t1_pval_unadj.Rdata")
load("laz_t2_pval_unadj.Rdata")


# --------------------------------------
# rename analysis output objects
# for convenience
# --------------------------------------
t1sum  <- laz_t1_n_j
t1diffh1 <- laz_t1_h1_rd_unadj_j
t1pvalh1 <- laz_t1_h1_pval_unadj_j
t1diffh3 <- laz_t1_h3_rd_unadj_j
t1pvalh3 <- laz_t1_h3_pval_unadj_j

t2sum  <- laz_t2_n_j
t2diffh1 <- laz_t2_h1_rd_unadj_j
t2pvalh1 <- laz_t2_h1_pval_unadj_j
t2diffh3 <- laz_t2_h3_rd_unadj_j
t2pvalh3 <- laz_t2_h3_pval_unadj_j

#---------------------------------------
# load the anthropometry analysis data
# and subset to analysis children
#---------------------------------------
setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Jade")
try(setwd("~/Dropbox/WBK-primary-analysis/Data/Final/Jade/"))
ad1 <- read.csv("midline-anthro.csv")
head(ad1)
#Subset to target children without extreme HAZ values
ad1 <- ad1[ad1$targetchild==1 & !is.na(ad1$haz) & (ad1$haz >=-5 | ad1$haz <= 5),]


ad2 <- read.csv("endline-anthro.csv")
head(ad2)
#Subset to target children without extreme HAZ values
ad2 <- ad2[ad2$targetchild==1 & !is.na(ad2$haz) & (ad2$haz >=-5 | ad2$haz <= 5),]




# --------------------------------------
# plotting and analysis function
# for comparison of 2 groups using
# kernel densities
# --------------------------------------
lazdenplot <- function(x,y,namex,namey,dstats,ppval,main,letter,cols,ylab=TRUE,mulab=TRUE) {
	# x : a series of LAZ data from the first group
	# y : a series of LAZ data from the second group
	# namex : string. name for x group
	# namey : string. name for y group
	# dstats : a vector of length 5 with t-test statistics (diff, min, max, t-stat, P)
  # ppval : permutation test p-value
	# main : string. plot title
	# letter : letter for multipanel plots
	# cols  : colors used for plotting
	# ylab : Label the Y-axis?
  # mulab : logical. label group mean dots?


	# format the difference and 95% CI
	diff <- paste(sprintf("%1.2f",dstats[1])," (",sprintf("%1.2f",dstats[2]),", ",sprintf("%1.2f",dstats[3]),")",sep="")


	# make the empty plot
	op <- par(xpd=FALSE,mar=c(5,5,6,2)+0.1)
	ytics <- seq(0,0.4,by=0.1)
	xtics <- seq(-5,2,by=1)
	plot(density(x),type="n",
		main="",
		ylim=c(0,0.5),yaxt="n",ylab="",
		xlim=c(-5.1,2),xaxt="n",xlab="",
		las=1,bty="n"
		)
		axis(1,at=xtics,las=1,cex.axis=1.5)
		mtext("Length for Age Z-score (LAZ)",side=1,line=3)
		if(ylab==TRUE) {
		  axis(2,at=ytics,las=1,cex.axis=1.5,lwd=0,lwd.ticks=1)
		  mtext("Kernel Density",side=2,line=3.5)
		}
		mtext(main,side=3,line=2,cex=1.5,adj=0)
		mtext(letter,side=3,line=2,cex=2,adj=0,at=-6,font=2)

		# draw shaded regions for stunted and severely stunted (not used)
		minx <- min(xtics)
		miny <- min(ytics)
		maxy <- max(ytics)

		# draw kernal density distributions
		dx <- density(x)
		dy <- density(y)
		lines(dx,col=cols[1],lwd=3,lty=2)
		lines(dy,col=cols[2],lwd=3)

		# label group means
		if(mulab==TRUE) {
			segments(x0=mean(x),y0=miny-0.015,y1=0.05,col="gray40",lty=2)
			segments(x0=mean(y),y0=miny-0.015,y1=0.05,col="gray40",lty=2)
			text(x=mean(c(x,y)),y=0.07,"Group Means",col="gray40",cex=1)

		}

		#Plot kernal densities
		op <- par(xpd=TRUE)
		points(mean(x),miny-0.015,pch=21,cex=1.75,col=cols[1],bg=alpha(cols[1],alpha=0.5))

		points(mean(y),miny-0.015,pch=21,cex=1.75,col=cols[2],bg=alpha(cols[2],alpha=0.5))

		# draw a small table of statistics in the upper right
		txs <- c(-3.4,-2.6,-1.5,-0.6)
		txs2 <- c(2.2)
			mtext(c("","N","Mean","SD"),side=3,line=0,at=txs,cex=1,adj=1)
		mtext(namex,side=3,line=-1.2,at=txs[1],adj=1,col=cols[1])
			mtext(format(length(x),big.mark=","),side=3,line=-1.2,at=txs[2],adj=1,cex=0.9,col=cols[1])
			mtext(sprintf("%1.2f",mean(x)),side=3,line=-1.2,at=txs[3],adj=1,cex=0.9,col=cols[1])
			mtext(sprintf("%1.2f",sd(x))  ,side=3,line=-1.2,at=txs[4],adj=1,cex=0.9,col=cols[1])
		mtext(namey,side=3,line=-2.4,at=txs[1],adj=1,col=cols[2])
			mtext(format(length(y),big.mark=","),side=3,line=-2.4,at=txs[2],adj=1,cex=0.9,col=cols[2])
			mtext(sprintf("%1.2f",mean(y)),side=3,line=-2.4,at=txs[3],adj=1,cex=0.9,col=cols[2])
			mtext(sprintf("%1.2f",sd(y))  ,side=3,line=-2.4,at=txs[4],adj=1,cex=0.9,col=cols[2])

		mtext("Diff. (95% CI)",side=3,line=0,at=txs2,adj=1)
			mtext(diff,side=3,line=-2.4,at=txs2,adj=1,col="gray20",cex=0.9)

		mtext(paste("t-test p =",sprintf("%1.3f",dstats[5])),side=3,line=-4,at=txs2,adj=1,col="gray20",cex=0.9)
		mtext(paste("permute p =",sprintf("%1.3f",ppval)),side=3,line=-5.5,at=txs2,adj=1,col="gray20",cex=0.9)

		par(op)
}



# --------------------------------------
#  set up color palet
# --------------------------------------

#Set colors 
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



# --------------------------------------
#  make a multi-panel density plot - year 1
# --------------------------------------
try(setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/"))
try(setwd("~/Dropbox/WBK-primary-analysis/Results/"))
pdf("Figures/kenya-laz1.pdf",width=15,height=15)

# set up the layout
lo <- layout(mat=matrix(1:9,nrow=3,ncol=3,byrow=T))

lazdenplot(x=ad1$haz[ad1$tr=="Control"],y=ad1$haz[ad1$tr=="Passive Control"],namex="Control",namey="Passive Control", dstats=t1diffh1[1,], ppval=t1pvalh1[1,], main="Passive Control v. Control",letter="a",cols=cols[c(1,2)],mulab=TRUE)

lazdenplot(x=ad1$haz[ad1$tr=="Control"],y=ad1$haz[ad1$tr=="Water"],namex="Control",namey="Water", dstats=t1diffh1[2,], ppval=t1pvalh1[2,], main="Water v. Control",letter="b",cols=cols[c(1,3)],mulab=TRUE,ylab=FALSE)

lazdenplot(x=ad1$haz[ad1$tr=="Control"],y=ad1$haz[ad1$tr=="Sanitation"], namex="Control",namey="Sanitation", dstats=t1diffh1[3,], ppval=t1pvalh1[3,], main="Sanitation v. Control",letter="c",cols=cols[c(1,4)],mulab=TRUE,ylab=FALSE)

lazdenplot(x=ad1$haz[ad1$tr=="Control"],y=ad1$haz[ad1$tr=="Handwashing"], namex="Control",namey="Handwashing", dstats=t1diffh1[4,], ppval=t1pvalh1[4,], main="Handwashing v. Control",letter="d",cols=cols[c(1,5)],mulab=TRUE)

lazdenplot(x=ad1$haz[ad1$tr=="Control"],y=ad1$haz[ad1$tr=="WSH"],namex="Control",namey="WSH",dstats=t1diffh1[5,],ppval=t1pvalh1[5,], main="Combined WSH v. Control",letter="e",cols=cols[c(1,6)],mulab=TRUE,ylab=FALSE)

lazdenplot(x=ad1$haz[ad1$tr=="Control"],y=ad1$haz[ad1$tr=="Nutrition"], namex="Control",namey="Nutrition", dstats=t1diffh1[6,],ppval=t1pvalh1[6,], main="Nutrition v. Control",letter="f",cols=cols[c(1,7)],mulab=TRUE,ylab=FALSE)

lazdenplot(x=ad1$haz[ad1$tr=="Control"],y=ad1$haz[ad1$tr=="Nutrition + WSH"], namex="Control",namey="Nutrition + WSH", dstats=t1diffh1[7,], ppval=t1pvalh1[7,], main="Nutrition + WSH v. Control",letter="g",cols=cols[c(1,8)],mulab=TRUE)

lazdenplot(x=ad1$haz[ad1$tr=="Nutrition"],y=ad1$haz[ad1$tr=="Nutrition + WSH"], namex="Nutrition",namey="Nutrition + WSH", dstats=t1diffh3[1,],ppval=t1pvalh3[1,], main="Nutrition + WSH v. Nutrition",letter="h",cols=cols[c(7,8)],mulab=TRUE,ylab=FALSE)

lazdenplot(x=ad1$haz[ad1$tr=="WSH"],y=ad1$haz[ad1$tr=="Nutrition + WSH"], namex="WSH",namey="Nutrition + WSH", dstats=t1diffh3[2,],ppval=t1pvalh3[2,], main="Nutrition + WSH v. WSH",letter="i",cols=cols[c(6,8)],mulab=TRUE,ylab=FALSE)

dev.off()



# --------------------------------------
#  make a multi-panel density plot - year 2
# --------------------------------------
setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/")
pdf("Figures/kenya-laz2.pdf",width=15,height=15)

# set up the layout
lo <- layout(mat=matrix(1:9,nrow=3,ncol=3,byrow=T))

lazdenplot(x=ad2$haz[ad2$tr=="Control"],y=ad2$haz[ad1$tr=="Passive Control"],namex="Control",namey="Passive Control", dstats=t1diffh1[1,], ppval=t1pvalh1[1,], main="Passive Control v. Control",letter="a",cols=cols[c(1,2)],mulab=TRUE)

lazdenplot(x=ad2$haz[ad2$tr=="Control"],y=ad2$haz[ad2$tr=="Water"],namex="Control",namey="Water", dstats=t2diffh1[2,], ppval=t2pvalh1[2,], main="Water v. Control",letter="b",cols=cols[c(1,3)],mulab=TRUE, ylab=FALSE)

lazdenplot(x=ad2$haz[ad2$tr=="Control"],y=ad2$haz[ad2$tr=="Sanitation"], namex="Control",namey="Sanitation", dstats=t2diffh1[3,], ppval=t2pvalh1[3,], main="Sanitation v. Control",letter="c",cols=cols[c(1,4)],mulab=TRUE,ylab=FALSE)

lazdenplot(x=ad2$haz[ad2$tr=="Control"],y=ad2$haz[ad2$tr=="Handwashing"], namex="Control",namey="Handwashing", dstats=t2diffh1[4,], ppval=t2pvalh1[4,], main="Handwashing v. Control",letter="d",cols=cols[c(1,5)],mulab=TRUE)

lazdenplot(x=ad2$haz[ad2$tr=="Control"],y=ad2$haz[ad2$tr=="WSH"],namex="Control",namey="WSH",dstats=t2diffh1[5,],ppval=t2pvalh1[5,], main="Combined WSH v. Control",letter="e",cols=cols[c(1,6)],mulab=TRUE,ylab=FALSE)

lazdenplot(x=ad2$haz[ad2$tr=="Control"],y=ad2$haz[ad2$tr=="Nutrition"], namex="Control",namey="Nutrition", dstats=t2diffh1[6,],ppval=t2pvalh1[6,], main="Nutrition v. Control",letter="f",cols=cols[c(1,7)],mulab=TRUE, ylab=FALSE)

lazdenplot(x=ad2$haz[ad2$tr=="Control"],y=ad2$haz[ad2$tr=="Nutrition + WSH"], namex="Control",namey="Nutrition + WSH", dstats=t2diffh1[7,], ppval=t2pvalh1[7,], main="Nutrition + WSH v. Control",letter="g",cols=cols[c(1,8)],mulab=TRUE)

lazdenplot(x=ad2$haz[ad2$tr=="Nutrition"],y=ad2$haz[ad2$tr=="Nutrition + WSH"], namex="Nutrition",namey="Nutrition + WSH", dstats=t2diffh3[1,],ppval=t2pvalh3[1,], main="Nutrition + WSH v. Nutrition",letter="h",cols=cols[c(7,8)],mulab=TRUE, ylab=FALSE)

lazdenplot(x=ad2$haz[ad2$tr=="WSH"],y=ad2$haz[ad2$tr=="Nutrition + WSH"], namex="WSH",namey="Nutrition + WSH", dstats=t2diffh3[2,],ppval=t2pvalh3[2,], main="Nutrition + WSH v. WSH",letter="i",cols=cols[c(6,8)],mulab=TRUE,ylab=FALSE)

dev.off()

