# --------------------------------------
# diar-figure.R
# andrew mertens (amertens@berkeley.edu)
#
# description:
# plot the WASH B kenya year 2 diarrhea
# results 
#
#
# --------------------------------------

# --------------------------------------
# input files:
#	diar_rd_unadj.RData
# diar_pr_unadj.RData
# diar_prev.RData
#
# output files:
#	  kenya-diar.pdf
# --------------------------------------

# --------------------------------------
# preamble
# --------------------------------------

source(here::here("primary/analysis/0-config.R"))
source(here("primary/analysis/0-base-programs.R"))

# --------------------------------------
# load the analysis output files
# --------------------------------------


load(here("primary/res_data/diar_rd_unadj.RData"))
load(here("primary/res_data/diar_pr_unadj.RData"))
load(here("primary/res_data/diar_prev.RData"))

# --------------------------------------
# format the objects for plotting
# --------------------------------------

#Creat vector of labels
glab <- c("Control\n","Passive Control\n","Water\n","Sanitation\n","Handwashing\n","Combined\nWSH","Nutrition\n","Combined\nNutrition+WSH")
glab2 <- c("C","PC","W","S","H","WSH","N","WSHN")



#Rename the object to be plotted with "fuPrev"
t0n <- diar_t0_n_j
t1n <- diar_t1_n_j
t0p <- diar_t0_prev_j
t1p <- diar_t1_prev_j
fuPrev <- diar_t12_prev_j

h1pr <- diar_h1_pr_unadj_j
h2pr <- diar_h2_pr_unadj_j
h3pr <- diar_h3_pr_unadj_j


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




#--------------------------------------------------------------------
# Year 2 diarrhea figure 
#--------------------------------------------------------------------

pdf(here("primary/res_figures/kenya-diar.pdf"),width=14,height=4)


ytics <- seq(18,36,by=2)  #<----------Set the Y-axis range here

op <- par(mar=c(1,9,9,0)+0.1,xpd=TRUE)
# set up an empty plot
MidPts <- barplot(1:8,names.arg=NA,border=NA,col=NA,
	ylim=range(ytics),ylab="",yaxt="n",
	las=1,bty="n"
	)
	segments(x0=0,x1=max(MidPts+0.5),y0=ytics,lty=2,lwd=1,col="gray80")
	axis(2,at=ytics,las=1)
	mtext("7-day\nDiarrhoea\nPrevalence\nDuring\nFollow-up\n(%)",side=2,line=3,las=1)

	# plot estimates
	arrows(x0=MidPts, y0=fuPrev[,2]*100, y1=fuPrev[,3]*100, col=cols,lwd=2,length=0.05,angle=90,code=3)
	points(MidPts,fuPrev[,1]*100,pch=21,cex=2,lwd=2,col=cols,bg="white")
	points(MidPts,fuPrev[,1]*100,pch=21,cex=2,lwd=0,col=cols,bg=alpha(cols,alpha=0.5))
	text(x=MidPts+0.05,y=fuPrev[,1]*100,sprintf("%1.1f",fuPrev[,1]*100),pos=4,cex=1,col=cols,font=1)
	
	# print header and footer labels
	mtext(glab,at=MidPts,side=3,line=6,col=cols,font=1  )
	hx <- MidPts[1]-0.5
	prform <- function(pr,lb,ub) {
		paste(sprintf("%1.2f",pr)," (",sprintf("%1.2f",lb),", ",sprintf("%1.2f",ub),")",sep="")
	}
	
	# print header table - PRs for H1	
	mtext("Prevalence Ratio (95% CI)",side=3,line=5.5,at=hx,adj=1,cex=1)
	mtext("Intervention v. Control",side=3,line=4.5,at=hx,adj=1,cex=0.8,col="gray30")
	mtext(c("ref",prform(h1pr[,1],h1pr[,2],h1pr[,3])),side=3,line=4.5,at=MidPts,cex=0.8,col="gray30")
	
	# print header table - PRs for H2a - c
	mtext("WSH v. Water",side=3,line=3,at=hx,adj=1,cex=0.8,col="gray30")
	mtext(c("ref",prform(h2pr[1,1],h2pr[1,2],h2pr[1,3])),side=3,line=3,at=MidPts[c(3,6)],cex=0.8,col=c(cols[3],"gray30"))
	
	mtext("WSH v. Sanitation",side=3,line=2,at=hx,adj=1,cex=0.8,col="gray30")
	mtext(c("ref",prform(h2pr[2,1],h2pr[2,2],h2pr[2,3])),side=3,line=2,at=MidPts[c(4,6)],cex=0.8,col=c(cols[4],"gray30"))
	
	mtext("WSH v. Handwashing",side=3,line=1,at=hx,adj=1,cex=0.8,col="gray30")
	mtext(c("ref",prform(h2pr[3,1],h2pr[3,2],h2pr[3,3])),side=3,line=1,at=MidPts[c(5,6)],cex=0.8,col=c(cols[5],"gray30"))

par(op)
dev.off()