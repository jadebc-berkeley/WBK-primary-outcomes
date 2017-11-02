
#---------------------------------------
# Anthro Analysis.R
#
# andrew mertens (amertens@berkeley.edu)
#
# The outcome analysis in Wash Benefits Kenya for all anthropometry measures and indices.
#---------------------------------------

#---------------------------------------
# preamble
#---------------------------------------
###Load in data
rm(list=ls())
library(foreign)
library(tidyverse)
library(magrittr)
library(rmarkdown)
library(washb)
library(SuperLearner)
library(ggthemes)

setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Data/Final/Andrew")

enrol<-read.dta("washb-kenya-enrol.dta")
anthro<-read.dta("washb-kenya-anthro.dta")


setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Andrew")



# source the base functions
# which includes the design matrix function used below
source("C:/Users/andre/Documents/washb_Kenya_primary_outcomes_Andrew/R Primary Outcome Code/washb_functions.R")

####set arms
arms<-c("Control","Water","Sanitation","Handwashing","WSH","Nutrition","WSH+N","PassiveControl")



#-------------------
#Merge and subset data
#-------------------



#merge in adjustment covariates
enroltomerge<-subset(enrol, select=c('childid','Ncomp','momage','momedu','momheight','dminwat','Nlt18','electricity','radio','television','mobile','clock','bicycle','motorcycle','stove','cow','goat','dog','chicken','roof', 'floor','HHS' ))


#merge in adjustment covariates
dim(anthro)
anthro_enrol<-merge(anthro, enroltomerge, by=c("childid"),all.x=T,all.y=F)
dim(anthro_enrol)




#Subset to index children with LAZ measurements
d <- anthro_enrol %>% filter(childtype=="Study Child"|childtype=="Study Child Twin") %>%
                      filter(!is.na(laz) & laz!=1)




#-------------------
# Plot ages by treatment arms
#-------------------

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




levels(d$tr)
d$tr <- factor(d$tr, 
               levels = c("Control", "Passive Control", "Water", "Sanitation", "Handwashing","WSH", "Nutrition", "Nutrition + WSH"))

d$tr_code <- NA
d$tr_code[d$tr=="Control"] <- "C"
d$tr_code[d$tr=="Passive Control"] <- "P"
d$tr_code[d$tr=="Water"] <- "W"
d$tr_code[d$tr=="Sanitation"] <- "S"
d$tr_code[d$tr=="Handwashing"] <- "H"
d$tr_code[d$tr=="WSH"] <- "WSH"
d$tr_code[d$tr=="Nutrition"] <- "N"
d$tr_code[d$tr=="Nutrition + WSH"] <- "WSHN"
d$tr_code <- factor(d$tr_code, levels = c("C", "P", "W", "S", "H","WSH", "N", "WSHN"))


d$year <- NA
d$year[d$studyyear==1] <- "Year 1"
d$year[d$studyyear==2] <- "Year 2"
d$year <- as.factor(d$year)



#Get median and 1st and 3rd quartiles to print in plot
IQR <- d %>% group_by(year, tr_code) %>%
             summarize(med= median(agem),
                       firstQ= quantile(agem, probs=0.25),
                       thirdQ= quantile(agem, probs=0.75)) %>%
             as.data.frame()

# meds <- ddply(DF, .(TYPE, PROVIDER), summarize, med = median(VALUE))
# 
# ggplot(DF, aes(x=PROVIDER,y=  VALUE)) + 
#    geom_boxplot() + facet_wrap(~TYPE) + 
#    geom_text(data = meds, aes(y = med, label = round(med,2)),size = 3, vjust = -0.5)

med <- ddply(d, .(tr_code, year), summarize, med = median(agem))
firstQ <- ddply(d, .(tr_code, year), summarize, firstQ = quantile(agem, probs=0.25))
thirdQ <- ddply(d, .(tr_code, year), summarize, thirdQ = quantile(agem, probs=0.75))

#Create a text box stating the % of kids who were in our target age range 
#for each round, lumping all arms together since obviously they were balanced (target 
#age ranges were >=9 months but <15 months at Y1 and >=21 months but <27 months at Y2)
inrange_y1 <- round(mean(d[d$studyyear==1,"agem"] >= 9 & d[d$studyyear==1,"agem"] < 15) * 100, 1)
inrange_y2 <- round(mean(d[d$studyyear==2,"agem"] >= 21 & d[d$studyyear==2,"agem"] < 27) * 100, 1)
inrange_y1 <- paste0(inrange_y1,"% of children in the\nyear 1 target age range\n[9-15 months)")
inrange_y2 <- paste0(inrange_y2,"% of children in the\nyear 2 target age range\n[21-27 months)")

ann_text1 <- data.frame(tr_code = "S", agem = 23, lab = "a",
                       year = factor("Year 1",levels = c("Year 1", "Year 2")))
ann_text2 <- data.frame(tr_code = "H", agem = 10, lab = "b",
                       year = factor("Year 2",levels = c("Year 1", "Year 2")))


p <- ggplot(data=d, aes(tr_code, agem, fill=tr_code, colour = tr_code)) + 
  geom_boxplot() + 
  scale_fill_manual(values=alpha(cols, 0.5), guide=FALSE) +
  scale_colour_manual(values=cols, guide=FALSE) +
  coord_cartesian(ylim = c(0, 35)) +
  xlab("") + ylab("Child age (months)") +
  facet_grid(~year) + 
  theme_minimal() +
  #geom_text(data = med, aes(y = med, label = round(med,1)),size = 3, vjust = -0.5) +
  geom_text(data = firstQ, aes(y = firstQ, label = sprintf("%.1f", firstQ)),size = 3, vjust = 1.7, hjust=1.08) +
  geom_text(data = thirdQ, aes(y = thirdQ, label = sprintf("%.1f", thirdQ)),size = 3, vjust = -0.6, hjust=1.08) +
  geom_text(data = ann_text1,label = inrange_y1, colour="black") +
  geom_text(data = ann_text2,label = inrange_y2, colour="black") +
  theme(strip.text = element_text(face="bold", size=16),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x=element_text(colour=cols),
        plot.margin = unit(c(0,1,0,1), "cm")) 
p

  #theme_classic()
  #theme_bw()
  #theme_base()
#NEED TO MAKE X-axis labels colores


#-------------------
# Plot ages by treatment arms
#-------------------


try(setwd("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/Figures/"))
try(setwd("~/Dropbox/WBK-primary-analysis/Results/Figures/"))

pdf("kenya-age-boxplots.pdf",width=14,height=4)
p

dev.off()

try(setwd("C:/Users/andre/Documents/WBK-primary-outcomes/primary/figures"))

pdf("kenya-age-boxplots.pdf",width=14,height=4)
p

dev.off()




