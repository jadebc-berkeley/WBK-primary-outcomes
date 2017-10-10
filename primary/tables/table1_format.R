
##############################################
# WASH Benefits Kenya

# Table 1 formatting

# by Jade
##############################################
rm(list=ls())


load("C:/Users/andre/Dropbox/WBK-primary-analysis/Results/jade/table1.RData")

# n.comp.f=matrix(NA,1,8)
# for(i in 1:length(n.comp)){
#   n.comp.f[i]=paste("(N=",n.comp[i],")",sep="")
# }

n.hh.f=matrix(NA,1,8)
for(i in 1:length(n.hh)){
  n.hh.f[i]=paste("(N=",n.hh[i],")",sep="")
}

#Extract N
table1_N = table1_j[,c(1,2,4,6,8,10,12,14,16)]

#drop n columns
table1_j = table1_j[,c(1,3,5,7,9,11,13,15,17)]
table1_SD_j = table1_SD_j[,c(1,3,5,7,9,11,13,15,17)]


for(i in c(2:9)){
  for(j in c(1,5:7,12)){
    table1_j[j,i]=paste0(sprintf("%1.0f",as.numeric(table1_j[j,i])), " (", sprintf("%1.0f",as.numeric(table1_SD_j[j,i])),")")
  }
  for(j in c(2:4,8:11,13:23)){
    table1_j[j,i]=paste0(table1_N[j,i], " (",sprintf("%1.0f",as.numeric(table1_j[j,i])*100), "\\%)")
  } 
}
table1_j
table1=table1_j

blank=rep("",8)

table1_f=   rbind(
               c("\\textbf{Maternal}",blank),
               table1[c(1:2),],
               c( "\\textbf{Paternal}",blank),
               table1[c(3:4),],
               c("\\textbf{Household}",blank),
               table1[c(5:10),],
               c("\\textbf{Drinking Water}",blank),
               table1[c(11:13),],
               c("\\textbf{Sanitation}",blank),
               c("Always or usually use primary toilet for defecation",blank),
               table1[c(14:15),],
               c("Daily defecating in the open",blank),
               table1[c(16:17),],
               c("Latrine",blank),
               table1[c(18:20),],
               c("\\textbf{Handwashing location}",blank),
               table1[c(21:22),],
               c("\\textbf{Nutrition}",blank),
               table1[c(23),])

rownames(table1_f)=NULL

for(i in c(2:3,5:6,8:13,15:17,19,22,25,28,30:31,33)){
  table1_f$lab[i]=paste("~~~",table1_f$lab[i],sep="")
}
for(i in c(20:21,23:24,26:27)){
  table1_f$lab[i]=paste("~~~~~",table1_f$lab[i],sep="")
}

save(n.hh.f,table1_f,file="C:/Users/andre/Dropbox/WBK-primary-analysis/Results/jade/table1_f.RData")



