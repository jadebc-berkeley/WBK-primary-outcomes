
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
  for(j in c(1,3,7:9,16)){
    table1_j[j,i]=paste0(sprintf("%1.0f",as.numeric(table1_j[j,i])), " (", sprintf("%1.0f",as.numeric(table1_SD_j[j,i])),")")
  }
  for(j in c(2,4:6,10:15,17:27)){
    table1_j[j,i]=paste0(table1_N[j,i], " (",sprintf("%1.0f",as.numeric(table1_j[j,i])*100), "\\%)")
  } 
}
table1_j
table1=table1_j

blank=rep("",8)

table1_f=   rbind(
               c("\\textbf{Maternal}",blank),
               table1[c(1:4),],
               c( "\\textbf{Paternal}",blank),
               table1[c(5:6),],
               c("\\textbf{Household}",blank),
               table1[c(7:14),],
               c("\\textbf{Drinking Water}",blank),
               table1[c(15:17),],
               c("\\textbf{Sanitation}",blank),
               c("Always or usually use primary toilet for defecation",blank),
               table1[c(18:19),],
               c("Daily defecating in the open",blank),
               table1[c(20:21),],
               c("Latrine",blank),
               table1[c(22:24),],
               c("\\textbf{Handwashing location}",blank),
               table1[c(25:26),],
               c("\\textbf{Nutrition}",blank),
               table1[c(27),])

rownames(table1_f)=NULL

for(i in c(2:5,7:8,10:17,19:21,24:25,27:28,30:32,34:35,37)){
  table1_f$lab[i]=paste("~~~",table1_f$lab[i],sep="")
}
for(i in c(24:25,27:28,30:31)){
  table1_f$lab[i]=paste("~~~~~",table1_f$lab[i],sep="")
}

save(n.hh.f,table1_f,file="C:/Users/andre/Dropbox/WBK-primary-analysis/Results/jade/table1_f.RData")



