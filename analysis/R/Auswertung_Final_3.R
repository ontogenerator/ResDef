rm(list=ls())
setwd("C:/Users/AG_Winter/Documents/2014-Sabine/2014_LaSelva/Rohdaten")
getwd()

# Directory for home computer
#setwd("C:/Users/sabin_000/Documents/2014_LaSelva/Auswertung/Statistik")
#getwd()

require(reshape2)
require(dplyr)
require(SamplingStrata)
require (lme4)
require(MCMCglmm)
require(ggplot2)
require(extrafont)
require(extrafontdb)
library(extrafont)
require(lattice)

font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
#fonts() 

# DATA IMPORT & PREPARATION
#-------------------------------------------------------------------------------------------------------

flowers <- read.csv2("Flowers.csv", sep=";")
flowers_flex <- read.csv2("Flowers_flex.csv", sep=";")

# Part1 - 50% Reward Probability
############
titles <- c("DateTime","IdRFID","IdLabel","unitLabel","eventDuration","sense1duration","sense1Events",
            "senseRFIDrecords","reinforce1value","reinforce1Total","reinforce1Account","outFuncLabel",
            "outLabel", "SystemMsg","MsgValue1","MsgValue2","MsgValue3","Dummy") 


group1_p1_d2 <- read.csv2("Gruppe1_Teil1_Tag2-14.08.27.csv", sep=";", dec = ",", header=TRUE,
                   fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group1_p1_d3 <- read.csv2("Gruppe1_Teil1_Tag3-14.08.28.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group1_p1_d4 <- read.csv2("Gruppe1_Teil1_Tag4-14.08.29.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group1_p1_d5 <- read.csv2("Gruppe1_Teil1_Tag5-14.08.30.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group2_p1_d2 <- read.csv2("Gruppe2_Teil1_Tag2-14.08.28.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group2_p1_d3 <- read.csv2("Gruppe2_Teil1_Tag3-14.08.29.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group2_p1_d4 <- read.csv2("Gruppe2_Teil1_Tag4-14.08.30.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group2_p1_d5 <- read.csv2("Gruppe2_Teil1_Tag5-14.08.31.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group3_p1_d2 <- read.csv2("Gruppe3_Teil1_Tag2-14.09.24.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group3_p1_d3 <- read.csv2("Gruppe3_Teil1_Tag3-14.09.25.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group3_p1_d4 <- read.csv2("Gruppe3_Teil1_Tag4-14.09.26.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group3_p1_d5 <- read.csv2("Gruppe3_Teil1_Tag5-14.09.27.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group4_p1_d2 <- read.csv2("Gruppe4_Teil1_Tag2-14.09.25.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group4_p1_d3 <- read.csv2("Gruppe4_Teil1_Tag3-14.09.26.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group4_p1_d4 <- read.csv2("Gruppe4_Teil1_Tag4-14.09.27.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group4_p1_d5 <- read.csv2("Gruppe4_Teil1_Tag5-14.09.28.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

Inds <- c("Ind37","Ind38","Ind39","Ind40","Ind41","Ind42","Ind43","Ind44")

# In Group4 are 4 Individuals that participtated before

group4_p1_d2<-filter(group4_p1_d2, IdLabel%in%Inds)
group4_p1_d3<-filter(group4_p1_d3, IdLabel%in%Inds)  
group4_p1_d4<-filter(group4_p1_d4, IdLabel%in%Inds)
group4_p1_d5<-filter(group4_p1_d5, IdLabel%in%Inds)
rm(Inds)

group1_p1_d2$Day <- 1
group1_p1_d3$Day <- 2 
group1_p1_d4$Day <- 3
group1_p1_d5$Day <- 4
group2_p1_d2$Day <- 1 
group2_p1_d3$Day <- 2  
group2_p1_d4$Day <- 3 
group2_p1_d5$Day <- 4 
group3_p1_d2$Day <- 1 
group3_p1_d3$Day <- 2  
group3_p1_d4$Day <- 3 
group3_p1_d5$Day <- 4 
group4_p1_d2$Day <- 1 
group4_p1_d3$Day <- 2  
group4_p1_d4$Day <- 3 
group4_p1_d5$Day <- 4 

group1_p1_d2$Cage <- 1
group1_p1_d3$Cage <- 1
group1_p1_d4$Cage <- 1
group1_p1_d5$Cage <- 1
group2_p1_d2$Cage <- 2
group2_p1_d3$Cage <- 2  
group2_p1_d4$Cage <- 2 
group2_p1_d5$Cage <- 2
group3_p1_d2$Cage <- 3 
group3_p1_d3$Cage <- 3  
group3_p1_d4$Cage <- 3
group3_p1_d5$Cage <- 3
group4_p1_d2$Cage <- 4 
group4_p1_d3$Cage <- 4  
group4_p1_d4$Cage <- 4 
group4_p1_d5$Cage <- 4 


data_p1 <- rbind(group1_p1_d2,group1_p1_d3,group1_p1_d4,group1_p1_d5,group2_p1_d2,group2_p1_d3,
                  group2_p1_d4,group2_p1_d5,group3_p1_d2,group3_p1_d3,group3_p1_d4,group3_p1_d5,
                  group4_p1_d2,group4_p1_d3,group4_p1_d4,group4_p1_d5)
rm(group1_p1_d2,group1_p1_d3,group1_p1_d4,group1_p1_d5,group2_p1_d2,group2_p1_d3,
                  group2_p1_d4,group2_p1_d5,group3_p1_d2,group3_p1_d3,group3_p1_d4,group3_p1_d5,
                  group4_p1_d2,group4_p1_d3,group4_p1_d4,group4_p1_d5,titles)

data_p1$DateTime <- sub(",",".",data_p1$DateTime) 


data_p1$DateTime <- as.character(as.POSIXct(as.numeric(data_p1$DateTime) * (60*60*24),
                         origin="1899-12-30", tz="GMT")) # convert to POSIXct (R DateTime format)

data_p1 <- data_p1 [,-c(7,8,10,12,13,14,15,16,17,18)]


# VISITS (1 if it is a real visit (= Condmod + Eventduration > 300ms))

data_p1_rel <- filter(data_p1,substr(data_p1[,3],1,3)=="Ind",substr(data_p1[,4],1,4)=="Cond",
                      eventDuration >300)

# New column as FLOWER

lut<- c("CondMod1"= 1,"CondMod2"= 2,"CondMod3"= 3,"CondMod4"= 4,"CondMod5"= 5,"CondMod6"= 6,
        "CondMod7"= 7,"CondMod8"= 8,"CondMod9"= 9,"CondMod10"= 10,"CondMod11"= 11,"CondMod12"= 12)
flow <- data_p1_rel$unitLabel
flow <- lut[flow]
data_p1_rel$Flower <- flow
rm(flow,lut)
# New colomn as HOUR

data_p1_new <- data.frame(DateTime=data_p1_rel$DateTime,
                time=format(as.POSIXct(data_p1_rel$DateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))

data_p1_rel <- cbind(data_p1_rel, data_p1_new[,2])
colnames(data_p1_rel)[12]<- "Hour"
rm(data_p1_new)


# Only relevant data

time <- c("18","19","20","21","22","23","00","01","02","03","04","05")
data_p1_rel<-filter(data_p1_rel, Hour%in%time)
rm(time)

data_p1_rel <- data_p1_rel [,-c(2,6,8)]

# VISITONACTIVE (1 if it is a real visit at the active flower for eacht individual)

day <- 0
Indiv <- 0
Fl <- 0
data_p1_rel$VisitonActive <- 0

for (i in 1:length(data_p1_rel$Day))
{
  day <- data_p1_rel[i,6]
  Indiv <- data_p1_rel[i,2]
  a <- filter(flowers,Ind== Indiv)
  a <- filter (a, Day == day)
  ifelse( data_p1_rel[i,8]== a[1,3]|data_p1_rel[i,8]== a[1,4], data_p1_rel[i,10]<-1,
          data_p1_rel[i,10]<-0)
}

rm(a,day,Fl, i, Indiv)

# VISITCOUNTER

data_p1_rel <- arrange(data_p1_rel,IdLabel,Day)
data_p1_rel$VisitCounter <- 1

for (i in 2:length(data_p1_rel$IdLabel))
{
  ifelse(data_p1_rel[i,6]== data_p1_rel[i-1,6], data_p1_rel[i,11]<- data_p1_rel[i-1,11]+1,
         data_p1_rel[i,11]<-1 )
  }

data_p1_rel <- arrange(data_p1_rel,DateTime)

# Data relevant for further calculations

data1 <- data_p1_rel[,c(2,6,7,10,11)]

# CONDITION

Inds1 <-c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind13","Ind14","Ind15","Ind16","Ind17","Ind18","Ind25",
          "Ind26","Ind27","Ind28","Ind29","Ind30","Ind37","Ind38","Ind39","Ind40","Ind41","Ind42")

Inds2 <-c("Ind7","Ind8","Ind9","Ind10","Ind11","Ind12","Ind19","Ind20","Ind21","Ind22","Ind23","Ind24",
          "Ind31","Ind32","Ind33","Ind34","Ind35","Ind36","Ind43","Ind44")

data1_Group1<-filter(data1, IdLabel%in%Inds1)
data1_Group1$Condition <- 0.5
data1_Group1$Group <- 1

data1_Group2<-filter(data1, IdLabel%in%Inds2)
data1_Group2$Condition <- 0.5
data1_Group2$Group <- 2

data1 <- rbind(data1_Group1,data1_Group2)
rm(data1_Group1,data1_Group2,Inds1,Inds2)

#-------------------------------------------------------------------------------------------------------
# Part2 - 30%/83% Reward Probability
############


titles <- c("DateTime","IdRFID","IdLabel","unitLabel","eventDuration","sense1duration","sense1Events",
            "senseRFIDrecords","reinforce1value","reinforce1Total","reinforce1Account","outFuncLabel",
            "outLabel", "SystemMsg","MsgValue1","MsgValue2","MsgValue3","Dummy") 


group1_p2_d1 <- read.csv2("Gruppe1_Teil2_Tag1-14.09.01.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group1_p2_d2 <- read.csv2("Gruppe1_Teil2_Tag2-14.09.02.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group1_p2_d3 <- read.csv2("Gruppe1_Teil2_Tag3-14.09.03.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group1_p2_d4 <- read.csv2("Gruppe1_Teil2_Tag4-14.09.04.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group2_p2_d1 <- read.csv2("Gruppe2_Teil2_Tag1-14.09.02.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group2_p2_d2 <- read.csv2("Gruppe2_Teil2_Tag2-14.09.03.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group2_p2_d3 <- read.csv2("Gruppe2_Teil2_Tag3-14.09.04.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group2_p2_d4 <- read.csv2("Gruppe2_Teil2_Tag4-14.09.05.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group3_p2_d1 <- read.csv2("Gruppe3_Teil2_Tag1-14.09.29.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group3_p2_d2 <- read.csv2("Gruppe3_Teil2_Tag2-14.09.30.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group3_p2_d3 <- read.csv2("Gruppe3_Teil2_Tag3-14.10.01.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group3_p2_d4 <- read.csv2("Gruppe3_Teil2_Tag4-14.10.02.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group4_p2_d1 <- read.csv2("Gruppe4_Teil2_Tag1-14.09.30.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group4_p2_d2 <- read.csv2("Gruppe4_Teil2_Tag2-14.10.01.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group4_p2_d3 <- read.csv2("Gruppe4_Teil2_Tag3-14.10.02.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group4_p2_d4 <- read.csv2("Gruppe4_Teil2_Tag4-14.10.03.csv", sep=";", dec = ",", header=TRUE,
                           fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

# In Group4 are 4 Individuals that participtated before
Inds <- c("Ind37","Ind38","Ind39","Ind40","Ind41","Ind42","Ind43","Ind44")
group4_p2_d1<-filter(group4_p2_d1, IdLabel%in%Inds)
group4_p2_d2<-filter(group4_p2_d2, IdLabel%in%Inds)  
group4_p2_d3<-filter(group4_p2_d3, IdLabel%in%Inds)
group4_p2_d4<-filter(group4_p2_d4, IdLabel%in%Inds)

group1_p2_d1$Day <- 5
group1_p2_d2$Day <- 6 
group1_p2_d3$Day <- 7
group1_p2_d4$Day <- 8
group2_p2_d1$Day <- 5 
group2_p2_d2$Day <- 6  
group2_p2_d3$Day <- 7 
group2_p2_d4$Day <- 8 
group3_p2_d1$Day <- 5 
group3_p2_d2$Day <- 6  
group3_p2_d3$Day <- 7 
group3_p2_d4$Day <- 8 
group4_p2_d1$Day <- 5 
group4_p2_d2$Day <- 6  
group4_p2_d3$Day <- 7 
group4_p2_d4$Day <- 8 

group1_p2_d1$Cage <- 1
group1_p2_d2$Cage <- 1
group1_p2_d3$Cage <- 1
group1_p2_d4$Cage <- 1
group2_p2_d1$Cage <- 2
group2_p2_d2$Cage <- 2  
group2_p2_d3$Cage <- 2 
group2_p2_d4$Cage <- 2
group3_p2_d1$Cage <- 3 
group3_p2_d2$Cage <- 3  
group3_p2_d3$Cage <- 3
group3_p2_d4$Cage <- 3
group4_p2_d1$Cage <- 4 
group4_p2_d2$Cage <- 4  
group4_p2_d3$Cage <- 4 
group4_p2_d4$Cage <- 4 


data_p2 <- rbind(group1_p2_d2,group1_p2_d3,group1_p2_d4,group1_p2_d1,group2_p2_d2,group2_p2_d3,
                  group2_p2_d4,group2_p2_d1,group3_p2_d2,group3_p2_d3,group3_p2_d4,group3_p2_d1,
                  group4_p2_d2,group4_p2_d3,group4_p2_d4,group4_p2_d1)
rm(group1_p2_d2,group1_p2_d3,group1_p2_d4,group1_p2_d1,group2_p2_d2,group2_p2_d3,
                 group2_p2_d4,group2_p2_d1,group3_p2_d2,group3_p2_d3,group3_p2_d4,group3_p2_d1,
                 group4_p2_d2,group4_p2_d3,group4_p2_d4,group4_p2_d1,titles)

data_p2$DateTime <- sub(",",".",data_p2$DateTime) 


data_p2$DateTime <- as.character(as.POSIXct(as.numeric(data_p2$DateTime) * (60*60*24),
                                             origin="1899-12-30", tz="GMT")) # convert to POSIXct (R DateTime format)

data_p2 <- data_p2 [,-c(7,8,10,12,13,14,15,16,17,18)]


# VISITS (1 if it is a real visit (= Condmod + Eventduration > 300ms))

data_p2_rel <- filter(data_p2,substr(data_p2[,3],1,3)=="Ind",substr(data_p2[,4],1,4)=="Cond",
                      eventDuration >300)

# New column as FLOWER

lut<- c("CondMod1"= 1,"CondMod2"= 2,"CondMod3"= 3,"CondMod4"= 4,"CondMod5"= 5,"CondMod6"= 6,
        "CondMod7"= 7,"CondMod8"= 8,"CondMod9"= 9,"CondMod10"= 10,"CondMod11"= 11,"CondMod12"= 12)
flow <- data_p2_rel$unitLabel
flow <- lut[flow]
data_p2_rel$Flower <- flow
rm(flow,lut)
# New colomn as HOUR

data_p2_new <- data.frame(DateTime=data_p2_rel$DateTime,
                    time=format(as.POSIXct(data_p2_rel$DateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))

data_p2_rel <- cbind(data_p2_rel, data_p2_new[,2])
colnames(data_p2_rel)[12]<- "Hour"
rm(data_p2_new)


# Only relevant data

time <- c("18","19","20","21","22","23","00","01","02","03","04","05")
data_p2_rel<-filter(data_p2_rel, Hour%in%time)
rm(time)

data_p2_rel <- data_p2_rel [,-c(2,6,8)]

# VISITONACTIVE (1 if it is a real visit at the active flower for eacht individual)

day <- 0
Indiv <- 0
Fl <- 0
data_p2_rel$VisitonActive <- 0

for (i in 1:length(data_p2_rel$Day))
{
  day <- data_p2_rel[i,6]
  Indiv <- data_p2_rel[i,2]
  a <- filter(flowers,Ind== Indiv)
  a <- filter (a, Day == day)
  ifelse( data_p2_rel[i,8]== a[1,3]|data_p2_rel[i,8]== a[1,4], data_p2_rel[i,10]<-1,
          data_p2_rel[i,10]<-0)
}

rm(a,day,Fl, i, Indiv)

# VISITCOUNTER

data_p2_rel <- arrange(data_p2_rel,IdLabel,Day)
data_p2_rel$VisitCounter <- 1

for (i in 2:length(data_p2_rel$IdLabel))
{
  ifelse(data_p2_rel[i,6]== data_p2_rel[i-1,6], data_p2_rel[i,11]<- data_p2_rel[i-1,11]+1,
         data_p2_rel[i,11]<-1 )
}

data_p2_rel <- arrange(data_p2_rel,DateTime)

# Data relevant for further calculations

data2 <- data_p2_rel[,c(2,6,7,10,11)]

# CONDITION

Inds1 <-c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind13","Ind14","Ind15","Ind16","Ind17","Ind18","Ind25",
          "Ind26","Ind27","Ind28","Ind29","Ind30","Ind37","Ind38","Ind39","Ind40","Ind41","Ind42")

Inds2 <-c("Ind7","Ind8","Ind9","Ind10","Ind11","Ind12","Ind19","Ind20","Ind21","Ind22","Ind23","Ind24",
          "Ind31","Ind32","Ind33","Ind34","Ind35","Ind36","Ind43","Ind44")

data2_Group1<-filter(data2, IdLabel%in%Inds1)
data2_Group1$Condition <- 0.3
data2_Group1$Group <- 1

data2_Group2<-filter(data2, IdLabel%in%Inds2)
data2_Group2$Condition <- 0.83
data2_Group2$Group <- 2

data2 <- rbind(data2_Group1,data2_Group2)
rm(data2_Group1,data2_Group2,Inds1,Inds2)

#------------------------------------------------------------------------------------------------------
# Part3 - 83%/30% Reward Probability
#####

titles <- c("DateTime","IdRFID","IdLabel","unitLabel","eventDuration","sense1duration","sense1Events",
            "senseRFIDrecords","reinforce1value","reinforce1Total","reinforce1Account","outFuncLabel",
            "outLabel", "SystemMsg","MsgValue1","MsgValue2","MsgValue3","Dummy") 

group1_p3_d1 <- read.csv2("Gruppe1_Teil3_Tag1-14.09.06.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group1_p3_d2 <- read.csv2("Gruppe1_Teil3_Tag2-14.09.07.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group1_p3_d3 <- read.csv2("Gruppe1_Teil3_Tag3-14.09.08.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group1_p3_d4 <- read.csv2("Gruppe1_Teil3_Tag4-14.09.09.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group2_p3_d1 <- read.csv2("Gruppe2_Teil3_Tag1-14.09.07.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group2_p3_d2 <- read.csv2("Gruppe2_Teil3_Tag2-14.09.08.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group2_p3_d3 <- read.csv2("Gruppe2_Teil3_Tag3-14.09.09.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group2_p3_d4 <- read.csv2("Gruppe2_Teil3_Tag4_W-14.09.12.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group3_p3_d1 <- read.csv2("Gruppe3_Teil3_Tag1-14.10.04.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group3_p3_d2 <- read.csv2("Gruppe3_Teil3_Tag2-14.10.05.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group3_p3_d3 <- read.csv2("Gruppe3_Teil3_Tag3-14.10.06.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group3_p3_d4 <- read.csv2("Gruppe3_Teil3_Tag4-14.10.07.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group4_p3_d1 <- read.csv2("Gruppe4_Teil3_Tag1-14.10.05.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group4_p3_d2 <- read.csv2("Gruppe4_Teil3_Tag2-14.10.06.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group4_p3_d3 <- read.csv2("Gruppe4_Teil3_Tag3-14.10.07.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group4_p3_d4 <- read.csv2("Gruppe4_Teil3_Tag4-14.10.08.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

# In Group4 are 4 Individuals that participtated before

Inds <- c("Ind37","Ind38","Ind39","Ind40","Ind41","Ind42","Ind43","Ind44")
group4_p3_d1<-filter(group4_p3_d1, IdLabel%in%Inds)
group4_p3_d2<-filter(group4_p3_d2, IdLabel%in%Inds)  
group4_p3_d3<-filter(group4_p3_d3, IdLabel%in%Inds)
group4_p3_d4<-filter(group4_p3_d4, IdLabel%in%Inds)

group1_p3_d1$Day <- 9
group1_p3_d2$Day <- 10 
group1_p3_d3$Day <- 11
group1_p3_d4$Day <- 12
group2_p3_d1$Day <- 9 
group2_p3_d2$Day <- 10  
group2_p3_d3$Day <- 11 
group2_p3_d4$Day <- 12
group3_p3_d1$Day <- 9
group3_p3_d2$Day <- 10  
group3_p3_d3$Day <- 11
group3_p3_d4$Day <- 12
group4_p3_d1$Day <- 9
group4_p3_d2$Day <- 10  
group4_p3_d3$Day <- 11
group4_p3_d4$Day <- 12

group1_p3_d1$Cage <- 1
group1_p3_d2$Cage <- 1
group1_p3_d3$Cage <- 1
group1_p3_d4$Cage <- 1
group2_p3_d1$Cage <- 2
group2_p3_d2$Cage <- 2  
group2_p3_d3$Cage <- 2 
group2_p3_d4$Cage <- 2
group3_p3_d1$Cage <- 3 
group3_p3_d2$Cage <- 3  
group3_p3_d3$Cage <- 3
group3_p3_d4$Cage <- 3
group4_p3_d1$Cage <- 4 
group4_p3_d2$Cage <- 4  
group4_p3_d3$Cage <- 4 
group4_p3_d4$Cage <- 4 


data_p3 <- rbind(group1_p3_d2,group1_p3_d3,group1_p3_d4,group1_p3_d1,group2_p3_d2,group2_p3_d3,
                 group2_p3_d4,group2_p3_d1,group3_p3_d2,group3_p3_d3,group3_p3_d4,group3_p3_d1,
                 group4_p3_d2,group4_p3_d3,group4_p3_d4,group4_p3_d1)
rm(group1_p3_d2,group1_p3_d3,group1_p3_d4,group1_p3_d1,group2_p3_d2,group2_p3_d3,
   group2_p3_d4,group2_p3_d1,group3_p3_d2,group3_p3_d3,group3_p3_d4,group3_p3_d1,
   group4_p3_d2,group4_p3_d3,group4_p3_d4,group4_p3_d1,titles)

data_p3$DateTime <- sub(",",".",data_p3$DateTime) 


data_p3$DateTime <- as.character(as.POSIXct(as.numeric(data_p3$DateTime) * (60*60*24),
                              origin="1899-12-30", tz="GMT")) # convert to POSIXct (R DateTime format)

data_p3 <- data_p3 [,-c(7,8,10,12,13,14,15,16,17,18)]


# VISITS (1 if it is a real visit (= Condmod + Eventduration > 300ms))

data_p3_rel <- filter(data_p3,substr(data_p3[,3],1,3)=="Ind",substr(data_p3[,4],1,4)=="Cond",
                      eventDuration >300)

# New column as FLOWER

lut<- c("CondMod1"= 1,"CondMod2"= 2,"CondMod3"= 3,"CondMod4"= 4,"CondMod5"= 5,"CondMod6"= 6,
        "CondMod7"= 7,"CondMod8"= 8,"CondMod9"= 9,"CondMod10"= 10,"CondMod11"= 11,"CondMod12"= 12)
flow <- data_p3_rel$unitLabel
flow <- lut[flow]
data_p3_rel$Flower <- flow
rm(flow,lut)

# New colomn as HOUR

data_p3_new <- data.frame(DateTime=data_p3_rel$DateTime,
                  time=format(as.POSIXct(data_p3_rel$DateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))

data_p3_rel <- cbind(data_p3_rel, data_p3_new[,2])
colnames(data_p3_rel)[12]<- "Hour"
rm(data_p3_new)


# Only relevant data

time <- c("18","19","20","21","22","23","00","01","02","03","04","05")
data_p3_rel<-filter(data_p3_rel, Hour%in%time)
rm(time)

data_p3_rel <- data_p3_rel [,-c(2,6,8)]

# VISITONACTIVE (1 if it is a real visit at the active flower for eacht individual)

day <- 0
Indiv <- 0
Fl <- 0
data_p3_rel$VisitonActive <- 0

for (i in 1:length(data_p3_rel$Day))
{
  day <- data_p3_rel[i,6]
  Indiv <- data_p3_rel[i,2]
  a <- filter(flowers,Ind== Indiv)
  a <- filter (a, Day == day)
  ifelse( data_p3_rel[i,8]== a[1,3]|data_p3_rel[i,8]== a[1,4], data_p3_rel[i,10]<-1,
          data_p3_rel[i,10]<-0)
}

rm(a,day,Fl, i, Indiv)

# VISITCOUNTER

data_p3_rel <- arrange(data_p3_rel,IdLabel,Day)
data_p3_rel$VisitCounter <- 1

for (i in 2:length(data_p3_rel$IdLabel))
{
  ifelse(data_p3_rel[i,6]== data_p3_rel[i-1,6], data_p3_rel[i,11]<- data_p3_rel[i-1,11]+1,
         data_p3_rel[i,11]<-1 )
}

data_p3_rel <- arrange(data_p3_rel,DateTime)

# Data relevant for further calculations

data3 <- data_p3_rel[,c(2,6,7,10,11)]

# CONDITION

Inds1 <-c("Ind1","Ind2","Ind3","Ind4","Ind5","Ind6","Ind13","Ind14","Ind15","Ind16","Ind17","Ind18","Ind25",
          "Ind26","Ind27","Ind28","Ind29","Ind30","Ind37","Ind38","Ind39","Ind40","Ind41","Ind42")

Inds2 <-c("Ind7","Ind8","Ind9","Ind10","Ind11","Ind12","Ind19","Ind20","Ind21","Ind22","Ind23","Ind24",
          "Ind31","Ind32","Ind33","Ind34","Ind35","Ind36","Ind43","Ind44")

data3_Group1<-filter(data3, IdLabel%in%Inds1)
data3_Group1$Condition <- 0.83
data3_Group1$Group <- 1

data3_Group2<-filter(data3, IdLabel%in%Inds2)
data3_Group2$Condition <- 0.3
data3_Group2$Group <- 2

data3 <- rbind(data3_Group1,data3_Group2)
rm(data3_Group1,data3_Group2,Inds1,Inds2)
rm(Inds,i)

#------------------------------------------------------------------------------------------------------
# Part-Flexibility
#######

titles <- c("DateTime","IdRFID","IdLabel","unitLabel","eventDuration","sense1duration","sense1Events",
            "senseRFIDrecords","reinforce1value","reinforce1Total","reinforce1Account","outFuncLabel",
            "outLabel", "SystemMsg","MsgValue1","MsgValue2","MsgValue3","Dummy") 

group1_flex_d1 <- read.csv2("Gruppe1_Flex_Tag1-14.09.12.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group1_flex_d2 <- read.csv2("Gruppe1_Flex_Tag2-14.09.13.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group1_flex_d3 <- read.csv2("Gruppe1_Flex_Tag3-14.09.14.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group1_flex_d4 <- read.csv2("Gruppe1_Flex_Tag4-14.09.15.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group2_flex_d1 <- read.csv2("Gruppe2_Flex_Tag1-14.09.15.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group2_flex_d2 <- read.csv2("Gruppe2_Flex_Tag2-14.09.16.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group2_flex_d3 <- read.csv2("Gruppe2_Flex_Tag3-14.09.17.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group2_flex_d4 <- read.csv2("Gruppe2_Flex_Tag4-14.09.18.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group3_flex_d1 <- read.csv2("Gruppe3_Flex_Tag1-14.10.10.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group3_flex_d2 <- read.csv2("Gruppe3_Flex_Tag2-14.10.11.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group3_flex_d3 <- read.csv2("Gruppe3_Flex_Tag3-14.10.12.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group3_flex_d4 <- read.csv2("Gruppe3_Flex_Tag4-14.10.13.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

group4_flex_d1 <- read.csv2("Gruppe4_Flex_Tag1-14.10.11.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles) 
group4_flex_d2 <- read.csv2("Gruppe4_Flex_Tag2-14.10.12.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group4_flex_d3 <- read.csv2("Gruppe4_Flex_Tag3-14.10.13.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)
group4_flex_d4 <- read.csv2("Gruppe4_Flex_Tag4-14.10.14.csv", sep=";", dec = ",", header=TRUE,
                          fileEncoding="UTF-16LE",as.is = T, row.names=NULL,col.names = titles)

# In Group4 are 4 Individuals that participtated before

Inds <- c("Ind37","Ind38","Ind39","Ind40","Ind41","Ind42","Ind43","Ind44")
group4_flex_d1<-filter(group4_flex_d1, IdLabel%in%Inds)
group4_flex_d2<-filter(group4_flex_d2, IdLabel%in%Inds)  
group4_flex_d3<-filter(group4_flex_d3, IdLabel%in%Inds)
group4_flex_d4<-filter(group4_flex_d4, IdLabel%in%Inds)

group1_flex_d1$Day <- 13
group1_flex_d2$Day <- 14 
group1_flex_d3$Day <- 15
group1_flex_d4$Day <- 16
group2_flex_d1$Day <- 13
group2_flex_d2$Day <- 14  
group2_flex_d3$Day <- 15 
group2_flex_d4$Day <- 16
group3_flex_d1$Day <- 13
group3_flex_d2$Day <- 14  
group3_flex_d3$Day <- 15
group3_flex_d4$Day <- 16
group4_flex_d1$Day <- 13
group4_flex_d2$Day <- 14  
group4_flex_d3$Day <- 15
group4_flex_d4$Day <- 16

group1_flex_d1$Cage <- 1
group1_flex_d2$Cage <- 1
group1_flex_d3$Cage <- 1
group1_flex_d4$Cage <- 1
group2_flex_d1$Cage <- 2
group2_flex_d2$Cage <- 2  
group2_flex_d3$Cage <- 2 
group2_flex_d4$Cage <- 2
group3_flex_d1$Cage <- 3 
group3_flex_d2$Cage <- 3  
group3_flex_d3$Cage <- 3
group3_flex_d4$Cage <- 3
group4_flex_d1$Cage <- 4 
group4_flex_d2$Cage <- 4  
group4_flex_d3$Cage <- 4 
group4_flex_d4$Cage <- 4 


data_flex <- rbind(group1_flex_d2,group1_flex_d3,group1_flex_d4,group1_flex_d1,group2_flex_d2,group2_flex_d3,
                 group2_flex_d4,group2_flex_d1,group3_flex_d2,group3_flex_d3,group3_flex_d4,group3_flex_d1,
                 group4_flex_d2,group4_flex_d3,group4_flex_d4,group4_flex_d1)
rm(group1_flex_d2,group1_flex_d3,group1_flex_d4,group1_flex_d1,group2_flex_d2,group2_flex_d3,
   group2_flex_d4,group2_flex_d1,group3_flex_d2,group3_flex_d3,group3_flex_d4,group3_flex_d1,
   group4_flex_d2,group4_flex_d3,group4_flex_d4,group4_flex_d1,titles)

data_flex$DateTime <- sub(",",".",data_flex$DateTime) 


data_flex$DateTime <- as.character(as.POSIXct(as.numeric(data_flex$DateTime) * (60*60*24),
                            origin="1899-12-30", tz="GMT")) # convert to POSIXct (R DateTime format)

data_flex <- data_flex[,-c(7,8,10,12,13,14,16,17,18)]


# VISITS (1 if it is a real visit (= Condmod + Eventduration > 300ms))

data_flex$Visit <- 0
for (i in 1:length(data_flex$DateTime))
{
  ifelse(substr(data_flex[i,3],1,3)=="Ind" && substr(data_flex[i,4],1,4) =="Cond" && data_flex[i,5] > 300,
         data_flex[i,12]<-1,data_flex[i,12]<-0)

}

for (i in 1:length(data_flex$DateTime))
{
  ifelse(data_flex[i,9]=="switch",
         data_flex[i,12]<-2,data_flex[i,12]<-data_flex[i,12])
  
}

# New column as FLOWER

lut<- c("CondMod1"= 1,"CondMod2"= 2,"CondMod3"= 3,"CondMod4"= 4,"CondMod5"= 5,"CondMod6"= 6,
        "CondMod7"= 7,"CondMod8"= 8,"CondMod9"= 9,"CondMod10"= 10,"CondMod11"= 11,"CondMod12"= 12)
flow <- data_flex$unitLabel
flow <- lut[flow]
data_flex$Flower <- flow
rm(flow,lut)

# New colomn as HOUR

data_flex_new <- data.frame(DateTime=data_flex$DateTime,
                          time=format(as.POSIXct(data_flex$DateTime, format="%Y-%m-%d %H:%M:%S"), format="%H"))

data_flex <- cbind(data_flex, data_flex_new[,2])
colnames(data_flex)[14]<- "Hour"
rm(data_flex_new)


# Only relevant data
now <- c(1,2)
data_flex_rel <- filter(data_flex,Visit%in%now)
rm(now)

time <- c("18","19","20","21","22","23","00","01","02","03","04","05")
data_flex_rel<-filter(data_flex_rel, Hour%in%time)
rm(time)

data_flex_rel <- data_flex_rel [,-c(2,6,8,9)]

# VISITCOUNTER

data_flex_rel <- arrange(data_flex_rel,IdLabel,Day)
data_flex_rel$VisitCounter <- 1

for (i in 2:length(data_flex_rel$IdLabel))
{
  ifelse(data_flex_rel[i,6]== data_flex_rel[i-1,6] && data_flex_rel[i,8]<2, 
         data_flex_rel[i,11]<- data_flex_rel[i-1,11]+1,
         data_flex_rel[i,11]<-1 )
}

# creates a column that has 1 befor switch and 2 after switch

data_flex_rel$Part <- 1

for (i in 2:length(data_flex_rel$Day))
{
  ifelse(data_flex_rel[i,8]==2,data_flex_rel[i,12]<-2,
         ifelse(data_flex_rel[i,6]!=data_flex_rel[i-1,6], data_flex_rel[i,12]<-1,
           data_flex_rel[i,12]<-data_flex_rel[i-1,12]))
}


# VISITONACTIVE (1 if it is a real visit at the active flower for eacht individual)

day <- 0
Indiv <- 0

data_flex_rel$VisitonActive <- 0

for (i in 1:length(data_flex_rel$Day))
{
  day <- data_flex_rel[i,6]
  Indiv <- data_flex_rel[i,2]
  a <- filter(flowers_flex,Ind== Indiv)
  a <- filter (a, Day == day)
  ifelse( data_flex_rel[i,12]==1 && (data_flex_rel[i,9]== a[1,3]|data_flex_rel[i,9]== a[1,4]),
          data_flex_rel[i,13]<-1,
          ifelse(data_flex_rel[i,12]==2 && (data_flex_rel[i,9]== a[1,5]|data_flex_rel[i,9]== a[1,6]),
                 data_flex_rel[i,13]<-1,data_flex_rel[i,13] <- 0))
}

rm(a,day, i, Indiv)

#data_flex_rel <- arrange(data_flex_rel,DateTime)

# Spalte die einsen bei aktiven Blüten von Part1 macht
data_flex_rel$VisitonActive_Part1 <- 0

day <- 0
Indiv <- 0

for (i in 1:length(data_flex_rel$Day))
{
  day <- data_flex_rel[i,6]
  Indiv <- data_flex_rel[i,2]
  a <- filter(flowers_flex,Ind== Indiv)
  a <- filter (a, Day == day)
  ifelse(data_flex_rel[i,9]== a[1,3]|data_flex_rel[i,9]== a[1,4],
          data_flex_rel[i,14]<-1,data_flex_rel[i,14] <- 0)
}

rm(a,day, i, Indiv)

# Data relevant for further calculations

data4_fl <- data_flex_rel[,-c(1,3,4,5,10)]
rm(Inds)
#------------------------------------------------------------------------------------------------------
# Preparation for statistics
######

data <- rbind(data1,data2,data3)
rm(data1,data2,data3)

data <- arrange(data,IdLabel,Condition,Day)

breaks <- seq(0,3000,20)
data$groupedvisits <- as.numeric(cut(data$VisitCounter,breaks,include.lowest=TRUE))
rm(breaks)

# dataframe with number of visits on active flower per group of 20 visits
sum_data<- data.frame(summarise(group_by(data,IdLabel,Condition,Day,Group,Cage,groupedvisits),
                                sum(VisitonActive)))

colnames(sum_data)[7] <- "groupsum"



# Anzahl der Besuche in jeder Gruppe, um nachher zu wissen wie viele Besuche in der letzten Gruppe sind

sum_data2<-group_by(data,IdLabel,Condition,Day,Group,Cage,groupedvisits) %>%
  summarize(count = n())
sum_data2 <- as.data.frame(sum_data2)
sum_data<-cbind(sum_data,sum_data2[,7])
colnames(sum_data)[8]<-"Count"
rm(sum_data2)

# Daily mean of visits on active flowers per 20 visits
daily_mean <- data.frame(summarise(group_by(sum_data,IdLabel,Group,Condition,Day),mean(groupsum)))
colnames(daily_mean)[5] <- "dailycrit"
daily_mean <- filter(daily_mean,dailycrit>0)

# Macht in einer neuen Spalte (Overcrit) eine 1 wenn die Gruppensumme ueber dem 
# taeglichen Mittelwert liegt (Ind = aktuelles Individuum, d = aktueller Tag, a = Daten von aktuellem 
# Individuum, b = Daten von aktuellem Tag und aktuellem Ind, v = daily criterion)

for(i in 1:length(sum_data$Day))
{
  Ind = sum_data[i,1]
  d = sum_data[i,3]
  a<-data.frame(filter(daily_mean,IdLabel == Ind))
  b <- filter(a,Day==d)
  v <- b[1,5]
  
  sum_data[i,9] <-ifelse(sum_data[i,7]>v,1,0)
}
colnames(sum_data)[9] <- "overmean"

# Macht eine 1 immer wenn das Kriterium (zwei aufeinanderfolgende einsen in Overmean) 
# erreicht wird

for (i in 2: length(sum_data$Day))
{
  c <- sum_data[i,9]
  d <- sum_data[i-1,9]
  s<-c+d
  sum_data[i,10] <-ifelse(s>1,1,0)
}
sum_data[1,10]<- 0
colnames(sum_data)[10] <- "Overcrit"

# traegt eine 1 in jede Spalte ein, ab dem ersten Erreichen des kriteriums

vec <- array(data=0,dim=length(sum_data$Day))

for(i in 2:length(sum_data$Day))
  
{
  ifelse(sum_data[i,3]== sum_data[i-1,3],vec==vec,
         vec<-array(data=0,dim=length(sum_data$Day)))
  x<-sum(vec)
  ifelse( x>=1,
          sum_data[i,11]<-1,sum_data[i,11]<-0)
  vec[i]<-sum_data[i+1,10]  
  
}
sum_data[1,11]<- 0
colnames(sum_data)[11] <- "Asym"

rm(a,b,c,d,Ind,s,v,vec,x)

# Berechnet Successes(Besuche and inaktiven Blüten) und 
# Failueres(Besuche an aktiven Blüten) nach Erreichen des Kriteriums

data_temp <- filter(sum_data,Asym == 1)
data_temp_failures <- data.frame(summarize(group_by(data_temp,IdLabel,Group,Condition, Day,Cage),sum(groupsum)))
data_temp_totalvisits <- data.frame(summarize(group_by(data_temp,IdLabel,Group,Condition, Day,Cage),sum(Count)))

Sampling_data <- cbind(data_temp_failures,data_temp_totalvisits[,6])
colnames(Sampling_data)[c(6,7)] <- c("failures","totalvisits")
Sampling_data$successes <- Sampling_data$totalvisits-Sampling_data$failures

# successes <- filter(sum_data,Asym == 1)
# successes <- data.frame(summarise(group_by(successes,IdLabel,Group,Condition,Day),sum(groupsum)))
# 
# failures$totalvisits <- failures$sum.Asym. *20
# 
# combind <- data.frame(cbind(successes,failures$totalvisits))
# combind$successes <- combind[,6]-combind[,5]
# colnames(combind)[c(5,6,7)] <- c("failures","totalvisits","successes")
# 
# 
# 
# Sampling_data <- daily_mean[-c(259),] #daily_mean[-c(257,262),]
# Sampling_data <- cbind(Sampling_data,combind$failures,combind$successes)
# colnames(Sampling_data)[c(6,7)] <- c("failures","successes")

rm(data_temp,data_temp_failures,data_temp_totalvisits)

###############################################################################
#Analyse der ersten hundert Besuche nach Erreichen der Asymptote und der Besuche danach
# Nummerierung der Besuche während der Asymptote

sum_data$AsymNum<-0
b<-0

for(i in 2:length(sum_data$Day)-1)
{
  ifelse( sum_data[i,9]==1,
          b<-b+1,b<-0)
  ifelse(sum_data[i,3] == sum_data[i+1,3],sum_data[i,10]<-b, sum_data[i,10]<-0)
}
rm(b,i)

Asym_max <- data.frame(summarize(group_by(sum_data,IdLabel,Condition,Day),max(AsymNum)))
colnames(Asym_max)[4]<-"maxVisits"
Asym_max$halfAsym <- round(Asym_max$maxVisits/2) 

sum_data$Partition <- NA

# macht während der asymptotischen Phase eine 1, wenn Asymnum kleiner der Hälfte ist, ansonsten zwei
for (i in 1:length(sum_data$Day))
  
{
  Ind <- sum_data[i,1]
  d <- as.numeric(sum_data[i,3])
  b<- filter(Asym_max,IdLabel ==Ind,Day == d)
  half <- as.numeric(b[5])
  ifelse(sum_data[i,10] < half,sum_data[i,11]<-1,sum_data[i,11]<-2)
  if(sum_data[i,10]==0){sum_data[i,11]<-0}
  
}

# Daten der ersten Hälfte der Asymptote
Asym_data_first <- filter(sum_data, sum_data$Partition == 1)
Asym_data_first <- Asym_data_first[,-c(11)]

# Daten der Besuche nach den ersten hundert Besuchen
Asym_data_last <- filter(sum_data, sum_data$Partition == 2)
Asym_data_last <- Asym_data_last[,-c(11)]

sum_data <- sum_data[,-c(11)]

# Anzahl Failure und success für die erste Hälfte
Asym_data1 <- data.frame(summarize(group_by(Asym_data_first,IdLabel, Group, 
                                            Condition,Day),sum(groupsum)))
Asym_data2 <- data.frame(summarize(group_by(Asym_data_first,IdLabel, Group, 
                                            Condition,Day),sum(Asym)))
Asym_data2$totalvisits <- Asym_data2[,5]*20
Asym_data_first <- cbind(Asym_data1,Asym_data2[,6])
rm(Asym_data1,Asym_data2)
Asym_data_first$successes <- Asym_data_first[,6]-Asym_data_first[,5]
colnames(Asym_data_first)[c(5,6)]<-c("failures","totalvisits")

#-------------------------------------------------------------------------
# Analyse der Besuche der zweiten Hälfte

Asym_data_last1 <- data.frame(summarize(group_by(Asym_data_last,IdLabel, Group,Condition, Day),
                                        sum(groupsum)))
Asym_data_last2 <- data.frame(summarize(group_by(Asym_data_last,IdLabel, Group, 
                                                 Condition,Day),sum(Asym)))
Asym_data_last2$totalvisits <- Asym_data_last2[,5]*20
Asym_data_last <- cbind(Asym_data_last1,Asym_data_last2[,6])
rm(Asym_data_last1,Asym_data_last2)
Asym_data_last$successes <- Asym_data_last[,6]-Asym_data_last[,5]
colnames(Asym_data_last)[c(5,6)]<-c("failures","totalvisits")

#------------------------------------------------------------------------------------------------------

# additional data like weight,DEE and weight difference

add_data <- read.csv("Zusatzdaten.csv",sep = ";")
add_data <- arrange(add_data,Ind,Prob,Day)
#Tage entfernen, die in sampling_data und Asym_data_first nicht existieren
add_data_sampling <- add_data[-c(176,392,181,251,291,287,12,252,396), ] #add_data[-c(12,125,126,260,265,464), ]

Sampling_data <- cbind(Sampling_data,add_data_sampling$DEE,add_data_sampling$weight,
                       add_data_sampling$weightdiff)
# Asym_data_first <- cbind(Asym_data_first,add_data_sampling$DEE,add_data_sampling$weight,
#                          add_data_sampling$weightdiff)
# Asym_data_last <- cbind(Asym_data_last,add_data_sampling$DEE,
#                         add_data_sampling$weight,
#                         add_data_sampling$weightdiff)
colnames(Sampling_data)[c(9,10,11)]<-c("DEE","weight","weightdiff")
# colnames(Asym_data_first)[c(8,9,10)]<-c("DEE","weight","weightdiff")
# colnames(Asym_data_last)[c(8,9,10)]<-c("DEE","weight","weightdiff")
rm(add_data,add_data_sampling)

# DEE auf Gewicht standardisiert

Sampling_data$DEE_st <- Sampling_data[,9]/Sampling_data[,10]
# Asym_data_first$DEE_st <- Asym_data_first[,8]/Asym_data_first[,9]
# Asym_data_last$DEE_st <- Asym_data_last[,8]/Asym_data_last[,9]


Sampling_data[c(136,40,177),6]<-c(207,241,78)
Sampling_data[c(136,40,177),8]<-c(33,19,2) 




# Tage mit technischen Problemen entfernen

Sampling_data2 <- data.frame(Sampling_data[-c(394,515,28,29,134,390,518,21,34,46,142,499,511,
                                              65,102,174,195,197,48,51,61,75,111,176,186,199,
                                              105,122,192,193,255,289,227,274,328,340,352,
                                              333,407,440,443,379,363,385,451,89),])
# 89 ist nur weil kein weight für diesen Tag gemessen wurde


# Sampling-Mittelwert einfügen

Sampling_data2$Sampling <- (Sampling_data2$successes/(Sampling_data2$totalvisits))


#------------------------------------------------------------------------------------------------------
# STATISTIK
#####

#Prior für model mit random slope and intercept
prior.5 = list(R = list(V=diag(1), nu=0.002),
               G=list(G1=list(V=diag(2), nu=0.002, alpha.mu=c(0,0), 
                              alpha.V=diag(2)*10000000)))

# Prior für Model withour random effect
prior.2 = list(R = list(V=diag(1), nu=0.002))

#FULL MODEL
# UMM.samp -> whole dataset
UMM.samp <- MCMCglmm(cbind(successes,failures) ~ Condition+weight+Group+Cage, 
                     random = ~us(1+Condition):IdLabel,
                     #rcov=~idh(1-Condition):units,
                     data = Sampling_data2, family = "multinomial2",pr = TRUE,
                     prior = prior.5,verbose = FALSE,saveX = TRUE, saveZ = TRUE,
                     nitt = 200E3, thin = 100, burnin = 100E3)

summary(UMM.samp) #  fixed effects: only Condition is significant (DIC = 150328.1)

UMM.samp_Cond <- MCMCglmm(cbind(successes,failures) ~ weight+Group+Cage+Condition, 
                     #random = ~us(1+Condition):IdLabel,
                     #rcov=~idh(1-Condition):units,
                     data = Sampling_data2, family = "multinomial2",pr = TRUE,
                     prior = prior.2,verbose = FALSE,saveX = TRUE, saveZ = TRUE,
                     nitt = 200E3, thin = 100, burnin = 100E3)

summary(UMM.samp_Cond)
#DIC ohne Cage: 150328.4
#DIC ohne Group: 150328.4
#DIC ohne weight: 15032
#DIC ohne Condition: 15032

# Adjusted Repeatability
rep.samp <- UMM.samp$VCV[,1]/(UMM.samp$VCV[,1]+pi^2/3)
posterior.mode(rep.samp)   #repeatability estimate
HPDinterval(rep.samp)    #95% credibility interval

#-----
# Modelfit
#------------------------------------------------------------------------------------------------------
#Plotten von realer und gefitteter Daten
xyplot(Sampling+predict(UMM.samp, 
                        marginal = NULL,type = "response") ~ Condition | IdLabel,
       data = Sampling_data2, text =8,
       auto.key = list(text  = c("Data", "Prediction"),
                       lines = 1, points= F),type = c("p", "a"))

#trace plots

plot(UMM.samp)
plot(UMM.samp$VCV)
effectiveSize(UMM.samp$VCV)

# Autocorrelation
autocorr(UMM.samp$Sol)
autocorr(UMM.samp$VCV)

# Gelman diagnostic

UMM.samp2 <- MCMCglmm(cbind(successes,failures) ~ Condition+weight+Group+Cage, 
                      random = ~us(1+Condition):IdLabel,
                      #rcov=~idh(1-Condition):units,
                      data = Sampling_data2, family = "multinomial2",pr = TRUE,
                      prior = prior.5,verbose = FALSE,saveX = TRUE, saveZ = TRUE,
                      nitt = 200E3, thin = 100, burnin = 100E3)

summary(UMM.samp2)

gelman.diag(list(UMM.samp$Sol,UMM.samp2$Sol))
#----
# Repeatability für jede Wahrscheinlichkeit
#------------------------------------------------------------------------------------------------------

Samp_data0.3 <- filter(Sampling_data2, Condition == 0.3)
Samp_data0.5 <- filter(Sampling_data2, Condition == 0.5)
Samp_data0.83 <- filter(Sampling_data2, Condition == 0.83)

prior.3 = list(R = list(V = diag(1),nu=0.002), 
               G=list(G1=list(V=diag(1), nu=0.002, alpha.mu=c(0), 
                              alpha.V=diag(1)*10000000)))

UMM.samp_0.3<-MCMCglmm(cbind(successes,failures) ~ 1, 
                       random = ~IdLabel,
                       data = Samp_data0.3, family = "multinomial2",pr = TRUE,
                       prior = prior.3,verbose = FALSE,saveX = TRUE, saveZ = TRUE,
                       nitt = 200E3, thin = 100, burnin = 100E3)

summary(UMM.samp_0.3)

rep.samp<-(UMM.samp_0.3$VCV[,1]/
             (UMM.samp_0.3$VCV[,1]+(pi^2/3)))

posterior.mode(rep.samp)   #repeatability estimate
HPDinterval(rep.samp)    #95% credibility interval

#----
#Correlation Slope and Sampling at 83% reward probabilility
#-------------------------------------------------------------------------------------------------------
# Berechnung laut Dingemanse

corr <- UMM.samp$VCV[,2]/sqrt(UMM.samp$VCV[,1]*UMM.samp$VCV[,4])
posterior.mode(corr)
HPDinterval(corr)

#----
# Abbildung für jede Condition
#--------------------------------------------------------------------------------------------------------

Sampling0.3_mean <- as.data.frame(summarize(group_by(Samp_data0.3,IdLabel),mean(Sampling)))
Sampling0.5_mean <- as.data.frame(summarize(group_by(Samp_data0.5,IdLabel),mean(Sampling)))
Sampling0.83_mean <- as.data.frame(summarize(group_by(Samp_data0.83,IdLabel),mean(Sampling)))

Sampling0.3_sd <- as.data.frame(summarize(group_by(Samp_data0.3,IdLabel),sd(Sampling)))
Sampling0.5_sd <- as.data.frame(summarize(group_by(Samp_data0.5,IdLabel),sd(Sampling)))
Sampling0.83_sd <- as.data.frame(summarize(group_by(Samp_data0.83,IdLabel),sd(Sampling)))

Sampling0.3_sd$se <- Sampling0.3_sd[,2]/sqrt(44)
Sampling0.5_sd$se <- Sampling0.5_sd[,2]/sqrt(44)
Sampling0.83_sd$se <- Sampling0.83_sd[,2]/sqrt(44)

mean0.3 <- Sampling0.3_mean[,2]
se0.3 <- Sampling0.3_sd[,3]
mean0.5 <- Sampling0.5_mean[,2]
se0.5 <- Sampling0.5_sd[,3]
mean0.83 <- Sampling0.83_mean[,2]
se0.83 <- Sampling0.83_sd[,3]

# Define the top and bottom of the errorbars
limits <- aes(ymax = mean + sd, ymin=mean - sd)
dodge <- position_dodge(width=0.1)

data0.3 <- cbind(Sampling0.3_mean[,c(1,2)],Sampling0.3_sd[,2],Sampling0.3_sd[,3])
data0.5 <- cbind(Sampling0.5_mean[,c(1,2)],Sampling0.5_sd[,2],Sampling0.5_sd[,3])
data0.83 <- cbind(Sampling0.83_mean[,c(1,2)],Sampling0.83_sd[,2],Sampling0.83_sd[,3])
colnames(data0.3)<- c("IdLabel","mean","sd","se")
colnames(data0.5)<- c("IdLabel","mean","sd","se")
colnames(data0.83)<- c("IdLabel","mean","sd","se")

data0.3 <- arrange(data0.3,mean)
data0.5 <- arrange(data0.5,mean)
data0.83 <- arrange(data0.83,mean)
data0.3$Ind <- seq(1,44,1)
data0.5$Ind <- seq(1,44,1)
data0.83$Ind <- seq(1,44,1)

ggplot(data = data0.3, aes(x=Ind,
        y=mean))+geom_point() + geom_errorbar(limits, 
        position=dodge, width=0.25)+theme_bw()+labs(x="Individuals",
        y="Proportion of Sampling")+theme(axis.text=element_text(size=24,family = "Times New Roman"),
            panel.border = element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=24,face="bold",family = "Times New Roman"))+ ylim(0, 0.75)  


# plotten der raw data (nur Punkte)
ggplot(data = Sampling0.5_raw, aes(x=IdLabel,
                                   y=Sampling))+geom_point()+theme_bw()
#####
# Abbildung der gefitteten Kurven

data_mean <- data.frame(summarize(group_by(Sampling_data2,
                                           IdLabel,Condition),mean(Sampling)))
ggplot(data = data_mean,
       aes(x=Condition,y=mean.Sampling.,
           group=IdLabel))+geom_point(data=data_mean,
          aes(x=Condition,y=mean.Sampling.),size =2, color = "darkgrey")+geom_smooth(method=glm,
          family = binomial,aes(x=Condition,y=mean.Sampling.), se =FALSE,size=0.3,
          color = "black")+theme_bw()+theme(axis.text=element_text(size=26,family = "Times New Roman"),
          axis.title=element_text(size=26, 
          family = "Times New Roman"))+labs(x="Reward Probability",
            y="Proportion of Sampling")

#####
# FLEXIBILITY
#------------------------------------------------------------------------------------------------------

data4_fl_flex <- filter(data4_fl, Part == 2) 
data4_fl_flex<-filter(data4_fl_flex, VisitCounter <=100)
data4_fl_flex <- as.data.frame(summarize(group_by(data4_fl_flex,IdLabel,Day,Cage),sum(VisitonActive_Part1)))
colnames(data4_fl_flex)[4]<-"Flex"

M_flex1<- MCMCglmm(Flex ~ Day, 
                   #random = ~us(1+day):Ind,
                   random = ~IdLabel,
                   data = data4_fl_flex2, family = "poisson",pr = TRUE,
                   prior = prior.3,
                   verbose = FALSE,saveX = TRUE, saveZ = TRUE,
                   nitt = 200E3, thin = 100, burnin = 100E3)

summary(M_flex1)

data4_fl_flex3 <- data4_fl_flex[-c(16,57,124,140,163,164),]
M_flex2<- MCMCglmm(Flex ~ Day, 
                   #random = ~us(1+day):Ind,
                   random = ~IdLabel,
                   data = data4_fl_flex3, family = "poisson",pr = TRUE,
                   prior = prior.3,
                   verbose = FALSE,saveX = TRUE, saveZ = TRUE,
                   nitt = 200E3, thin = 100, burnin = 100E3)

summary(M_flex2)

rep.flex<-
  M_flex3$VCV[,"IdLabel"]/ ( M_flex3$VCV[,"IdLabel"]+ M_flex3$VCV[,"units"]+log(1/exp(4.528)+1))

posterior.mode(rep.flex)   #repeatability estimate
HPDinterval(rep.flex)    #95% credibility interval

# Outlier identifizieren
for (i in 1:172)
{
  Indiv <- data4_fl_flex[i,1]
  median <- filter(data4_plot,IdLabel==Indiv)
  data4_fl_flex[i,5]<-abs(data4_fl_flex[i,4]-median[1,2])
}
colnames(data4_fl_flex)[5] <- "Diff"
rm(median,Indiv)

ggplot(data = data4_fl_flex, aes(x=Inds,
                             y=Diff))+geom_point()+theme_bw()
#######
#-----------------------------------------------------------------------------------------------

# Mittlere Sd gegen MW

sd30 <- as.data.frame(summarize(group_by(Samp_data0.3),mean(Sampling)))
sd30$sd <- as.data.frame(summarize(group_by(Samp_data0.3),sd(Sampling)))
colnames(sd30)<-c("Mean","sd")
sd50 <- as.data.frame(summarize(group_by(Samp_data0.5),mean(Sampling)))
sd50$sd <- as.data.frame(summarize(group_by(Samp_data0.5),sd(Sampling)))
colnames(sd50)<-c("Mean","sd")
sd83 <- as.data.frame(summarize(group_by(Samp_data0.83),mean(Sampling)))
sd83$sd <- as.data.frame(summarize(group_by(Samp_data0.83),sd(Sampling)))
colnames(sd83)<-c("Mean","sd")
sd83<- as.data.frame(sd83)
sd <- as.data.frame(c(0.174,0.106,0.0705))
colnames(sd)<-c("Mean")
sd$sd <- c(0.17,0.09,0.09)

ggplot(data = sd, aes(x=Mean,
        y=sd))+geom_point(size=4) +theme_bw()+labs(x="Mean sampling rate",
        y="Sd of mean sampling rate")+theme(axis.text=element_text(size=26,family = "Times New Roman"),
        panel.border = element_blank(),axis.line = element_line(colour = "black"),
        axis.title=element_text(size=26,face="bold",family = "Times New Roman"))+ylim(0, 0.2)

----------------------------------------------------------------------------------------------
# Flexibility als Besuche an neuer Blüte

data4_fl_flex_new <- filter(data4_fl, Part == 2) 
data4_fl_flex_new<-filter(data4_fl_flex_new, VisitCounter <=100)
data4_fl_flex_new <- as.data.frame(summarize(group_by(data4_fl_flex_new,IdLabel,Day,Cage),sum(VisitonActive)))
colnames(data4_fl_flex_new)[4]<-"Flex"

M_flex_new<- MCMCglmm(Flex ~ Day, 
                   #random = ~us(1+day):Ind,
                   random = ~IdLabel,
                   data = data4_fl_flex_new, family = "poisson",pr = TRUE,
                   prior = prior.3,
                   verbose = FALSE,saveX = TRUE, saveZ = TRUE,
                   nitt = 200E3, thin = 100, burnin = 100E3)

summary(M_flex_new)


rep.flex<-
  M_flex_new$VCV[,"IdLabel"]/ ( M_flex_new$VCV[,"IdLabel"]+ M_flex_new$VCV[,"units"]+log(1/exp(3.05)+1))

posterior.mode(rep.flex)   #repeatability estimate
HPDinterval(rep.flex)    #95% credibility interval

data4_plot2 <- as.data.frame(summarize(group_by(data4_fl_flex_new,IdLabel),mean(Flex)))
data4_plot_sd2 <- as.data.frame(summarize(group_by(data4_fl_flex_new,IdLabel),sd(Flex)))
data4_plot2 <- cbind(data4_plot2,data4_plot_sd2[,2])
colnames(data4_plot2)<-c("IdLabel","Mean","sd")
rm(data4_plot_sd2)
data4_plot2 <- arrange(data4_plot2,Mean)
data4_plot2$Inds <- seq(1,44,1)

limits <- aes(ymax = Mean + sd, ymin=Mean - sd)
dodge <- position_dodge(width=0.1)
ggplot(data = data4_plot2, aes(x=Inds,
      y=Mean))+geom_point() + geom_errorbar(limits, 
      position=dodge, width=0.25)+theme_bw()+labs(x="Individuals",
      y="Visits at new flower")+theme(axis.text=element_text(size=26,family = "Times New Roman"),
      panel.border = element_blank(),axis.line = element_line(colour = "black"),
      axis.title=element_text(size=26,face="bold",family = "Times New Roman"))

data4_plot2<-arrange(data4_plot2, IdLabel)

# Plotten der einzelnen Tiere

data4_plot <- as.data.frame(summarize(group_by(data4_fl_flex,IdLabel),mean(Flex)))
data4_plot_sd <- as.data.frame(summarize(group_by(data4_fl_flex,IdLabel),sd(Flex)))
data4_plot <- cbind(data4_plot,data4_plot_sd[,2])
colnames(data4_plot)<-c("IdLabel","Mean","sd")
rm(data4_plot_sd)
data4_plot <- arrange(data4_plot,Mean)
data4_plot$Inds <- seq(1,44,1)

limits <- aes(ymax = Mean + sd, ymin=Mean - sd)
dodge <- position_dodge(width=0.1)
ggplot(data = data4_plot, aes(x=Inds,
      y=Mean))+geom_point() + geom_errorbar(limits, 
      position=dodge, width=0.25)+theme_bw()+labs(x="Individuals",
      y="Perseverance")+theme(axis.text=element_text(size=26,family = "Times New Roman"),
     panel.border = element_blank(),axis.line = element_line(colour = "black"),
      axis.title=element_text(size=26,face="bold",family = "Times New Roman"))

#####
# CORELATION PLASTICITY AND FLEXIBILITY
#-----------------------------------------------------------------------------------------------------

#slope direkt vom Model
Sol <- posterior.mode(UMM.samp$Sol)
slope <- Sol[50:93]
slope.general <- Sol[2]
slope <- slope + slope.general
rm(Sol,slope.general)
slope<-data.frame(slope)

data4_plot2 <- cbind(data4_plot2,slope)

# Histogramm der Steigungen

ggplot(data = data4_plot2, aes(x=slope))+geom_histogram(binwidth = 0.2, fill = "white", color = "black")+ 
  theme_bw()+labs(x="Slope of sampling rate change",
             y="Frequency")+theme(axis.text=element_text(size=26,family = "Times New Roman"),
                  panel.border = element_blank(),axis.line = element_line(colour = "black"),
                axis.title=element_text(size=26,face="bold",family = "Times New Roman"))




ggplot(data = data4_plot2, aes(x=Mean,
        y=slope))+geom_point() + theme_bw()+labs(x="Visits to new rewarding flower",
       y="Slope of sampling rate change")+theme(axis.text=element_text(size=26,family = "Times New Roman"),
      panel.border = element_blank(),axis.line = element_line(colour = "black"),
    axis.title=element_text(size=26,face="bold",family = "Times New Roman"))


M1 <- lm(slope~Mean,data = data4_plot2)
summary(M1)
