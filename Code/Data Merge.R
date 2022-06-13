# Clear the data
rm(list=ls())

#Import the data
Madin <- read.csv("C:/Users/FAN/Desktop/Data/Madin.csv", as.is=TRUE)
Madin$Author<-c("Madin")
Hira <- read.csv("C:/Users/FAN/Desktop/Data/Hira.csv", as.is=TRUE)
Hira$Author<-c("Hira")

#Merge the data
Total<-rbind(Madin,Hira)
T <- Total[order(Total$GenusSpecies, -Total$GrowthRate),]
library(dplyr)
library(plyr)
Replicated<-T%>%group_by(GenusSpecies)%>%filter(length(GenusSpecies)>1)
T<-T[!duplicated(T$GenusSpecies),]
write.table (Replicated, file ="C:/Users/FAN/Desktop/Data/Duplicates.csv", sep =",")
write.table (T, file ="C:/Users/FAN/Desktop/Data/Merged_Data.csv", sep =",")

