# Clear the data
rm(list=ls())

#Import the data
A<- read.csv("C:/Users/FAN/Desktop/Data/ARCHAEA.csv", as.is=TRUE)
B<- read.csv("C:/Users/FAN/Desktop/Data/BACTERIA.csv", as.is=TRUE)
P<- read.csv("C:/Users/FAN/Desktop/Data/PHYTOPLANKTON.csv", as.is=TRUE)
str(A)

#Merge the data
#Get the Tpk and Rmax
AB<-rbind(A,B)
library(dplyr)
AB<-select(AB,-c(X,Consumer,FinalID,OriginalTraitName,OriginalTraitValue,OriginalTraitUnit,MinVolume,MaxVolume))
P<-select(P,-c(X,Consumer,FinalID,OriginalTraitName,OriginalTraitValue,OriginalTraitUnit,StandardisedTraits))
P$ConKingdom[P$ConKingdom %in% c("Chromista","Plantae")] <- "Phytoplankton"
Hira<-rbind(AB,P)
Hira <- Hira[order(Hira$GenusSpecies, -Hira$StandardisedTraitValue),]
Hira<-Hira[!duplicated(Hira$GenusSpecies),]

#Output Hira's data
write.table (Hira, file ="C:/Users/FAN/Desktop/Data/Hira.csv", sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)


