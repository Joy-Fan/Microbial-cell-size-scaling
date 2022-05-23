# Clear the data
rm(list=ls())

#Import the merged data
Data <- read.csv("C:/Users/FAN/Desktop/bacteria_d/Merged_data.csv", as.is=TRUE)
Data$OptimumTemp<-Data$OptimumTemp+273.15
Data$TempUnit[Data$TempUnit%in%c("Celsius")]<-"K"

#Model fitting:E=0.65
E<-0.65
k <- 8.62e-5 
Data<- Data %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*OptimumTemp))))
E1<-ggplot(data=Data, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate), color=Superkingdom))+
  geom_point(shape=21,size=1,alpha=0.5)+
  geom_smooth(method = "lm",se=F)+
  ylab("log(Growth rate) (s^-1)") + 
  xlab("log(Volume) (m^3)") +
  scale_y_continuous(limits = c(5, 20))+
  theme_classic()
E1

#Model fitting:E=0.8
E<-0.8
k <- 8.62e-5 
Data<- Data %>% mutate(TCorrected_GrowthRate2 = GrowthRate/(exp(1)^(-E/(k*OptimumTemp))))
E2<-ggplot(data=Data, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate2), color=Superkingdom))+
  geom_point(shape=21,size=1,alpha=0.5)+
  geom_smooth(method = "lm",se=F)+
  ylab("log(Growth rate) (s^-1)") + 
  xlab("log(Volume) (m^3)") +
  scale_y_continuous(limits = c(10,25))+
  theme_classic()
E2

bacteria_d <- Data[Data$Superkingdom == "Bacteria",]
archaea_d<-Data[Data$Superkingdom == "Archaea",]
phyto_d<-Data[Data$Superkingdom == "Phytoplankton",]

A1<-lm(log(TCorrected_GrowthRate)~log(AverageVolume),data=bacteria_d)
summary(A1)
library(broom)
A1<-tidy(summary(A1))

A2<-lm(log(TCorrected_GrowthRate2)~log(AverageVolume),data=bacteria_d)
summary(A2)

B1<-lm(log(TCorrected_GrowthRate)~log(AverageVolume),data=archaea_d)
summary(B1)

B2<-lm(log(TCorrected_GrowthRate2)~log(AverageVolume),data=archaea_d)
summary(B2)

C1<-lm(log(TCorrected_GrowthRate)~log(AverageVolume),data=phyto_d)
summary(C1)

C2<-lm(log(TCorrected_GrowthRate2)~log(AverageVolume),data=phyto_d)
summary(C2)
