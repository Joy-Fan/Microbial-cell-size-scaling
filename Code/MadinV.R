# Clear the data
rm(list=ls())

#Packages
library(dplyr)

#Import the Data
Data <- read.csv("C:/Users/FAN/Desktop/Data/Tables/Data_new.csv", as.is=TRUE)
Data<-subset(Data,Author=="Madin")

#Arrhenius
k <- 8.62e-5 
Data$arrhenius_tmp<-1/(k*Data$Temp)

Data$x<-log(Data$GrowthRate)
Data$y<-log(Data$AverageVolume)
Data$z<-Data$arrhenius_tmp

B<-subset(Data,Superkingdom=="Bacteria")
P<-subset(Data,Superkingdom=="Phytoplankton")
A<-subset(Data,Superkingdom=="Archaea")

#Get the value
library(tidyverse)
library(broom)
fit<-lm(log(B$GrowthRate)~log(B$AverageVolume)+B$arrhenius_tmp,data=B)
Coef_B<-tidy(summary(fit))
B$a<-0.0209
B$E<--0.4229

fit<-lm(log(A$GrowthRate)~log(A$AverageVolume)+A$arrhenius_tmp,data=A)
Coef_A<-tidy(summary(fit))
A$a<-0.0995
A$E<--0.23

#Merge the data Data_T1
Data_T1<-rbind(A,B,P)

#Figure
Data_T1<- Data_T1 %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*Temp))))
png(filename = "M1.jpg",width =1000, height =1000)  
Data_T1$Superkingdom<-as.factor(Data_T1$Superkingdom)
ggplot(data=Data_T1, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=Superkingdom))+
  geom_point(aes(shape=Author),size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_continuous(name="Log(Volume)")+
  geom_smooth(method = "lm",se=T,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"), panel.spacing.y = unit(2, "cm"),
        legend.title  = element_text(size =20),legend.text = element_text(size = 18),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ Superkingdom,scales= "free")
dev.off()

#Based on breakpoint 
Meso<-subset(B,TempPref=="Mesophile")
Thermo<-subset(B,TempPref=="Thermophile")
fit<-lm(log(GrowthRate)~log(AverageVolume)+arrhenius_tmp,data=Meso)
Coef_Meso<-tidy(summary(fit))
Meso$E<-0.0626
Meso$a<--0.1425
fit<-lm(log(GrowthRate)~log(AverageVolume)+arrhenius_tmp,data=Thermo)
Coef_Thermo<-tidy(summary(fit))
Thermo$E<--0.1234
Thermo$a<--0.6891

#Merge the data Data_T2
B<-rbind(Meso,Thermo)

#Figure
B<- B %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*Temp))))
png(filename = "M2.jpg",width =1000, height =1000) 
ggplot(data=B, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate)))+
  geom_point(aes(shape=Author),size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_continuous(name="Log(Volume)")+
  geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        legend.title  = element_text(size =20),legend.text = element_text(size = 18),legend.position = "bottom")
dev.off()
