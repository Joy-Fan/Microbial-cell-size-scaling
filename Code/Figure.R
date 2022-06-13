# Clear the data
rm(list=ls())

#Packages
library(ggplot2)

#Import the data
Data1 <- read.csv("C:/Users/FAN/Desktop/Data1.csv", as.is=TRUE)
Data_TM <- read.csv("C:/Users/FAN/Desktop/Data_TM.csv", as.is=TRUE)

#Clear the data
Data1<- Data1 %>% mutate(lograte = log(TCorrected_GrowthRate))
Data<- Data_TM %>% mutate(lograte = log(TCorrected_GrowthRate))
Data1 <- Data1[!is.na(Data1$E),]
Data <- Data[!is.na(Data$E),]

#Mean
mean(Data1$E)

#Parameter
k <- 8.62e-5 

#Classification
A1<-subset(Data1,Superkingdom=="Archaea")
B1<-subset(Data1,Superkingdom=="Bacteria")
P1<-subset(Data1,Superkingdom=="Phytoplankton")
unique(A1$ConPhylum)
unique(B1$ConPhylum)
unique(P1$ConPhylum)

##Plot1-G vs 1/kT
setwd("C:/Users/FAN/Desktop")

#Archaea
png(filename = "A1.jpg",width =1000, height =700)
A1$ConPhylum<-as.factor(A1$ConPhylum)
ggplot(data=A1, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=ConPhylum))+
    geom_point(aes(shape=Author),size=4,alpha=0.6)+
    scale_y_continuous(name="Log(Growth rate)")+
    scale_x_continuous(name="Log(Volume)")+
    geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
    theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
          panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),
          strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
    facet_wrap(. ~ ConPhylum,scales= "free")
dev.off()

#Bacteria
png(filename = "B1.jpg",width =1800, height =1500)  
B1$ConPhylum<-as.factor(B1$ConPhylum)
ggplot(data=B1, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=ConPhylum))+
  geom_point(aes(shape=Author),size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_continuous(name="Log(Volume)")+
  geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"), panel.spacing.y = unit(2, "cm"),
        legend.title  = element_text(size =24),legend.text = element_text(size = 20),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ ConPhylum,ncol=3,scales= "free")
dev.off()

#Phytoplankton color = "blue", size = 1, alpha = 0.5) 
png(filename = "P1.jpg",width =1000, height =1000)  
P1$ConPhylum<-as.factor(P1$ConPhylum)
ggplot(data=P1, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=ConPhylum))+
  geom_point(aes(shape=Author),size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_continuous(name="Log(Volume)")+
  geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"), panel.spacing.y = unit(2, "cm"),
        legend.title  = element_text(size =20),legend.text = element_text(size = 18),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ ConPhylum,ncol=2,scales= "free")
dev.off()

##Plot-Breakpoint
#Classification
A<-subset(Data,Superkingdom=="Archaea")
B<-subset(Data,Superkingdom=="Bacteria")
P<-subset(Data,Superkingdom=="Phytoplankton")
unique(A$ConPhylum)
unique(B$ConPhylum)
unique(P$ConPhylum)

#Archaea
png(filename = "A.jpg",width =1000, height =700) 
A$ConPhylum<-as.factor(A$ConPhylum)
ggplot(data=A, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=ConPhylum))+
  geom_point(aes(shape=Author),size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_continuous(name="Log(Volume)")+
  geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ ConPhylum,scales= "free")
dev.off()

#Bacteria
png(filename = "B.jpg",width =1800, height =1500)  
B$ConPhylum<-as.factor(B$ConPhylum)
ggplot(data=B, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=ConPhylum))+
  geom_point(aes(shape=Author),size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_continuous(name="Log(Volume)")+
  geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"), panel.spacing.y = unit(2, "cm"),
        legend.title  = element_text(size =24),legend.text = element_text(size = 20),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ ConPhylum,ncol=3,scales= "free")
dev.off()

#Phytoplankton color = "blue", size = 1, alpha = 0.5) 
png(filename = "P.jpg",width =1000, height =1000)  
P$ConPhylum<-as.factor(P$ConPhylum)
ggplot(data=P, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=ConPhylum))+
  geom_point(aes(shape=Author),size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_continuous(name="Log(Volume)")+
  geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"), panel.spacing.y = unit(2, "cm"),
        legend.title  = element_text(size =20),legend.text = element_text(size = 18),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ ConPhylum,ncol=2,scales= "free")
dev.off()

