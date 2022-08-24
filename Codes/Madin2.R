# Clear the data
rm(list=ls())

#Packages
library("dplyr")
library("ggplot2")
library("Rmisc")
library("plyr")
library("broom")

#Import the Data
setwd("C:/Users/FAN/Desktop/Final")
Data <- read.csv("Madin.csv", as.is=TRUE)
Data$OptimumTemp<-Data$Optimum.Temp+273.15
k <- 8.62e-5 
Data$arrhenius_tmp<-1/(k*Data$OptimumTemp)

#Divide into Bacteria, Archaea
B <- Data[Data$Superkingdom == "Bacteria",]
A<-Data[Data$Superkingdom == "Archaea",]

#Distribution of V
mu1<-ddply(Data,"Superkingdom",summarise,mean=mean(log(AverageVolume)))
setwd("C:/Users/FAN/Desktop/Final/Figures")
png(filename = "V_M.jpg",width =1000, height =1000)
ggplot(Data,aes(x=log(AverageVolume),color=Superkingdom))+
  geom_density(alpha=0.6,size=1)+
  scale_x_continuous(name="Log10(Volume) (m^3)")+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  geom_vline(data=mu1,aes(xintercept=mean,color=Superkingdom),linetype="dashed",size=1)
dev.off()

##Breakpoint
# Using "segmented" R package to find analysis break-points in the data
library(segmented)

#Boltzmann constant 
x1 = 1 / (k*(B$OptimumTemp))
y1 = log(B$GrowthRate)
boltzmann_linear <- lm(y1 ~ x1)
os1 <- segmented(boltzmann_linear, seg.Z =~ x1) 
plot(x1,y1)
plot(os1, add=T)
summary.segmented(os1)
os1$psi[2]
bacteria_breakpoint_celsius <- (1/(k*os1$psi[2]))-273.15
bacteria_breakpoint_celsius

# repeat for archaea
x2 = 1 / (k*(A$OptimumTemp))
y2 = log(A$GrowthRate)

boltzmann_linear <- lm(y2 ~ x2)

os2 <- segmented(boltzmann_linear, seg.Z =~ x2)
plot.segmented(os2)
summary.segmented(os2)

os2$psi[2] # <-- this is the break point

archaea_breakpoint_celsius <- (1/(k*os2$psi[2]))-273.15
archaea_breakpoint_celsius

#classify prokaryotes into Thermo and Meso
Data$TempPref <- NA
for (i in 1:length(Data$OptimumTemp)){
  if (!is.na(Data$OptimumTemp[i])){
    if (Data$Superkingdom[i] == "Bacteria"){
      if (Data$OptimumTemp[i] > 313.15){ #40C
        Data$TempPref[i] <- "Thermophile"
      }
      else{
        Data$TempPref[i] <- "Mesophile"
      }
    }
    else if (Data$Superkingdom[i] == "Archaea"){
      if (Data$OptimumTemp[i] >358.864){ #85.714c
        Data$TempPref[i] <- "Thermophile"
      }
      else{
        Data$TempPref[i] <- "Mesophile"
      }
    }
    else{
      Data$TempPref[i] <- "NA"
    }
  }
}


B<- Data[Data$Superkingdom == "Bacteria",]
A<-Data[Data$Superkingdom == "Archaea",]
write.table (Data, file ="C:/Users/FAN/Desktop/Final/Tables/Merged_Breakpoint.csv", sep =",")

#Figure
png(filename = "Madin_Breakpoint.jpg",width =1200, height =1000) 
ggplot(data=Data,aes(x =1/(k*OptimumTemp), y = log(GrowthRate), shape= TempPref,color=Superkingdom)) +
  geom_point(size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_reverse(name="1/kT",sec.axis = sec_axis(~(1/(.*k)-273.15), name = "Temperature(Celsius)"))+
  geom_smooth(method = "lm",se=T,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ Superkingdom,ncol=3,scales= "free")
dev.off()

#a,E for bacteria based on breakpoint
Meso1<-subset(B,TempPref=="Mesophile")
fit<-lm(log(GrowthRate)~log(AverageVolume)+arrhenius_tmp,data=Meso1)
summary(fit)
Coef_Madin_MesoB<-tidy(summary(fit))
Meso1$a<- 0.05225
Meso1$E<- -0.07942
Coef_Madin_MesoB$P<-  0.6833
Coef_Madin_MesoB$R2<-0.005349
Coef_Madin_MesoB$Superkingdom<-"Bacteria"

#a,E for archaea based on breakpoint
Meso2<-subset(A,TempPref=="Mesophile")
fit<-lm(log(GrowthRate)~log(AverageVolume)+arrhenius_tmp,data=Meso2)
summary(fit)
Coef_Madin_MesoA<-tidy(summary(fit))
Meso2$E<-- 0.21695
Meso2$a<- 0.06452
Coef_Madin_MesoA$P<-4.059e-06
Coef_Madin_MesoA$R2<-0.168
Coef_Madin_MesoA$Superkingdom<-"Archaea"

#Merge the data Data_T2
Coef_Madin_M_Breakpoint<-rbind(Coef_Madin_MesoA,Coef_Madin_MesoB)
write.table(Coef_Madin_M_Breakpoint,"C:/Users/FAN/Desktop/Final/Tables/Coef_Madin_M_Breakpoint.csv",sep=",")

#Figure 3c
setwd("C:/Users/FAN/Desktop/Final/Figures")
Meso<-rbind(Meso1,Meso2)
Meso<-Meso %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*OptimumTemp))))
png(filename = "Madin_Breakpoint2.jpg",width =1200, height =1000)
ggplot(data=Meso, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=Superkingdom,shape=Superkingdom))+
  geom_point(size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate) (s^-1)")+
  scale_x_continuous(name="Log(Volume)(m^3)")+
  geom_smooth(method = "lm",se=T,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        legend.title  = element_text(size =30),legend.text = element_text(size = 28),legend.position = "bottom")
dev.off()

##Based on Phylum
#remove the points<10
Data<-Data%>%group_by(ConPhylum)%>%filter(length(ConPhylum)>=10)

#based on phylum with breakpoint
Meso<-subset(Data,TempPref=="Mesophile")
Meso<-Meso%>%group_by(ConPhylum)%>%filter(length(ConPhylum)>10)

#Model fitting for Meso
#p value
list_of_Phylum = split(Meso,Meso$ConPhylum)
results = lapply(list_of_Phylum, function(Meso) lm(log(GrowthRate)~log(AverageVolume)+arrhenius_tmp, data = Meso))
fit<-lapply(results,summary)
p_M<- sapply(fit, function(Meso) Meso$coefficients[,"Pr(>|t|)"])
p_M<- t(p_M)
colnames(p_M) <- c ("p_intercept","p_volume","p_temp") 
p_M<-p_M[,-1]


#Extract the values
Meso$x<-log(Meso$GrowthRate)
Meso$y<-log(Meso$AverageVolume)
Meso$z<-Meso$arrhenius_tmp

Coef1<-ddply(Meso, .(ConPhylum), summarize,
             r.sqr = summary(lm(x~y+z))$r.squared,
             a = summary(lm(x~y+z))$coefficients[[2]],
             RE = summary(lm(x~y+z))$coefficients[[3]])
Coef_M<-cbind(Coef1,p_M)
Data_M<-merge(Meso,Coef_M,by="ConPhylum")
Data_M$E<--(Data_M$RE)

#Temperature corrected R-TM
Data_M<- Data_M %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*OptimumTemp))))
write.table (Data_M, file ="C:/Users/FAN/Desktop/Final/Tables/Data_Madin_phylum2.csv", sep =",")

#Table 1
write.table (Coef_M, file ="C:/Users/FAN/Desktop/Final/Tables/Coef_Madin_phylum2.csv", sep =",")

#Phylum
E<-subset(Data_M,ConPhylum==c("Euryarchaeota"))
F<-subset(Data_M,ConPhylum=="Firmicutes")
P<-subset(Data_M,ConPhylum=="Proteobacteria")

#Model fitting
fit<-lm(x~y+z,data=E)
summary(fit)

fit<-lm(x~y+z,data=F)
summary(fit)

fit<-lm(x~y+z,data=P)
summary(fit)

Data_M<-rbind(E,F,P)

#Plot based on phylum (Figure 4b)
png(filename = "Madin_phylum2.jpg",width =1000, height =400) 
ggplot(data=Data_M, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),color=ConPhylum))+
  geom_point(size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate) (s^-1)")+
  scale_x_continuous(name="Log(Volume)(m^3)")+
  geom_smooth(method = "lm",linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ ConPhylum,scales= "free")
dev.off()


#Phylum
E<-subset(Data_M,ConPhylum==c("Euryarchaeota"))
F<-subset(Data_M,ConPhylum=="Firmicutes")
P<-subset(Data_M,ConPhylum=="Proteobacteria")
Data_M<-rbind(E,F,P)

#Distribution of cell volumes in Madin's data (Figure 2b)
png(filename = "V_P_M.jpg",width =1200, height =1000)
mu2<-ddply(Data_M,"ConPhylum",summarise,mean=mean(log10(AverageVolume)))
ggplot(Data_M,aes(x=log10(AverageVolume),color=ConPhylum))+
  geom_density(alpha=0.6,size=1)+
  scale_x_continuous(name="Log10(Volume) (m^3)")+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  geom_vline(data=mu2,aes(xintercept=mean,color=ConPhylum),linetype="dashed",size=1)
dev.off()

#ANCOVA(Table 2)
Data_M$ConPhylum<-factor(Data_M$ConPhylum)
fit<-aov(log(TCorrected_GrowthRate)~log(AverageVolume)*ConPhylum,data=Data_M)

summary(fit)
Group<-tidy(fit)
write.table (Group, file ="C:/Users/FAN/Desktop/Final/Tables/Group_Madin.csv", sep =",")

#ANOVA(Table 1)
fit<-aov(log(AverageVolume)~ConPhylum,data=Data_M)
summary(fit)
Group<-tidy(fit)
