
# Clear the data
rm(list=ls())

#Import the merged data
Data <- read.csv("C:/Users/FAN/Desktop/Data_TM.csv", as.is=TRUE)

#V of bacteria£¬archae and Phyto
library(ggplot2)
library(plyr)
mu <- ddply(Data, "Superkingdom", summarise,mu=mean(AverageVolume))
F3<-ggplot(Data, aes(x=log10(AverageVolume), color=Superkingdom,fill=Superkingdom)) +
  geom_density(alpha=0.4)+
  geom_vline(data=mu, aes(xintercept=log10(mu), color=Superkingdom),
             linetype="dashed")+
  theme_classic()
F3

#Growth rate and optimum_temp
library(ggplot2)
F1<-ggplot(data = Data,aes(x=OptimumTemp,y=log10(GrowthRate),color=Superkingdom))+
  geom_point(size=2,alpha=0.6)+
  theme_classic()

#unit from Celsius to K
Data$OptimumTemp<-Data$Optimum.Temp+273.15
Data$TempUnit[Data$TempUnit%in%c("Celsius")]<-"K"

# Using "segmented" R package to find analysis break-points in the data
library(segmented)

#Divide into Bacteria, Archaea and Phytoplankton
bacteria_d <- Data[Data$Superkingdom == "Bacteria",]
archaea_d<-Data[Data$Superkingdom == "Archaea",]
phyto_d<-Data[Data$Superkingdom == "Phytoplankton",]

ggplot(Data, aes(x = 1/OptimumTemp, y = log(GrowthRate), col =Superkingdom)) +
  geom_point()
ggplot(bacteria_d, aes(x = 1/OptimumTemp, y = log(GrowthRate))) +
  geom_point()
ggplot(archaea_d, aes(x= 1/OptimumTemp, y = log(GrowthRate))) +
  geom_point()
ggplot(phyto_d, aes(x = 1/OptimumTemp, y = log(GrowthRate))) +
  geom_point()

#Boltzmann constant 
k <- 8.62e-5 
x1 = 1 / (k*(bacteria_d$OptimumTemp))
y1 = log(bacteria_d$GrowthRate)
boltzmann_linear <- lm(y1 ~ x1)
os1 <- segmented(boltzmann_linear, seg.Z =~ x1) 
plot(x1,y1)
plot(os1, add=T)
summary.segmented(os1)
os1$psi[2]
bacteria_breakpoint_celsius <- (1/(k*os1$psi[2]))-273.15
bacteria_breakpoint_celsius

# repeat for archaea
x2 = 1 / (k*(archaea_d$OptimumTemp))
y2 = log(archaea_d$GrowthRate)

boltzmann_linear <- lm(y2 ~ x2)

os2 <- segmented(boltzmann_linear, seg.Z =~ x2)
plot.segmented(os2)
summary.segmented(os2)

os2$psi[2] # <-- this is the break point

archaea_breakpoint_celsius <- (1/(k*os2$psi[2]))-273.15
archaea_breakpoint_celsius

#repeat for phytoplankton
x3 = 1 / (k*(phyto_d$OptimumTemp))
y3 = log(phyto_d$GrowthRate)

boltzmann_linear <- lm(y3 ~ x3)

os3 <- segmented(boltzmann_linear, seg.Z =~ x3)
plot.segmented(os3)
summary.segmented(os3)

os3$psi[2] # <-- this is the break point

archaea_breakpoint_celsius <- (1/(k*os3$psi[2]))-273.15
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
      if (Data$OptimumTemp[i] >358.86){ #85.71c
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
  if (Data$Superkingdom[i] == "Phytoplankton"){
    if (Data$OptimumTemp[i] > 278.05){ #4.9C
      Data$TempPref[i] <- "Thermophile"
    }
    else{
      Data$TempPref[i] <- "Mesophile"}
    }
  }


bacteria_d <- Data[Data$Superkingdom == "Bacteria",]
archaea_d<-Data[Data$Superkingdom == "Archaea",]
phyto_d<-Data[Data$Superkingdom == "Phytoplankton",]
write.table (bacteria_d, file ="C:/Users/FAN/Desktop/Data/bacteria_d.csv", sep =",")

#Figure
png(filename = "Breakpoint.jpg",width =1200, height =1000) 
ggplot(data=Data,aes(x =1/(k*Temp), y = log(GrowthRate), color= TempPref,fill=Superkingdom)) +
  geom_point(aes(shape=Author),size=4,alpha=0.6)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_reverse(name="1/kT",sec.axis = sec_axis(~(1/(.*k)-273.15), name = "Temperature(Celsius)"))+
  geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ Superkingdom,ncol=3,scales= "free")
dev.off()
