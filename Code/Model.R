#Packages
library(dplyr)

# Clear the data
rm(list=ls())

#Import the Data
Data <- read.csv("C:/Users/FAN/Desktop/Data/Tables/Data_new.csv", as.is=TRUE)

#Arrhenius
k <- 8.62e-5 
Data$arrhenius_tmp<-1/(k*Data$Temp)
Data

#Model fitting
list_of_Phylum = split(Data,Data$ConPhylum)
results = lapply(list_of_Phylum, function(Data) lm(log(GrowthRate)~log(AverageVolume)+arrhenius_tmp, data = Data))
lapply(results,summary)

#Extract the values
Data$x<-log(Data$GrowthRate)
Data$y<-log(Data$AverageVolume)
Data$z<-Data$arrhenius_tmp

Coef1<-Data %>% group_by(ConPhylum) %>% 
  summarise(
    r.sqr = summary(lm(x~y+z))$r.squared,
    lntercept=summary(lm(x~y+z))$coefficients[[1]],
    a = summary(lm(x~y+z))$coefficients[[2]],
    RE = summary(lm(x~y+z))$coefficients[[3]],
  )
write.table (Coef1, file ="C:/Users/FAN/Desktop/Data/Coef1.csv", sep =",")
Data1<-merge(Coef1,Data,by="ConPhylum")
Data1$E<--(Data1$RE)

#Temperature corrected R
Data1<- Data1 %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*Temp))))
write.table (Data1, file ="C:/Users/FAN/Desktop/Data1.csv", sep =",")

##classified based on breakpoint(40c)
Meso<-subset(Data,TempPref=="Mesophile")
Thermo<-subset(Data,TempPref=="Thermophile")

#Model fitting for Meso
list_of_Phylum = split(Meso,Meso$ConPhylum)
results1 = lapply(list_of_Phylum, function(Meso) lm(log(GrowthRate)~log(AverageVolume)+arrhenius_tmp, data = Meso))
lapply(results1,summary)

Coef_M<-Meso %>% group_by(ConPhylum) %>% 
  summarise(
    r.sqr = summary(lm(x~y+z))$r.squared,
    lntercept=summary(lm(x~y+z))$coefficients[[1]],
    a = summary(lm(x~y+z))$coefficients[[2]],
    RE = summary(lm(x~y+z))$coefficients[[3]],
  )

write.table (Coef_M, file ="C:/Users/FAN/Desktop/Data/Coef_M.csv", sep =",")

#Model fitting for Thermo
list_of_Phylum = split(Thermo,Thermo$ConPhylum)
results2 = lapply(list_of_Phylum, function(Thermo) lm(log(GrowthRate)~log(AverageVolume)+arrhenius_tmp, data = Thermo))
lapply(results2,summary)

Coef_T<-Thermo%>% group_by(ConPhylum) %>% 
  summarise(
    r.sqr = summary(lm(x~y+z))$r.squared,
    lntercept=summary(lm(x~y+z))$coefficients[[1]],
    a = summary(lm(x~y+z))$coefficients[[2]],
    RE = summary(lm(x~y+z))$coefficients[[3]],
  )

write.table (Coef_T, file ="C:/Users/FAN/Desktop/Data/Coef_T.csv", sep =",")

#Merge the data(T&M)
Data_T<- merge(Thermo,Coef_T, by = "ConPhylum")
Data_M<-merge(Meso,Coef_M,by="ConPhylum")
Data_TM<-rbind(Data_M,Data_T)

#Q£ºE<0
W<-subset(Data_TM,RE>0)

Data_TM$E<--(Data_TM$RE)

#Temperature corrected R-TM
Data_TM<- Data_TM %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*Temp))))
write.table (Data_TM, file ="C:/Users/FAN/Desktop/Data_TM.csv", sep =",")
