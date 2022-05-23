# Clear the data
rm(list=ls())

#Import the merged data
Data <- read.csv("C:/Users/FAN/Desktop/Data/Merged_data.csv", as.is=TRUE)
length(unique(Data$ConPhylum))
#Growth rate and optimum_temp
library(ggplot2)
F1<-ggplot(data = Data,aes(x=OptimumTemp,y=log10(GrowthRate),color=Superkingdom))+
  geom_point(size=2,alpha=0.6)+
  theme_classic()

#unit from Celsius to K
Data$OptimumTemp<-Data$OptimumTemp+273.15
Data$TempUnit[Data$TempUnit%in%c("Celsius")]<-"K"

#Data transformation
library(dplyr)
k <- 8.62e-5 
Data<- Data %>% mutate(arrhenius_tmp = 1/(k*OptimumTemp))
Data<- Data %>% mutate(lograte = log(GrowthRate))
Data<-Data[!is.na(Data$ConPhylum),]

#if choose species<40c
bacteria_d <- read.csv("C:/Users/FAN/Desktop/Data/bacteria_d.csv", as.is=TRUE)
bacteria_d<-subset(bacteria_d,TempPref=="Mesophile")
bacteria_d<- bacteria_d %>% mutate(lograte = log(GrowthRate))
bacteria_d<-bacteria_d %>% mutate(arrhenius_tmp = 1/(k*OptimumTemp))

#One figure
bacteria_d$ConPhylum<- factor(bacteria_d$ConPhylum,ordered = TRUE,
                              levels = c("Firmicutes","Proteobacteria","Synergistetes","Spirochaetes","Cyanobacteria","Cyanophyta",
                                         "Chloroflexi","Balneolaeota","Acidobacteria","Caldiserica","Chlorobi","Actinobacteria","Deferribacteres","Deinococcus-Thermus","Dictyoglomi" ))
ggplot(data=bacteria_d, aes(x=arrhenius_tmp, y=lograte,color=ConPhylum)) +
  geom_point(size=2)+
  theme_classic()+
  geom_smooth(method="lm",linetype=1,se=F,span=1)

#Set the frame
Fit<-data.frame(Intercept=numeric(),Slope=numeric(),ConPhylum=character(),R2=numeric())
for(i in unique(Data$ConPhylum)){
  sub_data <- subset(Data,Data$ConPhylum==i)
  fit<-lm(lograte~arrhenius_tmp,data=sub_data)
  Intercept[i]=coef(fit)[1]
  Slope[i]=coef(fit)[2]
  ConPhylum[i]=i
  summary(fit)
  R2[i]<-summary(fit)$r.squared
  Fit<-cbind(ConPhylum,Intercept,Slope,R2)
}
  write.table (Fit, file ="C:/Users/FAN/Desktop/Data/TotalFit.csv", sep =",")

#Mean
  TFit<- read.csv("C:/Users/FAN/Desktop/Data/TotalFit.csv", as.is=TRUE)
  TFit<-TFit[!is.na(TFit$Slope),]
  TFit$E<-abs(TFit$Slope)
  mean(TFit$E)
  write.table (TFit, file ="C:/Users/FAN/Desktop/Data/TFit.csv", sep =",")
  TData<-merge(TFit,Data,by='ConPhylum')
  Phylum<-aggregate(TData$GenusSpecies, by=list(type=TData$ConPhylum),length)
  write.table (TData, file ="C:/Users/FAN/Desktop/Data/TSlope.csv", sep =",")
  write.table (Phylum, file ="C:/Users/FAN/Desktop/Data/Phylum.csv", sep =",")
  
  #Classification
  B <- TData[TData$Superkingdom == "Bacteria",]
  A<-TData[TData$Superkingdom == "Archaea",]
  P<-TData[TData$Superkingdom == "Phytoplankton",]
  
#Plot
m<- split(TData,TData$ConPhylum)
for(i in 1:length(m)){
  sub_data <-  m[[i]]
  B<-names(m[i])
  png(filename = paste0(B, "_", ".jpg"),width = 2400,height = 1800,res = 200)
  print(ggplot(data=sub_data, aes(x=arrhenius_tmp,y=lograte))+
  geom_point()+
  theme_classic()+
  geom_smooth(method = "lm",linetype=1,span=1))
  dev.off()
} 

#Model fitting
TData<- TData %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*OptimumTemp))))
TData<-TData[!is.na(TData$TCorrected_GrowthRate),]
Fit2<-data.frame(Intercept=numeric(),Slope=numeric(),ConPhylum=character(),R2=numeric())
for(i in unique(TData$ConPhylum)){
  sub_data <- subset(TData,TData$ConPhylum==i)
  fit<-lm(log(TCorrected_GrowthRate)~log(AverageVolume),data=sub_data)
  Intercept[i]=coef(fit)[1]
  Slope[i]=coef(fit)[2]
  ConPhylum[i]=i
  summary(fit)
  R2[i]<-summary(fit)$r.squared
  Fit2<-cbind(ConPhylum,Intercept,Slope,R2)
}
write.table (Fit2, file ="C:/Users/FAN/Desktop/Data/Fit2.csv", sep =",")

m<- split(TData,TData$ConPhylum)
for(i in 1:length(m)){
  sub_data <-  m[[i]]
  B<-names(m[i])
  png(filename = paste0(B, "_V", ".jpg"),width = 2400,height = 1800,res = 200)
  print(ggplot(data=sub_data, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate)))+
          geom_point()+
          theme_classic()+
          geom_smooth(method = "lm",linetype=1,span=1))
  dev.off()
} 


  

