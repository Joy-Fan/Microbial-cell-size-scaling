# Clear the data
rm(list=ls())

#Import the merged data
Data <- read.csv("C:/Users/FAN/Desktop/Data/Merged_data.csv", as.is=TRUE,na.strings=c("","","NA"))
Data$ConPhylum[is.na(Data$ConPhylum)] <- "Others"
length(unique(Data$ConPhylum))

#Growth rate and optimum_temp
library(ggplot2)
F1<-ggplot(data = Data,aes(x=OptimumTemp,y=log10(GrowthRate),color=Superkingdom))+
  geom_point(size=2,alpha=0.6)+
  theme_classic()

#unit from Celsius to K
Data$Temp<-Data$Optimum.Temp+273.15

#Data transformation
library(dplyr)
k <- 8.62e-5 
Data<- Data %>% mutate(arrhenius_tmp = 1/(k*Temp))
Data<- Data %>% mutate(lograte = log(GrowthRate))
range(Data$arrhenius_tmp)
range(Data$lograte)

#Model fitting
 library(plyr)
  models <- dlply(Data, "ConPhylum", function(Data) 
    lm(lograte~arrhenius_tmp, data = Data))
  TotalFit<-ldply(models, coef)
  l_ply(models, summary, .print = TRUE)
  write.table (TotalFit, file ="C:/Users/FAN/Desktop/TotalFit.csv", sep =",")
  
#Mean
  TFit<- read.csv("C:/Users/FAN/Desktop/Data/TotalFit.csv", as.is=TRUE)
  TFit <- TFit[!is.na(TFit$Slope),]
  TFit$E<-abs(TFit$Slope)
  mean(TFit$E)
  write.table (TFit, file ="C:/Users/FAN/Desktop/Data/TFit.csv", sep =",")
  TData<-merge(TFit,Data,by='ConPhylum')
  Phylum<-aggregate(TData$GenusSpecies, by=list(type=TData$ConPhylum),length)
  write.table (TData, file ="C:/Users/FAN/Desktop/Data/TData.csv", sep =",")
  write.table (Phylum, file ="C:/Users/FAN/Desktop/Data/Phylum.csv", sep =",")
  
  #Classification
  B <- TData[TData$Superkingdom == "Bacteria",]
  A<-TData[TData$Superkingdom == "Archaea",]
  P<-TData[TData$Superkingdom == "Phytoplankton",]
  
#Plot
TData <- read.csv("C:/Users/FAN/Desktop/Data/TData.csv", as.is=TRUE)
plot_list = list() 
pdf("Phylum.pdf")
m<- split(TData,TData$ConPhylum)
setwd ('C:/Users/FAN/Desktop/Data/Figures')
for(i in 1:length(m)){ sub_data <-  m[[i]]
  B<-names(m[i])
  p<-ggplot(data=sub_data, aes(x=1/(k*Temp),y=lograte))+
  geom_point(aes(shape=Author,color=Author),size=4,alpha=0.7)+
  scale_y_continuous(name="Log(Growth rate)")+
  scale_x_reverse(name="1/kT",sec.axis = sec_axis(~(1/(.*k)-273.15), name = "Temperature(Celsius)"))+
  geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"))+
  labs(title=names(m[i]))
  plot_list[[i]] = p
  print(plot_list[[i]])
}
dev.off()

#Model fitting
TData<- TData %>% mutate(a = (exp(1))^(-E/(k*Temp)))
TData<- TData %>% mutate(TCorrected_GrowthRate = GrowthRate/a)
write.table (TData, file ="C:/Users/FAN/Desktop/Data/TData.csv", sep =",")
TData<-TData[!is.na(TData$TCorrected_GrowthRate),]
models2 <- dlply(TData, "ConPhylum", function(TData) 
  lm(log(TCorrected_GrowthRate)~log(AverageVolume), data = TData))
Fit2<-ldply(models2, coef)
l_ply(models2, summary, .print = TRUE)
write.table (Fit2, file ="C:/Users/FAN/Desktop/Data/Fit2.csv", sep =",")

plot_list = list() 
pdf("Phylum_V.pdf")
m<- split(TData,TData$ConPhylum)
for(i in 1:length(m)){
  sub_data <-  m[[i]]
  B<-names(m[i])
 p<-ggplot(data=sub_data, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate)))+
          geom_point(aes(shape=Author,color=Author),size=4,alpha=0.7)+
          scale_y_continuous(name="Log(Growth rate)")+
          scale_x_continuous(name="Log(Volume)")+
          geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
          theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"))+
          labs(title=names(m[i]))
 plot_list[[i]] = p
 print(plot_list[[i]])
}
dev.off()

#Classify Madin and Hira
m<- split(TData,TData$ConPhylum)
plot_list = list() 
pdf("Madin and Hira.pdf")
for(i in 1:length(m)){
  sub_data <-  m[[i]]
  B<-names(m[i])
 p<-ggplot(data=sub_data, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate),shape=Author,color=Author))+
          geom_point()+
          scale_y_continuous(name="Log(Growth rate)")+
          scale_x_continuous(name="Log(Volume)")+
          geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
          theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"))+
          labs(title=names(m[i]))
  plot_list[[i]] = p
  print(plot_list[[i]])
}
dev.off()



  

