# Clear the data
rm(list=ls())

#Import the Data
library(dplyr)
TData <- read.csv("C:/Users/FAN/Desktop/Data/TData.csv", as.is=TRUE)
Data2 <- read.csv("C:/Users/FAN/Desktop/Data/Data2.csv", as.is=TRUE)
TData2 <- TData %>% left_join(Data2[,c("GenusSpecies","TempPref")], by = "GenusSpecies")

#Model fitting
library(dplyr)
Meso<-subset(TData2,TempPref=="Mesophile")
Thermo<-subset(TData2,TempPref=="Thermophile")
models <- dlply(Meso, "ConPhylum", function(Meso)lm(lograte~arrhenius_tmp, data = Meso))
TotalFit_MB<-ldply(models, coef)
l_ply(models, summary, .print = TRUE)
names(TotalFit_MB)[2] <-"Intercept2"
names(TotalFit_MB)[3] <-"Slope2"
TotalFit_MB$E2<-abs(TotalFit_MB$Slope2)
TotalFit_MB$TempPref<-"Mesophile"

models <- dlply(Thermo, "ConPhylum", function(Thermo)lm(lograte~arrhenius_tmp, data = Thermo))
TotalFit_TB<-ldply(models, coef)
l_ply(models, summary, .print = TRUE)
names(TotalFit_TB)[2] <-"Intercept2"
names(TotalFit_TB)[3] <-"Slope2"
TotalFit_TB$E2<-abs(TotalFit_TB$Slope2)
TotalFit_TB$TempPref<-"Thermophile"
TotalFit_B<-rbind(TotalFit_TB,TotalFit_MB)
TData_MB<-full_join(TotalFit_B,TData2,by=c("ConPhylum","TempPref"))
write.table (TotalFit_B, file ="C:/Users/FAN/Desktop/TotalFit_B.csv", sep =",")
write.table (TData_MB, file ="C:/Users/FAN/Desktop/TData_MB.csv", sep =",")

#Plot
m<- split(TData_MB,TData_MB$ConPhylum)
k<-8.62e-5 
setwd ('C:/Users/FAN/Desktop/Data/Figures')
plot_list = list() 
pdf("Phylum_B.pdf")
for(i in 1:length(m)){ sub_data <-  m[[i]]
B<-names(m[i])
p<-ggplot(data=sub_data, aes(x=1/(k*Temp),y=lograte,color=TempPref))+
        geom_point(aes(shape=Author),size=4,alpha=0.7)+
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
TData_MB<- TData_MB %>% mutate(TCorrected_GrowthRate = GrowthRate/(exp(1)^(-E/(k*Temp))))
TData_MB<-TData_MB%>% mutate(TCorrected_GrowthRate2 = GrowthRate/(exp(1)^(-E2/(k*Temp))))
write.table (TData_MB, file ="C:/Users/FAN/Desktop/Data/TData_MB.csv", sep =",")
TData_MB<-TData_MB[!is.na(TData_MB$TCorrected_GrowthRate2),]
models2 <- dlply(TData_MB, "ConPhylum", function(TData_MB) 
  lm(log(TCorrected_GrowthRate2)~log(AverageVolume), data = TData_MB))
Fit2_MB<-ldply(models2, coef)
l_ply(models2, summary, .print = TRUE)
write.table (Fit2_MB, file ="C:/Users/FAN/Desktop/Data/Fit2_MB.csv", sep =",")

setwd ('C:/Users/FAN/Desktop/Data/Figures/Phylum_V')
m<- split(TData_MB,TData_MB$ConPhylum)
setwd ('C:/Users/FAN/Desktop/Data/Figures')
plot_list = list() 
pdf("Phylum_BV.pdf")
for(i in 1:length(m)){
  sub_data <-  m[[i]]
  B<-names(m[i])
  p<-ggplot(data=sub_data, aes(x=log(AverageVolume),y=log(TCorrected_GrowthRate2)))+
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

