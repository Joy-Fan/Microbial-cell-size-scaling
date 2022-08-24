# Clear the data
rm(list=ls())

#Main dataframe
library(dplyr)
df <- read.csv("C:/Users/FAN/Desktop/Data/bacteria-archaea-traits-1.0.0/output/condensed_species_NCBI.csv", as.is=TRUE)
Data <- read.csv("C:/Users/FAN/Desktop/Final/Tables/Data_Madin_phylum2.csv", as.is=TRUE)
names(df)[2] <-"GenusSpecies"
Data <-Data%>%left_join(df[,c("GenusSpecies","genome_size")],by = "GenusSpecies")
Data <- Data[!is.na(Data$genome_size),]
write.table (Data, file ="C:/Users/FAN/Desktop/Final/Tables/Data_genome.csv", sep =",")
Data<-Data%>%group_by(ConPhylum)%>%filter(length(ConPhylum)>2)

#Classification
A<-subset(Data,Superkingdom=="Archaea")
B<-subset(Data,Superkingdom=="Bacteria")

##Total mdel fitting between genome and volume
fit<-lm(log(genome_size)~log(AverageVolume),data=B)
summary(fit)
Coef_G_B_All<-tidy(summary(fit))
Coef_G_B_All$Kingdom<-"Bacteria"
Coef_G_B_All$R2<-  0.07663
Coef_G_B_All$P<-0.006053

fit<-lm(log(genome_size)~log(AverageVolume),data=A)
summary(fit)
Coef_G_A_All<-tidy(summary(fit))
Coef_G_A_All$Kingdom<-"Archaea"
Coef_G_A_All$R2<- 0.05903 
Coef_G_A_All$P<-0.02596

Coef_G_All<-rbind(Coef_G_A_All,Coef_G_B_All)

write.table (Coef_G_All, file ="C:/Users/FAN/Desktop/Final/Tables/Coef_G_All.csv", sep =",")

#PLot
png(filename = "G_All.jpg",width =1000, height =700)
Data$Superkingdom<-as.factor(Data$superkingdom)
ggplot(data=Data, aes(x=log(AverageVolume),y=log(genome_size),color=Superkingdom))+
  geom_point()+
  scale_y_continuous(name="Log(Genome size)(Bp)")+
  scale_x_continuous(name="Log(Volume)(m^-3)")+
  geom_smooth(method = "lm",se=T,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ Superkingdom,scales= "free")
dev.off()

##Phylum
#MOdel fitting based on phylum
E<-subset(Data,ConPhylum==c("Euryarchaeota"))
F<-subset(Data,ConPhylum=="Firmicutes")
P<-subset(Data,ConPhylum=="Proteobacteria")
list_of_Phylum = split(Data,Data$ConPhylum)
results = lapply(list_of_Phylum, function(Data)lm(log(AverageVolume)~log(genome_size), data = Data))
fit<-lapply(results,summary)
p <- sapply(fit, function(Data) Data$coefficients[,"Pr(>|t|)"])
p<- t(p)
colnames(p) <- c ("p_intercept","p_genome") 
p<-p[,-1]

#Extract the values
Coef1<-ddply(Data, .(ConPhylum), summarize,
             r.sqr = summary(lm(log(AverageVolume)~log(genome_size)))$r.squared,
             a = summary(lm(log(AverageVolume)~log(genome_size)))$coefficients[[2]])

Coef_G_Phyl_all<-cbind(Coef1,p)
write.table (Coef_G_Phyl_all, file ="C:/Users/FAN/Desktop/Final/Tables/Coef_G_Phyl_all.csv", sep =",")

#Model fitting
fit<-lm(log(genome_size)~log(AverageVolume),data=E)
summary(fit)

fit<-lm(log(genome_size)~log(AverageVolume),data=F)
summary(fit)

fit<-lm(log(genome_size)~log(AverageVolume),data=P)
summary(fit)

#Plot (Figure 5)
setwd ('C:/Users/FAN/Desktop/Final/Figures')
png(filename = "G_Phyl.jpg",width =1000, height =700)
Data$ConPhylum<-as.factor(Data$ConPhylum)
ggplot(data=Data, aes(x=log(AverageVolume),y=log(genome_size),shape=ConPhylum,color=ConPhylum))+
  geom_point(size=4,alpha=0.6)+
  scale_y_continuous(name="log(genome_size)(Bp)")+
  scale_x_continuous(name="log(Volume)(m^-3)")+
  geom_smooth(method = "lm",se=T,linetype=1,size=1.5)+
  theme(plot.title = element_text(size=30),axis.title = element_text(size=28,color="black"), axis.text = element_text(size=24),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"),
        panel.spacing.x = unit(1, "cm"),legend.title  = element_text(size =24),legend.text = element_text(size = 20),legend.position = "bottom",
        strip.background.x = element_rect(fill="white"), strip.placement = "outside", strip.text.x = element_text(size = 28, color = "black"))+
  facet_wrap(. ~ ConPhylum,scales= "free")
dev.off()

