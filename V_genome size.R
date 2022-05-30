# Clear the data
rm(list=ls())

#Main dataframe
library(dplyr)
df <- read.csv("C:/Users/FAN/Desktop/Data/bacteria-archaea-traits-1.0.0/output/condensed_species_NCBI.csv", as.is=TRUE)
Data <- read.csv("C:/Users/FAN/Desktop/Data/TData_MB.csv", as.is=TRUE)
names(df)[2] <-"GenusSpecies"
Data <-Data%>%left_join(df[,c("GenusSpecies","genome_size")],by = "GenusSpecies")
Data <- Data[!is.na(Data$genome_size),]
write.table (Data, file ="C:/Users/FAN/Desktop/Data/Data_genome.csv", sep =",")

#MOdel fitting
models <- dlply(Data, "ConPhylum", function(Data) 
  lm(AverageVolume~genome_size, data = Data))
Fit<-ldply(models, coef)
l_ply(models, summary, .print = TRUE)
write.table (Fit, file ="C:/Users/FAN/Desktop/Data/Fit_V.csv", sep =",")

#Plot
setwd ('C:/Users/FAN/Desktop/Data/Figures')
plot_list = list() 
pdf("Genome_Size.pdf")
m<- split(Data,Data$ConPhylum)
for(i in 1:length(m)){
  sub_data <-  m[[i]]
  B<-names(m[i])
  p<-ggplot(data=sub_data, aes(x=genome_size,y=AverageVolume))+
          geom_point(size=4,alpha=0.7)+
          scale_y_continuous(name="Volume(m^3)")+
          scale_x_continuous(name="genome size(Bp)")+
          geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
          theme(plot.title = element_text(size=30),axis.title = element_text(size=20,color="black"), axis.text = element_text(size=15),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"))+
          labs(title=names(m[i]))
        plot_list[[i]] = p
        print(plot_list[[i]])
}
dev.off()

#All Data from Madin
df <- read.csv("C:/Users/FAN/Desktop/Data/V.csv", as.is=TRUE,na.strings=c("","","NA"))
Data <- df[!is.na(df$genome_size),]
Data$phylum[is.na(Data$phylum)] <- "Others"

#MOdel fitting
models <- dlply(Data, "phylum", function(Data) 
  lm(volume~genome_size, data = Data))
Fit<-ldply(models, coef)
l_ply(models, summary, .print = TRUE)
write.table (Fit, file ="C:/Users/FAN/Desktop/Data/Fit_V2.csv", sep =",")

#Plot
setwd("C:/Users/FAN/Desktop/Data/Figures/V_genome size")
m<- split(Data,Data$phylum)
setwd ('C:/Users/FAN/Desktop/Data/Figures')
plot_list = list() 
pdf("Genome_Size_T.pdf")
for(i in 1:length(m)){
  sub_data <-  m[[i]]
  B<-names(m[i])
  p<-ggplot(data=sub_data, aes(x=genome_size,y=volume))+
          geom_point(size=4,alpha=0.7)+
          scale_y_continuous(name="Volume(m^3)")+
          scale_x_continuous(name="genome size(Bp)")+
          geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
          theme(plot.title = element_text(size=30),axis.title = element_text(size=20,color="black"), axis.text = element_text(size=15),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"))+
          labs(title=names(m[i]))
  plot_list[[i]] = p
  print(plot_list[[i]])
}
dev.off()


