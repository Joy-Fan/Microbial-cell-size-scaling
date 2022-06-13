# Clear the data
rm(list=ls())

#Import the Data
library(dplyr)
Data <- read.csv("C:/Users/FAN/Desktop/Data/TData_MB.csv", as.is=TRUE)

#Model fitting
library(dplyr)
models <- dlply(Data, "ConPhylum", function(Data)lm(log(GrowthRate)~log(AverageVolume), data = Data))
Fit<-ldply(models, coef)
l_ply(models, summary, .print = TRUE)
names(Fit)[2] <-"Intercept"
names(Fit)[3] <-"Slope"
write.table (Fit, file ="C:/Users/FAN/Desktop/Data/Uncorrected.csv", sep =",")

#Plot
m<- split(Data,Data$ConPhylum)
setwd ('C:/Users/FAN/Desktop/Data/Figures')
plot_list = list() 
pdf("UncorrectedR_V.pdf")
for(i in 1:length(m)){ sub_data <-  m[[i]]
B<-names(m[i])
p =ggplot(data=sub_data, aes(x=log(AverageVolume),y=log(GrowthRate)))+
        geom_point(size=4,alpha=0.7)+
        scale_y_continuous(name="Log(Growth rate)")+
        scale_x_continuous(name="Log(Volume)")+
        geom_smooth(method = "lm",se=F,linetype=1,size=1.5)+
        theme(plot.title = element_text(size=28),axis.title = element_text(size=24,color="black"), axis.text = element_text(size=20),panel.background = element_rect(fill = "white",colour = "black",size=1),panel.grid =element_line(color="lightgrey"))+
        labs(title=names(m[i]))
plot_list[[i]] = p
print(plot_list[[i]])
}
dev.off()

