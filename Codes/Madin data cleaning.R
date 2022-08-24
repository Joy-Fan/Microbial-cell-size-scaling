# Clear the data
rm(list=ls())

#Main dataframe
df <- read.csv("C:/Users/FAN/Desktop/Data/bacteria-archaea-traits-1.0.0/output/condensed_species_NCBI.csv", as.is=TRUE)

#Clear the data
df <- df[!is.na(df$species),]
df <- df[!is.na(df$optimum_tmp),]
df <- df[!is.na(df$doubling_h_norm),]

#Group by shape
library(dplyr)
library(ggplot2)
Shape<-df%>%group_by(cell_shape)%>%summarise(total_N=length(species))
p <- ggplot(Shape,aes(cell_shape,total_N))+geom_bar(stat = 'identity')

#Create shapeagg column(exclude na)
df$shapeagg <- df$cell_shape
df$shapeagg[!is.na(df$shapeagg)&df$shapeagg %in% c("coccus")] <- "spheroid"
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("bacillus","coccobacillus","vibrio")] <- "rod"
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("pleomorphic","filament", "star", "spiral", "irregular", "flask", "spindle", "fusiform", "disc", "disc ", "square", "branced", "triangular")] <- NA

#Create shapeagg column(include na)
df$shapeagg <- df$cell_shape
df$shapeagg[is.na(df$shapeagg)] <- "oval"
df$shapeagg[df$shapeagg %in% c("coccus")] <- "oval"
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("bacillus","coccobacillus","vibrio")] <- "rod"
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("pleomorphic","filament", "star", "spiral", "irregular", "flask", "spindle", "fusiform", "disc", "disc ", "square", "branced", "triangular")] <- NA

#Calculate mid diameter and volumes
library(dplyr)
df$d1_mid <- ifelse(!is.na(df$d1_up), (df$d1_lo + df$d1_up)/2, df$d1_lo)
df$d2_mid <- ifelse(!is.na(df$d2_up), (df$d2_lo + df$d2_up)/2, df$d2_lo)

#Get the value needed(volume calculation)
tmp1 <- df[!is.na(df$d1_mid) &!is.na(df$shapeagg) & df$shapeagg %in% c("rod","oval"), c("species_tax_id","d1_mid","d2_mid","shapeagg","cell_shape")]
tmp1$volume <- NA
for(i in 1:nrow(tmp1)) {
  if(!is.na(tmp1$d1_mid[i])) {
    if(tmp1$shapeagg[i] == "oval") {
      d <- NA
      if(!is.na(tmp1$d2_mid[i])) {
        d1 <- tmp1$d1_mid[i]
        d2<-tmp1$d2_mid[i]
      } else {
        d1 <- tmp1$d1_mid[i]
        d2 <- tmp1$d1_mid[i]
      }
      tmp1$volume[i] <- 4/3*pi*(d1/2)^2*d2*(10^(-18))
    } else {
      if(!is.na(tmp1$d2_mid[i])) {
        #Calculate as rod with hemispherical ends
        #end volume:
        ends <- 4/3*pi*(tmp1$d1_mid[i]/2)^3*(10^(-18))
        #body: length minus diameter used in ends
        body <- pi*(tmp1$d1_mid[i]/2)^2*(tmp1$d2_mid[i]-tmp1$d1_mid[i])*(10^(-18))
        if(body>0) {
          tmp1$volume[i] <- ends+body
        } else {
          tmp1$volume[i] <- ends
        }
      }
    }
  }
}
tmp1 <- tmp1[!is.na(tmp1$volume),]
str(tmp1)

#Compare tmp(222species) and tmp1(352species)
mean(log10(tmp1$volume))
mean(log10(tmp$volume))
library(ggplot2)
F1<-ggplot(tmp,aes(x=log10(volume)))+
             geom_density(color="blue", fill="lightblue",alpha=0.6)+
             geom_density(data=tmp1,aes(x=log10(volume)),color="black", fill="green",alpha=0.4)+
             theme_classic()

#Move to main data frame
df <- df %>% left_join(tmp1[,c("species_tax_id","volume")], by = "species_tax_id")
df<- df[!is.na(df$volume),]

# Add growth rate
df<-mutate(df,growth_rate=log(2)/doubling_h_norm)

#Output
write.table (df, file ="C:/Users/FAN/Desktop/Data/Madin.csv", sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)

##Data Merge to produce merged dataset 1
# Clear the data
rm(list=ls())

#Import the data
Madin <- read.csv("C:/Users/FAN/Desktop/Data/Madin.csv", as.is=TRUE)
Madin$Author<-c("Madin")
Hira <- read.csv("C:/Users/FAN/Desktop/Data/Hira.csv", as.is=TRUE)
Hira$Author<-c("Hira")

#Merge the data
Total<-rbind(Madin,Hira)
T <- Total[order(Total$GenusSpecies, -Total$GrowthRate),]
library(dplyr)
library(plyr)
Replicated<-T%>%group_by(GenusSpecies)%>%filter(length(GenusSpecies)>1)
T<-T[!duplicated(T$GenusSpecies),]
write.table (Replicated, file ="C:/Users/FAN/Desktop/Data/Duplicates.csv", sep =",")
write.table (T, file ="C:/Users/FAN/Desktop/Data/Merged_Data.csv", sep =",")


