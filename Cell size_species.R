# Clear the data
rm(list=ls())

#Main dataframe
df <- read.csv("C:/Users/FAN/Desktop/Data/bacteria-archaea-traits-1.0.0/output/condensed_species_GTDB.csv", as.is=TRUE)

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

#Get the value needed(include na)
tmp1 <- df[!is.na(df$d1_mid) & !is.na(df$shapeagg) & df$shapeagg %in% c("rod","oval"), c("species_tax_id","d1_mid","d2_mid","shapeagg","cell_shape")]
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

#Get the value needed(exclude na)
tmp <- df[!is.na(df$d1_mid) & !is.na(df$shapeagg) & df$shapeagg %in% c("rod","spheroid"), c("species_tax_id","d1_mid","d2_mid","shapeagg","cell_shape")]
tmp$volume <- NA
for(i in 1:nrow(tmp)) {
  if(!is.na(tmp$d1_mid[i])) {
    if(tmp$shapeagg[i] == "oval") {
      d <- NA
      #If there is a descrepancy bewteen width and lenghth, use mean
      if(!is.na(tmp$d2_mid[i])) {
        #Calculate mean of the two values
        d <- (tmp$d1_mid[i]+tmp$d2_mid[i])/2
      } else {
        d <- tmp$d1_mid[i]
      }
      tmp$volume[i] <- 4/3*pi*(d/2)^3*(10^(-18))
    } else {
      if(!is.na(tmp$d2_mid[i])) {
        #Calculate as rod with hemispherical ends
        #end volume:
        ends <- 4/3*pi*(tmp$d1_mid[i]/2)^3*(10^(-18))
        #body: length minus diameter used in ends
        body <- pi*(tmp$d1_mid[i]/2)^2*(tmp$d2_mid[i]-tmp$d1_mid[i])*(10^(-18))
        if(body>0) {
          tmp$volume[i] <- ends+body
        } else {
          tmp$volume[i] <- ends
        }
      }
    }
  }
}
tmp <- tmp[!is.na(tmp$volume),]
str(tmp)
write.table (tmp1, file ="C:/Users/FAN/Desktop/Volume1.csv")

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
write.table (df, file ="C:/Users/FAN/Desktop/Volume_Madin.csv", sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)

#Growth rate and growth_temp
figure3<-ggplot(data = df,aes(x=growth_tmp,y=log10(growth_rate),color=superkingdom))+
  geom_point(size=2,alpha=0.6)+
  theme_classic()

#Growth rate and optimum_temp
figure5<-ggplot(data = df,aes(x=optimum_tmp,y=log10(growth_rate),color=superkingdom))+
  geom_point(size=2,alpha=0.6)+
  theme_classic()

# Using "segmented" R package to find analysis break-points in the data
library(segmented)
Data<-read.csv('C:/Users/FAN/Desktop/Data/Volume_Madin.csv')
Data<- Data[!is.na(Data$optimum_tmp),]
Data$optimum_tmp<-Data$optimum_tmp+273.15
  
bacteria_d <- Data[Data$superkingdom == "Bacteria",]
archaea_d<-Data[Data$superkingdom == "Archaea",]

ggplot(Data, aes(x = 1/optimum_tmp, y = log(growth_rate), col =superkingdom)) +
  geom_point()

ggplot(bacteria_d, aes(x = 1/optimum_tmp, y = log(growth_rate))) +
  geom_point()

ggplot(archaea_d, aes(x = 1/optimum_tmp, y = log(growth_rate))) +
  geom_point()

k <- 8.62e-5 
x = 1 / (k*(bacteria_d$optimum_tmp))
y = log(bacteria_d$growth_rate)
boltzmann_linear <- lm(y ~ x)
os <- segmented(boltzmann_linear, seg.Z =~ x) 
plot(x,y)
plot(os, add=T)
summary.segmented(os)
os$psi[2]
bacteria_breakpoint_celsius <- (1/(k*os$psi[2]))-273.15
bacteria_breakpoint_celsius

# repeat for archaea
x = 1 / (k*(archaea_d$optimum_tmp))
y = log(archaea_d$growth_rate)

boltzmann_linear <- lm(y ~ x)

os <- segmented(boltzmann_linear, seg.Z =~ x)
plot.segmented(os)
summary.segmented(os)

os$psi[2] # <-- this is the break point

archaea_breakpoint_celsius <- (1/(k*os$psi[2]))-273.15
archaea_breakpoint_celsius

#classify prokaryotes into Thermo and Meso
Data$TempPref <- NA
for (i in 1:length(Data$optimum_tmp)){
  if (!is.na(Data$optimum_tmp[i])){
    if (Data$superkingdom[i] == "Bacteria"){
      if (Data$optimum_tmp[i] > 312.53){ #39.38C
        Data$TempPref[i] <- "Thermophile"
      }
      else{
        Data$TempPref[i] <- "Mesophile"
      }
    }
    else if (Data$superkingdom[i] == "Archaea"){
      if (Data$optimum_tmp[i] >299.16){ #26.01c
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
  else{
    data$TempPref[i] <- "NA"
  }
}

bacteria_d <- Data[Data$superkingdom == "Bacteria",]
archaea_d<-Data[Data$superkingdom == "Archaea",]

ggplot(bacteria_d, aes(x =1/(k*optimum_tmp), y = log(growth_rate), col = TempPref)) +
geom_point()

ggplot(archaea_d, aes(x =1/(k*optimum_tmp), y = log(growth_rate), col = TempPref)) +
  geom_point()

#Clear the data from Smith
V_S<- read.csv("C:/Users/FAN/Desktop/Data/Volume_Smith.csv", as.is=TRUE)
V_S<-V_S[!is.na(V_S$ConLabGrowthTemp),]
V_S <- V_S[!is.na(V_S$ConSize),]
V_S <- V_S[!is.na(V_S$StandardisedTraitValue),]

#Integrated data from Hira and Madin
V<- read.csv("C:/Users/FAN/Desktop/Data/Volume_Madin and Hira.csv", as.is=TRUE)

#classify archaea, bacteria and phytoplankton
da<-filter(V,Superkingdom=="Archaea")
Archaea<-da%>%group_by(Species)%>%summarize(Volume_a=mean(Volume))
Archaea$Superkingdom<-"Archaea"
write.table (df, file ="C:/Users/FAN/Desktop/Volume_Archaea.csv", sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)
  
db<-filter(V,Superkingdom=="Bacteria")
Bacteria<-db%>%group_by(Species)%>%summarize(Volume_a=mean(Volume))
Bacteria$Superkingdom<-"Bacteria"
write.table (df, file ="C:/Users/FAN/Desktop/Volume_Bacteria.csv", sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)

dp<-filter(V,Superkingdom=="Phytoplankton")
Phyto<-dp%>%group_by(Species)%>%summarize(Volume_a=mean(Volume))
Phyto$Superkingdom<-"Phytoplankton"
write.table (df, file ="C:/Users/FAN/Desktop/Volume_Phyto.csv", sep =",", row.names =TRUE, col.names =TRUE, quote =TRUE)

Total<-rbind(Archaea,Bacteria,Phyto)

library(plyr)
mu <- ddply(Total, "Superkingdom", summarise,mu=mean(Volume_a))

#V of bacteria£¬archae and Phyto
library(ggplot2)
F2<-ggplot(Total, aes(x=log10(Volume_a), color=Superkingdom,fill=Superkingdom)) +
  geom_density(alpha=0.4)+
  geom_vline(data=mu, aes(xintercept=log10(mu), color=Superkingdom),
             linetype="dashed")+
  theme_classic()




