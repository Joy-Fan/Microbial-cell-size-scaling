# Clear the data
rm(list=ls())

#Main dataframe
df <- read.csv("C:/Users/FAN/Desktop/Data/bacteria-archaea-traits-1.0.0/output/condensed_species_GTDB.csv", as.is=TRUE)

#Clear the data
df <- df[!is.na(df$species),]
df <- df[!is.na(df$growth_tmp),]
df <- df[!is.na(df$doubling_h),]

#Group by shape
library(dplyr)
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

#arrhenius tmp
df <- df %>% mutate(arrhenius_tmp = ifelse(!is.na(growth_tmp), 1/(growth_tmp + 273), NA))
figure4<-ggplot(data = df,aes(x=arrhenius_tmp,y=log10(growth_rate),color=superkingdom))+
  geom_point(size=2)+
  theme_classic()

model <- lm(log10(growth_rate)~arrhenius_tmp, data = df)
summary(model)




#Integrated data
V<- read.csv("C:/Users/FAN/Desktop/Data/Volume_Madin and Hira.csv", as.is=TRUE)

#classify archaea, bacteria and phytoplankton
da<-filter(V,Superkingdom=="Archaea")
Archaea<-da%>%group_by(Species)%>%summarize(Volume_a=mean(Volume))
Archaea$Superkingdom<-"Archaea"
  
db<-filter(V,Superkingdom=="Bacteria")
Bacteria<-db%>%group_by(Species)%>%summarize(Volume_a=mean(Volume))
Bacteria$Superkingdom<-"Bacteria"

dp<-filter(V,Superkingdom=="Phytoplankton")
Phyto<-dp%>%group_by(Species)%>%summarize(Volume_a=mean(Volume))
Phyto$Superkingdom<-"Phytoplankton"

Total<-rbind(Archaea,Bacteria,Phyto)

library(plyr)
mu <- ddply(Total, "Superkingdom", summarise,mu=mean(Volume))

#V of bacteria and archae
figure2<-ggplot(df, aes(x=log10(volume), color=superkingdom,fill=superkingdom)) +
  geom_density(alpha=0.4)+
  geom_vline(data=mu, aes(xintercept=log10(mu), color=superkingdom),
             linetype="dashed")+
  theme_classic()

