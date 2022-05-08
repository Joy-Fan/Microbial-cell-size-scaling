# Clear the data
rm(list=ls())

#Main dataframe
df <- read.csv("C:/Users/FAN/Desktop/Data/bacteria-archaea-traits-1.0.0/output/condensed_species_GTDB.csv", as.is=TRUE)

#Clear the data
df <- df[!is.na(df$species),]
df <- df[!is.na(df$growth_tmp),]
df <- df[!is.na(df$doubling_h),]

#Create shapeagg column
df$shapeagg <- df$cell_shape
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("coccus")] <- "spheroid"
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("bacillus","coccobacillus","vibrio")] <- "rod"
df$shapeagg[!is.na(df$shapeagg) & df$shapeagg %in% c("pleomorphic","filament", "star", "spiral", "irregular", "flask", "spindle", "fusiform", "disc", "disc ", "square", "branced", "triangular")] <- NA

#Calculate mid diameter and volumes 


# Add growth rate 
library(dplyr)
df<-mutate(df,growth_rate <- log(2)/doubling_h)

#Growth rate and growth_temp
plot(log10(df$growth_rate)~log10(df$growth_tmp))

#Growth rate and optimum_temp
df2 <- df[!is.na(df$optimum_tmp),]
plot(log10(df2$growth_rate)~log10(df2$optimum_tmp))

#
