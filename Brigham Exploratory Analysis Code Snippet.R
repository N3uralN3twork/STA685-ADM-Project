#STA 685 Cover Type Exploratory Analysis Clip

#################################
#                               #
#     Exploratory Analysis      #
#                               #
#################################

#Dimensions and Variable Names
dim(df)
colnames(df)

#Summary Statistics
summary(df)
summary(df[,1:15]) #summary of all predictors minus the soil type dummy variables

#Tables of Observation Frequencies
table(df$class)
table(df$SoilType)
table(df$WildernessArea)

#Missing Values
sum(is.na(df)) #no missing values

#Correlation Plot
col = c(colnames(df)[1:10], 'class')
d = df
d$class = as.numeric(d$class)
c = cor(d[1:1000, col])

corr = round(cor(d[, col]), 1)
ggcorrplot(c, hc.order = TRUE, type = "lower",
           lab = TRUE) +ggtitle('Correlation Plot for Numeric Variables') + ylab('Proportion')  

#Plot soil type results
ggplot(df, aes(x=SoilType, fill=CoverName)) + geom_bar(aes(y=(..count..)/sum(..count..))) + 
  ggtitle('Proportion of Observations by Soil Type') + ylab('Proportion')  + 
  theme(legend.title=element_blank(), legend.position='bottom') +
  scale_x_discrete(breaks = 1:40) + theme(text = element_text(size=16))

#Plot Densities Stratified by Elevation - Good Separation of Cover Types
plot(density(df$Elevation[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Elevation')
lines(density(df$Elevation[which(df$class == 2)]), col = 2)
lines(density(df$Elevation[which(df$class == 3)]), col = 3)
lines(density(df$Elevation[which(df$class == 4)]), col = 4)
lines(density(df$Elevation[which(df$class == 5)]), col = 5)
lines(density(df$Elevation[which(df$class == 6)]), col = 6)
lines(density(df$Elevation[which(df$class == 7)]), col = 7)
legend("topright", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                            "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)


#Plot Densities Stratified by Slope - Poor Separation of Cover Types
plot(density(df$Slope[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Slope')
lines(density(df$Slope[which(df$class == 2)]), col = 2)
lines(density(df$Slope[which(df$class == 3)]), col = 3)
lines(density(df$Slope[which(df$class == 4)]), col = 4)
lines(density(df$Slope[which(df$class == 5)]), col = 5)
lines(density(df$Slope[which(df$class == 6)]), col = 6)
lines(density(df$Slope[which(df$class == 7)]), col = 7)
legend("topright", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                            "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)

#Plot Densities Stratified by Aspect - Poor Separation of Cover Types
plot(density(df$Aspect[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Aspect')
lines(density(df$Aspect[which(df$class == 2)]), col = 2)
lines(density(df$Aspect[which(df$class == 3)]), col = 3)
lines(density(df$Aspect[which(df$class == 4)]), col = 4)
lines(density(df$Aspect[which(df$class == 5)]), col = 5)
lines(density(df$Aspect[which(df$class == 6)]), col = 6)
lines(density(df$Aspect[which(df$class == 7)]), col = 7)
legend("topright", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                            "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)

#Plot Densities Stratified by Hillshade_9am - Poor Separation of Cover Types
plot(density(df$Hillshade_9am[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Hillshade_9am')
lines(density(df$Hillshade_9am[which(df$class == 2)]), col = 2)
lines(density(df$Hillshade_9am[which(df$class == 3)]), col = 3)
lines(density(df$Hillshade_9am[which(df$class == 4)]), col = 4)
lines(density(df$Hillshade_9am[which(df$class == 5)]), col = 5)
lines(density(df$Hillshade_9am[which(df$class == 6)]), col = 6)
lines(density(df$Hillshade_9am[which(df$class == 7)]), col = 7)
legend("topleft", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                           "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)

#Plot Densities Stratified by Hillshade_Noon - Poor Separation of Cover Types
plot(density(df$Hillshade_Noon[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Hillshade_Noon')
lines(density(df$Hillshade_Noon[which(df$class == 2)]), col = 2)
lines(density(df$Hillshade_Noon[which(df$class == 3)]), col = 3)
lines(density(df$Hillshade_Noon[which(df$class == 4)]), col = 4)
lines(density(df$Hillshade_Noon[which(df$class == 5)]), col = 5)
lines(density(df$Hillshade_Noon[which(df$class == 6)]), col = 6)
lines(density(df$Hillshade_Noon[which(df$class == 7)]), col = 7)
legend("topleft", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                           "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)

#Plot Densities Stratified by Hillshade_3pm - Poor Separation of Cover Types
plot(density(df$Hillshade_3pm[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Hillshade_3pm')
lines(density(df$Hillshade_3pm[which(df$class == 2)]), col = 2)
lines(density(df$Hillshade_3pm[which(df$class == 3)]), col = 3)
lines(density(df$Hillshade_3pm[which(df$class == 4)]), col = 4)
lines(density(df$Hillshade_3pm[which(df$class == 5)]), col = 5)
lines(density(df$Hillshade_3pm[which(df$class == 6)]), col = 6)
lines(density(df$Hillshade_3pm[which(df$class == 7)]), col = 7)
legend("topleft", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                           "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)

#Plot Densities Stratified by Horizontal_Distance_To_Hydrology - Poor Separation of Cover Types
plot(density(df$Horizontal_Distance_To_Hydrology[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Horizontal_Distance_To_Hydrology')
lines(density(df$Horizontal_Distance_To_Hydrology[which(df$class == 2)]), col = 2)
lines(density(df$Horizontal_Distance_To_Hydrology[which(df$class == 3)]), col = 3)
lines(density(df$Horizontal_Distance_To_Hydrology[which(df$class == 4)]), col = 4)
lines(density(df$Horizontal_Distance_To_Hydrology[which(df$class == 5)]), col = 5)
lines(density(df$Horizontal_Distance_To_Hydrology[which(df$class == 6)]), col = 6)
lines(density(df$Horizontal_Distance_To_Hydrology[which(df$class == 7)]), col = 7)
legend("topright", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                            "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)

#Plot Densities Stratified by Horizontal_Distance_To_Roadways - Poor Separation of Cover Types
plot(density(df$Horizontal_Distance_To_Roadways[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Horizontal_Distance_To_Roadways')
lines(density(df$Horizontal_Distance_To_Roadways[which(df$class == 2)]), col = 2)
lines(density(df$Horizontal_Distance_To_Roadways[which(df$class == 3)]), col = 3)
lines(density(df$Horizontal_Distance_To_Roadways[which(df$class == 4)]), col = 4)
lines(density(df$Horizontal_Distance_To_Roadways[which(df$class == 5)]), col = 5)
lines(density(df$Horizontal_Distance_To_Roadways[which(df$class == 6)]), col = 6)
lines(density(df$Horizontal_Distance_To_Roadways[which(df$class == 7)]), col = 7)
legend("topright", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                            "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)

#Plot Densities Stratified by Vertical_Distance_To_Hydrology - Poor Separation of Cover Types
plot(density(df$Vertical_Distance_To_Hydrology[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Vertical_Distance_To_Hydrology')
lines(density(df$Vertical_Distance_To_Hydrology[which(df$class == 2)]), col = 2)
lines(density(df$Vertical_Distance_To_Hydrology[which(df$class == 3)]), col = 3)
lines(density(df$Vertical_Distance_To_Hydrology[which(df$class == 4)]), col = 4)
lines(density(df$Vertical_Distance_To_Hydrology[which(df$class == 5)]), col = 5)
lines(density(df$Vertical_Distance_To_Hydrology[which(df$class == 6)]), col = 6)
lines(density(df$Vertical_Distance_To_Hydrology[which(df$class == 7)]), col = 7)
legend("topright", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                            "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)

#Plot Densities Stratified by Horizontal_Distance_To_Fire_Points - Poor Separation of Cover Types
plot(density(df$Horizontal_Distance_To_Fire_Points[which(df$class == 1)]), col = 1, xlim = c(0,1), 
     ylim = c(0,10), xlab = "Elevation", main = 'Density Plots for Cover Types by Horizontal_Distance_To_Fire_Points')
lines(density(df$Horizontal_Distance_To_Fire_Points[which(df$class == 2)]), col = 2)
lines(density(df$Horizontal_Distance_To_Fire_Points[which(df$class == 3)]), col = 3)
lines(density(df$Horizontal_Distance_To_Fire_Points[which(df$class == 4)]), col = 4)
lines(density(df$Horizontal_Distance_To_Fire_Points[which(df$class == 5)]), col = 5)
lines(density(df$Horizontal_Distance_To_Fire_Points[which(df$class == 6)]), col = 6)
lines(density(df$Horizontal_Distance_To_Fire_Points[which(df$class == 7)]), col = 7)
legend("topright", legend=c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine", 
                            "Cottonwood/Willow", "Aspen", "Douglas/Fir", "Krummholz"),
       col=1:7, lty = 1, cex=0.4)
