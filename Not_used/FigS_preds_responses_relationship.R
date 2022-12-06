#######################################
###
###
###
###
###
###
#######################################

### ===================================
###  Initialise the system
### ===================================
# Remove all R objects in the workspace
rm(list = ls())
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/")
# Packages
require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)
require("magrittr")
require("ggpubr")
library(egg)
### ===================================
###  Data
### ===================================
### Load data
extend.raster=raster("DATA/Selected descriptors/Results_2022_04_28/belowgound.tif") # Extent raster for masking
div <- stack(x = "DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif") # Responses
climate = stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/Climate_PCA_CH_stack.tif") 
vegetation=stack("DATA/Data ready for analyses/Plant_PC_19_revised.tif")
beehive=stack("DATA/Data ready for analyses/beehive-2012-2018.tif")
urban=stack("DATA/Data ready for analyses/Urban_perc_18.tif")
lu=stack("DATA/Data ready for analyses/LU_all_stack.tif") # Responses Land-use, selecting layers 4, 8, 12
### Rename responses
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD","LCBD_fun" ,"LCBD_taxo","phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
### Create a data frame
div.metrics=as.data.frame(div)
div.metrics=na.omit(div.metrics)
### Mask climate raster to CH extent
masked.climate=mask(x =climate,extend.raster )
### Create a data frame of predictors
climate.df=as.data.frame(masked.climate)
climate.df=cbind(climate.df, coordinates(masked.climate))
vegetation.df=as.data.frame(vegetation)
climate.df=na.omit(climate.df)
climate.df$coordsmerged=paste(climate.df$x, climate.df$y, sep="")
vegetation.df=na.omit(vegetation.df)
### Adapt land use  
lu.df=as.data.frame(lu) ## Order: urban, agricultural, forest
lu.df=cbind(lu.df, coordinates(masked.climate)) # Add coordinates
lu.df$coordsmerged=paste(lu.df$x, lu.df$y, sep="") # Merge coordinates in a new column
lu.df=lu.df[lu.df$coordsmerged %in% climate.df$coordsmerged ,]  # select all the cells with climate values (Avoid removing to many NAs, so  the df match in dimensions)
# Adapt beekeeping
beehive.df=as.data.frame(beehive)
beehive.df=cbind(beehive.df, coordinates(masked.climate)) # Add coordinates
beehive.df$coordsmerged=paste(beehive.df$x, beehive.df$y, sep="") # Merge coordinates in a new column
beehive.df=beehive.df[beehive.df$coordsmerged %in% climate.df$coordsmerged ,] # select all the cells with climate values (Avoid removing to many NAs, so  the df match in dimensions)
### Merge al predictors
dat=cbind(div.metrics,climate.df,vegetation.df,beehive.df[,1:8], lu.df[,c(4, 8, 12)])
dat$ID= seq(from=1, to=nrow(dat)) ## dumy column for sampling
### Make a sample
sample.id= sample(dat$ID, size = 5000) ## sample 5000 random points
dat.random=dat[dat$ID %in% sample.id,]

### ===================================
###  Figure
### ===================================

for(r in 1:length(names(div))){
# Plot climate
  p.clim=  ggplot(data = dat.random, aes(y=get(paste(names(div)[r])), x = Climate_PCA_CH_stack.1)) + 
    geom_smooth(method="loess", span = 1, col="#1E1F26", fill="#1E1F26") +
    geom_smooth(method="loess", span = 1, aes(x=Climate_PCA_CH_stack.2), col="#283655", fill="#283655") +
    geom_smooth(method="loess", span = 1, aes(x=Climate_PCA_CH_stack.3), col="#4D648D", fill="#4D648D") +
    geom_smooth(method="loess", span = 1, aes(x=Climate_PCA_CH_stack.4), col="#D0E1F9", fill="#D0E1F9") +
    xlab("Environmental gradient") +
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
  ylab(paste(names(div)[r]))

  # Plot vegetation
p.veg = ggplot(data = dat.random, aes(y=get(paste(names(div)[r])), x = Plant_PC_19_revised.2)) + 
  geom_smooth(method="loess", span = 1, col="#6fb98f", fill="#6fb98f") +
  geom_smooth(method="loess", span = 1, aes(x=Plant_PC_19_revised.3), col="#4B7447", fill="#4B7447") +
  geom_smooth(method="loess", span = 1, aes(x=Plant_PC_19_revised.4), col="#7CAA2D", fill="#7CAA2D") +
  xlab("Environmental gradient") +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank()) +
  ylab(paste(names(div)[r]))
# Plot LU
p.lu = ggplot(data = dat.random,aes(y=get(paste(names(div)[r])), x = LU_all_stack.4)) +
  geom_smooth(method="loess", span = 1, col="#CDCDC0", fill="#CDCDC0") +
  geom_smooth(method="loess", span = 1, aes(x=LU_all_stack.8), col="#ED5752", fill="#ED5752") +
  geom_smooth(method="loess", span = 1, aes(x=LU_all_stack.12), col="#B38867", fill="#B38867") +
  xlab("Prop. in 2500 m") +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank()) +
  ylab(paste(names(div)[r]))
# Plot beekeeping
p.beekeeping =  ggplot(data = dat.random, aes(y=get(paste(names(div)[r])), x =beehive.2012.2018.8)) +  
  geom_smooth(method="loess", span = 1, col="#F4CC70", fill="#F4CC70") +
  xlab("N. of beehives in 2500 m") +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank()) +
  ylab(paste(names(div)[r]))
# Arrange plot
plot.arranged=ggarrange(p.clim,p.veg,p.lu,p.beekeeping,
                        nrow = 1, ncol=4)
# Export plot
plot.arranged %>% ggexport(filename = paste("OUTPUT/Responses_predictors_direct/FigS",names(div)[r],"_predictors.png", sep=""),
                                width = 1400, height = 400)
plot.arranged %>% ggexport(filename = paste("OUTPUT/Responses_predictors_direct/FigS",names(div)[r],"_predictors.pdf", sep=""),
                                width = 14, height = 4)
}



