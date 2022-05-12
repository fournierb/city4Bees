#######################################
### Paper: 
### Script to produce Figures SX-SX, variable importance and PDP of 8 functional traits
### Author:
### Date: 
###
###
#######################################
### ===================================
###  Initialise the system
### ===================================
# Remove all R objects in the workspace
rm(list = ls())
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/")
# Package
require(raster)
require(viridis)
require(ggplot2)
require(egg)
library(ggpubr)
require("magrittr")
require(vegan)
# Function for decimal places
scaleFUN <- function(x) sprintf("%.2f", x)
### ===================================
###  Data
### ===================================
### load the data -----------------------------------------------------------------------
pdp.belowground <- read.delim("DATA/Selected descriptors/Results_2022_04_28/belowgound_pdp.txt")
pdp.cleptoparasite <- read.delim("DATA/Selected descriptors/Results_2022_04_28/cleptoparasite_pdp.txt")
pdp.feeding_specialization <- read.delim("DATA/Selected descriptors/Results_2022_04_28/feeding_specialization_pdp.txt")
pdp.ITD <- read.delim("DATA/Selected descriptors/Results_2022_04_28/ITD_pdp.txt")
pdp.phenoduration <- read.delim("DATA/Selected descriptors/Results_2022_04_28/phenoduration_pdp.txt")
pdp.phenostart<- read.delim("DATA/Selected descriptors/Results_2022_04_28/phenostart_pdp.txt")
pdp.solitary <- read.delim("DATA/Selected descriptors/Results_2022_04_28/solitary_pdp.txt")
pdp.tong_length <- read.delim("DATA/Selected descriptors/Results_2022_04_28/tong_length_pdp.txt")
### ===================================
###  Function to plot
### ===================================
Make_PDP_plot_all <- function(dat, ylabel, labels){
  ## Climate
  palette_predictors_climate=c( "#1E1F26","#283655","#4D648D","#D0E1F9")
  var.list = c("clim_PC1","clim_PC2","clim_PC3","clim_PC4")
  dat.clim <- dat[dat$Var %in% var.list,]  
  
  #for(i in var.list){dat.clim$valueX[dat.clim$Var==i] = decostand(dat.clim$valueX[dat.clim$Var==i], "range")}
  
  p.climate <- ggplot(data = dat.clim, aes(y=valueY, x = valueX, 
                                           group = Var, colour = Var)) + 
    geom_smooth(method="loess", span = 1, aes(fill=Var)) +
    scale_color_manual(values =palette_predictors_climate) +
    scale_fill_manual(values = palette_predictors_climate) +
    scale_y_continuous(name = ylabel, breaks =labels , labels=scaleFUN, limits = c(min(labels), max(labels))) + 
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
    xlab("")
  ## Vegetation
  palette_predictors_vegetation=c(  "#6fb98f","#4B7447","#7CAA2D")
  var.list = c( "plant_PC2", "plant_PC3", "plant_PC4")
  dat.veg <- dat[dat$Var %in% var.list,]  
  
 # for(i in var.list){dat.veg$valueX[dat.veg$Var==i] = decostand(dat.veg$valueX[dat.veg$Var==i], "range")  }
  
  p.vegetation <- ggplot(data = dat.veg, aes(y=valueY, x = valueX, 
                                             group = Var, colour = Var)) + 
    geom_smooth(method="loess", span = 1, aes(fill=Var)) +
    scale_color_manual(values =palette_predictors_vegetation) +
    scale_fill_manual(values = palette_predictors_vegetation) +
    scale_y_continuous(name = ylabel, breaks =labels , labels=scaleFUN, limits = c(min(labels), max(labels))) + 
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
    xlab("")
  ## Land-use
  palette_predictors_landuse=c("#ED5752",  "#B38867",  "#CDCDC0")
  var.list = c( "agri2500", "forest2500", "urb2500")
  dat.lu <- dat[dat$Var %in% var.list,]  
  
  #for(i in var.list){dat.lu$valueX[dat.lu$Var==i] = decostand(dat.lu$valueX[dat.lu$Var==i], "range")}
  
  p.landuse <- ggplot(data = dat.lu, aes(y=valueY, x = valueX, 
                                         group = Var, colour = Var)) + 
    geom_smooth(method="loess", span = 1, aes(fill=Var)) +
    scale_color_manual(values =palette_predictors_landuse) +
    scale_fill_manual(values = palette_predictors_landuse) +
    scale_y_continuous(name = ylabel, breaks =labels , labels=scaleFUN, limits = c(min(labels), max(labels))) + 
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
    xlab("")
  ## Beekeeping
  palette_predictors_hive=c( "#F4CC70")
  var.list = c("hive_2500")
  dat.be <- dat[dat$Var %in% var.list,]  
  
  #for(i in var.list){dat.be$valueX[dat.be$Var==i] = decostand(dat.be$valueX[dat.be$Var==i], "range") }
  
  p.beekeeping <- ggplot(data = dat.be, aes(y=valueY, x = valueX, 
                                            group = Var, colour = Var)) + 
    geom_smooth(method="loess", span = 1, aes(fill=Var)) +
    scale_color_manual(values =palette_predictors_hive) +
    scale_fill_manual(values = palette_predictors_hive) +
    scale_y_continuous(name = ylabel, breaks =labels , labels=scaleFUN, limits = c(min(labels), max(labels))) + 
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
    xlab("")
  
  plot.arranged=ggarrange(p.climate,p.vegetation,p.landuse,p.beekeeping,
                          nrow = 1, ncol=4)
                        
#  return(plot.arranged)
  
}
### ===================================
###  Plot
### ===================================
p.belowground <- Make_PDP_plot_all(dat=pdp.belowground, ylabel="", labels=c(0.39,0.45,0.52))
p.cleptoparasite<- Make_PDP_plot_all(dat=pdp.cleptoparasite, ylabel="", labels=c(0.07,  0.13, 0.20))
p.feeding_specialization <- Make_PDP_plot_all(dat=pdp.feeding_specialization, ylabel="", labels=c(0.59, 0.66, 0.73))
p.ITD <- Make_PDP_plot_all(dat=pdp.ITD, ylabel="", labels=c(0.33,0.38 ,0.44))
p.phenoduration<- Make_PDP_plot_all(dat=pdp.phenoduration, ylabel="", labels=c(0.42, 0.47 ,0.52))
p.phenostart<-Make_PDP_plot_all(dat=pdp.phenostart, ylabel="", labels=c(0.42, 0.47 ,0.52))
p.solitary<- Make_PDP_plot_all(dat=pdp.solitary, ylabel="", labels=c(0.40,0.47, 0.54))
p.tong_length<- Make_PDP_plot_all(dat=pdp.tong_length, ylabel="", labels=c(0.42, 0.47, 0.53))
## Arrange
figure.all <-  ggpubr::ggarrange(p.belowground,p.cleptoparasite,p.feeding_specialization,p.ITD,p.phenoduration,p.phenostart,p.solitary,p.tong_length,
                         nrow = 8, ncol=1)
## Export
figure.all %>% ggexport(filename = "OUTPUT/Vimp_PDP/Fig5_PDP_traits.png",
                        width = 1500, height = 1200)

figure.all %>% ggexport(filename = "OUTPUT/Vimp_PDP/Fig5_PDP_traits.pdf",
                        width = 20, height = 17)


