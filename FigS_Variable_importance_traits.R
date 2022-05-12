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
require("magrittr")
require("ggpubr")
require(egg)
### ===================================
###  Data
### ===================================
varimp <- read.delim("DATA/Selected descriptors/Results_2022_04_28/Diversity_VariableImportance_metrics.txt")
varimp = varimp[varimp$variable %in% c("belowgound","cleptoparasite", "feeding_specialization", 
                                       "ITD","phenoduration","phenostart", "solitary", "tong_length"),]
### ===================================
###  Function to plot
### ===================================
plot_Imp <- function(varimp, var, title){
  
  list.varimp = list(NULL)
  for(i in 1:length(unique(varimp$variable))){
    var1=unique(varimp$variable)[i]
    varimp.var <- varimp[varimp$variable==var1,]
    varimp.var$varimp_rf = varimp.var$varimp_rf/max(varimp.var$varimp_rf)
    rownames(varimp.var) = varimp.var$model
    list.varimp[[i]] = varimp.var
  }
  
  n = which(unique(varimp$variable) == var)
  #var.type = c(rep("Climate",4), rep("Land use",3), rep("Vegetation",3), "Hive")
  
  var.type = unique(varimp$model)
  var.type = factor(var.type, levels = unique(varimp$model))
  color.codes=c("Climate","Climate","Climate","Climate","Urban", "Agriculture", "Forest","Vegetation" ,"Vegetation" ,"Vegetation" , "Beekeeping")
  color.codes=factor(color.codes, levels = unique(color.codes))
  
  dat = data.frame(list.varimp[[n]], var.type = color.codes)
  dat$model = factor(rownames(dat), levels = rownames(dat))
  dat$variable=as.factor(dat$variable)
  p.varimp.rich <- ggplot(data=dat, aes(varimp_rf, model, fill = model)) +
    geom_bar(stat="identity") +
    scale_fill_manual("", values = c("clim_PC1"="#4D85BD",
                                     "clim_PC2"="#283655",
                                     "clim_PC3"="#4D648D",
                                     "clim_PC4"="#D0E1F9",
                                     "plant_PC2"="#6fb98f",
                                     "plant_PC3"="#4B7447",
                                     "plant_PC4"="#7CAA2D",
                                     "agri2500" = "#ED5752",
                                     "forest2500" = "#B38867",
                                     "urb2500" = "#CDCDC0",
                                     "hive_2500" ="#F4CC70")) +
    ggtitle(title) +
    theme_classic(base_size = 20)+ 
    theme(
      # Set background color to white
      panel.background = element_rect(fill = "white"),
      # Set the color and the width of the grid lines for the horizontal axis
      panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
      # Remove tick marks by setting their length to 0
      axis.ticks.length = unit(0, "mm"),
      # Remove the title for both axes
      axis.title = element_blank(),
      # But customize labels for the horizontal axis
      axis.text.x = element_text(size = 12),
      # same for y axis
      axis.text.y = element_text(size = 12)
    )
  
  return(p.varimp.rich)
  
}


### ===================================
###  Plot
### ===================================
### Individual plots
imp.below <- plot_Imp(varimp=varimp, var="belowgound", title="Proportion belowgound")
imp.clepto  <- plot_Imp(varimp=varimp, var="cleptoparasite", title="Proportion cleptoparasite")
imp.feeding <- plot_Imp(varimp=varimp, var="feeding_specialization", title="Feeding specialization")
imp.ITD  <- plot_Imp(varimp=varimp, var="ITD", title="ITD")
imp.pheno <- plot_Imp(varimp=varimp, var="phenoduration", title="Duration phenology")
imp.phenostart <- plot_Imp(varimp=varimp, var="phenostart", title="Start phenology")
imp.solitary  <- plot_Imp(varimp=varimp, var="solitary", title="Proportion solitary")
imp.tong_length  <- plot_Imp(varimp=varimp, var="tong_length", title="Tongue length")
### Arrange
figure <- ggarrange(imp.below, imp.clepto, imp.feeding, 
                    imp.ITD, imp.pheno,imp.phenostart,imp.solitary,imp.tong_length,
                    labels = paste("(",letters[1:8], ")", sep=""),
                    nrow = 4, ncol=2)
### Export
figure %>% ggexport(filename = "OUTPUT/Vimp_PDP/FigS_Variable_Importance_revised_traits.png",
                    width = 1000, height = 1000)
figure %>% ggexport(filename = "OUTPUT/Vimp_PDP/FigS_Variable_Importance_revised_traits.pdf",
                    width = 17, height = 17)
