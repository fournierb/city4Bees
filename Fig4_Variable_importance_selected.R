# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)

### load the data -----------------------------------------------------------------------
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-11/Selected descriptors")
varimp <- read.delim("DATA/Selected descriptors/Results_2022_04_28/Diversity_VariableImportance_metrics.txt")
varimp = varimp[varimp$variable %in% c("Richness", "Shannon", "TED", "TOP", "FDis", "LCBD_taxo", "LCBD_fun"),]

eval <- read.delim("DATA/Selected descriptors/Results_2022_04_28/Diversity_evaluation_metrics.txt")
eval = eval[eval$model == "rf" & eval$variable %in% c("Richness", "Shannon", "TED", "TOP", "FDis", "LCBD_taxo", "LCBD_fun"),]

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

imp.rich <- plot_Imp(varimp=varimp, var="Richness", title="Richness")
imp.sha  <- plot_Imp(varimp=varimp, var="Shannon", title="Shannon")
imp.FDis <- plot_Imp(varimp=varimp, var="FDis", title="FDis")
imp.TOP  <- plot_Imp(varimp=varimp, var="TOP", title="TOP")
imp.TED  <- plot_Imp(varimp=varimp, var="TED", title="TED")
imp.LCBD_taxo  <- plot_Imp(varimp=varimp, var="LCBD_taxo", title="LCBD taxonomic")
imp.LCBD_fun  <- plot_Imp(varimp=varimp, var="LCBD_fun", title="LCBD functional")

require(egg)
figure <- ggarrange(imp.rich, imp.sha, imp.TOP, imp.TED, imp.FDis,imp.LCBD_taxo,imp.LCBD_fun,
                    labels = paste("(",letters[1:7], ")", sep=""),
                    nrow = 4, ncol=2, 
                    widths = c(1,1),
                    heights = c(1,1,1))

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Figures")
require("magrittr")
require("ggpubr")
figure %>% ggexport(filename = "OUTPUT/Fig4_Variable_Importance_revised.png",
                    width = 1000, height = 1000)
figure %>% ggexport(filename = "OUTPUT/Fig4_Variable_Importance_revised.pdf",
                   width = 17, height = 17)

