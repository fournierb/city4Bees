# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)

### load the data -----------------------------------------------------------------------
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-11/Selected descriptors")
varimp <- read.delim("Diversity_VariableImportance_metrics.txt")
varimp = varimp[varimp$variable %in% c("Richness", "Shannon", "TED", "TOP", "FDis"),]

eval <- read.delim("Diversity_evaluation_metrics.txt")
eval = eval[eval$model == "rf" & eval$variable %in% c("Richness", "Shannon", "TED", "TOP", "FDis"),]

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
  
  var.type = c(rep("Climate",4), rep("Land use",3), rep("Vegetation",3), "Hive")
  var.type = factor(var.type, levels = c("Hive", "Vegetation", "Land use","Climate"))
  color <- brewer.pal(n = length(levels(var.type)), name = "Dark2")

  dat = data.frame(list.varimp[[n]], var.type = var.type)
  dat$model = factor(rownames(dat), levels = rownames(dat))
  
  p.varimp.rich <- ggplot(data=dat, aes(varimp_rf, model, fill = var.type)) +
    geom_bar(stat="identity") +
    scale_fill_manual("legend", values = c("Climate"=color[1],
                                           "Land use" = color[2],
                                           "Vegetation" = color[3], 
                                           "Hive" =color[4])) +
    ggtitle(title) +
    
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

require(RColorBrewer)
imp.rich <- plot_Imp(varimp=varimp, var="Richness", title="Richness")
imp.sha  <- plot_Imp(varimp=varimp, var="Shannon", title="Shannon")
imp.FDis <- plot_Imp(varimp=varimp, var="FDis", title="FDis")
imp.TOP  <- plot_Imp(varimp=varimp, var="TOP", title="TOP")
imp.TED  <- plot_Imp(varimp=varimp, var="TED", title="TED")

require(egg)
figure <- ggarrange(imp.rich, imp.sha, imp.TOP, imp.TED, imp.FDis,
                    labels = LETTERS[1:5],
                    nrow = 3, ncol=2, 
                    widths = c(1,1),
                    heights = c(1,1,1))

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Figures")
require("magrittr")
require("ggpubr")
figure %>% ggexport(filename = "Fig4_Variable_Importance_revised.png",
                    width = 1000, height = 1000)
figure %>% ggexport(filename = "Fig4_Variable_Importance_revised.pdf",
                   width = 9, height = 9)

