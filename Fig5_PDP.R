# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)

### load the data -----------------------------------------------------------------------
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Analyses")
source("Load environmental data.R")

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-11/Selected descriptors")
pdp.TOP <- read.delim("DATA/Selected descriptors/Results_2022_04_28/TOP_pdp.txt")
pdp.TED <- read.delim("DATA/Selected descriptors/Results_2022_04_28/TEd_pdp.txt")
pdp.FDis <- read.delim("DATA/Selected descriptors/Results_2022_04_28/FDis_pdp.txt")
pdp.rich <- read.delim("DATA/Selected descriptors/Results_2022_04_28/Richness_pdp.txt")
pdp.sha <- read.delim("DATA/Selected descriptors/Results_2022_04_28/Shannon_pdp.txt")
pdp.betataxo <- read.delim("DATA/Selected descriptors/Results_2022_04_28/LCBD_taxo_pdp.txt")
pdp.betataxo$valueY=pdp.betataxo$valueY*1000
pdp.betafun <- read.delim("DATA/Selected descriptors/Results_2022_04_28/LCBD_fun_pdp.txt")
pdp.betafun$valueY=pdp.betafun$valueY*1000


### PDPs plots
require(ggplot2)
require(vegan)

palette_predictors=c("#ED5752", "#344D90", "#B38867", "#F4CC70", "#4B7447", "#6fb98f")

Make_PDP_plot <- function(dat, ylabel){
  
  var.list = c("hive_2500", "plant_PC2", "urb2500", "agri2500", "forest2500", "clim_PC1")
  dat <- dat[dat$Var %in% var.list,]  
  
  for(i in var.list){
    dat$valueX[dat$Var==i] = decostand(dat$valueX[dat$Var==i], "range") 
  }
  
  p.TOP <- ggplot(data = dat, aes(y=valueY, x = valueX, 
                                  group = Var, colour = Var)) + 
    geom_smooth(method="loess", span = 1) +
    scale_color_manual(values =palette_predictors ) +
    ylab(ylabel) + theme_classic() +
    xlab("Environmental gradients")
  return(p.TOP)
}

p.TOP <- Make_PDP_plot(dat=pdp.TOP, ylabel="TOP")
p.TED <- Make_PDP_plot(dat=pdp.TED, ylabel="TED")
p.FDis <- Make_PDP_plot(dat=pdp.FDis, ylabel="FDis")
p.rich <- Make_PDP_plot(dat=pdp.rich, ylabel="Species richness")
p.sha <- Make_PDP_plot(dat=pdp.sha, ylabel="Shannon")
p.betataxo<- Make_PDP_plot(dat=pdp.betataxo, ylabel="LCBD taxonomic")
p.beta.fun<- Make_PDP_plot(dat=pdp.betafun, ylabel="LCBD functional")


require(egg)
library(ggpubr)
figure <- ggarrange(p.TOP,p.TED,p.FDis,p.rich,p.sha,p.betataxo,p.beta.fun,
                    nrow = 4, ncol=2, 
                    labels = LETTERS[1:7],
                    common.legend = TRUE, legend="right")
figure

require("magrittr")
require("ggpubr")
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Figures")
figure %>% ggexport(filename = "OUTPUT/Fig5_PDP.png",
                    width = 700, height = 700)

figure %>% ggexport(filename = "OUTPUT/Fig5_PDP.pdf",
                    width = 7, height = 7)


