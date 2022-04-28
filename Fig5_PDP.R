# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)

### load the data -----------------------------------------------------------------------
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Analyses")
source("Load environmental data.R")

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-11/Selected descriptors")
pdp.TOP <- read.delim("TOP_pdp.txt")
pdp.TED <- read.delim("TEd_pdp.txt")
pdp.FDis <- read.delim("FDis_pdp.txt")
pdp.rich <- read.delim("Richness_pdp.txt")
pdp.sha <- read.delim("Shannon_pdp.txt")


### PDPs plots
require(ggplot2)
require(vegan)



Make_PDP_plot <- function(dat, ylabel){
  
  var.list = c("hive_2500", "plant_PC2", "urb2500", "agri2500", "forest2500", "clim_PC1")
  dat <- dat[dat$Var %in% var.list,]  
  
  for(i in var.list){
    dat$valueX[dat$Var==i] = decostand(dat$valueX[dat$Var==i], "range") 
  }
  
  p.TOP <- ggplot(data = dat, aes(y=valueY, x = valueX, 
                                  group = Var, colour = Var)) + 
    geom_smooth(method="loess", span = 1) +
    ylab(ylabel) + theme_classic() +
    xlab("Environmental gradients")
  return(p.TOP)
}

p.TOP <- Make_PDP_plot(dat=pdp.TOP, ylabel="TOP")
p.TED <- Make_PDP_plot(dat=pdp.TED, ylabel="TED")
p.FDis <- Make_PDP_plot(dat=pdp.FDis, ylabel="FDis")
p.rich <- Make_PDP_plot(dat=pdp.rich, ylabel="Species richness")
p.sha <- Make_PDP_plot(dat=pdp.sha, ylabel="Shannon")


require(egg)
library(ggpubr)
figure <- ggarrange(p.TOP,p.TED,p.FDis,p.rich,p.sha,
                    nrow = 3, ncol=2, 
                    labels = LETTERS[1:5],
                    common.legend = TRUE, legend="right")
figure

require("magrittr")
require("ggpubr")
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Figures")
figure %>% ggexport(filename = "Fig5_PDP.png",
                    width = 700, height = 700)

figure %>% ggexport(filename = "Fig5_PDP.pdf",
                    width = 7, height = 7)


