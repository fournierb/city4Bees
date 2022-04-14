# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)

### load the data -----------------------------------------------------------------------
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Analyses")
source("Load environmental data.R")

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-11/All descriptors")
div <- stack(x = "Diversity_stack_revised_All_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD", "phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
TOP <- div$TOP
TED <- div$TED
FDis <- div$FDis
rich <- div$Richness
shannon <- div$Shannon


### categorical map -> function
map_cat <- function(ras, title, labels){
  require(RColorBrewer)
  
  ras.df <- as.data.frame(cut(ras, breaks=c(quantile(ras)), right=FALSE), xy = TRUE)
  names(ras.df) = c("x","y","layer")
  p <- ggplot() +
    geom_raster(data = ras.df, 
                aes(x = x, y = y, 
                    fill = as.factor(layer))) + 
    coord_equal() +
    scale_fill_manual(values = brewer.pal(n = 4, name = "Greens"), 
                      name=title,
                      labels = labels,
                      na.translate = F) +
    guides(fill = guide_legend(reverse=T)) +
    # theme_bw() +
    ggtitle(title) + 
    theme(
      # Set background color to white
      panel.background = element_rect(fill = "white"),
      # Set the color and the width of the grid lines for the horizontal axis
      panel.grid.major.x = element_blank(),
      # Remove tick marks by setting their length to 0
      axis.ticks.length = unit(0, "mm"),
      # Remove the title for both axes
      axis.title = element_blank(),
      # But customize labels for the horizontal axis
      axis.text.x = element_blank(),
      # same for y axis
      axis.text.y = element_blank(),
      # Remove the legend title
      title = element_blank()
    )
  
  return(p)
}

quantile(TOP)
p_TOP = map_cat(ras = TOP, title = "Functional richness (TOP)", labels=  c("<15", "15-20", "20-25", ">25"))

quantile(TED)
p_TED = map_cat(ras = TED, title = "Functional evenness (TED)", labels=  c("<0.92", "0.92-0.93", "0.93-0.94", ">0.94"))

quantile(FDis)
p_FDis = map_cat(ras = FDis, title = "Functional dispersion (FDis)", labels=  c("<1.1", "1.1-1.45", "1.45-1.7", ">1.7"))

quantile(rich)
p_rich = map_cat(ras = rich, title = "Species richness", labels=  c("<6", "6-9", "9-12", ">12"))

quantile(shannon)
p_sha = map_cat(ras = shannon, title = "Species diversity (Shannon)", labels=  c("<1.1", "1.1-1.4", "1.4-1.7", ">1.7"))


### correlation matrix among diversity facets
dat.cor = sampleRandom(stack(TOP,TED,FDis, rich, shannon), 100000)
mat.cor = cor(dat.cor)

library(ggcorrplot)
p_cor <- ggcorrplot(mat.cor, 
                    hc.order = TRUE, 
                    show.legend=FALSE,
                    type = "lower",
                    lab = TRUE) +
  ggtitle("Correlation matrix")


require(egg)
figure <- ggarrange(p_rich, p_sha, p_TOP, p_TED, p_FDis, p_cor,
                    labels = LETTERS[1:6],
                    nrow = 3, ncol=2, 
                    heights = rep(1,6))

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Figures")
require("magrittr")
require("ggpubr")
figure %>% ggexport(filename = "Fig2_Diversity_Maps_revised.png",
                    width = 1000, height = 1000)
figure %>% ggexport(filename = "Fig2_Diversity_Maps_revised.pdf",
                    width = 10, height = 10)

p_TOP %>% ggexport(filename = "TOP_Diversity_Maps_revised.png",
                   width = 1000, height = 1000)
p_TOP %>% ggexport(filename = "TOP_Diversity_Maps_revised.pdf",
                   width = 9, height = 9)

p_TED %>% ggexport(filename = "TED_Diversity_Maps_revised.png",
                   width = 1000, height = 1000)
p_TED %>% ggexport(filename = "TED_Diversity_Maps_revised.pdf",
                   width = 9, height = 9)

p_FDis %>% ggexport(filename = "FDis_Diversity_Maps_revised.png",
                    width = 1000, height = 1000)
p_FDis %>% ggexport(filename = "FDis_Diversity_Maps_revised.pdf",
                    width = 9, height = 9)

p_rich %>% ggexport(filename = "Richness_Diversity_Maps_revised.png",
                    width = 1000, height = 1000)
p_rich %>% ggexport(filename = "Richness_Diversity_Maps_revised.pdf",
                    width = 9, height = 9)

p_sha %>% ggexport(filename = "Shannon_Diversity_Maps_revised.png",
                   width = 1000, height = 1000)
p_sha %>% ggexport(filename = "Shannon_Diversity_Maps_revised.pdf",
                   width = 9, height = 9)
