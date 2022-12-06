#######################################
### Paper: Wild bee diversity in Switzerland
### Script: Figure 1
### Author: Joan Casanelles Abella
### Date: 05.2022
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
library(sp)
library(sf)
library(ggcorrplot)
require(RColorBrewer)
require("magrittr")
require("ggpubr")
require(egg)
### load the data -----------------------------------------------------------------------
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/")
div <- stack(x = "Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD","LCBD_fun" ,"LCBD_taxo","phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
TOP <- div$TOP
TED <- div$TED
FDis <- div$FDis
rich <- div$Richness
shannon <- div$Shannon
LCBD_fun <- div$LCBD_fun*1000
LCBD_taxo<- div$LCBD_taxo*1000
### water bodies
water_bodies=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/water_bodies1.tif")
### categorical map -> function
map_cat <- function(ras, title, labels){
  require(RColorBrewer)
  ras=mask(x = ras, mask = water_bodies,maskvalue = 1)
  writeRaster(x = ras, filename = paste("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/Masked_responses/maskedmasked.",title,".tiff",sep=""), overwrite=T)
  ras.df <- as.data.frame(cut(ras, breaks=c(quantile(ras)), right=FALSE), xy = TRUE)
  names(ras.df) = c("x","y","layer")
  p <- ggplot() +
    geom_raster(data = ras.df, 
                aes(x = x, y = y, 
                    fill = as.factor(layer))) + 
    coord_equal() +
    scale_fill_viridis(discrete = T, option = "D" ,
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

# quantile(TOP)
p_TOP = map_cat(ras = TOP, title = "Functional richness (TOP)", labels=  c("3-15", "15-20", "20-25", "25-88"))

# quantile(TED)
p_TED = map_cat(ras = TED, title = "Functional evenness (TED)", labels=  c("0.68-0.92", "0.92-0.93", "0.93-0.94", "0.94-0.98"))

# quantile(FDis)
p_FDis = map_cat(ras = FDis, title = "Functional dispersion (FDis)", labels=  c("0.01-1.1", "1.1-1.45", "1.45-1.7", "1.7-3"))

# quantile(rich)
p_rich = map_cat(ras = rich, title = "Species richness", labels=  c("1-6", "6-9", "9-12", "12-71"))

# quantile(shannon)
p_sha = map_cat(ras = shannon, title = "Species diversity (Shannon)", labels=  c("0.02-1.1", "1.1-1.4", "1.4-1.7", "1.7-4"))

# quantile(LCBD_fun)
p_lcbd_f = map_cat(ras = LCBD_fun, title = "LCBD functional",labels=  c("0.02-0.17", "0.17-0.21", "0.21-0.24", "0.25-0.92"))

# quantile(LCBD_taxo)
p_lcbd_t = map_cat(ras = LCBD_taxo, title = "LCBD taxonomic", labels=c("0.11-0.15", "0.15-0.16", "0.16-0.17", "0.17-0.19"))

### correlation matrix among diversity facets
dat.cor = sampleRandom(stack(TOP,TED,FDis, rich, shannon, LCBD_fun, LCBD_taxo), 100000)
mat.cor = cor(dat.cor)

p_cor <- ggcorrplot(mat.cor, 
                    hc.order = TRUE, 
                    show.legend=FALSE,
                    type = "lower",
                    lab = TRUE) +
  ggtitle("Correlation matrix")


figure <- ggpubr::ggarrange(p_rich, p_sha, p_TOP, p_TED, p_FDis,p_lcbd_t, p_lcbd_f,p_cor,
                    labels = paste("(",letters[1:8],")", sep=""),
                    nrow = 4, ncol=2, 
                    heights = rep(1,6))


figure %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/Fig2_Diversity_Maps_revised2.png",
                    width = 1300, height = 1300)
figure %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/Fig2_Diversity_Maps_revised2.pdf",
                    width = 17, height = 17)

### Individual plots
p_TOP %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/TOP_Diversity_Maps_revised.png",
                   width = 1000, height = 1000)
p_TOP %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/TOP_Diversity_Maps_revised.pdf",
                   width = 9, height = 9)

p_TED %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/TED_Diversity_Maps_revised.png",
                   width = 1000, height = 1000)
p_TED %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/TED_Diversity_Maps_revised.pdf",
                   width = 9, height = 9)

p_FDis %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/FDis_Diversity_Maps_revised.png",
                    width = 1000, height = 1000)
p_FDis %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/FDis_Diversity_Maps_revised.pdf",
                    width = 9, height = 9)

p_rich %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/Richness_Diversity_Maps_revised.png",
                    width = 1000, height = 1000)
p_rich %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/Richness_Diversity_Maps_revised.pdf",
                    width = 9, height = 9)

p_sha %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/Shannon_Diversity_Maps_revised.png",
                   width = 1000, height = 1000)
p_sha %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/Shannon_Diversity_Maps_revised.pdf",
                   width = 9, height = 9)

p_lcbd_f %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/p_lcbd_f_Diversity_Maps_revised.png",
                   width = 1000, height = 1000)
p_lcbd_f %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/p_lcbd_f_Diversity_Maps_revised.pdf",
                   width = 9, height = 9)

p_lcbd_t %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/p_lcbd_t_Diversity_Maps_revised.png",
                   width = 1000, height = 1000)
p_lcbd_t %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/p_lcbd_t_Diversity_Maps_revised.pdf",
                   width = 9, height = 9)

