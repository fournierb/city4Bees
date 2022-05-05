# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)
### load the data -----------------------------------------------------------------------
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/")
div <- stack(x = "DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD","LCBD_fun" ,"LCBD_taxo","phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
belowgound <- div$belowgound
cleptoparasite <- div$cleptoparasite
feeding_specialization <- div$feeding_specialization
phenoduration <- div$phenoduration
ITD <- div$ITD
solitary <- div$solitary
tong_length<- div$tong_length
### water bodies
water_bodies=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/water_bodies1.tif")
### categorical map -> function
map_cat <- function(ras, title, labels){
  require(RColorBrewer)
  ras=mask(x = ras, mask = water_bodies,maskvalue = 1)
  writeRaster(x = ras, filename = paste("DATA/Selected descriptors/Results_2022_04_28/Masked_responses/maskedmasked.",title,".tiff",sep=""), overwrite=T)
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

quantile(belowgound)
p_belowgound = map_cat(ras = belowgound, title = "Proportion belouground nesting ", labels=  c("<0.3", "0.3-0.42", "0.42-0.51", ">0.51"))

quantile(cleptoparasite)
p_cleptoparasite = map_cat(ras = cleptoparasite, title = "Proportion cleptoparasite", labels=  c("<0.03", "0.03-0.06", "0.06-0.0.09", ">0.09"))

quantile(feeding_specialization)
p_feeding_specialization = map_cat(ras = feeding_specialization, title = "Feeding specialisation", labels=  c("<0.67", "0.67-0.73", "0.73-0.0.78", ">0.78"))

quantile(phenoduration)
p_phenoduration = map_cat(ras = phenoduration, title = "Duration phenology (weeks)", labels=  c("<0.41", "0.41-0.44", "0.44-0.0.47", ">0.47"))

quantile(solitary)
p_solitary = map_cat(ras = solitary, title = "Proportion solitary", labels=  c("<0.31", "0.31-0.40", "0.40-0.0.49", ">0.49"))

quantile(tong_length)
p_tong_length = map_cat(ras = tong_length, title = "Tongue lenght (mm)", labels=  c("<0.31", "0.31-0.42", "0.42-0.0.53", ">0.53"))

quantile(ITD)
p_ITD = map_cat(ras = ITD, title = "Intertegular distance (mm)", labels=  c("<0.33", "0.33-0.40", "0.40-0.0.48", ">0.48"))

### correlation matrix among diversity facets
dat.cor = sampleRandom(stack(belowgound,cleptoparasite,feeding_specialization, phenoduration, solitary, tong_length, ITD), 100000)
mat.cor = cor(dat.cor)

library(ggcorrplot)
p_cor <- ggcorrplot(mat.cor, 
                    hc.order = TRUE, 
                    show.legend=FALSE,
                    type = "lower",
                    lab = TRUE) +
  ggtitle("Correlation matrix")


require(egg)
figure.maps.traits <- ggarrange(p_belowgound, p_cleptoparasite, p_feeding_specialization, p_phenoduration, p_solitary,p_tong_length,p_ITD, p_cor,
                    labels = paste("(",letters[1:8],")", sep=""),
                    nrow = 4, ncol=2, 
                    heights = rep(1,6))

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Figures")
require("magrittr")
require("ggpubr")
figure.maps.traits %>% ggexport(filename = "OUTPUT/maps/Community_atributes/Fig2_Diversity_Maps_traits.png",
                    width = 1300, height = 1300)
figure.maps.traits %>% ggexport(filename = "OUTPUT/maps/Community_atributes/Fig2_Diversity_Maps_traits.pdf",
                    width = 17, height = 17)
