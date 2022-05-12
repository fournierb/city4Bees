# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)

### load the data -----------------------------------------------------------------------
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Analyses")
source("Load environmental data.R")

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-11/Selected descriptors")
div <- stack(x = "DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD", "LCBD_fun", "LCBD_taxo", "phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
TOP <- div$TOP
TED <- div$TED
FDis <- div$FDis
rich <- div$Richness
shannon <- div$Shannon
LCBD_taxo<- div$LCBD_taxo
LCBD_fun<- div$LCBD_fun
### Identifying biodiversity hotspots -> TD + FD > 90 percentile

thresh = 0.90

TOP.q3 <- TOP 
TOP.q3[TOP.q3<quantile(TOP, probs = thresh)] = NA
plot(TOP.q3)

TED.q3 <- TED 
TED.q3[TED.q3<quantile(TED, probs = thresh)] = NA
plot(TED.q3)

FDis.q3 <- FDis 
FDis.q3[FDis.q3<quantile(FDis, probs = thresh)] = NA
plot(FDis.q3)

rich.q3 <- rich 
rich.q3[rich.q3<quantile(rich, probs = thresh)] = NA
plot(rich.q3)

shannon.q3 <- shannon 
shannon.q3[shannon.q3<quantile(shannon, probs = thresh)] = NA
plot(shannon.q3)

ras.hot = TOP
ras.hot[!(is.na(ras.hot))] = 0
ras.hot[!(is.na(TOP.q3)) & !(is.na(TED.q3)) & !(is.na(FDis.q3))
        & !(is.na(rich.q3))  & !(is.na(shannon.q3))] = 1


ras.df <- as.data.frame(ras.hot, xy = TRUE)
names(ras.df) = c("x","y","layer")
p.hot.apha <- ggplot() +
  geom_raster(data = ras.df, 
              aes(x = x, y = y, 
                  fill = as.factor(layer))) + 
  coord_equal() +
  scale_fill_manual(values = c("gray20", "chartreuse2"), 
                    name="Hotspots",
                    labels = c("no", "yes"),
                    na.translate = F) +
  guides(fill = guide_legend(reverse=T)) +
  # theme_bw() +
  ggtitle("Hotspots (TD + FD)") + 
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
    axis.text.y = element_blank()
  )
p.hot.apha

LCBD_fun.q3 <- LCBD_fun 
LCBD_fun.q3[LCBD_fun.q3<quantile(LCBD_fun, probs = thresh)] = NA
plot(LCBD_fun.q3)

LCBD_taxo.q3 <- LCBD_taxo 
LCBD_taxo.q3[LCBD_taxo.q3<quantile(LCBD_taxo, probs = thresh)] = NA
plot(LCBD_taxo.q3)

ras.hot = LCBD_fun
ras.hot[!(is.na(ras.hot))] = 0
ras.hot[!(is.na(LCBD_fun.q3)) & !(is.na(LCBD_taxo.q3))] = 1


ras.df <- as.data.frame(ras.hot, xy = TRUE)
names(ras.df) = c("x","y","layer")
p.hot.beta <- ggplot() +
  geom_raster(data = ras.df, 
              aes(x = x, y = y, 
                  fill = as.factor(layer))) + 
  coord_equal() +
  scale_fill_manual(values = c("gray20", "chartreuse2"), 
                    name="Hotspots",
                    labels = c("no", "yes"),
                    na.translate = F) +
  guides(fill = guide_legend(reverse=T)) +
  # theme_bw() +
  ggtitle("Hotspots (TD + FD)") + 
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
    axis.text.y = element_blank()
  )
p.hot.beta

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Figures")
require("magrittr")
require("ggpubr")
p.hot.apha %>% ggexport(filename = "OUTPUT/Fig_alphaHotspot_Maps_revised90.png",
                   width = 1000, height = 1000)
p.hot.apha %>% ggexport(filename = "OUTPUT/Fig_alphaHotspot_Maps_revised90.pdf",
                   width = 9, height = 9)


p.hot.beta %>% ggexport(filename = "OUTPUT/Fig_betaHotspot_Maps_revised90.png",
                        width = 1000, height = 1000)
p.hot.beta %>% ggexport(filename = "OUTPUT/Fig_betaHotspot_Maps_revised90.pdf",
                        width = 9, height = 9)
