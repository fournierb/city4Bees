# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)
library(sf)

### load the data -----------------------------------------------------------------------
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Analyses")
source("Load environmental data.R")

boundaries.shp=readOGR("DATA/Boundaries/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp")
boundaries.r=raster("DATA/boundaries.tiff")
boundaries=as.data.frame(boundaries.r)
boundaries=cbind(boundaries, coordinates(boundaries.r))
boundaries=na.omit(boundaries)
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

LCBD_fun.q3 <- LCBD_fun 
LCBD_fun.q3[LCBD_fun.q3<quantile(LCBD_fun, probs = thresh)] = NA
plot(LCBD_fun.q3)

LCBD_taxo.q3 <- LCBD_taxo 
LCBD_taxo.q3[LCBD_taxo.q3<quantile(LCBD_taxo, probs = thresh)] = NA
plot(LCBD_taxo.q3)

ras.hot.alpha.tax = rich
ras.hot.alpha.tax[!(is.na(ras.hot.alpha.tax))] = 0
ras.hot.alpha.tax[!(is.na(rich.q3))  & !(is.na(shannon.q3))] = 1

ras.hot.alpha.funct = TOP
ras.hot.alpha.funct[!(is.na(ras.hot.alpha.funct))] = 0
ras.hot.alpha.funct[!(is.na(TOP.q3)) & !(is.na(TED.q3)) & !(is.na(FDis.q3))] = 2

ras.hot.beta.tax = LCBD_taxo
ras.hot.beta.tax[!(is.na(ras.hot.beta.tax))] = 0
ras.hot.beta.tax[!(is.na(LCBD_taxo.q3))] = 1

ras.hot.beta.funct = LCBD_fun
ras.hot.beta.funct[!(is.na(ras.hot.beta.funct))] = 0
ras.hot.beta.funct[!(is.na(LCBD_fun.q3))] = 2

ras.hot.alpha.tax.df <- as.data.frame(ras.hot.alpha.tax, xy = TRUE)
ras.hot.alpha.tax.df=na.omit(ras.hot.alpha.tax.df)
names(ras.hot.alpha.tax.df) = c("x","y","layer")

ras.hot.alpha.funct.df <- as.data.frame(ras.hot.alpha.funct, xy = TRUE)
ras.hot.alpha.funct.df=na.omit(ras.hot.alpha.funct.df)
names(ras.hot.alpha.funct.df) = c("x","y","layer")

ras.hot.beta.tax.df <- as.data.frame(ras.hot.beta.tax, xy = TRUE)
names(ras.hot.beta.tax.df) = c("x","y","layer")

ras.hot.beta.funct.df <- as.data.frame(ras.hot.beta.funct, xy = TRUE)
names(ras.hot.beta.funct.df) = c("x","y","layer")

ALPHA.HOTS=rbind(ras.hot.alpha.tax.df,ras.hot.alpha.funct.df[ras.hot.alpha.funct.df$layer != 0,])

p.hot.apha <- ggplot() +
geom_raster(data = ALPHA.HOTS, 
              aes(x = x, y = y, 
                  fill = as.factor(layer)), alpha=0.9) + 
  coord_equal() +
  scale_fill_manual(values = c("grey90", "#DE7A22", "#6AB187"), 
                    name="Hotspots",
                    labels = c("", "taxonomic", "functional"),
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



BETA.HOTS=rbind(ras.hot.beta.tax.df,ras.hot.beta.funct.df[ras.hot.beta.funct.df$layer != 0,])
BETA.HOTS=na.omit(BETA.HOTS)
p.hot.beta <- ggplot() +
  geom_raster(data = BETA.HOTS, 
              aes(x = x, y = y, 
                  fill = as.factor(layer)), alpha=0.9) + 
  coord_equal() +
  scale_fill_manual(values = c("grey90", "#DE7A22", "#6AB187"), 
                    name="Hotspots",
                    labels = c("",  "taxonomic", "functional"),
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
