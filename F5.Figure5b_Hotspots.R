#######################################
### Paper: Wild bee diversity in Switzerland
### Script: Calculating biodiversity hotspots
### Author: Joan Casanelles Abella
### Date: 05.2022
#######################################
### ===================================
###  Initialise the system
### ===================================
# Remove all R objects in the workspace
rm(list = ls())
# Packages -----------------------------------------------------------------------
require(raster)
require(viridis)
require(ggplot2)
library(sf)
require("magrittr")
require(egg)
library(ggpubr)
### ===================================
###  Data
### ===================================
### load the data -----------------------------------------------------------------------
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/")
boundaries.shp=readOGR("DATA/Boundaries/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp")
boundaries.r=raster("DATA/boundaries.tiff")
boundaries=as.data.frame(boundaries.r)
boundaries=cbind(boundaries, coordinates(boundaries.r))
boundaries=na.omit(boundaries)
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
# Set the threshold to define hotspots
thresh = 0.90

TOP.q3 <- TOP 
TOP.q3[TOP.q3<quantile(TOP, probs = thresh)] = NA

TED.q3 <- TED 
TED.q3[TED.q3<quantile(TED, probs = thresh)] = NA

FDis.q3 <- FDis 
FDis.q3[FDis.q3<quantile(FDis, probs = thresh)] = NA

rich.q3 <- rich 
rich.q3[rich.q3<quantile(rich, probs = thresh)] = NA

shannon.q3 <- shannon 
shannon.q3[shannon.q3<quantile(shannon, probs = thresh)] = NA

LCBD_fun.q3 <- LCBD_fun 
LCBD_fun.q3[LCBD_fun.q3<quantile(LCBD_fun, probs = thresh)] = NA

LCBD_taxo.q3 <- LCBD_taxo 
LCBD_taxo.q3[LCBD_taxo.q3<quantile(LCBD_taxo, probs = thresh)] = NA

### Taxonomic hotspots -----------------------------------------------------------------------
# binarize raster (is the cell above or below the threshold)
# Alpha taxonomic
ras.hot.alpha.tax = rich
ras.hot.alpha.tax[!(is.na(ras.hot.alpha.tax))] = 0
ras.hot.alpha.tax[!(is.na(rich.q3))  & !(is.na(shannon.q3))] = 1
# Alpha functional
ras.hot.alpha.funct = TOP
ras.hot.alpha.funct[!(is.na(ras.hot.alpha.funct))] = 0
ras.hot.alpha.funct[!(is.na(TOP.q3)) & !(is.na(TED.q3)) & !(is.na(FDis.q3))] = 2
# Beta taxonomic
ras.hot.beta.tax = LCBD_taxo
ras.hot.beta.tax[!(is.na(ras.hot.beta.tax))] = 0
ras.hot.beta.tax[!(is.na(LCBD_taxo.q3))] = 1
# Beta functional
ras.hot.beta.funct = LCBD_fun
ras.hot.beta.funct[!(is.na(ras.hot.beta.funct))] = 0
ras.hot.beta.funct[!(is.na(LCBD_fun.q3))] = 2
### Prepare dataframes
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
# Final data frame alpha diversity
ALPHA.HOTS=rbind(ras.hot.alpha.tax.df,ras.hot.alpha.funct.df[ras.hot.alpha.funct.df$layer != 0,])
# Plot (Figure 6)
p.hot.apha <- ggplot() +
  geom_raster(data = ALPHA.HOTS[ALPHA.HOTS$layer==0,], 
              aes(x = x, y = y), alpha=1, 
              fill = "grey90") + 
  geom_raster(data = ALPHA.HOTS[ALPHA.HOTS$layer==1,], 
              aes(x = x, y = y), alpha=0.65, 
              fill = "#DE7A22") + 
  geom_raster(data = ALPHA.HOTS[ALPHA.HOTS$layer==2,], 
              aes(x = x, y = y), alpha=0.65, 
              fill = "#6AB187") +
  coord_equal() +
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


# Final data frame beta diversity
BETA.HOTS=rbind(ras.hot.beta.tax.df,ras.hot.beta.funct.df[ras.hot.beta.funct.df$layer != 0,])
BETA.HOTS=na.omit(BETA.HOTS)
# Plot (Figure 6)
p.hot.beta <- ggplot() +
  geom_raster(data = BETA.HOTS[BETA.HOTS$layer==0,], 
              aes(x = x, y = y), alpha=1, 
              fill = "grey90") + 
  geom_raster(data = BETA.HOTS[BETA.HOTS$layer==1,], 
              aes(x = x, y = y), alpha=0.65, 
              fill = "#DE7A22") + 
  geom_raster(data = BETA.HOTS[BETA.HOTS$layer==2,], 
              aes(x = x, y = y), alpha=0.65, 
              fill = "#6AB187") + 
  coord_equal() +

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
  ) +
scale_fill_manual(values = c("grey90", "#DE7A22", "#6AB187"), 
                  name="Hotspots",
                  labels = c("",  "taxonomic", "functional"),
                  na.translate = F) +

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




thresholds=seq(0,1, by=0.1)
for(t in 4:length(thresholds)){
  print(thresholds[t])
  thresh = thresholds[t]
  
  TOP.q3 <- TOP 
  TOP.q3[TOP.q3<quantile(TOP, probs = thresh)] = NA

  TED.q3 <- TED 
  TED.q3[TED.q3<quantile(TED, probs = thresh)] = NA

  FDis.q3 <- FDis 
  FDis.q3[FDis.q3<quantile(FDis, probs = thresh)] = NA

  rich.q3 <- rich 
  rich.q3[rich.q3<quantile(rich, probs = thresh)] = NA

  shannon.q3 <- shannon 
  shannon.q3[shannon.q3<quantile(shannon, probs = thresh)] = NA

  LCBD_fun.q3 <- LCBD_fun 
  LCBD_fun.q3[LCBD_fun.q3<quantile(LCBD_fun, probs = thresh)] = NA

  LCBD_taxo.q3 <- LCBD_taxo 
  LCBD_taxo.q3[LCBD_taxo.q3<quantile(LCBD_taxo, probs = thresh)] = NA

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
    geom_raster(data = ALPHA.HOTS[ALPHA.HOTS$layer==0,], 
                aes(x = x, y = y), alpha=1, 
                fill = "grey90") + 
    geom_raster(data = ALPHA.HOTS[ALPHA.HOTS$layer==1,], 
                aes(x = x, y = y), alpha=0.65, 
                fill = "#DE7A22") + 
    geom_raster(data = ALPHA.HOTS[ALPHA.HOTS$layer==2,], 
                aes(x = x, y = y), alpha=0.65, 
                fill = "#6AB187") +
    coord_equal() +
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
#  p.hot.apha
  
  
  
  BETA.HOTS=rbind(ras.hot.beta.tax.df,ras.hot.beta.funct.df[ras.hot.beta.funct.df$layer != 0,])
  BETA.HOTS=na.omit(BETA.HOTS)
  p.hot.beta <- ggplot() +
    geom_raster(data = BETA.HOTS[BETA.HOTS$layer==0,], 
                aes(x = x, y = y), alpha=1, 
                fill = "grey90") + 
    geom_raster(data = BETA.HOTS[BETA.HOTS$layer==1,], 
                aes(x = x, y = y), alpha=0.65, 
                fill = "#DE7A22") + 
    geom_raster(data = BETA.HOTS[BETA.HOTS$layer==2,], 
                aes(x = x, y = y), alpha=0.65, 
                fill = "#6AB187") + 
    coord_equal() +
    
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

    
ggsave(filename = paste("OUTPUT/Fig_alphaHotspot_Maps_revised90",thresholds[t],".png", sep=""), 
       plot =p.hot.apha, device = "png", width = 1000,height = 1000, dpi = 300, units = "px")
ggsave(filename = paste("OUTPUT/Fig_betaHotspot_Maps_revised90.",thresholds[t],".png", sep=""), 
       plot =p.hot.beta, device = "png", width = 1000,height = 1000, dpi = 300, units = "px")

}


#### Protected vs. non protected
### Sensu lato ---------------------------------
## Taxonomic
#alpha
alpha.tax.pa=raster::extract(x = PA.sensu.lato, y = ALPHA.HOTS[ALPHA.HOTS$layer==1, 1:2])
alpha.tax.pa[is.na(alpha.tax.pa)] = 2
table(alpha.tax.pa)   
#beta
beta.tax.pa=raster::extract(x = PA.sensu.lato, y = BETA.HOTS[BETA.HOTS$layer==1, 1:2])
beta.tax.pa[is.na(beta.tax.pa)] = 2
table(beta.tax.pa)                
## Functional
#alpha
alpha.fun.pa=raster::extract(x = PA.sensu.lato, y = ALPHA.HOTS[ALPHA.HOTS$layer==2, 1:2])
alpha.fun.pa[is.na(alpha.fun.pa)] = 2
table(alpha.fun.pa)    
#beta
beta.fun.pa=raster::extract(x = PA.sensu.lato, y = BETA.HOTS[BETA.HOTS$layer==2, 1:2])
beta.fun.pa[is.na(beta.fun.pa)] = 2
table(beta.fun.pa)                
### Sensu stricto ---------------------------------
## Taxonomic
#alpha
alpha.tax.pa=raster::extract(x = PA.sensu.stricto, y = ALPHA.HOTS[ALPHA.HOTS$layer==1, 1:2])
alpha.tax.pa[is.na(alpha.tax.pa)] = 2
table(alpha.tax.pa)   
#beta
beta.tax.pa=raster::extract(x = PA.sensu.stricto, y = BETA.HOTS[BETA.HOTS$layer==1, 1:2])
beta.tax.pa[is.na(beta.tax.pa)] = 2
table(beta.tax.pa)                
## Functional
#alpha
alpha.fun.pa=raster::extract(x = PA.sensu.stricto, y = ALPHA.HOTS[ALPHA.HOTS$layer==2, 1:2])
alpha.fun.pa[is.na(alpha.fun.pa)] = 2
table(alpha.fun.pa)    
#beta
beta.fun.pa=raster::extract(x = PA.sensu.stricto, y = BETA.HOTS[BETA.HOTS$layer==2, 1:2])
beta.fun.pa[is.na(beta.fun.pa)] = 2
table(beta.fun.pa)   

#### Prepare data frames
### Proportion overlap cells
proportion.overlap.PA.df=data.frame(metric=c("Alpha taxonomic", "Alpha functional", "Beta taxonomic", "Beta functional"), 
           Prop_allPA_allSurface=c(14,18.5,49, 39.5),
           Prop_allPA_PASurface=c(70,52, 94.5,88),
           Prop_PAss_PASurface=c(30,48, 5.5,12),
           Prop_noPA=c(86, 81.5, 51, 60.5))
# Convert from wide to long
proportion.overlap.PA.df.long=reshape2::melt(proportion.overlap.PA.df, 
                                             id.vars="metric",
                                             variable.name="proportion_type",
                                             value.name="proportion_value")

### Surface overlap cells
surface.overlap.PA.df=data.frame(metric=c("Alpha taxonomic", "Alpha functional", "Beta taxonomic", "Beta functional"), 
                                 Surface_allPA=c(33727,4041,205303,164249),
                                 Surface_PAss=c(10247,1946,11444,19951),
                                 Surface_noPA=c(205663,17851,210040,251095))
### Plots
## PA vs. No PA
# Set palette
palette_PA_NoPA=c("#FFC20A","#0C7BDC")

Alpha_PAvsNoPA=ggplot(data=proportion.overlap.PA.df.long[proportion.overlap.PA.df.long$proportion_type %in% c("Prop_allPA_allSurface","Prop_noPA") & 
                                            proportion.overlap.PA.df.long$metric %in% c("Alpha taxonomic", "Alpha functional"),], 
       aes(x=proportion_value, y=metric, fill=proportion_type)) +
  geom_bar(stat="identity", show.legend = F) + 
  theme_classic(base_size = 25) + 
  scale_x_continuous("% hotspot cells", breaks = c(0, 25, 50, 75, 100), labels =c(0, 25, 50, 75, 100) ) + 
  scale_y_discrete("", labels=c(expression(paste(alpha, " taxonomic")), expression(paste(alpha, " functional")))) +
  scale_fill_manual(values =palette_PA_NoPA )
ggsave(plot = Alpha_PAvsNoPA, filename = "OUTPUT/Protected_areas/Proportion_overlap_Alpha_PAvsNoPA.png", device = "png", width = 8, height = 2)

Beta_PAvsNoPA=ggplot(data=proportion.overlap.PA.df.long[proportion.overlap.PA.df.long$proportion_type %in% c("Prop_allPA_allSurface","Prop_noPA") & 
                                            proportion.overlap.PA.df.long$metric %in% c("Beta taxonomic", "Beta functional"),], 
       aes(x=proportion_value, y=metric, fill=proportion_type)) +
  geom_bar(stat="identity", show.legend = F) + 
  theme_classic(base_size = 25) + 
  scale_x_continuous("% hotspot cells", breaks = c(0, 25, 50, 75, 100), labels =c(0, 25, 50, 75, 100) ) + 
  scale_y_discrete("", labels=c(expression(paste(beta, " taxonomic")), expression(paste(beta, " functional")))) +
  scale_fill_manual(values =palette_PA_NoPA ) 
ggsave(plot = Beta_PAvsNoPA, filename = "OUTPUT/Protected_areas/Proportion_overlap_Beta_PAvsNoPA.png", device = "png", width = 8, height = 2)


palette_PAsl_PAss=c("#E1BE6A","#40B0A6")

Alpha_PAss_PAsl=ggplot(data=proportion.overlap.PA.df.long[proportion.overlap.PA.df.long$proportion_type %in% c("Prop_allPA_PASurface","Prop_PAss_PASurface") & 
                                            proportion.overlap.PA.df.long$metric %in% c("Alpha taxonomic", "Alpha functional"),], 
       aes(x=proportion_value, y=metric, fill=proportion_type)) +
  geom_bar(stat="identity", show.legend = F) + 
  theme_classic(base_size = 25) + 
  scale_x_continuous("% hotspot cells", breaks = c(0, 25, 50, 75, 100), labels =c(0, 25, 50, 75, 100) ) + 
  scale_y_discrete("", labels=c(expression(paste(alpha, " taxonomic")), expression(paste(alpha, " functional")))) +
  scale_fill_manual(values =palette_PAsl_PAss ) 
ggsave(plot = Alpha_PAss_PAsl, filename = "OUTPUT/Protected_areas/Proportion_overlap_Alpha__PAss_PAsl.png", device = "png", width = 8, height = 2)

Beta_PAss_PAsl=ggplot(data=proportion.overlap.PA.df.long[proportion.overlap.PA.df.long$proportion_type %in% c("Prop_allPA_PASurface","Prop_PAss_PASurface") & 
                                            proportion.overlap.PA.df.long$metric %in% c("Beta taxonomic", "Beta functional"),], 
       aes(x=proportion_value, y=metric, fill=proportion_type)) +
  geom_bar(stat="identity", show.legend = F) + 
  theme_classic(base_size = 25) + 
  scale_x_continuous("% hotspot cells", breaks = c(0, 25, 50, 75, 100), labels =c(0, 25, 50, 75, 100) ) + 
  scale_y_discrete("", labels=c(expression(paste(beta, " taxonomic")), expression(paste(beta, " functional")))) +
  scale_fill_manual(values =palette_PAsl_PAss )
ggsave(plot = Beta_PAss_PAsl, filename = "OUTPUT/Protected_areas/Proportion_overlap_Beta__PAss_PAsl.png", device = "png", width = 8, height = 2)

