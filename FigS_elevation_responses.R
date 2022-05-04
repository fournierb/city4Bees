# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)
### load the data -----------------------------------------------------------------------
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/")
source("Load environmental data.R")

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-11/All descriptors")
l=list.files("DATA/Selected descriptors/Results_2022_04_28/Masked_responses/", full.names=T)
l.rasters=lapply(X = l, raster)
rasterstack.responses=stack(l.rasters)
raster::stackSave(x = rasterstack.responses, filename = "DATA/Selected descriptors/Results_2022_04_28/Masked_responses/rasterstack.responses.tif" )
names(rasterstack.responses) = c("FDIs", "TED", "TOP", "LCBD_fun", "LCBD_taxo", "shannon", "rich")

div <- stack("DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
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
### Elevation
elevation = raster("DATA/dhm_25.tif")

### Extract values responses
TOP.extr=data.frame(TOP=raster::extract(rasterstack.responses$TOP, coordinates(water_bodies)), coordinates(water_bodies))
TOP.extr=na.omit(TOP.extr)
TED.extr=data.frame(TED=raster::extract(rasterstack.responses$TED, coordinates(water_bodies)), coordinates(water_bodies))
TED.extr=na.omit(TED.extr)
FDis.extr=data.frame(FDis=raster::extract(rasterstack.responses$FDis, coordinates(water_bodies)), coordinates(water_bodies))
FDis.extr=na.omit(FDis.extr)
rich.extr=data.frame(rich=raster::extract(rasterstack.responses$rich, coordinates(water_bodies)), coordinates(water_bodies))
rich.extr=na.omit(rich.extr)
shannon.extr=data.frame(shannon=raster::extract(rasterstack.responses$shannon, coordinates(water_bodies)), coordinates(water_bodies))
shannon.extr=na.omit(shannon.extr)
LCBD_taxo.extr=data.frame(LCBD_taxo=raster::extract(rasterstack.responses$LCBD_taxo, coordinates(water_bodies)), coordinates(water_bodies))
LCBD_taxo.extr=na.omit(LCBD_taxo.extr)
LCBD_fun.extr=data.frame(LCBD_fun=raster::extract(rasterstack.responses$LCBD_fun, coordinates(water_bodies)), coordinates(water_bodies))
LCBD_fun.extr=na.omit(LCBD_fun.extr)
### Extract elevation
elevation.extr=data.frame(elevation=raster::extract(elevation, coordinates(water_bodies)), coordinates(water_bodies))
elevation.extr=na.omit(elevation.extr)
elevation.extr$coordsmerge=paste(elevation.extr$x, elevation.extr$y)

elevation.p=ggplot(elevation.extr, aes(x=elevation)) +
  geom_histogram(bins = 45) +
  theme_classic(base_size = 15) +
  ylab("Frequency") +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  xlab("Elevation (m)")
ggsave(plot = elevation.p, filename = "OUTPUT/Elevation_vs_responses/Elevation_histogram.png", device = "png", width = 4, height =4) 

random.coordsmerge=sample(response.dat$coordsmerge, size = 10000)
elevation.extr.random=elevation.extr[elevation.extr$coordsmerge %in% random.coordsmerge,]
list.responses=list(TOP.extr,TED.extr,FDis.extr,rich.extr,shannon.extr,LCBD_taxo.extr,LCBD_fun.extr)

for(r in 1:length(list.responses)){
  response.dat = list.responses[[r]]
  response.dat$coordsmerge=paste(response.dat$x, response.dat$y)
  response.dat.random=  response.dat[ response.dat$coordsmerge %in% random.coordsmerge,]
  response.dat.random.ele=merge(response.dat.random, elevation.extr.random, by=c("x", "y"))
  plot.elevation=ggplot(response.dat.random.ele, aes(x=elevation, y=get(colnames(response.dat.random.ele)[3]))) + 
    geom_smooth(aes(fill=paste(colnames(response.dat.random.ele)[3])), alpha=0.1) +
    scale_fill_manual(values = "blue") +
    theme_classic(base_size = 15) +
    scale_x_continuous(name="Elevation (m)", breaks = c(0, 1000, 2000, 3000, 4000), labels = c(0, 1000, 2000, 3000, 4000),limits=c(197, 4800)) +
    ylab(paste(colnames(response.dat.random.ele)[3])) + 
    theme(
      legend.title = element_blank())
   ggsave(plot = plot.elevation, filename = paste("OUTPUT/Elevation_vs_responses/Elevation_", colnames(response.dat.random.ele)[3], ".png", sep=""), device = "png", width = 5, height = 4) 
}
