# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)
### load the data -----------------------------------------------------------------------
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/")


rasterstack.responses=stackOpen("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/Masked_responses/rasterstack.responses.tif")
names(rasterstack.responses) = c("FDis", "TED", "TOP", "LCBD_fun", "LCBD_taxo", "shannon", "rich")

div <- stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD","LCBD_fun" ,"LCBD_taxo","phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
## Community attributes
TOP <- div$TOP
TED <- div$TED
FDis <- div$FDis
rich <- div$Richness
shannon <- div$Shannon
LCBD_fun <- div$LCBD_fun*1000
LCBD_taxo<- div$LCBD_taxo*1000
## CWM traits
belowgound <- div$belowgound
cleptoparasite <- div$cleptoparasite
feeding_specialization <- div$feeding_specialization
phenoduration <- div$phenoduration
ITD <- div$ITD
solitary <- div$solitary
tong_length<- div$tong_length
### water bodies
water_bodies=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/water_bodies1.tif")
### Elevation
elevation = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/dhm_25.tif")

### Extract community attributes
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
### Extract CWM traits
belowgound.extr=data.frame(belowgound=raster::extract(belowgound, coordinates(water_bodies)), coordinates(water_bodies))
belowgound.extr=na.omit(belowgound.extr)
cleptoparasite.extr=data.frame(cleptoparasite=raster::extract(cleptoparasite, coordinates(water_bodies)), coordinates(water_bodies))
cleptoparasite.extr=na.omit(cleptoparasite.extr)
feeding_specialization.extr=data.frame(feeding_specialization=raster::extract(feeding_specialization, coordinates(water_bodies)), coordinates(water_bodies))
feeding_specialization.extr=na.omit(feeding_specialization.extr)
phenoduration.extr=data.frame(phenoduration=raster::extract(phenoduration, coordinates(water_bodies)), coordinates(water_bodies))
phenoduration.extr=na.omit(phenoduration.extr)
ITD.extr=data.frame(ITD=raster::extract(ITD, coordinates(water_bodies)), coordinates(water_bodies))
ITD.extr=na.omit(ITD.extr)
solitary.extr=data.frame(solitary=raster::extract(solitary, coordinates(water_bodies)), coordinates(water_bodies))
solitary.extr=na.omit(solitary.extr)
tong_length.extr=data.frame(tong_length=raster::extract(tong_length, coordinates(water_bodies)), coordinates(water_bodies))
tong_length.extr=na.omit(tong_length.extr)
### Extract elevation
elevation.extr=data.frame(elevation=raster::extract(elevation, coordinates(water_bodies)), coordinates(water_bodies))
elevation.extr=na.omit(elevation.extr)
elevation.extr$coordsmerge=paste(elevation.extr$x, elevation.extr$y)
random.coordsmerge=sample(response.dat$coordsmerge, size = 10000)
elevation.extr.random=elevation.extr[elevation.extr$coordsmerge %in% random.coordsmerge,]

list.responses=list(TOP.extr,TED.extr,FDis.extr,rich.extr,shannon.extr,LCBD_taxo.extr,LCBD_fun.extr)

for(r in 1:length(list.responses)){
  response.dat = list.responses[[r]]
  response.dat$coordsmerge=paste(response.dat$x, response.dat$y)
  response.dat.random=  response.dat[ response.dat$coordsmerge %in% random.coordsmerge,]
  response.dat.random.ele=merge(response.dat.random, elevation.extr.random, by=c("x", "y"))
  plot.elevation=ggplot(response.dat.random.ele, aes(x=elevation, y=get(colnames(response.dat.random.ele)[3]))) + 
    geom_smooth(aes(fill=paste(colnames(response.dat.random.ele)[3])), alpha=0.1, method="loess") +
    scale_fill_manual(values = "blue") +
    theme_classic(base_size = 15) +
    scale_x_continuous(name="Elevation (m)", breaks = c(0, 1000, 2000, 3000), labels = c(0, 1000, 2000, 3000),limits=c(197, 3500)) +
    ylab(paste(colnames(response.dat.random.ele)[3])) + 
    theme(
      legend.title = element_blank(), legend.position = "none")
   ggsave(plot = plot.elevation, filename = paste("~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/Elevation_vs_responses/Elevation_", colnames(response.dat.random.ele)[3], ".png", sep=""), device = "png", width = 4, height = 4) 
}


list.traits=list(belowgound.extr,cleptoparasite.extr,feeding_specialization.extr,phenoduration.extr,ITD.extr,solitary.extr,tong_length.extr)

for(r in 1:length(list.traits)){
  response.dat = list.traits[[r]]
  response.dat$coordsmerge=paste(response.dat$x, response.dat$y)
  response.dat.random=  response.dat[ response.dat$coordsmerge %in% random.coordsmerge,]
  response.dat.random.ele=merge(response.dat.random, elevation.extr.random, by=c("x", "y"))
  plot.elevation=ggplot(response.dat.random.ele, aes(x=elevation, y=get(colnames(response.dat.random.ele)[3]))) + 
    geom_smooth(aes(fill=paste(colnames(response.dat.random.ele)[3])), alpha=0.1, method = "loess") +
    scale_fill_manual(values = "blue") +
    theme_classic(base_size = 15) +
    scale_x_continuous(name="Elevation (m)", breaks = c(0, 1000, 2000, 3000), labels = c(0, 1000, 2000, 3000),limits=c(197, 3500)) +
    ylab(paste(colnames(response.dat.random.ele)[3])) + 
    theme(
      legend.title = element_blank(), legend.position = "none")
  ggsave(plot = plot.elevation, filename = paste("~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/Elevation_vs_responses/Elevation_", colnames(response.dat.random.ele)[3], ".png", sep=""), device = "png", width = 4, height = 4) 
}

names.r=names(rasterstack.responses)
calc.responses=list()
for(i in 1:length(names.r)){
  cat(names.r[i])
  dat.extr1=data.frame(rich=raster::extract(rasterstack.responses[[i]], coordinates(water_bodies)), coordinates(water_bodies))
  elevation.extr1=data.frame(elevation=raster::extract(elevation, coordinates(water_bodies)), coordinates(water_bodies))
  
  calc_res=cbind(dat.extr1,elevation.extr1[,1])
  calc_res=na.omit(calc_res)
  calc_res_2000 = data.frame(mean=mean(calc_res[calc_res$`elevation.extr1[, 1]` %in% 2000:3500,1], na.rm=T), 
                             median=median(calc_res[calc_res$`elevation.extr1[, 1]` %in% 2000:3500,1], na.rm=T),
                             sd=sd(calc_res[calc_res$`elevation.extr1[, 1]` %in% 2000:3500,1], na.rm=T), 
                             min=min(calc_res[calc_res$`elevation.extr1[, 1]` %in% 2000:3500,1], na.rm=T),
                             max=max(calc_res[calc_res$`elevation.extr1[, 1]` %in% 2000:3500,1], na.rm=T),
                             elevation=">2000",
                             response=paste(names.r[i]))
  
  calc_res_1500_2000 =data.frame(mean=mean(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1500:2000,1], na.rm=T), 
                                 median=median(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1500:2000,1], na.rm=T),
                                 sd=sd(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1500:2000,1], na.rm=T), 
                                 min=min(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1500:2000,1], na.rm=T),
                                 max=max(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1500:2000,1], na.rm=T),
                                 elevation="1500.2000",
                                 response=paste(names.r[i]))
  
  calc_res_1000_1500=data.frame(mean=mean(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1000:1500,1], na.rm=T), 
                                median=median(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1000:1500,1], na.rm=T),
                                sd=sd(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1000:1500,1], na.rm=T), 
                                min=min(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1000:1500,1], na.rm=T),
                                max=max(calc_res[calc_res$`elevation.extr1[, 1]` %in% 1000:1500,1], na.rm=T),
                                elevation="1000-1500",
                                response=paste(names.r[i]))
  
  calc_res_500_1000=data.frame(mean=mean(calc_res[calc_res$`elevation.extr1[, 1]` %in% 500:1000,1], na.rm=T), 
                               median=median(calc_res[calc_res$`elevation.extr1[, 1]` %in% 500:1000,1], na.rm=T),
                               sd=sd(calc_res[calc_res$`elevation.extr1[, 1]` %in% 500:1000,1], na.rm=T), 
                               min=min(calc_res[calc_res$`elevation.extr1[, 1]` %in% 500:1000,1], na.rm=T),
                               max=max(calc_res[calc_res$`elevation.extr1[, 1]` %in% 500:1000,1], na.rm=T),
                               elevation="500-1000",
                               response=paste(names.r[i]))
  
  calc_res_100_500=data.frame(mean=mean(calc_res[calc_res$`elevation.extr1[, 1]` %in% 100:500,]$rich, na.rm=T), 
                              median=median(calc_res[calc_res$`elevation.extr1[, 1]` %in% 100:500,]$rich, na.rm=T),
                              sd=sd(calc_res[calc_res$`elevation.extr1[, 1]` %in% 100:500,]$rich, na.rm=T), 
                              min=min(calc_res[calc_res$`elevation.extr1[, 1]` %in% 100:500,]$rich, na.rm=T),
                              max=max(calc_res[calc_res$`elevation.extr1[, 1]` %in% 100:500,]$rich, na.rm=T),
                              elevation="100-500",
                              response=paste(names.r[i]))
  
  calc.responses[[i]]=rbind(calc_res_2000,calc_res_1500_2000,calc_res_1000_1500,calc_res_500_1000,calc_res_100_500)
}

calc.responses.ul=do.call(what = rbind, calc.responses)
