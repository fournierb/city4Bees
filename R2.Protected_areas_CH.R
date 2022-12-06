#######################################
###
###
###
###
###
###
#######################################
### ===================================
###  Initialise the system
### ===================================
# Remove all R objects in the workspace
rm(list = ls())
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/")
# Packages 
library(dplyr)
require(raster)
require(viridis)
require(ggplot2)
require("magrittr")
require("ggpubr")
source("~/Dropbox/City4bees/Analyses/bees_switzerland/city4Bees/R_rainclouds.R")
### ===================================
###  Data
### ===================================
### load the data -----------------------------------------------------------------------
PAs=raster("DATA/PAs.tif")
PAs2=raster("DATA/PAs2.tiff") # tlm model
DryMeadows=raster("DATA/DryMeadows.tiff") # Biotopes of national importance
floodplains=raster("DATA/FLOODPLAINS.tiff") # Biotopes of national importance
ForestReserves=raster("DATA/FORESTRESERVES.tiff")
ProNaturaForest=raster("DATA/ProNaturaForest.tiff")
ProNaturaNature=raster("DATA/ProNaturaNature.tiff")
NationalPark=raster("DATA/NationalPark.tiff")
Amphibians=raster("DATA/AMPHIBIANS.tiff")  # Biotopes of national importance
Ramsar=raster("DATA/RAMSAR.tiff")
FensBogs=raster("DATA/fens_bogs.tiff")  # Biotopes of national importance
BirdsWater=raster("DATA/BIRDS.tiff")
Emerald=raster("DATA/emerald.tif")
UNESCO=raster("DATA/UNESCO.tif")
Biosphere=raster("DATA/BIOSPHERE.tiff")
game_reserves=raster("DATA/game_reserves.tif")
water=raster("DATA/water_bodies1.tif")

### ALL Protected areas -----------------------------------------------------------------------
list.raster.sensu.lato=list(PAs,
                            PAs2,
                            Emerald,
                            floodplains,
                            UNESCO,
                            Biosphere,
                            DryMeadows,
                            ForestReserves,
                            ProNaturaForest,
                            ProNaturaNature,
                            NationalPark,
                            Amphibians,
                            Ramsar,
                            BirdsWater,
                            game_reserves,
                            FensBogs)
PA.sensu.lato=do.call(merge, list.raster.sensu.lato)
PA.sensu.lato[PA.sensu.lato> 0] <- 1
writeRaster(PA.sensu.lato, "DATA/PA.sensu.lato.tiff", overwrite=T)

### Protected sense stricto -----------------------------------------------------------------------
list.raster.sensu.stricto=list(floodplains,
                               DryMeadows,
                               ForestReserves,
                               ProNaturaForest,
                               ProNaturaNature,
                               NationalPark,
                               Amphibians,
                               Ramsar,
                               FensBogs)
                               
PA.sensu.stricto=do.call(merge, list.raster.sensu.stricto)
PA.sensu.stricto[PA.sensu.stricto > 0] <- 1
writeRaster(PA.sensu.stricto, "DATA/PA.sensu.stricto.tiff", overwrite=T)

### Protected sense lato only-----------------------------------------------------------------------

PA.sensu.stricto.t=is.na(PA.sensu.stricto) <= 0
PA.sensu.lato.t=is.na(PA.sensu.lato) <= 0

PA.sensu.lato_only=PA.sensu.lato.t-PA.sensu.stricto.t
PA.sensu.lato_only[PA.sensu.lato_only<1] <- NA
writeRaster(PA.sensu.lato_only, "DATA/PA.sensu.lato2.tiff", overwrite=T)

PA.sensu.stricto.df=as.data.frame(PA.sensu.stricto)
PA.sensu.stricto.df=cbind(PA.sensu.stricto.df, coordinates(PA.sensu.stricto))
PA.sensu.stricto.df=na.omit(PA.sensu.stricto.df)
