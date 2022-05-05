# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)

### Elevation
elevation = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/dhm_25.tif")
### CH
extend.raster=raster("DATA/Selected descriptors/Results_2022_04_28/belowgound.tif")
### Predictors
climate = stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/Climate_PCA_CH_stack.tif")
vegetation = stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/Plant_PC_19_revised.tif")
beekeeping = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/beehive-2012-2018.tif")
urban = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/Urban_perc_18.tif")
rural = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/")
forest = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/Urban_perc_18.tif")


masked.climate=mask(x =climate,extend.raster )
png(filename = "OUTPUT/maps_climate.png", width = ncol(masked.climate), height = nrow(masked.climate))
plot(masked.climate, legend=T, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
dev.off()

