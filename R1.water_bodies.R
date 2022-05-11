# Remove all R objects in the workspace
rm(list = ls())

### Library
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)

### Create raster layer with water bodies
## LU map switzerland
LU1 = read_sf("~/Dropbox/PhD_Zurich/PROJECTS/Competition/OLD_analyses/landuse_09.shp")
TOP= raster("DATA/Selected descriptors/Results_2022_04_28/TOP.tif")
## As data frame
landuse_ch09_2 <- as.data.frame(LU1)
## Select the subcategories (10 main subcateories)
landuse_ch09_2 <- landuse_ch09_2[,c(2,3,14)]
landuse_ch09_2$lu09_10 <- as.factor(landuse_ch09_2$lu09_10)
## Set CRS 
crs="+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "
## Rasterise
r_lu09 <- rasterFromXYZ(xyz = landuse_ch09_2, crs = crs, res = c(100,100)) ## 400 =water

## Set extent
ex = extent(c(485500,833800,75300,295900))
## Align rasters
r_lu09 = crop(x = r_lu09, y = ex)
r.new = resample(r_lu09, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/LU_10.tiff", overwrite=T)
## Select water bodies (category 400)
water_bodies1 <- r.new %in% 400
## Export
writeRaster(x = water_bodies1, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/water_bodies1.tiff", overwrite=T)



### Create raster layer with PROTECTED AREAS
PAs = readOGR("~/Dropbox/City4bees/Raw_data/Unprocessed/SHAPEFILE/swissTLMRegio_Product_LV95/Miscellaneous/swissTLMRegio_ProtectedArea.shp")
germG <- spTransform(PAs, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- extent(germG)
r <- raster(ext, res=c(100,100))  
r <- rasterize(germG, r, field=1)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_PAs = crop(x = r, y = ex)
r.new = resample(r_PAs, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PAs.tiff", overwrite=T)


### Create raster layer with DRY MEADOWS

PAs = readOGR("~/Dropbox/City4bees/Raw_data/Unprocessed/SHAPEFILE/swissTLMRegio_Product_LV95/Miscellaneous/swissTLMRegio_ProtectedArea.shp")
germG <- spTransform(PAs, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- extent(germG)
r <- raster(ext, res=c(100,100))  
r <- rasterize(germG, r, field=1)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_PAs = crop(x = r, y = ex)
r.new = resample(r_PAs, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PAs.tiff", overwrite=T)