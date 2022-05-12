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
# plot(r)

ex = extent(c(485500,833800,75300,295900))
r_PAs = crop(x = r, y = ex)
r.new = resample(r_PAs, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PAs.tiff", overwrite=T)

germG@data[germG@data$NA3=="National Park","NA3"] = 1
germG@data[germG@data$NA3=="Habitat / species, management area","NA3"] = 2
germG@data[germG@data$NA3=="Protected landscape / seascape","NA3"] = 3
germG@data[germG@data$NA3=="Strict nature reserve / wilderness Area","NA3"] = 4
germG@data[germG@data$NA3=="Unknown","NA3"] = 5


ext <- extent(germG)
r <- raster(ext, res=c(100,100))  
r <- rasterize(germG, r, field=as.numeric(germG$NA3))
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_PAs2 = crop(x = r, y = ex)
r.new = resample(r_PAs2, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PAs2.tiff", overwrite=T)

### Create raster layer with DRY MEADOWS

DGS = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Dry_grasslands/merged_grasslands.shp")
germG <- spTransform(DGS, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r, field=1)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_DGS = crop(x = r, y = ex)
r.new = resample(r_DGS, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/DGS.tiff", overwrite=T)

### Create raster layer with floodplains

FP = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/floodplains/Ramsar_LV03/ra.shp")
germG <- spTransform(FP, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r, field=1)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_FP = crop(x = r, y = ex)
r.new = resample(r_FP, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/FP.tiff", overwrite=T)

### Create raster layer with myres

myres = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/myres/myres.shp")
germG <- spTransform(myres, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_myres = crop(x = r, y = ex)
r.new = resample(r_myres, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/myres.tiff", overwrite=T)

### Create raster layer with biogeographic

BioGeo = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/biogeographic/BiogeographischeRegionen/N2020_Revision_BiogeoRegion.shp")
germG <- spTransform(BioGeo, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_BioGeo = crop(x = r, y = ex)
r.new = resample(r_BioGeo, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/BioGeo.tiff", overwrite=T)

#### Prepare df
PAs=raster("DATA/PAs.tif")
PAs2=raster("DATA/PAs2.tiff")
DGS=raster("DATA/DGS.tiff")
FP=raster("DATA/FP.tiff")
Myres=raster("DATA/myres.tiff")

PA.df=as.data.frame(PAs)
PA2.df=as.data.frame(PAs2)
DGS.df=as.data.frame(DGS)
FP.df=as.data.frame(FP)
Myres.df=as.data.frame(Myres)

TOP.df=as.data.frame(TOP)
TOP.df=cbind(TOP.df, coordinates(TOP))
TOP.df=na.omit(TOP.df)
TOP.df$coordsmerged=paste(TOP.df$x, TOP.df$y, sep="")
list.protected=cbind(PA.df,PA2.df,DGS.df,FP.df, Myres.df, coordinates(PAs))
list.protected$coordsmerged=paste(list.protected$x, list.protected$y, sep="")
list.protected=list.protected[list.protected$coordsmerged %in% TOP.df$coordsmerged,]
write.csv(list.protected, "DATA/protected_areas.csv")
