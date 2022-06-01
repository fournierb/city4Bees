# Remove all R objects in the workspace
rm(list = ls())

### Library
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)

### Create raster layer with water bodies -----------------------------------------------------------------------
## LU map switzerland
LU1 = read_sf("~/Dropbox/PhD_Zurich/PROJECTS/Competition/OLD_analyses/landuse_09.shp")
TOP= raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/TOP.tif")
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




boundaries=readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Boundaries/swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET.shp")
germG <- spTransform(boundaries, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- extent(germG)
r <- raster(ext, res=c(100,100))  
r <- rasterize(germG, r, field=1)
# plot(r)

ex = extent(c(485500,833800,75300,295900))
r_boundaries = crop(x = r, y = ex)
r.new = resample(r_boundaries, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/boundaries.tiff", overwrite=T)




### Create raster layer with PROTECTED AREAS -----------------------------------------------------------------------
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

### Create raster layer with Dry meadows-----------------------------------------------------------------------

DGS = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/Dry_grasslands/merged_grasslands.shp")
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
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/DryMeadows.tiff", overwrite=T)

### Create raster layer with floodplains-----------------------------------------------------------------------

FLOODPLAINS = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/floodplains/Auen_LV03/au_A2.shp")
germG <- spTransform(FLOODPLAINS, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r, field=1)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_FLOODPLAINS = crop(x = r, y = ex)
r.new = resample(r_FLOODPLAINS, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/FLOODPLAINS.tiff", overwrite=T)

### Create raster layer with fens and bogs-----------------------------------------------------------------------

fens_bogs = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/fens_bogs/myres.shp")
germG <- spTransform(fens_bogs, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_fens_bogs = crop(x = r, y = ex)
r.new = resample(r_fens_bogs, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/fens_bogs.tiff", overwrite=T)


### Create raster layer with protection areas RAMSAR-----------------------------------------------------------------------

RAMSAR = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/Ramsar//Ramsar_LV03/ra.shp")
germG <- spTransform(RAMSAR, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r, field=1)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_RAMSAR = crop(x = r, y = ex)
r.new = resample(r_RAMSAR, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/RAMSAR.tiff", overwrite=T)

### Create raster layer with protection areas Amphibians-----------------------------------------------------------------------

AMPHIBIANS = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/Amphibians/Amphibien_LV03/AM_A3.shp")
germG <- spTransform(AMPHIBIANS, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r, field=1)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_AMPHIBIANS = crop(x = r, y = ex)
r.new = resample(r_AMPHIBIANS, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/AMPHIBIANS.tiff", overwrite=T)

### Create raster layer with protection areas Birds-----------------------------------------------------------------------

BIRDS = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/Amphibians/Amphibien_LV03/AM_A3.shp")
germG <- spTransform(BIRDS, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r, field=1)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_BIRDS = crop(x = r, y = ex)
r.new = resample(r_BIRDS, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/BIRDS.tiff", overwrite=T)


### Create raster layer with biogeographic -----------------------------------------------------------------------
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

### Create raster layer with Myre landscape----------------------------------------------------------------------

myre_landsp = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/fens_bogs/myres.shp")
germG <- spTransform(myre_landsp, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_myre_landsp = crop(x = r, y = ex)
r.new = resample(r_myre_landsp, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/myre_landsp.tiff", overwrite=T)
### Create raster layer with Biosphere----------------------------------------------------------------------

BIOSPHERE = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/Biosphere/Biosph„renreservate_LV03/biores_1.shp")
germG <- spTransform(BIOSPHERE, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_BIOSPHERE = crop(x = r, y = ex)
r.new = resample(r_BIOSPHERE, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/BIOSPHERE.tiff", overwrite=T)
### Create raster layer with Forest Reserves----------------------------------------------------------------------

FORESTRESERVES = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/Forest reserves/waldreservate.shp")
germG <- spTransform(FORESTRESERVES, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_FORESTRESERVES= crop(x = r, y = ex)
r.new = resample(r_FORESTRESERVES, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/FORESTRESERVES.tiff", overwrite=T)
### Create raster layer with Forest ProNatura----------------------------------------------------------------------

ProNaturaForest = readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/Reserves_forestieres_Pro_Natura/Reserves_forestieres_Pro_Natura.shp")
germG <- spTransform(ProNaturaForest, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_ProNaturaForest= crop(x = r, y = ex)
r.new = resample(r_ProNaturaForest, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/ProNaturaForest.tiff", overwrite=T)
### Create raster layer with Nature ProNatura----------------------------------------------------------------------

ProNaturaNature= readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/Reserves_forestieres_Pro_Natura/Reserves_forestieres_Pro_Natura.shp")
germG <- spTransform(ProNaturaNature, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_ProNaturaNature= crop(x = r, y = ex)
r.new = resample(r_ProNaturaNature, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/ProNaturaNature.tiff", overwrite=T)

### Create raster layer with National Park----------------------------------------------------------------------

NationalPark= readOGR("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Protected_Areas/National_park/nat_park.shp")
germG <- spTransform(NationalPark, CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs "))

ext <- floor(extent(germG))
r <- raster(ext, res=100)  
r <- rasterize(germG, r)
plot(r)

ex = extent(c(485500,833800,75300,295900))
r_NationalPark= crop(x = r, y = ex)
r.new = resample(r_NationalPark, TOP, "bilinear")
## Generate the final aligned raster
r.new = mask(r.new, TOP)
## Export
writeRaster(x = r.new, filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/NationalPark.tiff", overwrite=T)




