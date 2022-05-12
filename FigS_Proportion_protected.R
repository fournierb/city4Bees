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

require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)
require("magrittr")
require("ggpubr")
require(egg)
require(ggpubr)

### ===================================
###  Data
### ===================================
### load the data -----------------------------------------------------------------------
rasterstack.responses=stackOpen("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/Masked_responses/rasterstack.responses.tif")
names(rasterstack.responses) = c("FDis", "TED", "TOP", "LCBD_fun", "LCBD_taxo", "shannon", "rich")
## PAs
PAs=raster("DATA/PAs.tif")
PAs.df1=data.frame(raster::extract(PAs, coordinates(rasterstack.responses[[1]])), coordinates(rasterstack.responses[[1]]))
PAs.df1[is.na(PAs.df1)] = 0
PAs.df1$coordsmerge=paste(PAs.df1$x, PAs.df1$y, sep=" ")
PAs.df=na.omit(PAs.df1)
PAs.df$coordsmerge=paste(PAs.df$x, PAs.df$y, sep=" ")
## Elevation
elevation=raster("DATA/dhm_25.tif")
crs(elevation) = crs(rasterstack.responses[[1]])
  # Extract
  elevation.df=data.frame(elevation=raster::extract(elevation, coordinates(PAs)), coordinates(PAs))  
  # Na omit
  elevation.df=na.omit(elevation.df)
  # Merging column
  elevation.df$coordsmerge=paste(elevation.df$x, elevation.df$y, sep=" ")
  # Merge
  elevation.pas=merge(elevation.df, PAs.df, "coordsmerge")
  # Omit
  elevation.pas=na.omit(elevation.pas)
## Responses
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
## Calculate Proportion protected areas
colnames(PAs.df1) = c("PAs", "x", "y", "coordsmerge")
list.responses=list(TOP.extr,TED.extr,FDis.extr,rich.extr,shannon.extr,LCBD_taxo.extr,LCBD_fun.extr)
names.responses=c("TOP", "TED", "FDis", "rich", "shannon", "LCBD_taxo", "LCBD_fun")
responses.list=list()
for(r in 1:length(list.responses)){
  cat(paste(r, " in progress!","\n"))
  response.dat = list.responses[[r]]
  response.dat$coordsmerge=paste(response.dat$x, response.dat$y, sep=" ")
  response.dat.pa=merge(response.dat,PAs.df1,by= "coordsmerge")
  response.dat.pa[is.na(response.dat.pa)] = 0
  response.dat.pa.l=reshape::melt(response.dat.pa,id.vars=paste(names.responses[r]), measure.vars="PAs", variable.name="PA")
  colnames(response.dat.pa.l) = c("Response", "PAs", "Presence")
  proportions=list()
  for(p in 3:100){
    cat(paste(p, " in progress!","\n"))
    # Select quantile
    prop.dat= response.dat.pa.l[ response.dat.pa.l$Response <=quantile(response.dat.pa.l$Response, probs=seq(0,1, 0.01)[p]) , ]
    prop.dat$prop=quantile(response.dat.pa[, 2], probs=seq(0,1, 0.01)[p])
    # Calculate N non protected
    prop.dat.NP= prop.dat %>% dplyr::group_by(prop, Presence) %>% dplyr::filter(Presence==0) %>% dplyr::summarise(n_NP=n())
    # Calculate N protected
    prop.dat.P= prop.dat %>% dplyr::group_by(prop, Presence) %>% dplyr::filter(Presence==1) %>% dplyr::summarise(n_P=n())
    # Data frame
    prop.dat.all=data.frame(prop.dat.NP,n_P=prop.dat.P$n_P)
    # Calculate total cells
    prop.dat.all$total=prop.dat.all$n_NP+prop.dat.all$n_P
    # Calculate proportion non protected
    prop.dat.all$prop_NP=prop.dat.all$n_NP/prop.dat.all$total
    # Calculate proportion  protected
    prop.dat.all$prop_P=prop.dat.all$n_P/prop.dat.all$total
    proportions[[p]]=prop.dat.all
  
    }
  responses.list[[r]]=do.call(rbind, proportions)
  

}
### Unlist responses
responses.unlist =do.call(rbind, responses.list)
### Add response name
responses.unlist$response=rep(names.responses, each=98)
### Create ID
responses.unlist$ID=seq(1, 98, times=7)
### Create palette
palette.responses=c(wesanderson::wes_palette("Darjeeling1"), wesanderson::wes_palette("Darjeeling2")[1:2])
### Plot proportion Protected
pp =ggplot(responses.unlist, aes(x=ID, y=prop_P, col=response)) +
  geom_line(aes(size = 0.5)) +
  scale_color_manual(values = palette.responses) +
  theme_classic(base_size = 20) +
  scale_x_continuous(" Quantile diversity (%)", breaks=c(0, 23, 48, 73, 98), labels = c(0, 25, 50, 75, 100)) +
  ylab("Proportion protected cells")
### Plot elevation PA frequency
plot_pas_elevation=ggplot(elevation.pas, aes(elevation)) +
  geom_histogram(bins = 60) + 
  theme_classic(base_size = 20) +
  xlab("Elevation (m)") +
  ylab("Frequency")
### Plot PA

ggplot(PAs.df, aes(x=x, y=y)) + geom_tile() + theme_classic()
tiff(filename = "OUTPUT/PA_TOP.tiff")
plot(rasterstack.responses[[7]], axes=F, legend=F, box=F, col=viridis(25))
plot(PAs, axes=F, legend=F, box=F, add=T, col="black")
dev.off()
### Arrange

figure= ggarrange(pp, plot_pas_elevation, 
          labels = paste("(",letters[1:2],")", sep=""),
          font.label = list(size=16),
          nrow = 1, ncol=2)
### Export
figure %>% ggexport(filename = "OUTPUT/Fig_Protected_cells.png",
                width = 1700, height = 1200)

figure %>% ggexport(filename = "OUTPUT//Fig_VProtected_cells.pdf",
                width = 20, height = 10)
