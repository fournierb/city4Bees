#######################################
### Paper: 
###
### Author:
### Date: 
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
require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)
require("magrittr")
require("ggpubr")
require(egg)
require(ggpubr)
library(dplyr)
### ===================================
###  Data
### ===================================
### load the data -----------------------------------------------------------------------
rasterstack.responses=stackOpen("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/Masked_responses/rasterstack.responses.tif")
names(rasterstack.responses) = c("FDis", "TED", "TOP", "LCBD_fun", "LCBD_taxo", "shannon", "rich")
## Water bodies
water_bodies=raster("DATA/water_bodies1.tif")
## PAs
PAs=raster("DATA/PA.sensu.stricto.tiff")
  # Extract in df
  PAs.df1=data.frame(raster::extract(PAs, coordinates(rasterstack.responses[[1]])), coordinates(rasterstack.responses[[1]]))
  # Na omit
  PAs.df1[is.na(PAs.df1)] = 0
  # Merging column
  PAs.df1$coordsmerge=paste(PAs.df1$x, PAs.df1$y, sep=" ") # DF for plots, with NA transformed to 0
  # Na omit
  PAs.df=na.omit(PAs.df1)
  # Merging column
  PAs.df$coordsmerge=paste(PAs.df$x, PAs.df$y, sep=" ") 
PAs2=raster("DATA/PA.sensu.lato.tiff")
  # Extract in df
  PAs2.df1=data.frame(raster::extract(PAs2, coordinates(rasterstack.responses[[1]])), coordinates(rasterstack.responses[[1]]))
  # Na omit
  PAs2.df1[is.na(PAs2.df1)] = 0
  # Merging column
  PAs2.df1$coordsmerge=paste(PAs2.df1$x, PAs2.df1$y, sep=" ") # DF for plots, with NA transformed to 0
  # Na omit
  PAs2.df=na.omit(PAs2.df1)
  # Merging column
  PAs2.df$coordsmerge=paste(PAs2.df$x, PAs2.df$y, sep=" ") 
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
  
  # Extract
  elevation2.df=data.frame(elevation=raster::extract(elevation, coordinates(PAs2)), coordinates(PAs2))  
  # Na omit
  elevation2.df=na.omit(elevation2.df)
  # Merging column
  elevation2.df$coordsmerge=paste(elevation2.df$x, elevation2.df$y, sep=" ")
  # Merge
  elevation2.pas=merge(elevation2.df, PAs2.df, "coordsmerge")
  # Omit
  elevation2.pas=na.omit(elevation2.pas)
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

### ===================================
###  Calculate Proportion protected areas
### ===================================
  ##  Rebane
colnames(PAs2.df1) = c("PAs", "x", "y", "coordsmerge")
  ##  RList responses for loop
list.responses=list(TOP.extr,TED.extr,FDis.extr,rich.extr,shannon.extr,LCBD_taxo.extr,LCBD_fun.extr)
  ## vector names
names.responses=c("TOP", "TED", "FDis", "rich", "shannon", "LCBD_taxo", "LCBD_fun")
  ## result list
responses.list=list()
### Loop
for(r in 1:length(list.responses)){
  cat(paste("Response ", r, " in progress!","\n"))
  response.dat = list.responses[[r]]
  response.dat$coordsmerge=paste(response.dat$x, response.dat$y, sep=" ")
  response.dat.pa=merge(response.dat,PAs2.df1,by= "coordsmerge")
  response.dat.pa[is.na(response.dat.pa)] = 0
  response.dat.pa.l=reshape::melt(response.dat.pa,id.vars=paste(names.responses[r]), measure.vars="PAs", variable.name="PA")
  colnames(response.dat.pa.l) = c("Response", "PAs", "Presence")
  proportions=list()
  for(p in 1:100){
    cat(paste("Quantile ",p, " in progress!","\n"))
    # Select quantile
    prop.dat= response.dat.pa.l[ response.dat.pa.l$Response >=quantile(response.dat.pa.l$Response, probs=seq(1,0, -0.01)[p]) , ]
    prop.dat$prop=quantile(response.dat.pa[, 2], probs=seq(1,0, -0.01)[p])
    if(length(unique(prop.dat$Presence))==2){
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
    if(length(unique(prop.dat$Presence))==1){
      # Calculate N non protected
      prop.dat.NP= prop.dat %>% dplyr::group_by(prop, Presence) %>% dplyr::filter(Presence==0) %>% dplyr::summarise(n_NP=n())
      # Calculate N protected
      prop.dat.P= data.frame(prop=quantile(response.dat.pa[, 2], probs=seq(1,0, -0.01)[p]) , Presence=1, n_P=0)
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
  
    }
  responses.list[[r]]=do.call(rbind, proportions)
  

}
### Unlist responses
responses.unlist =do.call(rbind, responses.list)
### Add response name
responses.unlist$response=rep(names.responses, each=100)
### Create ID
responses.unlist$ID=seq(1, 100, times=7)
### ===================================
###  Plot
### ===================================
### Create palette
palette.responses=c(wesanderson::wes_palette("Darjeeling1"), wesanderson::wes_palette("Darjeeling2")[1:2])
### Plot proportion Protected
pp =ggplot(responses.unlist, aes(x=ID, y=prop_P, col=response)) +
  geom_line(size=2, alpha=0.5) +
  scale_color_manual(values = palette.responses) +
  theme_classic(base_size = 20) +
  geom_hline(yintercept = 0.24, col="black")+
  scale_x_continuous("Diversity gradient (%)", breaks=seq(0,100, by=10), labels = seq(0,100, by=10)) +
  ylab("Proportion protected cells") +
  theme(legend.position = "none")
### Plot elevation PA frequency
colnames(elevation.pas) = c("coordsmerge", "elevation", "x", "y", "PA", "x1", "y2")
colnames(elevation2.pas) = c("coordsmerge", "elevation", "x", "y", "PA", "x1", "y2")

plot_pas_elevation=ggplot(elevation.pas, aes(elevation)) +
  geom_histogram(bins = 60) + 
  theme_classic(base_size = 20) +
  scale_x_continuous(name = "Elevation (m)", limits = c(0, 3500)) +
  ylab("Frequency")

elevation.pas2= elevation2.pas %>% dplyr::group_by(elevation) %>% dplyr::summarise(n_P=sum(PA), total=n())
elevation.pas2$prop_P=elevation.pas2$n_P/elevation.pas2$total
plot_pas_elevation2=ggplot(elevation.pas2, aes(x=elevation, y=prop_P)) +
  geom_line() +
  theme_classic(base_size = 20) +
  scale_x_continuous(name = "Elevation (m)", limits = c(0, 3500)) +
  ylab("Proportion protected cells")
### Plot PA
tiff(filename = "OUTPUT/PA_sensu_lato.tiff")
plot(TOP, axes=F, legend=F, box=F, col="grey90")
plot(PAs2, axes=F, legend=F, box=F, add=T, col="#693D3D")
dev.off()

tiff(filename = "OUTPUT/PA_sensu_stricto.tiff")
plot(TOP, axes=F, legend=F, box=F, col="grey90")
plot(PAs, axes=F, legend=F, box=F, add=T, col="#693D3D")
dev.off()
### Arrange
figure= ggpubr::ggarrange(pp, plot_pas_elevation2, plot_pas_elevation,
          labels = paste("(",letters[1:3],")", sep=""),
          font.label = list(size=16),
          nrow = 1, ncol=3)
### Export
figure %>% ggexport(filename = "OUTPUT/Fig_Protected_cells.png",
                width = 1700, height = 1200)

figure %>% ggexport(filename = "OUTPUT//Fig_VProtected_cells.pdf",
                width = 14, height = 5)


## Total protected ares
3149462/3980906 # mean non-protected
980574/3980906 # protected sensu lato
194189/3980906 # protected sensu stricto
