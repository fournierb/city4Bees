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
library(dplyr)
require(raster)
require(viridis)
require(ggplot2)
library(sf)
source("~/Dropbox/City4bees/Analyses/bees_switzerland/city4Bees/R_rainclouds.R")
## decimal places
scaleFUN <- function(x) sprintf("%.2f", x)
### ===================================
###  Data
### ===================================
### load the data -----------------------------------------------------------------------
rasterstack.responses=stackOpen("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/Masked_responses/rasterstack.responses.tif")
names(rasterstack.responses) = c("FDis", "TED", "TOP", "LCBD_fun", "LCBD_taxo", "shannon", "rich")

div <- stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD","LCBD_fun" ,"LCBD_taxo","phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
### water bodies
water_bodies=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/water_bodies1.tif")
### Elevation
elevation = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/dhm_25.tif")
### Raster individual -----------------------------------------------------------------------
## Functional metrics
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
phenostart=div$phenostart
ITD <- div$ITD
solitary <- div$solitary
tong_length<- div$tong_length

### ===================================
###  calculations
### ===================================
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
  phenostart.extr=data.frame(phenostart=raster::extract(phenostart, coordinates(water_bodies)), coordinates(water_bodies))
  phenostart.extr=na.omit(phenostart.extr)
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
  elevation.extr.random=elevation.extr[elevation.extr$coordsmerge %in% random.coordsmerge,]

  ### Community attributes
list.responses=list(TOP.extr,TED.extr,FDis.extr,rich.extr,shannon.extr,LCBD_taxo.extr,LCBD_fun.extr)
responses.df=do.call(cbind,list.responses)
responses.df=responses.df[, c(1,4,7,10,13,16,19,20,21)]
responses.df.elevation=merge(responses.df, elevation.extr, by=c("x", "y"))
responses.df.elevation.0.1000=responses.df.elevation[responses.df.elevation$elevation < 1000,]
responses.df.elevation.1000.2000=responses.df.elevation[responses.df.elevation$elevation %in% 1000:2000,]
responses.df.elevation.2000.4000=responses.df.elevation[responses.df.elevation$elevation %in% 2000:3500,]
### CWM traits
list.traits=list(belowgound.extr,cleptoparasite.extr,feeding_specialization.extr,phenoduration.extr,phenostart.extr,ITD.extr,solitary.extr,tong_length.extr)
cwm.traits.df=do.call(cbind,list.traits)
cwm.traits.df=cwm.traits.df[, c(1,4,7,10,13,16,19,22,23,24)]
cwm.traits.df.elevation=merge(cwm.traits.df, elevation.extr, by=c("x", "y"))
cwm.traits.df.elevation.0.1000=cwm.traits.df.elevation[cwm.traits.df.elevation$elevation < 1000,]
cwm.traits.df.elevation.1000.2000=cwm.traits.df.elevation[cwm.traits.df.elevation$elevation %in% 1000:2000,]
cwm.traits.df.elevation.2000.4000=cwm.traits.df.elevation[cwm.traits.df.elevation$elevation %in% 2000:3500,]

LU = raster("DATA/Landuse_100x100.tif")
LU.df=as.data.frame(LU)
LU.df=cbind(LU.df, coordinates(LU))
LU.df=na.omit(LU.df)

### Low elevation -----------------------------------------------------------------------
palette.lu=c("#ED5752", "#B38867", "#CDCDC0")

responses.df.elevation.0.1000.lu=merge(responses.df.elevation.0.1000,LU.df,by=c("x", "y"))
responses.df.elevation.0.1000.lu=responses.df.elevation.0.1000.lu[responses.df.elevation.0.1000.lu$Landuse_100x100 != 4, c(3:10,12)]
for(r in 1:7){
  dat.p=cbind(responses.df.elevation.0.1000.lu[,r], responses.df.elevation.0.1000.lu[,c(8:9)])
  colnames(dat.p) = c(paste(colnames(responses.df.elevation.0.1000.lu)[r]), "elevation", "Landuse_100x100")
  plot.low=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
 ggsave(filename = paste("OUTPUT/Violin_elevation/low_violin_lu_",colnames(responses.df.elevation.0.1000.lu)[r],".tiff", sep=""), plot = plot.low,device = "tiff", width = 7, height = 4, units = "in")
}

cwm.traits.df.elevation.0.1000.lu=merge(cwm.traits.df.elevation.0.1000,LU.df,by=c("x", "y"))
cwm.traits.df.elevation.0.1000.lu=cwm.traits.df.elevation.0.1000.lu[cwm.traits.df.elevation.0.1000.lu$Landuse_100x100 != 4, c(3:11,13)]
for(r in 1:8){
  dat.p=cbind(cwm.traits.df.elevation.0.1000.lu[,r], cwm.traits.df.elevation.0.1000.lu[,c(9:10)])
  colnames(dat.p) = c(paste(colnames(cwm.traits.df.elevation.0.1000.lu)[r]), "elevation", "Landuse_100x100")
  plot.low=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/low_violin_lu_",colnames(cwm.traits.df.elevation.0.1000.lu)[r],".tiff", sep=""), plot = plot.low,device = "tiff", width = 7, height = 4, units = "in")
}

n.0.1000=responses.df.elevation.0.1000.lu %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(n=n())
sum(n.0.1000$n)
277264*100/1760226
905274*100/1760226
577688*100/1760226

### Mid elevation -----------------------------------------------------------------------
responses.df.elevation.1000.2000.lu=merge(responses.df.elevation.1000.2000,LU.df,by=c("x", "y"))
responses.df.elevation.1000.2000.lu=responses.df.elevation.1000.2000.lu[responses.df.elevation.1000.2000.lu$Landuse_100x100 != 4, c(3:10,12)]
for(r in 1:7){
  dat.p=cbind(responses.df.elevation.1000.2000.lu[,r], responses.df.elevation.1000.2000.lu[,c(8:9)])
  colnames(dat.p) = c(paste(colnames(responses.df.elevation.1000.2000.lu)[r]), "elevation", "Landuse_100x100")
  plot.mid=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/mid_violin_lu_",colnames(responses.df.elevation.1000.2000.lu)[r],".tiff", sep=""), plot = plot.mid,device = "tiff", width = 7, height = 4, units = "in")
}

cwm.traits.df.elevation.1000.2000.lu=merge(cwm.traits.df.elevation.1000.2000,LU.df,by=c("x", "y"))
cwm.traits.df.elevation.1000.2000.lu=cwm.traits.df.elevation.1000.2000.lu[cwm.traits.df.elevation.1000.2000.lu$Landuse_100x100 != 4, c(3:11,13)]
for(r in 1:8){
  dat.p=cbind(cwm.traits.df.elevation.1000.2000.lu[,r], cwm.traits.df.elevation.1000.2000.lu[,c(9:10)])
  colnames(dat.p) = c(paste(colnames(cwm.traits.df.elevation.1000.2000.lu)[r]), "elevation", "Landuse_100x100")
  plot.mid=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/mid_violin_lu_",colnames(cwm.traits.df.elevation.0.1000.lu)[r],".tiff", sep=""), plot = plot.mid,device = "tiff", width = 7, height = 4, units = "in")
}

n.1000.2000=responses.df.elevation.1000.2000.lu %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(n=n())
sum(n.1000.2000$n)
29420*100/1044795
440311*100/1044795
575064*100/1044795

### High elevation -----------------------------------------------------------------------
responses.df.elevation.2000.4000=merge(responses.df.elevation.2000.4000,LU.df,by=c("x", "y"))
responses.df.elevation.2000.4000=responses.df.elevation.2000.4000[responses.df.elevation.2000.4000$Landuse_100x100 != 4, c(3:10,12)]
for(r in 1:7){
  dat.p=cbind(responses.df.elevation.2000.4000[,r], responses.df.elevation.2000.4000[,c(8:9)])
  colnames(dat.p) = c(paste(colnames(responses.df.elevation.2000.4000)[r]), "elevation", "Landuse_100x100")
  plot.high=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/high_violin_lu_",colnames(responses.df.elevation.2000.4000)[r],".tiff", sep=""), plot = plot.high,device = "tiff", width = 7, height = 4, units = "in")
}
cwm.traits.df.elevation.2000.4000.lu=merge(cwm.traits.df.elevation.2000.4000,LU.df,by=c("x", "y"))
cwm.traits.df.elevation.2000.4000.lu=cwm.traits.df.elevation.2000.4000.lu[cwm.traits.df.elevation.2000.4000.lu$Landuse_100x100 != 4, c(3:11,13)]
for(r in 1:8){
  dat.p=cbind(cwm.traits.df.elevation.2000.4000.lu[,r], cwm.traits.df.elevation.2000.4000.lu[,c(9:10)])
  colnames(dat.p) = c(paste(colnames(cwm.traits.df.elevation.2000.4000.lu)[r]), "elevation", "Landuse_100x100")
  plot.high=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/high_violin_lu_",colnames(cwm.traits.df.elevation.2000.4000.lu)[r],".tiff", sep=""), plot = plot.high,device = "tiff", width = 7, height = 4, units = "in")
}
n.2000.4000=responses.df.elevation.2000.4000 %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(n=n())
sum(n.2000.4000$n)

1222*100/246487
219560*100/246487
25705*100/246487


### -----------------------------------------------
palette.lu=c("#ED5752", "#B38867", "#CDCDC0")
d.per=data.frame(elevation=rep(c("0000-1000", "1000-2000", "2000-3500"), each=3),
                 LU=rep(c("Urban", "Agriculutlral", "Forest")), 
                 cover=c(15.75, 51.43, 32.82,2.82, 42.14, 55.04, 0.49, 89.07, 10.43))
p1=ggplot(d.per[d.per$elevation=="0000-1000",], 
       aes( x=cover, y=elevation, fill=LU)) + 
  geom_bar(stat = "identity",width = 1.2) + 
  scale_fill_manual(values = palette.lu) +
  theme_classic(base_size = 15) +
  xlab("Cover (%)") +
  ylab("") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 


p2=ggplot(d.per[d.per$elevation=="1000-2000",], 
       aes( x=cover, y=elevation, fill=LU)) + 
  geom_bar(stat = "identity",width = 1.2) + 
  scale_fill_manual(values = palette.lu) +
  theme_classic(base_size = 15) +
  xlab("Cover (%)") +
  ylab("Elevation (m)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 

p3=ggplot(d.per[d.per$elevation=="2000-3500",], 
       aes( x=cover, y=elevation, fill=LU)) + 
  geom_bar(stat = "identity",width = 1.2) + 
  scale_fill_manual(values = palette.lu) +
  theme_classic(base_size = 15) +
  xlab("Cover (%)") +
  ylab("") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) 

FI=ggarrange(p1,p2,p3, nrow = 3, ncol = 1)

require("magrittr")
require("ggpubr")
FI %>% ggexport(filename = "OUTPUT/Violin_plots/percentage.landcover.elevation.png",
                    width = 1200, height = 400)

FI %>% ggexport(filename = "OUTPUT/Violin_plots/percentage.landcover.elevation.pdf",
                    width = 6, height = 3)


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