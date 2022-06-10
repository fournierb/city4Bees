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
require("magrittr")
require("ggpubr")
library(egg)
source("~/Dropbox/City4bees/Analyses/bees_switzerland/city4Bees/R_rainclouds.R")
### ===================================
###  Data
### ===================================
### load the data -----------------------------------------------------------------------
## water bodies
water.bodies=raster("DATA/water_bodies1.tif")
## LU
LU = raster("DATA/Landuse_100x100.tif")
## divwrsity
div <- stack(x = "DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD", "LCBD_fun", "LCBD_taxo", "phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
for(i in 1:nlayers(div)){  div[[i]]=mask(x = div[[i]], mask = water.bodies,maskvalue = 1)}
### Raster individual -----------------------------------------------------------------------
## Functional metrics
TOP <- div$TOP
TED <- div$TED
FDis <- div$FDis
rich <- div$Richness
shannon <- div$Shannon
LCBD.taxo <- div$LCBD_taxo
LCBD.fun <- div$LCBD_fun
## CWM traits
belowgound <- div$belowgound
cleptoparasite <- div$cleptoparasite
feeding_specialization <- div$feeding_specialization
phenoduration <- div$phenoduration
phenostart=div$phenostart
ITD <- div$ITD
solitary <- div$solitary
tong_length<- div$tong_length
### Extract raster data -----------------------------------------------------------------------
## diversity index
ras = stack(LU,TOP,TED,FDis,rich,shannon,LCBD.taxo,LCBD.fun)
dat=as.data.frame(ras)
dat=cbind(dat, as.data.frame(coordinates(ras[[1]])))
dat = dat[dat$Landuse_100x100 != 4,]
dat=na.omit(dat)
dat$Landuse_100x100[dat$Landuse_100x100==1] = "Urban"
dat$Landuse_100x100[dat$Landuse_100x100==2] = "Agricultural"
dat$Landuse_100x100[dat$Landuse_100x100==3] = "Forest"
dat$Landuse_100x100 = as.factor(dat$Landuse_100x100)
dat$LCBD_taxo = dat$LCBD_taxo*1000
dat$LCBD_fun = dat$LCBD_fun*1000
dat$coordsmerge=paste(dat$x, dat$y)
## CWM
ras2 = stack(LU,belowgound,cleptoparasite,feeding_specialization,phenoduration,phenostart,ITD,solitary,tong_length)
dat2=as.data.frame(ras2)
dat2=cbind(dat2, as.data.frame(coordinates(ras2[[1]])))
dat2 = dat2[dat2$Landuse_100x100 != 4,]
dat2=na.omit(dat2)
dat2$Landuse_100x100[dat2$Landuse_100x100==1] = "Urban"
dat2$Landuse_100x100[dat2$Landuse_100x100==2] = "Agricultural"
dat2$Landuse_100x100[dat2$Landuse_100x100==3] = "Forest"
dat2$Landuse_100x100 = as.factor(dat2$Landuse_100x100)
dat2$coordsmerge=paste(dat2$x, dat2$y)
### Extract PA data -----------------------------------------------------------------------
## Protected areas sensu stricto
PAs = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PA.sensu.stricto.tiff")
pavals=raster::extract(PAs, coordinates(PAs))
PAs_df=data.frame(PA=pavals, coordinates(PAs))
PAs_df=na.omit(PAs_df)
PAs_df$coordmerged=paste(PAs_df$x,PAs_df$y)
PAs_df=cbind(PAs_df, raster::extract(LU, PAs_df[,2:3]), 
             raster::extract(TOP,PAs_df[,2:3]),
             raster::extract(TED,PAs_df[,2:3]),
             raster::extract(FDis,PAs_df[,2:3]),
             raster::extract(rich,PAs_df[,2:3]),
             raster::extract(shannon,PAs_df[,2:3]),
             raster::extract(LCBD.taxo,PAs_df[,2:3]),
             raster::extract(LCBD.fun,PAs_df[,2:3]),
             raster::extract(belowgound,PAs_df[,2:3]),
             raster::extract(cleptoparasite,PAs_df[,2:3]),
             raster::extract(feeding_specialization,PAs_df[,2:3]),
             raster::extract(phenoduration,PAs_df[,2:3]),
             raster::extract(phenostart,PAs_df[,2:3]),
             raster::extract(ITD,PAs_df[,2:3]),
             raster::extract(solitary,PAs_df[,2:3]),
             raster::extract(tong_length,PAs_df[,2:3]))

colnames(PAs_df) = c("PA", "x", "y", "coordsmerged","Landuse_100x100", "TOP", "TED", "FDis", "rich", "shannon", "LCBD.taxo","LCBD.fun",
                     "belowgound","cleptoparasite","feeding_specialization","phenoduration","phenostart","ITD","solitary","tong_length"  )
PAs_df$LCBD.taxo = PAs_df$LCBD.taxo*1000
PAs_df$LCBD.fun = PAs_df$LCBD.fun*1000
PAS_median_values=PAs_df %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(median_top=median(TOP),
                                                    median_ted=median(TED),
                                                    median_FDis=median(FDis),
                                                    median_rich=median(rich),
                                                    median_shannon=median(shannon),
                                                    median_LCBD.taxo=median(LCBD.taxo),
                                                    median_LCBD.fun=median(LCBD.fun),
                                                    
                                                    median_belowgound=median(belowgound),
                                                    median_cleptoparasite=median(cleptoparasite),
                                                    median_feeding_specialization=median(feeding_specialization),
                                                    median_phenoduration=median(phenoduration),
                                                    median_phenostart=median(phenostart),
                                                    median_ITD=median(ITD),
                                                    median_solitary=median(solitary),
                                                    median_tong_length=median(tong_length))

PAS_median_values$Landuse_100x100[PAS_median_values$Landuse_100x100==1] = "Urban"
PAS_median_values$Landuse_100x100[PAS_median_values$Landuse_100x100==2] = "Agricultural"
PAS_median_values$Landuse_100x100[PAS_median_values$Landuse_100x100==3] = "Forest"

## Protected areas sensu lato

PAs2 = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PA.sensu.lato.tiff")
pavals2=raster::extract(PAs2, coordinates(PAs2))
PAs2_df=data.frame(PA=pavals2, coordinates(PAs2))
PAs2_df=na.omit(PAs2_df)
PAs2_df$coordmerged=paste(PAs2_df$x,PAs2_df$y)
PAs2_df=cbind(PAs2_df, raster::extract(LU, PAs2_df[,2:3]), 
             raster::extract(TOP,PAs2_df[,2:3]),
             raster::extract(TED,PAs2_df[,2:3]),
             raster::extract(FDis,PAs2_df[,2:3]),
             raster::extract(rich,PAs2_df[,2:3]),
             raster::extract(shannon,PAs2_df[,2:3]),
             raster::extract(LCBD.taxo,PAs2_df[,2:3]),
             raster::extract(LCBD.fun,PAs2_df[,2:3]),
             
             raster::extract(belowgound,PAs2_df[,2:3]),
             raster::extract(cleptoparasite,PAs2_df[,2:3]),
             raster::extract(feeding_specialization,PAs2_df[,2:3]),
             raster::extract(phenoduration,PAs2_df[,2:3]),
             raster::extract(phenostart,PAs2_df[,2:3]),
             raster::extract(ITD,PAs2_df[,2:3]),
             raster::extract(solitary,PAs2_df[,2:3]),
             raster::extract(tong_length,PAs2_df[,2:3]))

colnames(PAs2_df) =c("PA", "x", "y", "coordsmerged","Landuse_100x100", "TOP", "TED", "FDis", "rich", "shannon", "LCBD.taxo","LCBD.fun",
                     "belowgound","cleptoparasite","feeding_specialization","phenoduration","phenostart","ITD","solitary","tong_length"  )
PAs2_df$LCBD.taxo = PAs2_df$LCBD.taxo*1000
PAs2_df$LCBD.fun = PAs2_df$LCBD.fun*1000
PAS2_median_values=PAs2_df %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(median_top=median(TOP),
                                                                                   median_ted=median(TED),
                                                                                   median_FDis=median(FDis),
                                                                                   median_rich=median(rich),
                                                                                   median_shannon=median(shannon),
                                                                                   median_LCBD.taxo=median(LCBD.taxo),
                                                                                   median_LCBD.fun=median(LCBD.fun),
                                                                                   
                                                                                   median_belowgound=median(belowgound),
                                                                                   median_cleptoparasite=median(cleptoparasite),
                                                                                   median_feeding_specialization=median(feeding_specialization),
                                                                                   median_phenoduration=median(phenoduration),
                                                                                   median_phenostart=median(phenostart),
                                                                                   median_ITD=median(ITD),
                                                                                   median_solitary=median(solitary),
                                                                                   median_tong_length=median(tong_length))

PAS2_median_values$Landuse_100x100[PAS2_median_values$Landuse_100x100==1] = "Urban"
PAS2_median_values$Landuse_100x100[PAS2_median_values$Landuse_100x100==2] = "Agricultural"
PAS2_median_values$Landuse_100x100[PAS2_median_values$Landuse_100x100==3] = "Forest"

### Violin plots
palette.lu=c("#ED5752", "#B38867", "#CDCDC0")
TOP.violin <- ggplot(dat, aes(x=Landuse_100x100, y=TOP, fill=Landuse_100x100)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
#  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "TOP",  limits = c(round(quantile(dat$TOP, probs = seq(0,1, by=0.1))[2]), round(quantile(dat$TOP, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
TOP.violin

TED.violin <- ggplot(dat, aes(x=Landuse_100x100, y=TED, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_ted,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_ted,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "TED", limits = c(round(quantile(dat$TED, probs = seq(0,1, by=0.1))[2], digits = 2), round(quantile(dat$TED, probs = seq(0,1, by=0.1))[10], digits = 2)),
                     breaks = c(0.92, 0.93, 0.94), labels = c(0.92, 0.93, 0.94)) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
TED.violin

FDis.violin <- ggplot(dat, aes(x=Landuse_100x100, y=FDis, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_FDis,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_FDis,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "FDIs",  limits = c(round(quantile(dat$FDis, probs = seq(0,1, by=0.1))[2]), round(quantile(dat$FDis, probs = seq(0,1, by=0.1))[10]))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
FDis.violin

Shannon.violin <- ggplot(dat, aes(x=Landuse_100x100, y=Shannon, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_shannon,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_shannon,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Shannon",  limits = c(round(quantile(dat$Shannon, probs = seq(0,1, by=0.1))[2]), round(quantile(dat$Shannon, probs = seq(0,1, by=0.1))[10]))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
Shannon.violin

Richness.violin <- ggplot(dat, aes(x=Landuse_100x100, y=Richness, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_rich,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_rich,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Richness",  limits = c(round(quantile(dat$Richness, probs = seq(0,1, by=0.1))[2]), round(quantile(dat$Richness, probs = seq(0,1, by=0.1))[10]))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
Richness.violin

LCBD_taxo.violin <- ggplot(dat, aes(x=Landuse_100x100, y=LCBD_taxo, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_LCBD.taxo,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_LCBD.taxo,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = expression(paste("LCBD taxonomic x" , 10^{-3})),  limits = c(round(quantile(dat$LCBD_taxo, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(dat$LCBD_taxo, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
LCBD_taxo.violin

LCBD_fun.violin <- ggplot(dat, aes(x=Landuse_100x100, y=LCBD_fun, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_LCBD.fun,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_LCBD.fun,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = expression(paste("LCBD functional x" , 10^{-3})),  limits = c(round(quantile(dat$LCBD_fun, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(dat$LCBD_fun, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
LCBD_fun.violin

figure_diversity_metrics <- ggarrange(Richness.violin, Shannon.violin, 
                    TOP.violin,FDis.violin, TED.violin, 
                    LCBD_taxo.violin, LCBD_fun.violin,
                    labels = paste("(",letters[1:7],")", sep=""),
                    font.label = list(size=16),
                    nrow = 4, ncol=2, 
                    widths = c(1,1),
                    heights = c(1,1,1,1))


figure_diversity_metrics %>% ggpubr::ggexport(filename = "OUTPUT/Violin_plots/Fig_Violin_discrete.png",
                    width = 1200, height = 1200)

figure_diversity_metrics %>% ggpubr::ggexport(filename = "OUTPUT/Violin_plots/Fig_Violin_discrete.pdf",
                    width = 17, height = 17)


### Violin plots
palette.lu=c("#ED5752", "#B38867", "#CDCDC0")

belowgound.violin <- ggplot(dat2, aes(x=Landuse_100x100, y=belowgound, fill=Landuse_100x100)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_belowgound,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_belowgound,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Belowground",  limits = c(round(quantile(dat2$belowgound, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(dat2$belowgound, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")

cleptoparasite.violin <- ggplot(dat2, aes(x=Landuse_100x100, y=cleptoparasite, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_cleptoparasite,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_cleptoparasite,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Cleptoparasite",  limits = c(round(quantile(dat2$cleptoparasite, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(dat2$cleptoparasite, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")

feeding_specialization.violin <- ggplot(dat2, aes(x=Landuse_100x100, y=feeding_specialization, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_feeding_specialization,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_feeding_specialization,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  ylab("Feeding specialization") +
  scale_y_continuous(name = "Feeding specialization",  limits = c(round(quantile(dat2$feeding_specialization, probs = seq(0,1, by=0.1))[2], digits = 3), 
                                                              round(quantile(dat2$feeding_specialization, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")

phenoduration.violin <- ggplot(dat2, aes(x=Landuse_100x100, y=phenoduration, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_phenoduration,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_phenoduration,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Phenology duration",  limits = c(round(quantile(dat2$phenoduration, probs = seq(0,1, by=0.1))[2], digits = 3), 
                                                              round(quantile(dat2$phenoduration, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  
    labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")

phenostart.violin <- ggplot(dat2, aes(x=Landuse_100x100, y=phenostart, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_phenostart,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_phenostart,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Phenology start",  limits = c(round(quantile(dat2$phenostart, probs = seq(0,1, by=0.1))[2], digits = 3), 
                                                              round(quantile(dat2$phenostart, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")

ITD.violin <- ggplot(dat2, aes(x=Landuse_100x100, y=ITD, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_ITD,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_ITD,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "ITD",  limits = c(round(quantile(dat2$ITD, probs = seq(0,1, by=0.1))[2], digits = 3), 
                                                           round(quantile(dat2$ITD, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")


solitary.violin <- ggplot(dat2, aes(x=Landuse_100x100, y=solitary, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_solitary,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_solitary,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Solitary",  limits = c(round(quantile(dat2$solitary, probs = seq(0,1, by=0.1))[2], digits = 3), 
                                               round(quantile(dat2$solitary, probs = seq(0,1, by=0.1))[10], digits = 3))) +

  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")

tong_length.violin <- ggplot(dat2, aes(x=Landuse_100x100, y=tong_length, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_tong_length,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  geom_point(data = PAS2_median_values[1:3,], aes(y=median_tong_length,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.3), size=5, shape=23) +
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Tongue length",  limits = c(round(quantile(dat2$tong_length, probs = seq(0,1, by=0.1))[2], digits = 3), 
                                                    round(quantile(dat2$tong_length, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")

figure_cwm <- ggarrange(belowgound.violin,cleptoparasite.violin , 
                                      feeding_specialization.violin,phenoduration.violin, 
                                      phenostart.violin, ITD.violin,
                                      solitary.violin, tong_length.violin,
                                      labels = paste("(",letters[1:8],")", sep=""),
                                      font.label = list(size=16),
                                      nrow = 4, ncol=2, 
                                      widths = c(1,1),
                                      heights = c(1,1,1,1))


figure_cwm %>% ggpubr::ggexport(filename = "OUTPUT/Violin_plots/Fig_Violin_CWM_discrete.png",
                                              width = 1200, height = 1200)

figure_cwm %>% ggpubr::ggexport(filename = "OUTPUT/Violin_plots/Fig_Violin_CWM_discrete.pdf",
                                              width = 17, height = 17)
