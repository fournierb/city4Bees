#######################################
### Paper: Wild bee diversity in Switzerland
### Author: Joan Casanelles-Abella & Bertrand Fournier
### Date: 11.11.2022 
### Script: Figure 4
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
div <- stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Masked_responses/masked.stack.all.diversity.tiff")
names(div) = c("feeding_specialization", "ITD", "phenoduration","phenostart", "belowgound","cleptoparasite", 
               "solitary","tong_length", "FDis",  "TED", "TOP","LCBD_fun" ,"LCBD_taxo","Shannon", "Richness")
### Elevation, LU and LC-----------------------------------------------------------------------
### Water bodies
water_bodies = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Water_bodies/water_bodies1.tif")
masking=raster("DATA/Masked_responses/Diversity_metrics/masked.Functional richness (TOP).tiff")
### Elevation
elevation = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Elevation/dhm_25.tif")
crs(elevation) = "+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs"
###LU & LC
LU = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/LU:LC/Landuse_100x100.tif")
LU=raster::mask(x = LU, mask = water_bodies, maskvalue = 1)
LU.df=as.data.frame(LU)
LU.df=cbind(LU.df, coordinates(LU))
LU.df=na.omit(LU.df)
LU.df$Landuse_100x100[LU.df$Landuse_100x100==1] ="Urban"
LU.df$Landuse_100x100[LU.df$Landuse_100x100==2] ="Agriculture"
LU.df$Landuse_100x100[LU.df$Landuse_100x100==3] ="Forest"
LU.df$Landuse_100x100[LU.df$Landuse_100x100==4] ="Unproductive"

LC.CORINE=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/LU:LC/LC_CORINE_CH_18.tif")
LC.df=data.frame(lc=raster::extract(LC.CORINE, coordinates(masking)), x=coordinates(masking)[,1], y=coordinates(masking)[,2])
LC.df=na.omit(LC.df)
LC.df=LC.df[LC.df$lc!=0,]
LC.df$lc.n=LC.df$lc
for(c in 1:2){
  LC.df$lc.n[LC.df$lc.n==c] = "Urban"
}
for(c in 3:9){
  LC.df$lc.n[LC.df$lc.n==c] = "Not included"
}
for(c in 10:11){
  LC.df$lc.n[LC.df$lc.n==c] = "Urban green"
}
for(c in 12:23){
  LC.df$lc.n[LC.df$lc.n==c] = "Agriculture"
}
for(c in 24:25){
  LC.df$lc.n[LC.df$lc.n==c] = "Forest"
}
for(c in 26:27){
  LC.df$lc.n[LC.df$lc.n==c] = "Natural and seminatural"
}
for(c in 28:29){
  LC.df$lc.n[LC.df$lc.n==c] = "Forest"
}
for(c in 30:34){
  LC.df$lc.n[LC.df$lc.n==c] = "Not included"
}
for(c in 35:36){
  LC.df$lc.n[LC.df$lc.n==c] = "Natural and seminatural"
}
for(c in 40:41){
  LC.df$lc.n[LC.df$lc.n==c] = "Not included"
}

### ===================================
###  calculations
### ===================================
ExtractValuesFilter=function (raster) {
  df.r=data.frame(rastervalues=raster::extract(x = raster, coordinates(water_bodies)), coordinates(water_bodies))
  names(df.r)=c(paste(names(raster)), "x", "y")
  df.r.noNA=na.omit(df.r)
  return(df.r.noNA)
}
### Extract community attributes
  TOP.extr=ExtractValuesFilter(raster = div$TOP)
  TED.extr=ExtractValuesFilter(raster = div$TED)
  FDis.extr=ExtractValuesFilter(raster = div$FDis)
  rich.extr=ExtractValuesFilter(raster = div$Richness)
  shannon.extr=ExtractValuesFilter(raster = div$Shannon)
  LCBD_taxo.extr=ExtractValuesFilter(raster = div$LCBD_taxo)
  LCBD_fun.extr=ExtractValuesFilter(raster = div$LCBD_fun)
### Extract CWM traits
  belowgound.extr=ExtractValuesFilter(raster = div$belowgound)
  cleptoparasite.extr=ExtractValuesFilter(raster = div$cleptoparasite)
  feeding_specialization.extr=ExtractValuesFilter(raster = div$feeding_specialization)
  phenoduration.extr=ExtractValuesFilter(raster = div$phenoduration)
  phenostart.extr=ExtractValuesFilter(raster = div$phenostart)
  ITD.extr=ExtractValuesFilter(raster = div$ITD)
  solitary.extr=ExtractValuesFilter(raster = div$solitary)
  tong_length.extr=ExtractValuesFilter(raster = div$tong_length)
### Extract elevation
  elevation.extr=data.frame(elevation=raster::extract(elevation, coordinates(water_bodies)), coordinates(water_bodies))
  elevation.extr=na.omit(elevation.extr)
### Community attributes
list.responses=list(TOP.extr,TED.extr,FDis.extr,rich.extr,shannon.extr,LCBD_taxo.extr,LCBD_fun.extr)
responses.df0=do.call(cbind,list.responses)
responses.df=responses.df0[, c(1,4,7,10,13,16,19,20,21)]
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

### ===================================
###  Plotting
### ===================================
### Palettes
palette.lu=c("#DC267F", "#B38867", "#CDCDC0" )
plot.lc=c("#DC267F","#B38867" ,"#44AA99", "#CDCDC0", "#117733")
### Low elevation -----------------------------------------------------------------------
## Diversity metrics
responses.df.elevation.0.1000.lu0=merge(responses.df.elevation.0.1000,LU.df,by=c("x", "y"))
responses.df.elevation.0.1000.lu1=merge(responses.df.elevation.0.1000.lu0,LC.df, by=c("x", "y") )
responses.df.elevation.0.1000.lu=responses.df.elevation.0.1000.lu1[responses.df.elevation.0.1000.lu1$Landuse_100x100!="Unproductive", c(3:10,11,12,13)]
for(r in 1:7){
  dat.p=cbind(responses.df.elevation.0.1000.lu[,r], responses.df.elevation.0.1000.lu[,c(9:11)])
  colnames(dat.p) = c(paste(colnames(responses.df.elevation.0.1000.lu)[r]), "Landuse_100x100", "lc", "lc.n")
  plot.lu.low=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,  alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2)), n.breaks = 3) +   
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
 ggsave(filename = paste("OUTPUT/Violin_elevation/low_violin_lu_",colnames(responses.df.elevation.0.1000.lu)[r],".tiff", 
                         sep=""), plot = plot.lu.low,device = "tiff", width = 6.5, height = 5.5, units = "in")

 plot.lc.low=ggplot(dat.p[dat.p$lc.n!="Not included",], aes(x=as.factor(lc.n), y=get(paste(colnames(dat.p)[1])), fill=as.factor(lc.n))) + 
   geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
   geom_boxplot(width = .1,  alpha=0.7, notch=T) + 
   theme_classic(base_size = 35) +
   scale_fill_manual(values =  plot.lc) + 
   scale_color_manual(values =  plot.lc) + 
   xlab("")+
   labs(fill="Land-use", color="") +
   scale_y_continuous(name = "",   labels=scaleFUN,
                      limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                 round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2)), n.breaks = 3) +
   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
 
 ggsave(filename = paste("OUTPUT/Violin_elevation/low_violin_lc_",colnames(responses.df.elevation.0.1000.lu)[r],".tiff", sep=""), 
        plot = plot.lc.low,device = "tiff", width = 6.5, height = 5.5, units = "in")
 
 }
## CMW
cwm.traits.df.elevation.0.1000.lu0=merge(cwm.traits.df.elevation.0.1000,LU.df,by=c("x", "y"))
cwm.traits.df.elevation.0.1000.lu1=merge(cwm.traits.df.elevation.0.1000.lu0,LC.df, by=c("x", "y") )
cwm.traits.df.elevation.0.1000.lu=cwm.traits.df.elevation.0.1000.lu1[cwm.traits.df.elevation.0.1000.lu1$Landuse_100x100!="Unproductive", c(3:10,11,12,13,14)]
for(r in 1:8){
  dat.p=cbind(cwm.traits.df.elevation.0.1000.lu[,r], cwm.traits.df.elevation.0.1000.lu[,c(10:12)])
  colnames(dat.p) = c(paste(colnames(cwm.traits.df.elevation.0.1000.lu)[r]), "Landuse_100x100", "lc", "lc.n")
  plot.lu.low=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,   alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/low_violin_lu_",colnames(cwm.traits.df.elevation.0.1000.lu)[r],".tiff", sep=""), 
         plot = plot.lu.low,device = "tiff", width = 7, height = 4, units = "in")

  plot.lc.low=ggplot(dat.p[dat.p$lc.n!="Not included",], aes(x=as.factor(lc.n), y=get(paste(colnames(dat.p)[1])), fill=as.factor(lc.n))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,  alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  plot.lc) + 
    scale_color_manual(values =  plot.lc) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/low_violin_lc_",colnames(cwm.traits.df.elevation.0.1000.lu)[r],".tiff", sep=""), 
         plot = plot.lc.low,device = "tiff", width = 6.5, height = 5.5, units = "in")
  
  }

### Mid elevation -----------------------------------------------------------------------
## Diversity metrics
responses.df.elevation.1000.2000.lu0=merge(responses.df.elevation.1000.2000,LU.df,by=c("x", "y"))
responses.df.elevation.1000.2000.lu1=merge(responses.df.elevation.1000.2000.lu0,LC.df, by=c("x", "y") )
responses.df.elevation.1000.2000.lu=responses.df.elevation.1000.2000.lu1[responses.df.elevation.1000.2000.lu1$Landuse_100x100!="Unproductive", c(3:10,11,12,13)]
for(r in 1:7){
  dat.p=cbind(responses.df.elevation.1000.2000.lu[,r], responses.df.elevation.1000.2000.lu[,c(9:11)])
  colnames(dat.p) = c(paste(colnames(responses.df.elevation.1000.2000.lu)[r]),  "Landuse_100x100", "lc", "lc.n")
  plot.lu.mid=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,   alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/mid_violin_lu_",colnames(responses.df.elevation.1000.2000.lu)[r],".tiff", sep=""),
         plot = plot.lu.mid,device = "tiff", width = 7, height = 4, units = "in")

  plot.lc.mid=ggplot(dat.p[dat.p$lc.n!="Not included",], aes(x=as.factor(lc.n), y=get(paste(colnames(dat.p)[1])), fill=as.factor(lc.n))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,  alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  plot.lc) + 
    scale_color_manual(values =  plot.lc) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/mid_violin_lc_",colnames(cwm.traits.df.elevation.0.1000.lu)[r],".tiff", sep=""), 
         plot =plot.lc.mid,
         device = "tiff", width = 6.5, height = 5.5, units = "in")
  
  }
## CWM
cwm.traits.df.elevation.1000.2000.lu0=merge(cwm.traits.df.elevation.1000.2000,LU.df,by=c("x", "y"))
cwm.traits.df.elevation.1000.2000.lu1=merge(cwm.traits.df.elevation.1000.2000.lu0,LC.df, by=c("x", "y") )
cwm.traits.df.elevation.1000.2000.lu=cwm.traits.df.elevation.1000.2000.lu1[cwm.traits.df.elevation.1000.2000.lu1$Landuse_100x100!="Unproductive", c(3:10,11,12,13,14)]
for(r in 1:8){
  dat.p=cbind(cwm.traits.df.elevation.1000.2000.lu[,r], cwm.traits.df.elevation.1000.2000.lu[,c(10:12)])
  colnames(dat.p) = c(paste(colnames(cwm.traits.df.elevation.1000.2000.lu)[r]),"Landuse_100x100", "lc", "lc.n")
  plot.lu.mid=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,   alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  ggsave(filename = paste("OUTPUT/Violin_elevation/mid_violin_lu_",colnames(cwm.traits.df.elevation.1000.2000.lu)[r],".tiff", 
                          sep=""), 
         plot = plot.lu.mid,device = "tiff", width = 7, height = 4, units = "in")

  plot.lc.mid=ggplot(dat.p[dat.p$lc.n!="Not included",], aes(x=as.factor(lc.n), y=get(paste(colnames(dat.p)[1])), fill=as.factor(lc.n))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,  alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  plot.lc) + 
    scale_color_manual(values =  plot.lc) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  ggsave(filename = paste("OUTPUT/Violin_elevation/mid_violin_lc_",colnames(cwm.traits.df.elevation.1000.2000.lu)[r],".tiff", sep=""), 
         plot = plot.lc.mid,device = "tiff", width = 6.5, height = 5.5, units = "in")
  
  }
### High elevation -----------------------------------------------------------------------
## Diversity metrics
responses.df.elevation.2000.4000.lu0=merge(responses.df.elevation.2000.4000,LU.df,by=c("x", "y"))
responses.df.elevation.2000.4000.lu1=merge(responses.df.elevation.2000.4000.lu0,LC.df, by=c("x", "y") )
responses.df.elevation.2000.4000.lu=responses.df.elevation.2000.4000.lu1[responses.df.elevation.2000.4000.lu1$Landuse_100x100!="Unproductive", c(3:10,11,12,13)]
for(r in 1:7){
  dat.p=cbind(responses.df.elevation.2000.4000.lu[,r], responses.df.elevation.2000.4000.lu[,c(9:11)])
  colnames(dat.p) = c(paste(colnames(responses.df.elevation.2000.4000.lu)[r]), "Landuse_100x100", "lc", "lc.n")
  plot.lu.high=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,  alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  
  ggsave(filename = paste("OUTPUT/Violin_elevation/high_violin_lu_",colnames(responses.df.elevation.2000.4000.lu)[r],".tiff", sep=""), 
         plot = plot.lu.high,device = "tiff", width = 7, height = 4, units = "in")
  plot.lc.high=ggplot(dat.p[dat.p$lc.n!="Not included",], aes(x=as.factor(lc.n), y=get(paste(colnames(dat.p)[1])), fill=as.factor(lc.n))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,  alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  plot.lc) + 
    scale_color_manual(values =  plot.lc) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  ggsave(filename = paste("OUTPUT/Violin_elevation/high_violin_lc_",colnames(responses.df.elevation.2000.4000.lu)[r],".tiff", sep=""), 
         plot = plot.lc.high,device = "tiff", width = 6.5, height = 5.5, units = "in")
  }


## CWM
cwm.traits.df.elevation.2000.4000.lu0=merge(cwm.traits.df.elevation.2000.4000,LU.df,by=c("x", "y"))
cwm.traits.df.elevation.2000.4000.lu1=merge(cwm.traits.df.elevation.2000.4000.lu0,LC.df, by=c("x", "y") )
cwm.traits.df.elevation.2000.4000.lu=cwm.traits.df.elevation.2000.4000.lu1[cwm.traits.df.elevation.2000.4000.lu1$Landuse_100x100!="Unproductive", c(3:10,11,12,13,14)]
for(r in 1:8){
  dat.p=cbind(cwm.traits.df.elevation.2000.4000.lu[,r], cwm.traits.df.elevation.2000.4000.lu[,c(10:12)])
  colnames(dat.p) = c(paste(colnames(cwm.traits.df.elevation.2000.4000.lu)[r]), "Landuse_100x100", "lc", "lc.n")
  plot.lu.high=ggplot(dat.p, aes(x=as.factor(Landuse_100x100), y=get(paste(colnames(dat.p)[1])), fill=as.factor(Landuse_100x100))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1, alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  palette.lu) + 
    scale_color_manual(values =  palette.lu) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  ggsave(filename = paste("OUTPUT/Violin_elevation/high_violin_lu_",colnames(cwm.traits.df.elevation.2000.4000.lu)[r],".tiff", sep=""), 
         plot = plot.lu.high,device = "tiff", width = 7, height = 4, units = "in")
  
  plot.lc.high=ggplot(dat.p[dat.p$lc.n!="Not included",], aes(x=as.factor(lc.n), y=get(paste(colnames(dat.p)[1])), fill=as.factor(lc.n))) + 
    geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
    geom_boxplot(width = .1,  alpha=0.7, notch=T) + 
    theme_classic(base_size = 35) +
    scale_fill_manual(values =  plot.lc) + 
    scale_color_manual(values =  plot.lc) + 
    xlab("")+
    labs(fill="Land-use", color="") +
    scale_y_continuous(name = "",   labels=scaleFUN,
                       limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
                                  round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2)))+
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
  ggsave(filename = paste("OUTPUT/Violin_elevation/high_violin_lc_",colnames(cwm.traits.df.elevation.2000.4000.lu)[r],".tiff", sep=""), 
         plot = plot.lc.high,device = "tiff", width = 7, height = 4, units = "in")
  }

#scale_y_continuous(name = "",   labels=scaleFUN,
#                   limits = c(round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[2], digits = 2), 
#                              round(quantile(dat.p[,1], probs = seq(0,1, by=0.1))[10], digits = 2))) +

### -----------------------------------------------

n.0.1000=responses.df.elevation.0.1000.lu %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(n=n())
n.0.1000$props=n.0.1000$n*100/sum(n.0.1000$n)

n.1000.2000=responses.df.elevation.1000.2000.lu %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(n=n())
n.1000.2000$props=n.1000.2000$n*100/sum(n.1000.2000$n)

n.2000.4000=responses.df.elevation.2000.4000.lu %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(n=n())
n.2000.4000$props=n.2000.4000$n*100/sum(n.2000.4000$n)


d.per=data.frame(elevation=rep(c("0000-1000", "1000-2000", "2000-3500"), each=4),
                 LU=rep(c("Urban", "Agriculutlral", "Forest", "Unproductive")), 
                 cover=c(15.29, 49.94, 31.85,2.91, 
                         42.14, 55.04, 0.49, 89.07, 
                         23.2,2.72,73.9,0.129))
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