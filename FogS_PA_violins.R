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
water.bodies=raster("DATA/water_bodies1.tif")
div <- stack(x = "DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD", "LCBD_fun", "LCBD_taxo", "phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")

for(i in 1:nlayers(div)){  div[[i]]=mask(x = div[[i]], mask = water.bodies,maskvalue = 1)}

TOP <- div$TOP
TED <- div$TED
FDis <- div$FDis
rich <- div$Richness
shannon <- div$Shannon
LCBD.taxo <- div$LCBD_taxo
LCBD.fun <- div$LCBD_fun

##
ras = stack(TOP,TED,FDis,rich,shannon,LCBD.taxo,LCBD.fun)
dat=as.data.frame(ras)
dat=cbind(dat, as.data.frame(coordinates(ras[[1]])))
dat=na.omit(dat)
dat$LCBD_taxo = dat$LCBD_taxo*1000
dat$LCBD_fun = dat$LCBD_fun*1000
dat$coordsmerged=paste(dat$x, dat$y, sep="")
## Protected areas
protected_all = read.csv("DATA/protected_areas.csv")
dat=merge(x = dat,y = protected_all,by= "coordsmerged" )
dat2=dat[, c(1:8, 14:16)]
dat2[is.na(dat2)] = 0
dat3=reshape2::melt(dat2, id.vars= colnames(dat2)[2:8], measure.vars=colnames(dat2)[9:11], variable.name="Type_biotope")
### Violin plots
palette.lu=c("#598234", "#AEBD38", "#68829E")


TOP.violin <- ggplot(dat3[dat3$value !=0,], aes(x=(Type_biotope), y=TOP, fill=Type_biotope)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  xlab("Type biotope")+
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  labs(fill="Type_biotope", color="") +
  scale_y_continuous(name = "TOP",  limits = c(round(quantile(dat3$TOP, probs = seq(0,1, by=0.1))[2]), round(quantile(dat3$TOP, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
TOP.violin

TED.violin <- ggplot(dat3[dat3$value !=0,], aes(x=(Type_biotope), y=TED, fill=Type_biotope)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  xlab("Type biotope")+
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  labs(fill="Type_biotope", color="") +
  scale_y_continuous(name = "TED", limits = c(round(quantile(dat$TED, probs = seq(0,1, by=0.1))[2], digits = 2), round(quantile(dat$TED, probs = seq(0,1, by=0.1))[10], digits = 2)),
                     breaks = c(0.92, 0.93, 0.94), labels = c(0.92, 0.93, 0.94))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
TED.violin

FDis.violin <- ggplot(dat3[dat3$value !=0,], aes(x=(Type_biotope), y=FDis, fill=Type_biotope)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  xlab("Type biotope")+
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  labs(fill="Type_biotope", color="") +
  scale_y_continuous(name = "FDis",  limits = c(round(quantile(dat3$FDis, probs = seq(0,1, by=0.1))[2]), round(quantile(dat3$FDis, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
FDis.violin

Shannon.violin <- ggplot(dat3[dat3$value !=0,], aes(x=(Type_biotope), y=Shannon, fill=Type_biotope)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  xlab("Type biotope")+
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  labs(fill="Type_biotope", color="") +
  scale_y_continuous(name = "Shannon",  limits = c(round(quantile(dat3$Shannon, probs = seq(0,1, by=0.1))[2]), round(quantile(dat3$Shannon, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
Shannon.violin

Richness.violin <- ggplot(dat3[dat3$value !=0,], aes(x=(Type_biotope), y=Richness, fill=Type_biotope)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  xlab("Type biotope")+
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  labs(fill="Type_biotope", color="") +
  scale_y_continuous(name = "Richness",  limits = c(round(quantile(dat3$Richness, probs = seq(0,1, by=0.1))[2]), round(quantile(dat3$Richness, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")
Richness.violin

LCBD_taxo.violin <-ggplot(dat3[dat3$value !=0,], aes(x=(Type_biotope), y=LCBD_taxo, fill=Type_biotope)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  xlab("Type biotope")+
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  labs(fill="Type_biotope", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none") +
  scale_y_continuous(name = expression(paste("LCBD taxonomic x" , 10^{-3})),  
                     limits = c(round(quantile(dat$LCBD_taxo, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(dat$LCBD_taxo, probs = seq(0,1, by=0.1))[10], digits = 3)))

LCBD_taxo.violin

LCBD_fun.violin <- ggplot(dat3[dat3$value !=0,], aes(x=(Type_biotope), y=LCBD_fun, fill=Type_biotope)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  xlab("Type biotope")+
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  labs(fill="Type_biotope", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none") +
  scale_y_continuous(name = expression(paste("LCBD functional x" , 10^{-3})),  
                     limits = c(round(quantile(dat$LCBD_fun, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(dat$LCBD_fun, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  labs(fill="Land-use", color="") 
LCBD_fun.violin

require(egg)
figure <- ggpubr::ggarrange(Richness.violin, Shannon.violin, 
                    TOP.violin,FDis.violin, TED.violin, 
                    LCBD_taxo.violin, LCBD_fun.violin,
                    labels = paste("(",letters[1:7],")", sep=""),
                    font.label = list(size=16),
                    nrow = 4, ncol=2, 
                    widths = c(1,1),
                    heights = c(1,1,1,1))
figure

figure %>% ggexport(filename = "OUTPUT/Violin_plots/Fig_Violin_PA.png",
                    width = 1200, height = 1200)

figure %>% ggexport(filename = "OUTPUT/Violin_plots/Fig_Violin_PA.pdf",
                    width = 17, height = 17)


