# Remove all R objects in the workspace
rm(list = ls())

library(dplyr)
require(raster)
require(viridis)
require(ggplot2)
library(sf)
source("~/Dropbox/City4bees/Analyses/bees_switzerland/city4Bees/R_rainclouds.R")

### load the data -----------------------------------------------------------------------
setwd("~/Dropbox/City4bees/Analyses/bees_switzerland/")
#source("Load environmental data.R")

##

water.bodies=raster("DATA/water_bodies1.tif")

#setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-22/Selected descriptors")
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

## LU
setwd("C:/Users/Bertrand/Dropbox/Projects/City_cooccurrences/Landuse")
LU = raster("DATA/Landuse_100x100.tif")

##
ras = stack(LU,TOP,TED,FDis,rich,shannon,LCBD.taxo,LCBD.fun)
dat=as.data.frame(ras)
dat = dat[dat$Landuse_100x100 != 4,]
dat=na.omit(dat)
dat$Landuse_100x100[dat$Landuse_100x100==1] = "Urban"
dat$Landuse_100x100[dat$Landuse_100x100==2] = "Agricultural"
dat$Landuse_100x100[dat$Landuse_100x100==3] = "Forest"
dat$Landuse_100x100 = as.factor(dat$Landuse_100x100)
dat$LCBD_taxo = dat$LCBD_taxo*1000
dat$LCBD_fun = dat$LCBD_fun*1000

## Protected areas
PAs = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PAs.tif")
pavals=raster::extract(PAs, coordinates(PAs))
PAs_df=data.frame(PA=pavals, coordinates(PAs))
PAs_df=na.omit(PAs_df)
PAs_df=cbind(PAs_df, raster::extract(LU, PAs_df[,2:3]), 
             raster::extract(TOP,PAs_df[,2:3]),
             raster::extract(TED,PAs_df[,2:3]),
             raster::extract(FDis,PAs_df[,2:3]),
             raster::extract(rich,PAs_df[,2:3]),
             raster::extract(shannon,PAs_df[,2:3]),
             raster::extract(LCBD.taxo,PAs_df[,2:3]),
             raster::extract(LCBD.fun,PAs_df[,2:3]))

colnames(PAs_df) = c("PA", "x", "y", "Landuse_100x100", "TOP", "TED", "FDis", "rich", "shannon", "LCBD.taxo","LCBD.fun" )
PAs_df$LCBD.taxo = PAs_df$LCBD.taxo*1000
PAs_df$LCBD.fun = PAs_df$LCBD.fun*1000
PAS_median_values=PAs_df %>% dplyr::group_by(Landuse_100x100) %>% dplyr::summarise(median_top=median(TOP),
                                                    median_ted=median(TED),
                                                    median_FDis=median(FDis),
                                                    median_rich=median(rich),
                                                    median_shannon=median(shannon),
                                                    median_LCBD.taxo=median(LCBD.taxo),
                                                    median_LCBD.fun=median(LCBD.fun))

PAS_median_values$Landuse_100x100[PAS_median_values$Landuse_100x100==1] = "Urban"
PAS_median_values$Landuse_100x100[PAS_median_values$Landuse_100x100==2] = "Agricultural"
PAS_median_values$Landuse_100x100[PAS_median_values$Landuse_100x100==3] = "Forest"

### Violin plots
palette.lu=c("#ED5752", "#B38867", "#6fb98f")

TOP.violin <- ggplot(dat, aes(x=Landuse_100x100, y=TOP, fill=Landuse_100x100)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2)) +
#  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "TOP", breaks = c(0,30,60), labels =c(0,30,60), limits = c(0, 60)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
TOP.violin

TED.violin <- ggplot(dat, aes(x=Landuse_100x100, y=TED, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_ted,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2)) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "TED", breaks = c(0.8,0.9,1), labels =c(0.8,0.9,1), limits = c(0.8,1)) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
TED.violin

FDis.violin <- ggplot(dat, aes(x=Landuse_100x100, y=FDis, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_FDis,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2)) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
FDis.violin

Shannon.violin <- ggplot(dat, aes(x=Landuse_100x100, y=Shannon, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_shannon,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2)) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  #scale_y_continuous(name = "TED", breaks = c(0.8,0.9,1), labels =c(0.8,0.9,1), limits = c(0.8,1)) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
Shannon.violin

Richness.violin <- ggplot(dat, aes(x=Landuse_100x100, y=Richness, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_rich,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2)) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "Richness", breaks = c(0, 20, 40 ), labels = c(0, 20, 40 ), limits = c(0,40)) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
Richness.violin

LCBD_taxo.violin <- ggplot(dat, aes(x=Landuse_100x100, y=LCBD_taxo, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_LCBD.taxo,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2)) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
 # scale_y_continuous(name = "TED", breaks = c(0.8,0.9,1), labels =c(0.8,0.9,1), limits = c(0.8,1)) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
LCBD_taxo.violin

LCBD_fun.violin <- ggplot(dat, aes(x=Landuse_100x100, y=LCBD_fun, fill=Landuse_100x100)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  geom_point(data = PAS_median_values[1:3,], aes(y=median_LCBD.fun,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2)) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  scale_y_continuous(name = "LCBD functional", breaks = c(0,0.25,0.5,0.75), labels =c(0,0.25,0.5,0.75), limits = c(0,0.75)) +
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
LCBD_fun.violin

require(egg)
figure <- ggarrange(Richness.violin, Shannon.violin, 
                    TOP.violin,FDis.violin, TED.violin, 
                    LCBD_taxo.violin, LCBD_fun.violin,
                    labels = LETTERS[1:7],
                    nrow = 4, ncol=2, 
                    widths = c(1,1),
                    heights = c(1,1,1,1))
figure

summary(fm1 <- aov(TOP~Landuse_100x100, data=dat))
TukeyHSD(fm1, "Landuse_100x100", ordered = TRUE)

summary(fm1 <- aov(TED~Landuse_100x100, data=dat))
TukeyHSD(fm1, "Landuse_100x100", ordered = TRUE)

summary(fm1 <- aov(FDis~Landuse_100x100, data=dat))
TukeyHSD(fm1, "Landuse_100x100", ordered = TRUE)

summary(fm1 <- aov(Richness~Landuse_100x100, data=dat))
TukeyHSD(fm1, "Landuse_100x100", ordered = TRUE)

summary(fm1 <- aov(Shannon~Landuse_100x100, data=dat))
TukeyHSD(fm1, "Landuse_100x100", ordered = TRUE)

summary(fm1 <- aov(LCBD_taxo~Landuse_100x100, data=dat))
TukeyHSD(fm1, "Landuse_100x100", ordered = TRUE)

summary(fm1 <- aov(LCBD_fun~Landuse_100x100, data=dat))
TukeyHSD(fm1, "Landuse_100x100", ordered = TRUE)

require("magrittr")
require("ggpubr")
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Figures")
figure %>% ggexport(filename = "Fig_Violin.png",
                    width = 900, height = 900)

figure %>% ggexport(filename = "Fig_Violin.pdf",
                    width = 9, height = 9)


