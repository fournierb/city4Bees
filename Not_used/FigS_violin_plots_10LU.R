# Remove all R objects in the workspace
rm(list = ls())

### Library
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
source("~/Dropbox/City4bees/Analyses/bees_switzerland/city4Bees/R_rainclouds.R")

TOP= raster("DATA/Selected descriptors/Results_2022_04_28/TOP.tif")
responses=stackOpen("DATA/Selected descriptors/Results_2022_04_28/Masked_responses/rasterstack.responses.tif")
names(responses) = c("FDis", "TED", "TOP", "LCBD_fun", "LCBD_taxo", "shannon", "rich")
r_lu09_10=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/LU_10_CAT_latlon.tif")
pointsLU=read.csv("~/Downloads/ag-b-00.03-37-area-csv.csv", sep=";")
r_lu09_10.crop <- raster::rasterFromXYZ(xyz = pointsLU[,c("E", "N", "LU18_10")],res = 100)
crs(r_lu09_10.crop)="+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs +type=crs"
r_lu09_10.crop.2=projectRaster(from = r_lu09_10.crop, crs=crs(TOP))
r_lu09_10.crop.3= raster::crop(r_lu09_10.crop.2, TOP)
lu_10=raster::extract(r_lu09_10.crop.3,coordinates(TOP))
elevation=raster("DATA/dhm_25.tif")
crs(elevation) = crs(TOP)
elevation.pr=projectRaster(from = elevation, to = TOP)
elevation.df=as.data.frame(elevation.pr)
TOP <- responses$TOP
TED <- responses$TED
FDis <- responses$FDis
rich <- responses$rich
shannon <- responses$shannon
LCBD_fun <- responses$LCBD_fun
LCBD_taxo<- responses$LCBD_taxo
ras.stack = stack(TOP,TED,FDis,rich,shannon,LCBD_taxo,LCBD_fun)
responses.df=as.data.frame(ras.stack)
responses.df=cbind(responses.df, lu_10)
responses.df=na.omit(cbind(responses.df, elevation.df))

responses.df2=merge(x = responses.df,
                   y =  data.frame(old.LU= c(100, 120, 140, 160, 200, 220, 240, 300, 400, 410, 420),
                                   new.LU=c(100,100,100,100,200,220,220, 300, 400, 410, 420)),
                   by.x="lu_10",
                   by.y="old.LU")
## Codes for the Land Use:
## 100 Buildings
## 120 transport surfaces
## 140 special infrastructure
## 160 UGS
## 200 Arbp. viti and horticulture
## 220 Conreus herbacis 
## 240 pastures alpestres
## 300 boscs
##

### Urban lC 100, 120, 140, 160

palette.lu=c("#DC267F", "#B38867","#81715E" ,"#CDCDC0")

ggplot(responses.df2[responses.df2$new.LU%in% c(100, 200,220, 300),], aes(x=as.factor(new.LU), y=rich, fill=as.factor(new.LU))) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 35) +
  scale_fill_manual(values =  palette.lu) + 
  scale_color_manual(values =  palette.lu) + 
  xlab("")+
  scale_y_continuous(name = "", trans = "log10")+
  labs(fill="Land-use", color="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "none")


palette.urban=c("#2F2E33","#C5BEBA","#4B4345","#A5C05B")

violin.rich.LU10.urban=ggplot(responses.df[responses.df$LU_10 %in% c(100,120,140,160),], aes(x=as.factor(LU_10), y=rich, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.urban) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "Richness",  limits = c(round(quantile(responses.df$rich, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$rich, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.rich.LU10.urban


violin.shannon.LU10.urban=ggplot(responses.df[responses.df$LU_10 %in% c(100,120,140,160),], aes(x=as.factor(LU_10), y=shannon, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.urban) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "shannonness",  limits = c(round(quantile(responses.df$shannon, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$shannon, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.shannon.LU10.urban

violin.TOP.LU10.urban=ggplot(responses.df[responses.df$LU_10 %in% c(100,120,140,160),], aes(x=as.factor(LU_10), y=TOP, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.urban) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "TOP",  limits = c(round(quantile(responses.df$TOP, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$TOP, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.TOP.LU10.urban

violin.TED.LU10.urban=ggplot(responses.df[responses.df$LU_10 %in% c(100,120,140,160),], aes(x=as.factor(LU_10), y=TED, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_TED,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.urban) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "TED", limits = c(round(quantile(responses.df$TED, probs = seq(0,1, by=0.1))[2], digits = 2), round(quantile(responses.df$TED, probs = seq(0,1, by=0.1))[10], digits = 2)),
                     breaks = c(0.92, 0.93, 0.94), labels = c(0.92, 0.93, 0.94)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.TED.LU10.urban

violin.FDis.LU10.urban=ggplot(responses.df[responses.df$LU_10 %in% c(100,120,140,160),], aes(x=as.factor(LU_10), y=FDis, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_FDIs,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.urban) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "FDis",  limits = c(round(quantile(responses.df$FDis, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$FDis, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.FDis.LU10.urban

violin.LCBD_taxo.LU10.urban=ggplot(responses.df[responses.df$LU_10 %in% c(100,120,140,160),], aes(x=as.factor(LU_10), y=LCBD_taxo, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_LCBD_taxo,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.urban) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = expression(paste("LCBD taxonomic x" , 10^{-3})),  limits = c(round(quantile(responses.df$LCBD_taxo, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(responses.df$LCBD_taxo, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.LCBD_taxo.LU10.urban

violin.LCBD_fun.LU10.urban=ggplot(responses.df[responses.df$LU_10 %in% c(100,120,140,160),], aes(x=as.factor(LU_10), y=LCBD_fun, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
   scale_fill_manual(values =  palette.urban) + 
  # scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = expression(paste("LCBD functional x" , 10^{-3})),  limits = c(round(quantile(responses.df$LCBD_fun, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(responses.df$LCBD_fun, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.LCBD_fun.LU10.urban

require(scales)
violin.dhm_25.LU10.urban=ggplot(responses.df[responses.df$LU_10 %in% c(100,120,140,160),], aes(x=as.factor(LU_10), y=dhm_25, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.urban) + 
  # scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous( "log10(Elevation (m))", trans = 'log10' ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.dhm_25.LU10.urban

figure.violin.urban <- ggarrange(violin.rich.LU10.urban, violin.shannon.LU10.urban, 
                                 violin.TOP.LU10.urban,violin.FDis.LU10.urban, violin.TED.LU10.urban, 
                    violin.LCBD_taxo.LU10.urban, violin.LCBD_fun.LU10.urban, violin.dhm_25.LU10.urban,
                    labels = paste("(",letters[1:8],")", sep=""),
                    font.label = list(size=16),
                    nrow = 4, ncol=2, 
                    widths = c(1,1),
                    heights = c(1,1,1,1), common.legend = T)
figure.violin.urban

require("magrittr")
require("ggpubr")
figure.violin.urban %>% ggexport(filename = "OUTPUT/Violin_plots/FigS_violin.urban.png",
                    width = 1200, height = 1200)

figure.violin.urban %>% ggexport(filename = "OUTPUT/Violin_plots/FigS_violin.urban.pdf",
                    width = 17, height = 17)

### Rural LC 200, 220, 240

palette.rural=c("#81715E","#68A225","#506D2F")

violin.rich.LU10.rural=ggplot(responses.df[responses.df$LU_10 %in% c(200, 220, 240),], aes(x=as.factor(LU_10), y=rich, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.rural) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "Richness",  limits = c(round(quantile(responses.df$rich, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$rich, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.rich.LU10.rural

violin.shannon.LU10.rural=ggplot(responses.df[responses.df$LU_10 %in% c(200, 220, 240),], aes(x=as.factor(LU_10), y=shannon, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.rural) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "shannonness",  limits = c(round(quantile(responses.df$shannon, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$shannon, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.shannon.LU10.rural

violin.TOP.LU10.rural=ggplot(responses.df[responses.df$LU_10 %in% c(200, 220, 240),], aes(x=as.factor(LU_10), y=TOP, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.rural) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "TOP",  limits = c(round(quantile(responses.df$TOP, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$TOP, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.TOP.LU10.rural

violin.TED.LU10.rural=ggplot(responses.df[responses.df$LU_10 %in% c(200, 220, 240),], aes(x=as.factor(LU_10), y=TED, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_TED,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.rural) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "TED", limits = c(round(quantile(responses.df$TED, probs = seq(0,1, by=0.1))[2], digits = 2), round(quantile(responses.df$TED, probs = seq(0,1, by=0.1))[10], digits = 2)),
                     breaks = c(0.92, 0.93, 0.94), labels = c(0.92, 0.93, 0.94)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.TED.LU10.rural

violin.FDis.LU10.rural=ggplot(responses.df[responses.df$LU_10 %in% c(200, 220, 240),], aes(x=as.factor(LU_10), y=FDis, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_FDIs,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.rural) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "FDis",  limits = c(round(quantile(responses.df$FDis, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$FDis, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.FDis.LU10.rural

violin.LCBD_taxo.LU10.rural=ggplot(responses.df[responses.df$LU_10 %in% c(200, 220, 240),], aes(x=as.factor(LU_10), y=LCBD_taxo, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_LCBD_taxo,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.rural) + 
  # scale_color_manual(values =  palette.urban) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = expression(paste("LCBD taxonomic x" , 10^{3})),  limits = c(round(quantile(responses.df$LCBD_taxo, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(responses.df$LCBD_taxo, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.LCBD_taxo.LU10.rural

violin.LCBD_fun.LU10.rural=ggplot(responses.df[responses.df$LU_10 %in% c(200, 220, 240),], aes(x=as.factor(LU_10), y=LCBD_fun, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.rural) + 
  # scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = expression(paste("LCBD functional x" , 10^{3})),  limits = c(round(quantile(responses.df$LCBD_fun, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(responses.df$LCBD_fun, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.LCBD_fun.LU10.rural

violin.dhm_25.LU10.rural=ggplot(responses.df[responses.df$LU_10 %in% c(200, 220, 240),], aes(x=as.factor(LU_10), y=dhm_25, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  scale_fill_manual(values =  palette.rural) + 
  # scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous( "log10(Elevation (m))", trans = 'log10' ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.dhm_25.LU10.rural

figure.violin.rural <- ggarrange(violin.rich.LU10.rural, violin.shannon.LU10.rural, 
                                 violin.TOP.LU10.rural,violin.FDis.LU10.rural, violin.TED.LU10.rural, 
                                 violin.LCBD_taxo.LU10.rural, violin.LCBD_fun.LU10.rural,violin.dhm_25.LU10.rural,
                                 labels = paste("(",letters[1:8],")", sep=""),
                                 font.label = list(size=16),
                                 nrow = 4, ncol=2, 
                                 widths = c(1,1),
                                 heights = c(1,1,1,1), common.legend = T)
figure.violin.rural

require("magrittr")
require("ggpubr")
figure.violin.rural %>% ggexport(filename = "OUTPUT/Violin_plots/FigS_violin.rural.png",
                                 width = 1200, height = 1200)

figure.violin.rural %>% ggexport(filename = "OUTPUT/Violin_plots/FigS_violin.rural.pdf",
                                 width = 17, height = 17)


## LU rasters to export
urban_buildings <- r_lu09_10 %in% 100
urban_transports <- r_lu09_10 %in% 120
urban_special <- r_lu09_10 %in% 140
urban_green <- r_lu09_10 %in% 160
agriculture_trees <- r_lu09_10 %in% 200
agriculture_herbaceous <- r_lu09_10 %in% 220
agriculture_alpines <- r_lu09_10 %in% 240
forest <- r_lu09_10 %in% 300
lu.list=list(urban_buildings,urban_transports,urban_special,urban_green,agriculture_trees,agriculture_herbaceous,agriculture_alpines,forest)