# Remove all R objects in the workspace
rm(list = ls())

### Library
library(raster)
library(sp)
library(sf)
library(rgeos)
library(rgdal)

TOP= raster("DATA/Selected descriptors/Results_2022_04_28/TOP.tif")
responses=stackOpen("DATA/Selected descriptors/Results_2022_04_28/Masked_responses/rasterstack.responses.tif")
names(responses) = c("FDIs", "TED", "TOP", "LCBD_fun", "LCBD_taxo", "shannon", "rich")
r_lu09_10=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/LU_10.tiff")

TOP <- responses$TOP
TED <- responses$TED
FDis <- responses$FDis
rich <- responses$rich
shannon <- responses$shannon
LCBD_fun <- responses$LCBD_fun
LCBD_taxo<- responses$LCBD_taxo
ras.stack = stack(r_lu09_10,TOP,TED,FDis,rich,shannon,LCBD_taxo,LCBD_fun)
responses.df=as.data.frame(ras.stack)
responses.df=na.omit(responses.df)
responses.df$LU_10=as.factor(responses.df$LU_10)
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

violin.TOP.LU10=ggplot(responses.df[responses.df$LU_10 %in% c(160,200,220,240),], aes(x=as.factor(LU_10), y=TOP, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  # scale_fill_manual(values =  palette.lu) + 
  # scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = "TOP",  limits = c(round(quantile(responses.df$TOP, probs = seq(0,1, by=0.1))[2]), round(quantile(responses.df$TOP, probs = seq(0,1, by=0.1))[10]))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.TOP.LU10

violin.LCBD_fun.LU10=ggplot(responses.df[responses.df$LU_10 %in% c(160,200,220,240),], aes(x=as.factor(LU_10), y=LCBD_fun, fill=LU_10)) + 
  geom_flat_violin(position = position_nudge(x = .2, y = 0),color=NA, alpha = .8, trim=F,adjust=1.5) +
  geom_boxplot(width = .1, outlier.shape = NA, alpha=0.7, notch=T) + 
  theme_classic(base_size = 20) +
  #  geom_point(data = PAS_median_values[1:3,], aes(y=median_top,x=as.factor(Landuse_100x100), col=as.factor(Landuse_100x100)),position = position_nudge(x = -0.2), size=5) +
  #  guides(fill = FALSE,color = FALSE)+ 
  # scale_fill_manual(values =  palette.lu) + 
  # scale_color_manual(values =  palette.lu) + 
  xlab("Land-use")+
  labs(fill="Land-use", color="") +
  scale_y_continuous(name = expression(paste("LCBD functional x" , 10^{3})),  limits = c(round(quantile(responses.df$LCBD_fun, probs = seq(0,1, by=0.1))[2], digits = 3), round(quantile(responses.df$LCBD_fun, probs = seq(0,1, by=0.1))[10], digits = 3))) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),legend.position = "right")
violin.LCBD_fun.LU10



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