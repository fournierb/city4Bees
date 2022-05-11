# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)

### Elevation
elevation = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/dhm_25.tif")
### CH
extend.raster=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Selected descriptors/Results_2022_04_28/belowgound.tif")
### Predictors
climate = stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/Climate_PCA_CH_stack.tif")
vegetation = stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/Plant_PC_19_revised.tif")
beekeeping = stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/beehive-2012-2018.tif")
lu=stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/LU_all_stack.tif")
elevation = raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/dhm_25.tif")
crs(elevation) = crs(extend.raster)
elevation=projectRaster(from = elevation, to=extend.raster, res = c(100,100))

climate.df=as.data.frame(climate)
vegetation.df=as.data.frame(vegetation)
lu.df=as.data.frame(lu)
beekeeping.df=as.data.frame(beekeeping)
elevation.df=as.data.frame(elevation)
coordinates.predictors= as.data.frame(coordinates(climate))

dat1= cbind(climate.df,vegetation.df,elevation.df,coordinates.predictors)
dat1=na.omit(dat1)
dat1$id=seq(from=1, to= nrow(dat1))
dat1$coordmerge=paste(dat1$x, dat1$y, sep="")
dat1.random=dat1[dat1$id %in% sample(dat1$id, 10000),]

dat2= cbind(lu.df,beekeeping.df,elevation.df,coordinates.predictors)
dat2$coordmerge=paste(dat2$x, dat2$y, sep="")
dat2=dat2[dat2$coordmerge %in% dat1.random$coordmerge,]
dat2[is.na(dat2)] <- 0
### Raster plots
masked.climate=mask(x =climate,extend.raster )

png(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps_climate.png", width = ncol(masked.climate), height = nrow(masked.climate))
par(mfrow = c(2, 2), mar = c(0.1, 0.1, 0.1, 0.1))
plot(masked.climate[[1]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(masked.climate[[2]], legend=T, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F, legend.args=list(text='PC axis', side=4, font=2, line=2.5, cex=0.8))
plot(masked.climate[[3]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(masked.climate[[4]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)

dev.off()


png(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps_vegetation.png", width = ncol(vegetation), height = nrow(vegetation))
par(mfrow = c(2, 2), mar = c(0.1, 0.1, 0.1, 0.1))
plot(vegetation[[1]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(vegetation[[2]], legend=T, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F, legend.args=list(text='PC axis', side=4, font=2, line=2.5, cex=0.8))
plot(vegetation[[3]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(vegetation[[4]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
dev.off()

png(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps_beekeeping.png", width = ncol(beekeeping), height = nrow(beekeeping))
par(mfrow = c(2, 2), mar = c(0.1, 0.1, 0.1, 0.1))
plot(beekeeping[[5]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(beekeeping[[6]], legend=T, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F, legend.args=list(text='N. beehives 2500', side=4, font=2, line=2.5, cex=0.8))
plot(beekeeping[[7]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(beekeeping[[8]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
dev.off()

png(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps_lu.png", width = ncol(lu), height = nrow(lu))
par(mfrow = c(3, 4), mar = c(0.1, 0.1, 0.1, 0.1))

plot(lu[[1]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[2]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[3]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[4]], legend=T, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F, legend.args=list(text='Prop. LU 2500 m', side=4, font=2, line=2.5, cex=0.8))

plot(lu[[5]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[6]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[7]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[8]], legend=T, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F, legend.args=list(text='Prop. LU 2500 m', side=4, font=2, line=2.5, cex=0.8))

plot(lu[[9]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[10]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[11]], legend=F, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F)
plot(lu[[12]], legend=T, col=viridis(n = 10,option = "D"),axes=F, main = "", box=F, legend.args=list(text='Prop. LU 2500 m', side=4, font=2, line=2.5, cex=0.8))
dev.off()

### Predictors with elevation

elevation.climate= ggplot(dat1.random, aes(x=dhm_25, y=Climate_PCA_CH_stack.1)) +
  geom_smooth(method="loess", span = 1, col="#1E1F26", fill="#1E1F26") +
  geom_smooth(method="loess", span = 1, aes(y=Climate_PCA_CH_stack.2), col="#283655", fill="#283655") +
  geom_smooth(method="loess", span = 1, aes(y=Climate_PCA_CH_stack.3), col="#4D648D", fill="#4D648D") +
  geom_smooth(method="loess", span = 1, aes(y=Climate_PCA_CH_stack.4), col="#D0E1F9", fill="#D0E1F9") +
  scale_x_continuous(name = "Elevation (m)", limits = c(0, 3500)) +
  ylab("PCA axis") +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank())

elevation.vegetation= ggplot(dat1.random, aes(x=dhm_25, y=Plant_PC_19_revised.2)) +
  geom_smooth(method="loess", span = 1, col="#6fb98f", fill="#6fb98f") +
  geom_smooth(method="loess", span = 1, aes(y=Plant_PC_19_revised.3), col="#4B7447", fill="#4B7447") +
  geom_smooth(method="loess", span = 1, aes(y=Plant_PC_19_revised.4), col="#7CAA2D", fill="#7CAA2D") +
  scale_x_continuous(name = "Elevation (m)", limits = c(0, 3500)) +
  ylab("PCA axis") +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank())

elevation.lu= ggplot(dat2, aes(x=dhm_25, y=LU_all_stack.4)) +
  geom_smooth(method="loess", span = 1, col="#CDCDC0", fill="#CDCDC0") +
  geom_smooth(method="loess", span = 1, aes(y=LU_all_stack.8), col="#ED5752", fill="#ED5752") +
  geom_smooth(method="loess", span = 1, aes(y=LU_all_stack.12), col="#B38867", fill="#B38867") +
  scale_x_continuous(name = "Elevation (m)", limits = c(0, 3500)) +
  scale_y_continuous("Proportion in 2500 m", limits=c(0,0.5)) +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank())

elevation.beekeeping= ggplot(dat2, aes(x=dhm_25, y=beehive.2012.2018.8)) +
  geom_smooth(method="loess", span = 1, col="#F4CC70", fill="#F4CC70") +
  scale_x_continuous(name = "Elevation (m)", limits = c(0, 3500)) +
  ylab("Number of beehives in 2500 m") +
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank())

plot.arranged=ggarrange(elevation.climate,elevation.vegetation,elevation.lu,elevation.beekeeping,
                        nrow = 2, ncol=4,
                        labels=paste("(", letters[1:4], ")", sep=""))

plot.arranged %>% ggexport(filename = ("OUTPUT/FigS_predictors_elevation.png"),
                           width = 1000, height = 1000)
plot.arranged %>% ggexport(filename = ("OUTPUT/FigS_predictors_elevation.pdf"),
                           width = 10, height = 10)
