# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)

### load the data -----------------------------------------------------------------------
setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Analyses")
source("Load environmental data.R")

setwd("C:/Users/Bertrand/Dropbox/Projects/City4Bees/Results_Diversity_Modelling_2022-04-22/Selected descriptors")
div <- stack(x = "Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD", "LCBD_fun", "LCBD_taxo", "phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
TOP <- div$TOP
TED <- div$TED
FDis <- div$FDis
rich <- div$Richness
shannon <- div$Shannon
LCBD.taxo <- div$LCBD_taxo
LCBD.fun <- div$LCBD_fun


setwd("C:/Users/Bertrand/Dropbox/Projects/City_cooccurrences/Landuse")
LU = raster("Landuse_100x100.tif")


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

### Violin plots
TOP.violin <- ggplot(dat, aes(x=Landuse_100x100, y=TOP)) +
  geom_violin(trim=TRUE, fill='#A4A4A4', color="black")+
  geom_boxplot(outlier.shape = NA, width=0.1) + 
  scale_y_continuous(limits = quantile(TOP, c(0.1, 0.9))) + 
  theme_minimal()
TOP.violin

TED.violin <- ggplot(dat, aes(x=Landuse_100x100, y=TED)) +
  geom_violin(trim=TRUE, fill='#A4A4A4', color="black")+
  geom_boxplot(outlier.shape = NA, width=0.1) + 
  scale_y_continuous(limits = quantile(TED, c(0.1, 0.9))) + 
  theme_minimal()
TED.violin

FDis.violin <- ggplot(dat, aes(x=Landuse_100x100, y=FDis)) +
  geom_violin(trim=TRUE, fill='#A4A4A4', color="black")+
  geom_boxplot(outlier.shape = NA, width=0.1) + 
  scale_y_continuous(limits = quantile(FDis, c(0.1, 0.9))) + 
  theme_minimal()
FDis.violin

Shannon.violin <- ggplot(dat, aes(x=Landuse_100x100, y=Shannon)) +
  geom_violin(trim=TRUE, fill='#A4A4A4', color="black")+
  geom_boxplot(outlier.shape = NA, width=0.1) + 
  scale_y_continuous(limits = quantile(shannon, c(0.1, 0.9))) + 
  theme_minimal()
Shannon.violin

Richness.violin <- ggplot(dat, aes(x=Landuse_100x100, y=Richness)) +
  geom_violin(trim=TRUE, fill='#A4A4A4', color="black")+
  geom_boxplot(outlier.shape = NA, width=0.1) + 
  scale_y_continuous(limits = quantile(rich, c(0.1, 0.9))) + 
  theme_minimal()
Richness.violin

LCBD_taxo.violin <- ggplot(dat, aes(x=Landuse_100x100, y=LCBD_taxo)) +
  geom_violin(trim=TRUE, fill='#A4A4A4', color="black")+
  geom_boxplot(outlier.shape = NA, width=0.1) + 
  scale_y_continuous(limits = quantile(LCBD.taxo*1000, c(0.1, 0.9))) + 
  theme_minimal()
LCBD_taxo.violin

LCBD_fun.violin <- ggplot(dat, aes(x=Landuse_100x100, y=LCBD_fun)) +
  geom_violin(trim=TRUE, fill='#A4A4A4', color="black")+
  geom_boxplot(outlier.shape = NA, width=0.1) + 
  scale_y_continuous(limits = quantile(LCBD.fun*1000, c(0.1, 0.9))) + 
  theme_minimal()
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


