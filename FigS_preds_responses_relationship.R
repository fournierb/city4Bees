
extend.raster=raster("DATA/Selected descriptors/Results_2022_04_28/belowgound.tif")


div <- stack(x = "DATA/Selected descriptors/Results_2022_04_28/Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD","LCBD_fun" ,"LCBD_taxo","phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
div.metrics=as.data.frame(div)
div.metrics=na.omit(div.metrics)
climate = stack("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/Data ready for analyses/Climate_PCA_CH_stack.tif")
masked.climate=mask(x =climate,extend.raster )
climate.df=as.data.frame(masked.climate, coordinates(masked.climate))
climate.df=na.omit(climate.df)

dat=cbind(div.metrics,climate.df)


palette_predictors_climate=c( "#1E1F26","#283655","#4D648D","#D0E1F9")

ggplot(data = dat, aes(y=TOP, x = Climate_PCA_CH_stack.4)) + 
  geom_smooth(method="loess", span = 1, aes()) +
  scale_color_manual(values =palette_predictors_climate) +
  scale_fill_manual(values = palette_predictors_climate) +
  scale_y_continuous(name = ylabel, breaks =labels , labels=labels, limits = c(min(labels), max(labels))) + 
  theme_classic(base_size = 20) +
  theme(legend.title = element_blank()) +
  xlab("")