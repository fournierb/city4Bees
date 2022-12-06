
library(raster)
library(dplyr)
library(reshape2)
library(ggplot2)

bee_cscf2 = read.csv("~/Dropbox/City4bees/Raw_data/Processed/Switzerland_scale/Bee_CSCF_mat_2.csv")
protected.areas.all=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PA.sensu.lato.tiff")
protected.areas.sl=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PA.sensu.lato2.tiff")
protected.areas.ss=raster("~/Dropbox/City4bees/Analyses/bees_switzerland/DATA/PA.sensu.stricto.tiff")
bee.sps.PA=cbind(bee_cscf2, 
                 data.frame(PA.all=raster::extract(protected.areas.all,bee_cscf2[,c("x","y")])),
                 data.frame(PA.sl=raster::extract(protected.areas.sl,bee_cscf2[,c("x","y")])),
                 data.frame(PA.ss=raster::extract(protected.areas.ss,bee_cscf2[,c("x","y")])))
bee.sps.PA.long=melt(bee.sps.PA[,6:ncol(bee.sps.PA)], id.vars = c("x", "y", "PA.all", "PA.sl", "PA.ss"), variable.name = "species", value.name  = "occurrence")
bee.sps.PA.long[is.na(bee.sps.PA.long)] = 0
bee.sps.PA.long=bee.sps.PA.long[bee.sps.PA.long$occurrence==1,]
BeeSpeciesData=list()
for(s in 1:length(unique(bee.sps.PA.long$species))){
  cat(s)
  dat.sps=bee.sps.PA.long[bee.sps.PA.long$species == unique(bee.sps.PA.long$species)[s],]
  BeeSpeciesData[[s]]=dat.sps %>% dplyr::group_by(species) %>% dplyr::summarise(n.PA.all=sum(PA.all) , n.PA.sl=sum(PA.sl), n.PA.ss=sum(PA.ss), n.total=n())
  
}

BeeSpeciesData.df=do.call(rbind,BeeSpeciesData)
BeeSpeciesData.df$id=seq(1, length(BeeSpeciesData.df$species), by=1)
BeeSpeciesData.df$id=seq(1, length(BeeSpeciesData.df$species), by=1)
 BeeSpeciesData.df.long=melt(BeeSpeciesData.df[, c(1,7,8,10,11)], id.vars = c("species", "id"), variable.name = "isPA", 
                              +                             value.name = "Proportion")
palette_isNA=c("#44AA99", "#88CCEE", "#DDCC77")
Occurrence.PA.plot=ggplot(BeeSpeciesData.df.long, aes(x=id, y=Proportion, fill=isPA)) + 
 geom_bar(stat = "identity", ,width = 1.2) + 
  theme_classic(base_size = 30) +
  scale_fill_manual(name = "", labels = c("PA sl", "PA ss","Not protected"),values =  palette_isNA) +
  ylab("Proportion occurrences") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous("Species", breaks = seq(from=0, to=550, by=50), labels = seq(from=0, to=550, by=50), limits=c(0,550)) 
ggsave(plot = Occurrence.PA.plot,filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/Protected_areas/Occurrence.PA.plot.png", device = "png", height = 10, width = 14)







bee_cscf = read.table("~/Dropbox/City4bees/PUBLICATION/Bees_ch/Submission_Conservation Biology/Diversity_data_complete.txt", header = T)
bee_cscf.fil=bee_cscf[bee_cscf$Richness>4,]
bee_cscf.fil=bee_cscf.fil[bee_cscf.fil$Richness< quantile(bee_cscf$Richness, 0.98),]
sum(bee_cscf.fil$Richness)
mean(bee_cscf.fil$Richness)
sd(bee_cscf.fil$Richness)
max(bee_cscf.fil$Richness)
min(bee_cscf.fil$Richness)

ttt=data.frame(lc.plots=raster::extract(LU, bee_cscf[, c("x", "y")]))



### categorical map viridis -> function
map_cat2 <- function(ras, title){
  require(viridis)
  ras=mask(x = ras, mask = water_bodies,maskvalue = 1)
  ras.df <- as.data.frame(ras, xy = TRUE)
  ras.df=na.omit(ras.df)
  names(ras.df) = c("x","y","layer")
  p <- ggplot() +
    geom_raster(data = ras.df, 
                aes(x = x, y = y, 
                    fill = (layer))) + 
    coord_equal() +
    scale_fill_viridis(option = "D",
                       name=title) +
    #   guides(fill = guide_legend(reverse=T)) +
    # theme_bw() +
    ggtitle(title) + 
    theme(
      # Set background color to white
      panel.background = element_rect(fill = "white"),
      # Set the color and the width of the grid lines for the horizontal axis
      panel.grid.major.x = element_blank(),
      # Remove tick marks by setting their length to 0
      axis.ticks.length = unit(0, "mm"),
      # Remove the title for both axes
      axis.title = element_blank(),
      # But customize labels for the horizontal axis
      axis.text.x = element_blank(),
      # same for y axis
      axis.text.y = element_blank(),
      # Remove the legend title
      title = element_blank()
    )
  
  return(p)
}

p_TOP = map_cat2(ras = TOP, title = "Functional richness (TOP)")
p_TED = map_cat2(ras = TED, title = "Functional evenness (TED)")
p_FDis = map_cat2(ras = FDis, title = "Functional dispersion (FDis)")
p_rich = map_cat2(ras = rich, title = "Species richness")
p_sha = map_cat2(ras = shannon, title = "Species diversity (Shannon)")
p_lcbd_f = map_cat2(ras = LCBD_fun, title = "LCBD functional")
p_lcbd_t = map_cat2(ras = LCBD_taxo, title = "LCBD taxonomic")

### correlation matrix among diversity facets
dat.cor = sampleRandom(stack(TOP,TED,FDis, rich, shannon, LCBD_fun, LCBD_taxo), 100000)
mat.cor = cor(dat.cor)

library(ggcorrplot)
p_cor <- ggcorrplot(mat.cor, 
                    hc.order = TRUE, 
                    show.legend=FALSE,
                    type = "lower",
                    lab = TRUE) +
  ggtitle("Correlation matrix")


require(egg)
figure <- ggarrange(p_rich, p_sha, p_TOP, p_TED, p_FDis, p_lcbd_t, p_lcbd_f, p_cor,
                    labels = LETTERS[1:8],
                    nrow = 4, ncol=2, heights = c(1,1,1,1,1,1,1,2), widths = c(1,1,1,1,1,1,1,2))

require("magrittr")
require("ggpubr")
figure %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/Fig2_Diversity_Maps_revised.png",
                    width = 1000, height = 1000)
figure %>% ggexport(filename = "~/Dropbox/City4bees/Analyses/bees_switzerland/OUTPUT/maps/Community_atributes/Fig2_Diversity_Maps_revised.pdf",
                    width = 10, height = 10)

