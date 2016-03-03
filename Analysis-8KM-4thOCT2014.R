

require(foreach); require(raster); require(ggplot2);require(rasterVis); require(rgdal)
filez.means<- list.files(file.path("J:/Results_N/stackmeans"),
                                pattern="*.tif", full.names=TRUE)

All.rasters<-foreach(filez=1:length(filez.means), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.means[[filez]])

result.stk.means_GIMMS<-stack(All.rasters[[1]], All.rasters[[17]], All.rasters[[21]],
                              All.rasters[[25]], All.rasters[[29]], All.rasters[[33]],
                              All.rasters[[37]], All.rasters[[41]], All.rasters[[45]],
                              All.rasters[[5]], All.rasters[[9]], All.rasters[[13]],
                              bands=NULL, native=FALSE, RAT=TRUE)

result.stk.means_VIP<-stack(All.rasters[[2]], All.rasters[[18]], All.rasters[[22]],
                              All.rasters[[26]], All.rasters[[30]], All.rasters[[34]],
                              All.rasters[[38]], All.rasters[[42]], All.rasters[[46]],
                              All.rasters[[6]], All.rasters[[10]], All.rasters[[14]],
                              bands=NULL, native=FALSE, RAT=TRUE)

################################################################
#       Distribution of GIMMS
###############################################################
Names<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")
## Define the legend breaks
my.at <- seq(-0.1, 1, 0.2)
colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = colors)
spplot(result.stk.means_GIMMS,xlim = bbox(result.stk.means_GIMMS)[1, ], 
       ylim = bbox(result.stk.means_GIMMS)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       strip=FALSE,
       main=list("Distribution of GIMMS Means", col="black",cex=2, fontface='bold'), 
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
         }, contour=FALSE, layout=c(2, 6))

############################################################
# Distribution of VIP 
##########################################################
spplot(result.stk.means_VIP, xlim = bbox(result.stk.means_VIP)[1, ], 
       ylim = bbox(result.stk.means_VIP)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("Distribution of VIP Means", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))

#################################################################################
#      Correlation Coefficient: spearman
##################################################################################
filez.cor<- list.files(file.path("J:/Results_N/cor_estimate"),
                         pattern="*.tif", full.names=TRUE)

All.rasters.cor<-foreach(filez=1:length(filez.cor), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.cor[[filez]])

result.stk.Cor.spearman<-stack(All.rasters.cor[[9]], All.rasters.cor[[7]], All.rasters.cor[[15]],
                              All.rasters.cor[[1]], All.rasters.cor[[17]], All.rasters.cor[[13]],
                              All.rasters.cor[[11]], All.rasters.cor[[3]], All.rasters.cor[[23]],
                              All.rasters.cor[[21]], All.rasters.cor[[19]], All.rasters.cor[[5]],
                              bands=NULL, native=FALSE, RAT=TRUE)

result.stk.Cor.pearson<-stack(All.rasters.cor[[10]], All.rasters.cor[[8]], All.rasters.cor[[16]],
                            All.rasters.cor[[2]], All.rasters.cor[[18]], All.rasters.cor[[14]],
                            All.rasters.cor[[12]], All.rasters.cor[[4]], All.rasters.cor[[24]],
                            All.rasters.cor[[22]], All.rasters.cor[[20]], All.rasters.cor[[6]],
                            bands=NULL, native=FALSE, RAT=TRUE)
################################################################
#       Correlation Coefficient
###############################################################
## Define the legend breaks
my.at.cor <- seq(-1, 1, 0.2)
spplot(result.stk.Cor.spearman,xlim = bbox(result.stk.Cor.spearman)[1, ], 
       ylim = bbox(result.stk.Cor.spearman)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at.cor,
       strip=FALSE,
       main=list("Spearman Correlation Coefficient", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')}, contour=FALSE, layout=c(2, 6))

################################################################
#       Pearson
###############################################################
spplot(result.stk.Cor.pearson,xlim = bbox(result.stk.Cor.pearson)[1, ], 
       ylim = bbox(result.stk.Cor.pearson)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at.cor,
       strip=FALSE,
       main=list("Pearson Correlation Coefficient", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))
##############################################################
#
#       Correlation Pvalues
#
#############################################################
filez.sig<- list.files(file.path("J:/Results_N/cor_pvalue"),
                       pattern="*.tif", full.names=TRUE)

All.rasters.sig<-foreach(filez=1:length(filez.sig), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.sig[[filez]])

result.stk.sig.spearman<-stack(All.rasters.sig[[9]], All.rasters.sig[[7]], All.rasters.sig[[15]],
                               All.rasters.sig[[1]], All.rasters.sig[[17]], All.rasters.sig[[13]],
                               All.rasters.sig[[11]], All.rasters.sig[[3]], All.rasters.sig[[23]],
                               All.rasters.sig[[21]], All.rasters.sig[[19]], All.rasters.sig[[5]],
                               bands=NULL, native=FALSE, RAT=TRUE)

result.stk.sig.pearson<-stack(All.rasters.sig[[10]], All.rasters.sig[[8]], All.rasters.sig[[16]],
                              All.rasters.sig[[2]], All.rasters.sig[[18]], All.rasters.sig[[14]],
                              All.rasters.sig[[12]], All.rasters.sig[[4]], All.rasters.sig[[24]],
                              All.rasters.sig[[22]], All.rasters.sig[[20]], All.rasters.sig[[6]],
                              bands=NULL, native=FALSE, RAT=TRUE)

################################################################
#       Correlation Coefficient
###############################################################
## Define the legend breaks
my.at.pval <- seq(0, 1, 0.05)
spplot(result.stk.sig.spearman, xlim = bbox(result.stk.sig.spearman)[1, ], 
       ylim = bbox(result.stk.sig.spearman)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at.pval,
       strip=FALSE,
       main=list("Spearman Correlation Significance", col="black",cex=2, fontface='bold'), 
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))
################################################################
#       Pearson
###############################################################
spplot(result.stk.sig.pearson,xlim = bbox(result.stk.sig.pearson)[1, ], 
       ylim = bbox(result.stk.sig.pearson)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at.pval,
       strip=FALSE,
       main=list("Pearson Correlation Significance", col="black",cex=2, fontface='bold'), 
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))

############################################################
# 90% confidence: alpha -0.1
##########################################################
rmask <- function(p){
  ifelse(p < 0.1, 1,
         ifelse(p >= 0.1, 0, NA))}
writeRaster(r.rmask1[[1]],"/vm/eokuto/RowMeans/Correlation_Analysis/Results/cor_estimate/r.rmaskjan.tif")
############################################################
# stacking cor and bullean pvalues
##########################################################
s.1<-stack(r.rmask1[[1]], result.stk.Cor[[1]])
s.2<-stack(r.rmask1[[2]], result.stk.Cor[[2]])
s.3<-stack(r.rmask1[[3]], result.stk.Cor[[3]])
s.4<-stack(r.rmask1[[4]], result.stk.Cor[[4]])
s.5<-stack(r.rmask1[[5]], result.stk.Cor[[5]])
s.6<-stack(r.rmask1[[6]], result.stk.Cor[[6]])
s.7<-stack(r.rmask1[[7]], result.stk.Cor[[7]])
s.8<-stack(r.rmask1[[8]], result.stk.Cor[[8]])
s.9<-stack(r.rmask1[[9]], result.stk.Cor[[9]])
s.10<-stack(r.rmask1[[10]], result.stk.Cor[[10]])
s.11<-stack(r.rmask1[[11]], result.stk.Cor[[11]])
s.12<-stack(r.rmask1[[12]], result.stk.Cor[[12]])
############################################################
# Masking out correlation rasters
##########################################################
#rr<-overlay(r.rmask1[[1]], result.stk.Cor[[1]], fun=function(x,y){return(x*y)})

fun.mask<-function(x,y){
  return(x*y)
}

masked.r <- for (i in 1:12) {
  overlay(get(paste('s.', i, sep='')), fun=fun.mask)
}

############################################################
# SDS: VIP & GIMMS
##########################################################
result.stk.SD.VIP<-stack(All.rasters[[4]], All.rasters[[20]], All.rasters[[24]],
                            All.rasters[[28]], All.rasters[[32]], All.rasters[[36]],
                            All.rasters[[40]], All.rasters[[44]], All.rasters[[48]],
                            All.rasters[[8]], All.rasters[[12]], All.rasters[[16]],
                            bands=NULL, native=FALSE, RAT=TRUE)

result.stk.SD.GIMMS<-stack(All.rasters[[3]], All.rasters[[19]], All.rasters[[23]],
                         All.rasters[[27]], All.rasters[[31]], All.rasters[[35]],
                         All.rasters[[39]], All.rasters[[43]], All.rasters[[47]],
                         All.rasters[[7]], All.rasters[[11]], All.rasters[[15]],
                         bands=NULL, native=FALSE, RAT=TRUE)

#######################################################################
my.atSD <- seq(0, 1, 0.05)
spplot(result.stk.SD.VIP,xlim = bbox(result.stk.SD.VIP)[1, ], 
       ylim = bbox(result.stk.SD.VIP)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.atSD,
       strip=FALSE,
       main=list("Standard Deviations of VIP", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))
############################################################
# Distribution of SDS
##########################################################
spplot(result.stk.SD.GIMMS,xlim = bbox(result.stk.SD.GIMMS)[1, ], 
       ylim = bbox(result.stk.SD.GIMMS)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.atSD,
       strip=FALSE,
       main=list("Standard Deviations of GIMMS", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))
###################################################################
#               Observation used
#
###################################################################
filez.obs<- list.files(file.path("J:/Results_N/obs_used"),
                       pattern="*.tif", full.names=TRUE)

All.rasters.obs<-foreach(filez=1:length(filez.obs), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.obs[[filez]])


obs.used.files<-stack(All.rasters.obs[[1]],  
                      All.rasters.obs[[5]],All.rasters.obs[[6]], 
                      All.rasters.obs[[7]], All.rasters.obs[[8]],
                      All.rasters.obs[[9]], All.rasters.obs[[10]],
                      All.rasters.obs[[11]], All.rasters.obs[[12]],
                      All.rasters.obs[[2]],
                      All.rasters.obs[[3]], All.rasters.obs[[4]], 
                         bands=NULL, native=FALSE, RAT=TRUE)

my.atobs <-  c(1, seq(2, 100, 2))
spplot(obs.used.files,xlim = bbox(obs.used.files)[1, ], 
       ylim = bbox(obs.used.files)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.atobs,
       strip=FALSE,
       main=list("Percentage Of Complete Cases", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))

###################################################################
#               PIXEL RELIABILITY ANALYSIS
#
###################################################################
filez.best<- list.files(file.path("J:/Results_N/PR_Results/Index_Zero"),
                       pattern="*.tif", full.names=TRUE)
filez.good<- list.files(file.path("J:/Results_N/PR_Results/Index_One"),
                        pattern="*.tif", full.names=TRUE)
filez.moderate<- list.files(file.path("J:/Results_N/PR_Results/Index_Two"),
                        pattern="*.tif", full.names=TRUE)

All.rasters.best<-foreach(i=1:length(filez.best), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.best[[i]])
All.rasters.good<-foreach(j=1:length(filez.good), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.good[[j]])
All.rasters.moderate<-foreach(k=1:length(filez.moderate), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.moderate[[k]])

best.pixels<-stack(All.rasters.best[[5]],  
                      All.rasters.best[[4]],All.rasters.best[[8]], 
                      All.rasters.best[[1]], All.rasters.best[[9]],
                      All.rasters.best[[7]], All.rasters.best[[6]],
                      All.rasters.best[[2]], All.rasters.best[[12]],
                      All.rasters.best[[11]],
                      All.rasters.best[[10]], All.rasters.best[[3]], 
                      bands=NULL, native=FALSE, RAT=TRUE)

good.pixels<-stack(All.rasters.good[[5]],  
                   All.rasters.good[[4]],All.rasters.good[[8]], 
                   All.rasters.good[[1]], All.rasters.good[[9]],
                   All.rasters.good[[7]], All.rasters.good[[6]],
                   All.rasters.good[[2]], All.rasters.good[[12]],
                   All.rasters.good[[11]],
                   All.rasters.good[[10]], All.rasters.good[[3]], 
                   bands=NULL, native=FALSE, RAT=TRUE)

moderate.pixels<-stack(All.rasters.moderate[[5]],  
                   All.rasters.moderate[[4]],All.rasters.moderate[[8]], 
                   All.rasters.moderate[[1]], All.rasters.moderate[[9]],
                   All.rasters.moderate[[7]], All.rasters.moderate[[6]],
                   All.rasters.moderate[[2]], All.rasters.moderate[[12]],
                   All.rasters.moderate[[11]],
                   All.rasters.moderate[[10]], All.rasters.moderate[[3]], 
                   bands=NULL, native=FALSE, RAT=TRUE)

spplot(best.pixels,xlim = bbox(best.pixels)[1, ], 
       ylim = bbox(best.pixels)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.atobs,
       strip=FALSE,
       main=list("Best Quality Data", col="black",cex=1.5, fontface='plain'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5)
       }, contour=FALSE, layout=c(2, 6))

spplot(good.pixels,xlim = bbox(good.pixels)[1, ], 
       ylim = bbox(good.pixels)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.atobs,
       strip=FALSE,
       main=list("Good Quality Data", col="black",cex=1.5, fontface='plain'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='plain')
       }, contour=FALSE, layout=c(2, 6))


spplot(moderate.pixels,xlim = bbox(moderate.pixels)[1, ], 
       ylim = bbox(moderate.pixels)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.atobs,
       strip=FALSE,
       main=list("Moderate Quality Data", col="black",cex=2, fontface='plain'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -70,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='plain')
       }, contour=FALSE, layout=c(2, 6))

###############################################################################################
#               LATITUDINAL GRAPHS
##############\#################################################################################
DF<-read.csv("J:/Results_N/Row_means/DF_Lat.csv")
attach(DF)
names(DF)
###############################################################################################
#               OVERLAYING GRAPHS
###############################################################################################
#           TEST: LATITUDINAL MEANS
###############################################################################################
#           MEANS: expected bounds
###############################################################################################
eb1 <- aes(ymax = Jan.Vip + Jan.sd_JanVIP, ymin = Jan.Vip - Jan.sd_JanVIP)
eb2 <- aes(ymax = Feb.Vip + Feb.sd_FebVIP, ymin = Feb.Vip - Feb.sd_FebVIP)
eb3 <- aes(ymax = Mar.Vip + Mar.sd_MarGIMMS, ymin = Mar.Vip - Mar.sd_MarGIMMS)
eb4 <- aes(ymax = April.Vip + April.sd_AprilVIP, ymin = April.Vip - April.sd_AprilVIP)
eb5 <- aes(ymax = May.Vip + May.sd_MayVIP, ymin = May.Vip - May.sd_MayVIP)
eb6 <- aes(ymax = Jun.Vip + Jun.sd_JunVIP, ymin = Jun.Vip - Jun.sd_JunVIP)
eb7 <- aes(ymax = Jul.Vip + Jul.sd_JulVIP, ymin = Jul.Vip - Jul.sd_JulVIP)
eb8 <- aes(ymax = Aug.Vip + Aug.sd_AugVIP, ymin = Aug.Vip - Aug.sd_AugVIP)
eb9 <- aes(ymax = Sep.Vip + Sep.sd_SepVIP, ymin = Sep.Vip - Sep.sd_SepVIP)
eb10 <- aes(ymax = Oct.Vip + Oct.sd_OctVIP, ymin = Oct.Vip - Oct.sd_OctVIP)
eb11 <- aes(ymax = Nov.Vip + Nov.sd_NovVIP, ymin = Nov.Vip - Nov.sd_NovVIP)
eb12 <- aes(ymax = Dec.Vip + Dec.sd_DecVIP, ymin = Dec.Vip - Dec.sd_DecVIP)
######################################################################
#   GIMMS
###############################################################
eb1a <- aes(ymax = Jan.GIMMS + Jan.sd_JanGIMMS, ymin = Jan.GIMMS - Jan.sd_JanGIMMS)
eb2a <- aes(ymax = Feb.GIMMS + Feb.sd_FebGIMMS, ymin = Feb.GIMMS - Feb.sd_FebGIMMS)
eb3a <- aes(ymax = Mar.GIMMS + Mar.sd_MarGIMMS, ymin = Mar.GIMMS - Mar.sd_MarGIMMS)
eb4a <- aes(ymax = April.GIMMS + April.sd_AprilGIMMS, ymin = April.GIMMS - April.sd_AprilGIMMS)
eb5a <- aes(ymax = May.GIMMS + May.sd_MayGIMMS, ymin = May.GIMMS - May.sd_MayGIMMS)
eb6a <- aes(ymax = Jun.GIMMS + Jun.sd_JunGIMMS, ymin = Jun.GIMMS - Jun.sd_JunGIMMS)
eb7a <- aes(ymax = Jul.GIMMS + Jul.sd_JulGIMMS, ymin = Jul.GIMMS - Jul.sd_JulGIMMS)
eb8a <- aes(ymax = Aug.GIMMS + Aug.sd_AugGIMMS, ymin = Aug.GIMMS - Aug.sd_AugGIMMS)
eb9a <- aes(ymax = Sep.GIMMS + Sep.sd_SepGIMMS, ymin = Sep.GIMMS - Sep.sd_SepGIMMS)
eb10a <- aes(ymax = Oct.GIMMS + Oct.sd_OctGIMMS, ymin = Oct.GIMMS - Oct.sd_OctGIMMS)
eb11a <- aes(ymax = Nov.GIMMS + Nov.sd_NovGIMMS, ymin = Nov.GIMMS - Nov.sd_NovGIMMS)
eb12a <- aes(ymax = Dec.GIMMS + Dec.sd_DecGIMMS, ymin = Dec.GIMMS - Dec.sd_DecGIMMS)
###############################################################################################
#              DISTRIBUTION OF MEANS
###############################################################################################
require(gridExtra)
pJan <- ggplot(DF, aes(Latitude))
pJan <- pJan + geom_ribbon(eb1, alpha = 0.5, fill='gray') + geom_line(aes(y=Jan.Vip), colour="black")
pJan <- pJan + geom_ribbon(eb1a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Jan.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  #theme(plot.margin=unit(c(1,-0.5,0.34,-1), "cm")) +
  #theme(panel.margin=unit(c(1,1,0,1), "mm")) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=0.8, label="Jan",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks=c(-1,-0.5,0,0.5,1))

pFeb <- ggplot(DF, aes(Latitude))
pFeb <- pFeb + geom_ribbon(eb2, alpha = 0.5,fill='gray') + geom_line(aes(y=Feb.Vip), colour="black")
pFeb <- pFeb + geom_ribbon(eb2a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Feb.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  #theme(plot.margin=unit(c(1,0,0.34,-0.5), "cm")) +
  annotate("text", x=-50, y=0.8, label="Feb",size=8, fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pMar <- ggplot(DF, aes(Latitude))
pMar <- pMar + geom_ribbon(eb3, alpha = 0.5,fill='gray') + geom_line(aes(y=Mar.Vip), colour="black")
pMar <- pMar + geom_ribbon(eb3a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Mar.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  #theme(plot.margin=unit(c(1,0.5,0.34,0), "cm")) +
  annotate("text", x=-50, y=0.8, label="Mar",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60), 
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pApr <- ggplot(DF, aes(Latitude))
pApr <- pApr + geom_ribbon(eb4, alpha = 0.5,fill='gray') + geom_line(aes(y=April.Vip), colour="black")
pApr <- pApr + geom_ribbon(eb4a, alpha = 0.5, fill='light blue') + geom_line(aes(y=April.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
 # theme(plot.margin=unit(c(1,1,0.34,0.5), "cm")) +
  annotate("text", x=-50, y=0.8, label="Apr",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pMay <- ggplot(DF, aes(Latitude))
pMay <- pMay + geom_ribbon(eb5, alpha = 0.5,fill='gray') + geom_line(aes(y=May.Vip), colour="black")
pMay <- pMay + geom_ribbon(eb5a, alpha = 0.5, fill='light blue') + geom_line(aes(y=May.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  #theme(plot.margin=unit(c(0.34,-0.5,-0.33,-1), "cm")) +
  annotate("text", x=-50, y=0.8, label="May",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pJun <- ggplot(DF, aes(Latitude))
pJun <- pJun + geom_ribbon(eb6, alpha = 0.5,fill='gray') + geom_line(aes(y=Jun.Vip), colour="black")
pJun <- pJun + geom_ribbon(eb6a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Jun.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  #theme(plot.margin=unit(c(0.34,0,-0.33,-0.5), "cm")) +
  annotate("text", x=-50, y=0.8, label="Jun",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pJul <- ggplot(DF, aes(Latitude))
pJul <- pJul + geom_ribbon(eb7, alpha = 0.5,fill='gray') + geom_line(aes(y=Jul.Vip), colour="black")
pJul <- pJul + geom_ribbon(eb7a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Jul.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  #theme(plot.margin=unit(c(0.34,0.5,-0.33,0), "cm")) +
  annotate("text", x=-50, y=0.8, label="Jul",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pAug <- ggplot(DF, aes(Latitude))
pAug <- pAug + geom_ribbon(eb8, alpha = 0.5,fill='gray') + 
  geom_line(aes(y=Aug.Vip), colour="black")
pAug <- pAug + geom_ribbon(eb8a, alpha = 0.5, fill='light blue') +
  geom_line(aes(y=Aug.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  #theme(plot.margin=unit(c(0.34,1,-0.33,0.5), "cm")) +
  annotate("text", x=-50, y=0.8, label="Aug",size=8,fontface="bold.italic",colour="black",parse=TRUE) +     
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pSep <- ggplot(DF, aes(Latitude))
pSep <- pSep + geom_ribbon(eb9, alpha = 0.5,fill='gray') + geom_line(aes(y=Sep.Vip), colour="black")
pSep <- pSep + geom_ribbon(eb9a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Sep.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  #theme(plot.margin=unit(c(-0.33,0.5,-0.5,-1), "cm")) +
  annotate("text", x=-50, y=0.8, label="Sep",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pOct <- ggplot(DF, aes(Latitude))
pOct <- pOct + geom_ribbon(eb10, alpha = 0.5,fill='gray') + geom_line(aes(y=Oct.Vip), colour="black")
pOct <- pOct + geom_ribbon(eb10a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Oct.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  #theme(plot.margin=unit(c(-0.33,0,-1,-0.5), "cm")) +
  annotate("text", x=-50, y=0.8, label="Oct",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pNov <- ggplot(DF, aes(Latitude))
pNov <- pNov + geom_ribbon(eb11, alpha = 0.5,fill='gray') + geom_line(aes(y=Nov.Vip), colour="black")
pNov <- pNov + geom_ribbon(eb11a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Nov.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_blank()) +
 # theme(plot.margin=unit(c(-0.33,0.5,-1,-0.5), "cm")) +
  annotate("text", x=-50, y=0.8, label="Nov",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))

pDec <- ggplot(DF, aes(Latitude))
pDec <- pDec + geom_ribbon(eb12, alpha = 0.5,fill='gray') + geom_line(aes(y=Dec.Vip), colour="black")
pDec <- pDec + geom_ribbon(eb12a, alpha = 0.5, fill='light blue') + geom_line(aes(y=Dec.GIMMS), colour="blue") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_blank()) +
  #theme(plot.margin=unit(c(-0.33,1,-1,0.5), "cm")) +
  annotate("text", x=-50, y=0.8, label="Dec",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-1,1),
                     breaks=c(-1,-0.5,0,0.5,1))
################################################################## plot.margin=unit(c(1,1,-0.5,1), "mm")
grid.arrange(arrangeGrob(pJan + theme(legend.position="none"),
                         pFeb + theme(legend.position="none"),
                         pMar + theme(legend.position="none"),
                         pApr + theme(legend.position="none"),
                         pMay + theme(legend.position="none"),
                         pJun + theme(legend.position="none"),
                         pJul + theme(legend.position="none"), 
                         pAug + theme(legend.position="none"), 
                         pSep + theme(legend.position="none"),
                         pOct + theme(legend.position="none"), 
                         pNov + theme(legend.position="none"), 
                         pDec + theme(legend.position="none"),
                         nrow=4, ncol=3,
                         clip = TRUE,
                         strip=FALSE,
                         legend = NULL,
                         main=textGrob("Monthly Distribution of VIP+GIMMS Means",
                                       vjust = 1, 
                                       gp = gpar(fontface = "bold", cex = 1)),
                         xlab=textGrob("NDVI Index",
                                       vjust = 1, 
                                       gp = gpar(fontface = "bold", cex = 1)),
                         left = textGrob("Latitude", rot = 90, vjust = 1, 
                                         gp = gpar(fontface = "bold",
                                                   cex = 1))))


####################################################################################
