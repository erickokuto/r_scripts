

require(foreach); require(raster); require(ggplot2);require(rasterVis); require(rgdal)
filez.means.GIMMS <- list.files(file.path("F:/Results/RESULTS-8KM/MEANS/GIMMS"),
                                pattern="*.tif", full.names=TRUE)
filez.means.VIP <- list.files(file.path("F:/Results/RESULTS-8KM/MEANS/VIP"),
                              pattern="*.tif", full.names=TRUE)

All.Monthly.means_GIMMS<-foreach(stackedmeans=1:length(filez.means.GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.means.GIMMS[[stackedmeans]])
All.Monthly.means_VIP<-foreach(stackedmeans=1:length(filez.means.VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.means.VIP[[stackedmeans]])

result.stk.means_GIMMS<-stack(All.Monthly.means_GIMMS,bands=NULL, native=FALSE, RAT=TRUE)
result.stk.means_VIP<-stack(All.Monthly.means_VIP,bands=NULL, native=FALSE, RAT=TRUE)
################################################################
#       Distribution of GIMMS
###############################################################
idx1 <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
result.stk.means_GIMMS <- setZ(result.stk.means_GIMMS, idx1)
names(result.stk.means_GIMMS) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")
## Define the legend breaks
my.at <- seq(0, 1, 0.2)

colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
#colors=colorRampPalette(c("white",brewer.pal(9,"RdYlGn")))(50)
#colors <- c("white", "red", "yellow", "green")
mytheme<-rasterTheme(region = colors)
spplot(result.stk.means_GIMMS,xlim = bbox(result.stk.means_GIMMS)[1, ], 
       ylim = bbox(result.stk.means_GIMMS)[2, ],
       scales = list(draw = FALSE), colorkey=TRUE,
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       main="Distribution of GIMMS 15 Day Monthly Means",cex.lab=10, 
       cex.axis=10, cex.main=10, cex.sub=10)
############################################################
# Distribution of VIP 
##########################################################
idx2 <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
result.stk.means_VIP <- setZ(result.stk.means_VIP, idx2)
names(result.stk.means_VIP) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")
colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
mytheme<-rasterTheme(region = colors)
spplot(result.stk.means_VIP,xlim = bbox(result.stk.means_VIP)[1, ], 
       ylim = bbox(result.stk.means_VIP)[2, ],
       scales = list(draw = FALSE), colorkey=TRUE,
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       main="Distribution of VIP 15 Day Monthly Means", cex.lab=10, 
       cex.axis=10, cex.main=10, cex.sub=10)

#################################################################################
#      Correlation Coefficient: spearman
##################################################################################
filez.Cor.spearman<- list.files(file.path("I:/Results/RESULTS-8KM/cor_estimate/Spearman"),
                       pattern="*.tif", full.names=TRUE)
filez.Cor.pearson<- list.files(file.path("I:/Results/RESULTS-8KM/cor_estimate/Pearson"),
                                pattern="*.tif", full.names=TRUE)

All.Cor.spearman<-foreach(stackedEstSpearman=1:length(filez.Cor.spearman), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.Cor.spearman[[stackedEstSpearman]])

All.Cor.pearson<-foreach(stackedEstpearson=1:length(filez.Cor.pearson), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.Cor.pearson[[stackedEstpearson]])

result.stk.Cor.spearman<-stack(All.Cor.spearman,bands=NULL, native=FALSE, RAT=TRUE)
result.stk.Cor.pearson<-stack(All.Cor.pearson,bands=NULL, native=FALSE, RAT=TRUE)

################################################################
#       Correlation Coefficient
###############################################################
idx4 <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
result.stk.Cor.spearman <- setZ(result.stk.Cor.spearman, idx4)
names(result.stk.Cor.spearman) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")

## Define the legend breaks
my.at.cor <- seq(-1, 1, 0.5)
colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
mytheme<-rasterTheme(region = colors)
spplot(result.stk.Cor.spearman,xlim = bbox(result.stk.Cor.spearman)[1, ], 
       ylim = bbox(result.stk.Cor.spearman)[2, ],
       scales = list(draw = FALSE), colorkey=TRUE,
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at.cor,
       main="Correlation Coefficient using Spearman Method",cex.lab=3, 
       cex.axis=3, cex.main=3, cex.sub=3)
################################################################
#       Pearson
###############################################################
idx5 <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
result.stk.Cor.pearson <- setZ(result.stk.Cor.pearson, idx5)
names(result.stk.Cor.pearson) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")

colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
mytheme<-rasterTheme(region = colors)
spplot(result.stk.Cor.pearson,xlim = bbox(result.stk.Cor.pearson)[1, ], 
       ylim = bbox(result.stk.Cor.pearson)[2, ],
       scales = list(draw = FALSE), colorkey=TRUE,
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at.cor,
       main="Correlation Coefficient using Pearson Method",cex.lab=3, 
       cex.axis=3, cex.main=3, cex.sub=3)

##############################################################
#
#       Correlation Pvalues
#
#############################################################

filez.sig.spearman<- list.files(file.path("I:/Results/RESULTS-8KM/cor_pvalue/Spearman"),
                                pattern="*.tif", full.names=TRUE)
filez.sig.pearson<- list.files(file.path("I:/Results/RESULTS-8KM/cor_pvalue/Pearson"),
                               pattern="*.tif", full.names=TRUE)

All.sig.spearman<-foreach(stackedsigSpearman=1:length(filez.sig.spearman), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.sig.spearman[[stackedsigSpearman]])

All.sig.pearson<-foreach(stackedsigpearson=1:length(filez.sig.pearson), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.sig.pearson[[stackedsigpearson]])

result.stk.sig.spearman<-stack(All.sig.spearman,bands=NULL, native=FALSE, RAT=TRUE)
result.stk.sig.pearson<-stack(All.sig.pearson,bands=NULL, native=FALSE, RAT=TRUE)

################################################################
#       Correlation Coefficient
###############################################################
idx6 <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
result.stk.sig.spearman <- setZ(result.stk.sig.spearman, idx6)
names(result.stk.sig.spearman) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")

## Define the legend breaks
my.at.pval <- seq(0, 1, 0.2)
colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
mytheme<-rasterTheme(region = colors)
spplot(result.stk.sig.spearman,xlim = bbox(result.stk.sig.spearman)[1, ], 
       ylim = bbox(result.stk.sig.spearman)[2, ],
       scales = list(draw = FALSE), colorkey=TRUE,
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at.pval,
       main="Correlation significance using Spearman Method",cex.lab=3, 
       cex.axis=3, cex.main=3, cex.sub=3)
################################################################
#       Pearson
###############################################################
idx7 <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
result.stk.sig.pearson <- setZ(result.stk.Cor.pearson, idx7)
names(result.stk.sig.pearson) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")

colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
mytheme<-rasterTheme(region = colors)
spplot(result.stk.Cor.pearson,xlim = bbox(result.stk.Cor.pearson)[1, ], 
       ylim = bbox(result.stk.Cor.pearson)[2, ],
       scales = list(draw = FALSE), colorkey=TRUE,
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at.pval,
       main="Correlation significance using Pearson Method",cex.lab=3, 
       cex.axis=3, cex.main=3, cex.sub=3)


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
my.atSD <- seq(0, 0.2, 0.05)
filez.SD.VIP<- list.files(file.path("F:/Results/RESULTS-8KM/SDS/VIP"),
                       pattern="*.tif", full.names=TRUE)
filez.SD.GIMMS<- list.files(file.path("F:/Results/RESULTS-8KM/SDS/GIMMS"),
                           pattern="*.tif", full.names=TRUE)

All.Monthly.SD.VIP<-foreach(stackedSD=1:length(filez.SD.VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.SD.VIP[[stackedSD]])
All.Monthly.SD.GIMMS<-foreach(stackedSD=1:length(filez.SD.GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.SD.GIMMS[[stackedSD]])

result.stk.SD.VIP<-stack(All.Monthly.SD.VIP,bands=NULL, native=FALSE, RAT=TRUE)
result.stk.SD.GIMMS<-stack(All.Monthly.SD.GIMMS,bands=NULL, native=FALSE, RAT=TRUE)

#######################################################################
idx5 <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
result.stk.SD.VIP <- setZ(result.stk.SD.VIP, idx5)
names(result.stk.SD.VIP) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")
colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
mytheme<-rasterTheme(region = colors)
spplot(result.stk.SD.VIP,xlim = bbox(result.stk.SD.VIP)[1, ], 
       ylim = bbox(result.stk.SD.VIP)[2, ],
       scales = list(draw = FALSE), colorkey=TRUE,
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.atSD,
       main="Distribution of 15 Day NDVI-VIP Monthly SDS", cex.lab=10, 
       cex.axis=10, cex.main=10, cex.sub=10)
############################################################
# Distribution of SDS
##########################################################
idx6 <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
result.stk.SD.GIMMS <- setZ(result.stk.SD.GIMMS, idx6)
names(result.stk.SD.GIMMS) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")
colors=colorRampPalette(c(brewer.pal(9,"RdYlGn")))(50)
mytheme<-rasterTheme(region = colors)
spplot(result.stk.SD.GIMMS,xlim = bbox(result.stk.SD.GIMMS)[1, ], 
       ylim = bbox(result.stk.SD.GIMMS)[2, ],
       scales = list(draw = FALSE), colorkey=TRUE,
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.atSD,
       main="Distribution of 15 Day NDVI-GIMMS Monthly SDS", cex.lab=10, 
       cex.axis=10, cex.main=10, cex.sub=10)
###############################################################################################
#               LATITUDINAL GRAPHS
###############################################################################################
DF<-read.csv("F:/Results/RESULTS-8KM/LAT-DATA/DF_All_8KM.csv")
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
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  labs(title = expression("Jan")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Feb")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Mar")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70), 
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Apr")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("May")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Jun")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Jul")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70),
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Aug")) +       
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70),
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Sep")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Oct")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70),
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Nov")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70), 
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))

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
  labs(title = expression("Dec")) +
  theme(title=element_text(face="bold", color = "black", size = 20)) +
  scale_x_continuous(limits = c(-70,70),
                     breaks=c(-60,0,60)) +
  scale_y_continuous(limits = c(-0.1,1),
                     breaks=c(-0.1,0.5,1))
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
                         nrow=4,
                         clip = TRUE,
                         legend = NULL,
                         main=textGrob("Distribution of 15 Day Monthly Means",
                                       vjust = 1, 
                                       gp = gpar(fontface = "bold", cex = 1.5)),
                         left = textGrob("Latitude", rot = 90, vjust = 1, 
                                         gp = gpar(fontface = "bold",
                                                   cex = 1.5))))



