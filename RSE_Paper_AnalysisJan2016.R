
require(foreach); require(raster); require(plyr); require(foreign); require(INLA);
require(rgdal);require(rasterVis);require(gridExtra);require(ggplot2);require(latticeExtra);
require(hydroGOF);  require(latticeExtra); require(lattice); require(MODIS)

###############################################################################################
#               LATITUDINAL GRAPHS
###############################################################################################
rm(list=ls())
DF0<-read.csv("F:/Results_Dec2015/RSE_Revised_Results/Lat_Means/DF_EVI2_2015.csv")
DF=DF0
DF$Jan.spde.mean = apply(cbind(Jan.spde.mean,Jan.raw.mean), 1, mean)
DF$feb.spde.mean = apply(cbind(feb.spde.mean,feb.raw.mean), 1, mean)
DF$mar.spde.mean = apply(cbind(mar.spde.mean,mar.raw.mean), 1, mean)
DF$apr.spde.mean = apply(cbind(apr.spde.mean,apr.raw.mean), 1, mean)
DF$may.spde.mean = apply(cbind(may.spde.mean,may.raw.mean), 1, mean)
DF$jun.spde.mean = apply(cbind(jun.spde.mean,jun.raw.mean), 1, mean)
DF$jul.spde.mean = apply(cbind(jul.spde.mean,jul.raw.mean), 1, mean)
DF$aug.spde.mean = apply(cbind(aug.spde.mean,aug.raw.mean), 1, mean)
DF$sep.spde.mean = apply(cbind(sep.spde.mean,sep.raw.mean), 1, mean)
DF$oct.spde.mean = apply(cbind(oct.spde.mean,oct.raw.mean), 1, mean)
DF$nov.spde.mean = apply(cbind(nov.spde.mean,nov.raw.mean), 1, mean)
DF$dec.spde.mean = apply(cbind(dec.spde.mean,dec.raw.mean), 1, mean)  

##spde sd error
DF$Jan.spde.sd = Jan.spde.sd/sqrt(length(Jan.spde.sd[!is.na(Jan.spde.sd)]))
DF$feb.spde.sd = feb.spde.sd/sqrt(length(feb.spde.sd[!is.na(feb.spde.sd)]))
DF$mar.spde.sd = mar.spde.sd/sqrt(length(mar.spde.sd[!is.na(mar.spde.sd)]))
DF$apr.spde.sd = apr.spde.sd/sqrt(length(apr.spde.sd[!is.na(apr.spde.sd)]))
DF$may.spde.sd = may.spde.sd/sqrt(length(may.spde.sd[!is.na(may.spde.sd)]))
DF$jun.spde.sd = jun.spde.sd/sqrt(length(jun.spde.sd[!is.na(jun.spde.sd)]))
DF$jul.spde.sd = jul.spde.sd/sqrt(length(jul.spde.sd[!is.na(jul.spde.sd)]))
DF$aug.spde.sd = aug.spde.sd/sqrt(length(aug.spde.sd[!is.na(aug.spde.sd)]))
DF$sep.spde.sd = sep.spde.sd/sqrt(length(sep.spde.sd[!is.na(sep.spde.sd)]))
DF$oct.spde.sd = oct.spde.sd/sqrt(length(oct.spde.sd[!is.na(oct.spde.sd)]))
DF$nov.spde.sd = nov.spde.sd/sqrt(length(nov.spde.sd[!is.na(nov.spde.sd)]))
DF$dec.spde.sd = dec.spde.sd/sqrt(length(dec.spde.sd[!is.na(dec.spde.sd)]))

#sgolay sd error
DF$Jan.sgolay.sd = Jan.sgolay.sd/sqrt(length(Jan.sgolay.sd[!is.na(Jan.sgolay.sd)]))
DF$feb.sgolay.sd = feb.sgolay.sd/sqrt(length(feb.sgolay.sd[!is.na(feb.sgolay.sd)]))
DF$mar.sgolay.sd = mar.sgolay.sd/sqrt(length(mar.sgolay.sd[!is.na(mar.sgolay.sd)]))
DF$apr.sgolay.sd = apr.sgolay.sd/sqrt(length(apr.sgolay.sd[!is.na(apr.sgolay.sd)]))
DF$may.sgolay.sd = may.sgolay.sd/sqrt(length(may.sgolay.sd[!is.na(may.sgolay.sd)]))
DF$jun.sgolay.sd = jun.sgolay.sd/sqrt(length(jun.sgolay.sd[!is.na(jun.sgolay.sd)]))
DF$jul.sgolay.sd = jul.sgolay.sd/sqrt(length(jul.sgolay.sd[!is.na(jul.sgolay.sd)]))
DF$aug.sgolay.sd = aug.sgolay.sd/sqrt(length(aug.sgolay.sd[!is.na(aug.sgolay.sd)]))
DF$sep.sgolay.sd = sep.sgolay.sd/sqrt(length(sep.sgolay.sd[!is.na(sep.sgolay.sd)]))
DF$oct.sgolay.sd = oct.sgolay.sd/sqrt(length(oct.sgolay.sd[!is.na(oct.sgolay.sd)]))
DF$nov.sgolay.sd = nov.sgolay.sd/sqrt(length(nov.sgolay.sd[!is.na(nov.sgolay.sd)]))
DF$dec.sgolay.sd = dec.sgolay.sd/sqrt(length(dec.sgolay.sd[!is.na(dec.sgolay.sd)]))

#unsmoothed/raw data sd error
DF$Jan.raw.sd = Jan.raw.sd/sqrt(length(Jan.raw.sd[!is.na(Jan.raw.sd)]))
DF$feb.raw.sd = feb.raw.sd/sqrt(length(feb.raw.sd[!is.na(feb.raw.sd)]))
DF$mar.raw.sd = mar.raw.sd/sqrt(length(mar.raw.sd[!is.na(mar.raw.sd)]))
DF$apr.raw.sd = apr.raw.sd/sqrt(length(apr.raw.sd[!is.na(apr.raw.sd)]))
DF$may.raw.sd = may.raw.sd/sqrt(length(may.raw.sd[!is.na(may.raw.sd)]))
DF$jun.raw.sd = jun.raw.sd/sqrt(length(jun.raw.sd[!is.na(jun.raw.sd)]))
DF$jul.raw.sd = jul.raw.sd/sqrt(length(jul.raw.sd[!is.na(jul.raw.sd)]))
DF$aug.raw.sd = aug.raw.sd/sqrt(length(aug.raw.sd[!is.na(aug.raw.sd)]))
DF$sep.raw.sd = sep.raw.sd/sqrt(length(sep.raw.sd[!is.na(sep.raw.sd)]))
DF$oct.raw.sd = oct.raw.sd/sqrt(length(oct.raw.sd[!is.na(oct.raw.sd)]))
DF$nov.raw.sd = nov.raw.sd/sqrt(length(nov.raw.sd[!is.na(nov.raw.sd)]))
DF$dec.raw.sd = dec.raw.sd/sqrt(length(dec.raw.sd[!is.na(dec.raw.sd)]))

attach(DF)
names(DF)
###############################################################################################
#               OVERLAYING GRAPHS
###############################################################################################
#           TEST: LATITUDINAL MEANS
###############################################################################################
#           MEANS: expected bounds: raw data
###############################################################################################
ebraw1 <- aes(ymax = Jan.raw.mean + Jan.raw.sd, ymin = Jan.raw.mean - Jan.raw.sd)
ebraw2 <- aes(ymax = feb.raw.mean + feb.raw.sd, ymin = feb.raw.mean - feb.raw.sd)
ebraw3 <- aes(ymax = mar.raw.mean + mar.raw.sd, ymin = mar.raw.mean - mar.raw.sd)
ebraw4 <- aes(ymax = apr.raw.mean + apr.raw.sd, ymin = apr.raw.mean - apr.raw.sd)
ebraw5 <- aes(ymax = may.raw.mean + may.raw.sd, ymin = may.raw.mean - may.raw.sd)
ebraw6 <- aes(ymax = jun.raw.mean + jun.raw.sd, ymin = jun.raw.mean - jun.raw.sd)
ebraw7 <- aes(ymax = jul.raw.mean + jul.raw.sd, ymin = jul.raw.mean - jul.raw.sd)
ebraw8 <- aes(ymax = aug.raw.mean + aug.raw.sd, ymin = aug.raw.mean - aug.raw.sd)
ebraw9 <- aes(ymax = sep.raw.mean + sep.raw.sd, ymin = sep.raw.mean - sep.raw.sd)
ebraw10 <- aes(ymax = oct.raw.mean + oct.raw.sd, ymin = oct.raw.mean - oct.raw.sd)
ebraw11 <- aes(ymax = nov.raw.mean + nov.raw.sd, ymin = nov.raw.mean - nov.raw.sd)
ebraw12 <- aes(ymax = dec.raw.mean + dec.raw.sd, ymin = dec.raw.mean - dec.raw.sd)
# ######################################################################
#   sgolay
###############################################################
ebsgolay1 <- aes(ymax = Jan.sgolay.mean + Jan.sgolay.sd, ymin = Jan.sgolay.mean - Jan.sgolay.sd)
ebsgolay2 <- aes(ymax = feb.sgolay.mean + feb.sgolay.sd, ymin = feb.sgolay.mean - feb.sgolay.sd)
ebsgolay3 <- aes(ymax = mar.sgolay.mean + mar.sgolay.sd, ymin = mar.sgolay.mean - mar.sgolay.sd)
ebsgolay4 <- aes(ymax = apr.sgolay.mean + apr.sgolay.sd, ymin = apr.sgolay.mean - apr.sgolay.sd)
ebsgolay5 <- aes(ymax = may.sgolay.mean + may.sgolay.sd, ymin = may.sgolay.mean - may.sgolay.sd)
ebsgolay6 <- aes(ymax = jun.sgolay.mean + jun.sgolay.sd, ymin = jun.sgolay.mean - jun.sgolay.sd)
ebsgolay7 <- aes(ymax = jul.sgolay.mean + jul.sgolay.sd, ymin = jul.sgolay.mean - jul.sgolay.sd)
ebsgolay8 <- aes(ymax = aug.sgolay.mean + aug.sgolay.sd, ymin = aug.sgolay.mean - aug.sgolay.sd)
ebsgolay9 <- aes(ymax = sep.sgolay.mean + sep.sgolay.sd, ymin = sep.sgolay.mean - sep.sgolay.sd)
ebsgolay10 <- aes(ymax = oct.sgolay.mean + oct.sgolay.sd, ymin = oct.sgolay.mean - oct.sgolay.sd)
ebsgolay11 <- aes(ymax = nov.sgolay.mean + nov.sgolay.sd, ymin = nov.sgolay.mean - nov.sgolay.sd)
ebsgolay12 <- aes(ymax = dec.sgolay.mean + dec.sgolay.sd, ymin = dec.sgolay.mean - dec.sgolay.sd)
###############################################################################################
#           MEANS: expected bounds: whit data
###############################################################################################
# ebwhit1 <- aes(ymax = Jan.whit.mean + Jan.whit.sd, ymin = Jan.whit.mean - Jan.whit.sd)
# ebwhit2 <- aes(ymax = feb.whit.mean + feb.whit.sd, ymin = feb.whit.mean - feb.whit.sd)
# ebwhit3 <- aes(ymax = mar.whit.mean + mar.whit.sd, ymin = mar.whit.mean - mar.whit.sd)
# ebwhit4 <- aes(ymax = apr.whit.mean + apr.whit.sd, ymin = apr.whit.mean - apr.whit.sd)
# ebwhit5 <- aes(ymax = may.whit.mean + may.whit.sd, ymin = may.whit.mean - may.whit.sd)
# ebwhit6 <- aes(ymax = jun.whit.mean + jun.whit.sd, ymin = jun.whit.mean - jun.whit.sd)
# ebwhit7 <- aes(ymax = jul.whit.mean + jul.whit.sd, ymin = jul.whit.mean - jul.whit.sd)
# ebwhit8 <- aes(ymax = aug.whit.mean + aug.whit.sd, ymin = aug.whit.mean - aug.whit.sd)
# ebwhit9 <- aes(ymax = sep.whit.mean + sep.whit.sd, ymin = sep.whit.mean - sep.whit.sd)
# ebwhit10 <- aes(ymax = oct.whit.mean + oct.whit.sd, ymin = oct.whit.mean - oct.whit.sd)
# ebwhit11 <- aes(ymax = nov.whit.mean + nov.whit.sd, ymin = nov.whit.mean - nov.whit.sd)
# ebwhit12 <- aes(ymax = dec.whit.mean + dec.whit.sd, ymin = dec.whit.mean - dec.whit.sd)
###############################################################################################
#           MEANS: expected bounds: spde data
###############################################################################################
ebspde1 <- aes(ymax = Jan.spde.mean + Jan.spde.sd, ymin = Jan.spde.mean - Jan.spde.sd)
ebspde2 <- aes(ymax = feb.spde.mean + feb.spde.sd, ymin = feb.spde.mean - feb.spde.sd)
ebspde3 <- aes(ymax = mar.spde.mean + mar.spde.sd, ymin = mar.spde.mean - mar.spde.sd)
ebspde4 <- aes(ymax = apr.spde.mean + apr.spde.sd, ymin = apr.spde.mean - apr.spde.sd)
ebspde5 <- aes(ymax = may.spde.mean + may.spde.sd, ymin = may.spde.mean - may.spde.sd)
ebspde6 <- aes(ymax = jun.spde.mean + jun.spde.sd, ymin = jun.spde.mean - jun.spde.sd)
ebspde7 <- aes(ymax = jul.spde.mean + jul.spde.sd, ymin = jul.spde.mean - jul.spde.sd)
ebspde8 <- aes(ymax = aug.spde.mean + aug.spde.sd, ymin = aug.spde.mean - aug.spde.sd)
ebspde9 <- aes(ymax = sep.spde.mean + sep.spde.sd, ymin = sep.spde.mean - sep.spde.sd)
ebspde10 <- aes(ymax = oct.spde.mean + oct.spde.sd, ymin = oct.spde.mean - oct.spde.sd)
ebspde11 <- aes(ymax = nov.spde.mean + nov.spde.sd, ymin = nov.spde.mean - nov.spde.sd)
ebspde12 <- aes(ymax = dec.spde.mean + dec.spde.sd, ymin = dec.spde.mean - dec.spde.sd)
###############################################################################################
#              DISTRIBUTION OF MEANS
###############################################################################################
require(gridExtra)
names(DF)
pJan <- ggplot(DF, aes(Latitude))
pJan <- pJan + geom_ribbon(ebraw1, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=Jan.raw.mean), colour="royalblue4")
pJan <- pJan + geom_ribbon(ebsgolay1, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=Jan.sgolay.mean), colour="springgreen4") 
pJan <- pJan + geom_ribbon(ebspde1, alpha = 0.5, fill='indianred1') + geom_line(aes(y=Jan.spde.mean), colour="indianred4") + 
  coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank(size=20,face="bold")) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=0.1, label="Jan",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pFeb <- ggplot(DF, aes(Latitude))
pFeb <- pFeb + geom_ribbon(ebraw2, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=feb.raw.mean), colour='royalblue4')
pFeb <- pFeb + geom_ribbon(ebsgolay2, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=feb.sgolay.mean), colour='springgreen4') 
pFeb <- pFeb + geom_ribbon(ebspde2, alpha = 0.5, fill='indianred1') + geom_line(aes(y=feb.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=0.1, label="Feb",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))


pMar<- ggplot(DF, aes(Latitude))
pMar <- pMar + geom_ribbon(ebraw3, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=mar.raw.mean), colour='royalblue4')
pMar <- pMar + geom_ribbon(ebsgolay3, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=mar.sgolay.mean), colour='springgreen4')
pMar <- pMar + geom_ribbon(ebspde3, alpha = 0.5, fill='indianred1') + geom_line(aes(y=mar.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=0.1, label="Mar",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pApr <- ggplot(DF, aes(Latitude))
pApr <- pApr + geom_ribbon(ebraw4, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=mar.raw.mean), colour='royalblue4')
pApr <- pApr + geom_ribbon(ebsgolay4, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=mar.sgolay.mean), colour='springgreen4') 
pApr <- pApr + geom_ribbon(ebspde4, alpha = 0.5, fill='indianred1') + geom_line(aes(y=mar.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,-0.5,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=0.1, label="Apr",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pMay <- ggplot(DF, aes(Latitude))
pMay <- pMay + geom_ribbon(ebraw5, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=may.raw.mean), colour='royalblue4')
pMay <- pMay + geom_ribbon(ebsgolay5, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=may.sgolay.mean), colour='springgreen4')
pMay <- pMay + geom_ribbon(ebspde5, alpha = 0.5, fill='indianred1') + geom_line(aes(y=may.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=0.1, label="May",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pJun <- ggplot(DF, aes(Latitude))
pJun <- pJun + geom_ribbon(ebraw6, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=jun.raw.mean), colour='royalblue4')
pJun <- pJun + geom_ribbon(ebsgolay6, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=jun.sgolay.mean), colour='springgreen4') 
pJun <- pJun + geom_ribbon(ebspde6, alpha = 0.5, fill='indianred1') + geom_line(aes(y=jun.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,-0.5,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=0.1, label="Jun",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pJul <- ggplot(DF, aes(Latitude))
pJul <- pJul + geom_ribbon(ebraw7, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=jul.raw.mean), colour='royalblue4')
pJul <- pJul + geom_ribbon(ebsgolay7, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=jul.sgolay.mean), colour='springgreen4')
pJul <- pJul + geom_ribbon(ebspde7, alpha = 0.5, fill='indianred1') + geom_line(aes(y=jul.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=0.1, label="Jul",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pAug <- ggplot(DF, aes(Latitude))
pAug <- pAug + geom_ribbon(ebraw8, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=aug.raw.mean), colour='royalblue4')
pAug <- pAug + geom_ribbon(ebsgolay8, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=aug.sgolay.mean), colour='springgreen4') 
pAug <- pAug + geom_ribbon(ebspde8, alpha = 0.5, fill='indianred1') + geom_line(aes(y=aug.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,-0.5,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=0.1, label="Aug",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))


pSep <- ggplot(DF, aes(Latitude))
pSep <- pSep + geom_ribbon(ebraw9, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=sep.raw.mean), colour='royalblue4')
pSep <- pSep + geom_ribbon(ebsgolay9, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=sep.sgolay.mean), colour='springgreen4')
pSep <- pSep + geom_ribbon(ebspde9, alpha = 0.5, fill='indianred1') + geom_line(aes(y=sep.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=0.1, label="Sep",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pOct <- ggplot(DF, aes(Latitude))
pOct <- pOct + geom_ribbon(ebraw10, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=oct.raw.mean), colour='royalblue4')
pOct <- pOct + geom_ribbon(ebsgolay10, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=oct.sgolay.mean), colour='springgreen4')  
pOct <- pOct + geom_ribbon(ebspde10, alpha = 0.5, fill='indianred1') + geom_line(aes(y=oct.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,-0.5,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=0.1, label="Oct",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pNov <- ggplot(DF, aes(Latitude))
pNov <- pNov + geom_ribbon(ebraw11, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=nov.raw.mean), colour='royalblue4')
pNov <- pNov + geom_ribbon(ebsgolay11, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=nov.sgolay.mean), colour='springgreen4') 
pNov <- pNov + geom_ribbon(ebspde11, alpha = 0.5, fill='indianred1') + geom_line(aes(y=nov.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=0.1, label="Nov",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

pDec <- ggplot(DF, aes(Latitude))
pDec <- pDec + geom_ribbon(ebraw12, alpha = 0.5, fill='royalblue1') + geom_line(aes(y=dec.raw.mean), colour='royalblue4')
pDec <- pDec + geom_ribbon(ebsgolay12, alpha = 0.5, fill='springgreen1') + geom_line(aes(y=dec.sgolay.mean), colour='springgreen4')
pDec <- pDec + geom_ribbon(ebspde12, alpha = 0.5, fill='indianred1') + geom_line(aes(y=dec.spde.mean), colour='indianred4') + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,1,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=0.1, label="Dec",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(0, 0.8),
                     breaks=c(0.1, 0.4, 0.6))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#    complementary shapefile data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(rworldmap)
data("countriesCoarse")

#Alternatively
require(maptools); require(rasterVis); require(RColorBrewer);
require(colorspace)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('F:/Results_Dec2015/RSE_Revised_Results/World_shapefile/world_admin_1998_countries.shp',
                          proj4string=proj)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#    Goodness of fit test: Kolmogorov-Sminov Testing
#       smoother: Savitzky-Golay optimization method
#                      D statistic
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.sgolay <- list.files(file.path("F:/Results_Dec2015/RSE_Revised_Results/ks_test_results/sgolay/D_stat_masked"),
                             pattern="*.tif", full.names=TRUE)
raster_ks.test.sgolay<-foreach(i=1:length(ks.test.sgolay), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(ks.test.sgolay[[i]])

stk.ks.test.sgolay<-stack(raster_ks.test.sgolay[[5]], 
                          raster_ks.test.sgolay[[4]],
                          raster_ks.test.sgolay[[8]],
                          raster_ks.test.sgolay[[1]],
                          raster_ks.test.sgolay[[9]],
                          raster_ks.test.sgolay[[7]],
                          raster_ks.test.sgolay[[6]],
                          raster_ks.test.sgolay[[2]],
                          raster_ks.test.sgolay[[12]],
                          raster_ks.test.sgolay[[11]],
                          raster_ks.test.sgolay[[10]],
                          raster_ks.test.sgolay[[3]],
                          bands=NULL, native=FALSE, RAT=TRUE)

Names1 <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
#mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at1 <-  seq(0, 1, 0.01)
cols1 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))

spplot(stk.ks.test.sgolay, xlim = bbox(stk.ks.test.sgolay)[1, ], 
       ylim = bbox(stk.ks.test.sgolay)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols1(100),
       #par.settings = mytheme,
       sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"),
       at=my.at1,
       strip=FALSE,
       main=list(""),
#        panel = function(x, y, z,...) {
#          panel.levelplot.raster(x, y, z,...)
#          panel.text(x = -130, y = -40,
#                     labels=Names1[panel.number()],
#                     col.text = "black",
#                     cex=1.5,fontface='bold')
#        }, 
contour=FALSE, layout=c(2, 6)) 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#            Goodness of fit test: Kolmogorov-Sminov Testing
#                          smoother: spde method 
#                                 D statistic
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.spde <- list.files(file.path("F:/Results_Dec2015/RSE_Revised_Results/ks_test_results/spde/D_stat_masked"),
                           pattern="*.tif", full.names=TRUE)
raster_ks.test.spde<-foreach(i=1:length(ks.test.spde), .combine=c, .verbose=FALSE) %do%
  raster(ks.test.spde[[i]])

stk.raster_ks.test.spde<-stack(raster_ks.test.spde[[5]], 
                               raster_ks.test.spde[[4]],
                               raster_ks.test.spde[[8]],
                               raster_ks.test.spde[[1]],
                               raster_ks.test.spde[[9]],
                               raster_ks.test.spde[[7]],
                               raster_ks.test.spde[[6]],
                               raster_ks.test.spde[[2]],
                               raster_ks.test.spde[[12]],
                               raster_ks.test.spde[[11]],
                               raster_ks.test.spde[[10]],
                               raster_ks.test.spde[[3]],
                               bands=NULL, native=FALSE, RAT=TRUE)

Names2 <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
cols2 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))
my.at2 <-  seq(0, 1, 0.01)

spplot(stk.raster_ks.test.spde, xlim = bbox(stk.raster_ks.test.spde)[1, ], 
       ylim = bbox(stk.raster_ks.test.spde)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols2(100),
       sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"),
       at=my.at2,
       strip=FALSE,
       main=list(""),
#        panel = function(x, y, z,...) {
#          panel.levelplot.raster(x, y, z,...)
#          panel.text(x = -130, y = -40,
#                     labels=Names2[panel.number()],
#                     col.text = "black",
#                     cex=1.5,fontface='bold')
#        }, 
contour=FALSE, layout=c(2, 6)) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#                    Trend Analysis: ThielSen slope estimate
#                       smoother: sgolay and spde methods
#                             slope estimates 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ThielSen.test<- list.files(file.path("F:/Results_Dec2015/RSE_Revised_Results/ThielSenOutputs/Masked_TheilSenSlopes"),
                           pattern="*.tif", full.names=TRUE)
raster_ThielSen.test<-foreach(i=1:length(ThielSen.test), .combine=c, .verbose=FALSE) %do%
  raster(ThielSen.test[[i]])
stk.ThielSen.test<-stack(raster_ThielSen.test, bands=NULL, native=FALSE, RAT=TRUE)

# require(raster)
# fcat=function(x){ifelse(x < -0.004, 1, 
#                          ifelse(x >= -0.004 & x < -0.002, 2, 
#                                ifelse(x >= -0.002 & x < 0, 3,
#                                       ifelse(x >= 0 & x < 0.002, 4,
#                                              ifelse(x >= 0.002 & x < 0.004, 5,
#                                         ifelse(x >= 0.004, 6, NA))))))
# } 
# 
# trend.cat.sgolay=calc(stk.ThielSen.test[[1]], fcat)
# trend.rat.sgolay <- ratify(trend.cat.sgolay)
# rat1 <- levels(trend.rat.sgolay)[[1]]
# rat1$Trend <- c('< -0.004', '-0.004 to < -0.002', 
#                '-0.002 to <0', '0 to <0.002', 
#                '0.002 to <0.004', '>=0.004')
# rat1$class <- c('A', 'B', 'C', 'D', 'E', 'F')
# levels(trend.rat.sgolay) <- rat1
# trend.rat.sgolay
# 
# 
# trend.cat.spde=calc(stk.ThielSen.test[[2]], fcat)
# trend.rat.spde <- ratify(trend.cat.spde)
# rat2 <- levels(trend.rat.spde)[[2]]
# rat2$Trend <- c('< -0.004', '-0.004 to < -0.002', 
#                 '-0.002 to <0', '0 to <0.002', 
#                 '0.002 to <0.004', '>=0.004')
# rat2$class <- c('A', 'B', 'C', 'D', 'E', 'F')
# levels(trend.rat.spde) <- rat2
# trend.rat.spde
# 
# p1.new=levelplot(trend.rat.sgolay, col.regions=c('tan4', 'tan3', 'tan2', 'greenyellow', 'lightgreen', 'green4'))
# p1.new + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))
# 
# 
# 

Names3 <- c("A", "B")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
require(RColorBrewer); require(gimms)
cols3 <- colorRampPalette(brewer.pal(11, "BrBG"))
my.at3 <-  seq(-0.04, 0.04, 0.01)

require(sp); require(maps); require(maptools); require(mapdata);
require(latticeExtra); require(fields)

spplot(stk.ThielSen.test, xlim = bbox(stk.ThielSen.test)[1, ], 
       ylim = bbox(stk.ThielSen.test)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols3(100),
       sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"),
       at=my.at3,
       strip=FALSE,
       main=list(""),
#        panel = function(x, y, z,...) {
#          panel.levelplot.raster(x, y, z,...)
#          panel.text(x = -130, y = -40,
#                     labels=Names3[panel.number()],
#                     col.text = "black",
#                     cex=1.5,fontface='bold')
#        }, 
contour=FALSE, layout=c(1, 2)) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#          Percentage data available before smoothing
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
obs.used.RSE <- list.files(file.path("F:/Results_Dec2015/RSE_Revised_Results/obs.used"),
                           pattern="*.tif", full.names=TRUE)
raster_obs.used.RSE<-foreach(i=1:length(obs.used.RSE), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(obs.used.RSE[[i]])
stk.obs.used.RSE<-stack(raster_obs.used.RSE[[5]], 
                        raster_obs.used.RSE[[4]],
                        raster_obs.used.RSE[[8]],
                        raster_obs.used.RSE[[1]],
                        raster_obs.used.RSE[[9]],
                        raster_obs.used.RSE[[7]],
                        raster_obs.used.RSE[[6]],
                        raster_obs.used.RSE[[2]],
                        raster_obs.used.RSE[[12]],
                        raster_obs.used.RSE[[11]],
                        raster_obs.used.RSE[[10]],
                        raster_obs.used.RSE[[3]],
                        bands=NULL, native=FALSE, RAT=TRUE)


require(RColorBrewer)
Names4 <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
my.at4 <-  seq(1, 100, 1)
cols2 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))

spplot(stk.obs.used.RSE, xlim = bbox(stk.obs.used.RSE)[1, ], 
       ylim = bbox(stk.obs.used.RSE)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE),
       col.regions = cols2(100),
       sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"),
       at=my.at4,
       strip=FALSE,
       main=list(""),
#        panel = function(x, y, z,...) {
#          panel.levelplot.raster(x, y, z,...)
#          panel.text(x = -130, y = -40,
#                     labels=Names4[panel.number()],
#                     col.text = "black",
#                     cex=1.5,fontface='bold')
#        }, 
contour=FALSE, layout=c(2, 6)) 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>