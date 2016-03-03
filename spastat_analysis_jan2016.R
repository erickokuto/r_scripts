
require(foreach); require(raster); require(plyr); require(foreign); require(INLA);
require(rgdal);require(rasterVis);require(gridExtra);require(ggplot2);require(latticeExtra);
require(hydroGOF);  require(latticeExtra); require(lattice); require(MODIS)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#    complementary shapefile data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(rworldmap)
data("countriesCoarse")

#Alternatively
require(maptools); require(rasterVis); require(RColorBrewer);
require(colorspace)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('F:/CLIM_DATA/Kenya_evi2_Records/kenyacounties/counties/counties.shp',
                          proj4string=proj)
plot(mapaSHP)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#    Goodness of fit test: Kolmogorov-Sminov Testing
#       smoother: Savitzky-Golay optimization method
#                      D statistic
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/ksTest"),
                      pattern="*.tif", full.names=TRUE)
raster_ks.test<-foreach(i=1:length(ks.test), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(ks.test[[i]])

stk.ks.test<-stack(raster_ks.test[[2]], 
                   raster_ks.test[[3]],
                   raster_ks.test[[1]],
                   raster_ks.test[[5]],
                   raster_ks.test[[6]],
                   raster_ks.test[[4]],
                   bands=NULL, native=FALSE, RAT=TRUE)

Names1 <- c("A", "B", "C","D", "E","F")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
my.at1 <-  seq(0, 0.5, 0.01)
cols1 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))

spplot(stk.ks.test, xlim = bbox(stk.ks.test)[1, ], 
       ylim = bbox(stk.ks.test)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols1(100),
       #par.settings = mytheme,
       sp.layout = list("sp.polygons", mapaSHP, col = "grey65"),
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
       contour=FALSE, layout=c(2, 3)) 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#            Goodness of fit test: Kolmogorov-Sminov Testing
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
slope.test <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/thielsen_slope"),
                         pattern="*.tif", full.names=TRUE)
raster_slope.test<-foreach(i=1:length(slope.test), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(slope.test[[i]])

stk.slope.test<-stack(raster_slope.test[[2]], 
                      raster_slope.test[[3]],
                      raster_slope.test[[1]],
                      raster_slope.test[[5]],
                      raster_slope.test[[6]],
                      raster_slope.test[[4]],
                      bands=NULL, native=FALSE, RAT=TRUE)
stk.slope.test = calc(stk.slope.test, fun=function(x){x * 0.1} )


Names1 <- c("A", "B", "C","D", "E","F")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
my.at2 <-  seq(-0.01, 0.02, 0.001)
cols2 <- colorRampPalette(brewer.pal(11, "BrBG"))

spplot(stk.slope.test, xlim = bbox(stk.slope.test)[1, ], 
       ylim = bbox(stk.slope.test)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols2(100),
       sp.layout = list("sp.polygons", mapaSHP, col = "grey65"),
       at=my.at2,
       strip=FALSE,
       main=list(""),
       #        panel = function(x, y, z,...) {
       #          panel.levelplot.raster(x, y, z,...)
       #          panel.text(x = -130, y = -40,
       #                     labels=Names1[panel.number()],
       #                     col.text = "black",
       #                     cex=1.5,fontface='bold')
       #        }, 
       contour=FALSE, layout=c(2, 3)) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Coefficient of efficiency
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
E.test <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/E"),
                     pattern="*.tif", full.names=TRUE)
raster_E.test<-foreach(i=1:length(E.test), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(E.test[[i]])

stk.E.test<-stack(raster_E.test[[4]], 
                  raster_E.test[[3]],
                  raster_E.test[[1]],
                  raster_E.test[[2]],
                  raster_E.test[[6]],
                  raster_E.test[[5]],
                  bands=NULL, native=FALSE, RAT=TRUE)
#stk.slope.test = calc(stk.slope.test, fun=function(x){x * 0.1} )


Names1 <- c("A", "B", "C","D", "E","F")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
my.at3 <-  seq(0.6, 1, 0.01)
cols3 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))

spplot(stk.E.test, xlim = bbox(stk.E.test)[1, ], 
       ylim = bbox(stk.E.test)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols3(100),
       sp.layout = list("sp.polygons", mapaSHP, col = "grey65"),
       at=my.at3,
       strip=FALSE,
       main=list(""),
       #        panel = function(x, y, z,...) {
       #          panel.levelplot.raster(x, y, z,...)
       #          panel.text(x = -130, y = -40,
       #                     labels=Names1[panel.number()],
       #                     col.text = "black",
       #                     cex=1.5,fontface='bold')
       #        }, 
       contour=FALSE, layout=c(2, 3)) 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Mean 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mean.test <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/mean"),
                        pattern="*.tif", full.names=TRUE)
raster_mean.test<-foreach(i=1:length(mean.test), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(mean.test[[i]])

stk.mean.test<-stack(raster_mean.test[[7]],
                     raster_mean.test[[4]], 
                     raster_mean.test[[3]],
                     raster_mean.test[[1]],
                     raster_mean.test[[2]],
                     raster_mean.test[[6]],
                     raster_mean.test[[5]],
                     bands=NULL, native=FALSE, RAT=TRUE)

Names1 <- c("A", "B", "C","D", "E","F")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
my.at6 <-  seq(-0.1, 0.4, 0.01)
cols6 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))

spplot(stk.mean.test, xlim = bbox(stk.mean.test)[1, ], 
       ylim = bbox(stk.mean.test)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols6(100),
       sp.layout = list("sp.polygons", mapaSHP, col = "grey65"),
       at=my.at6,
       strip=FALSE,
       main=list(""),
       #        panel = function(x, y, z,...) {
       #          panel.levelplot.raster(x, y, z,...)
       #          panel.text(x = -130, y = -40,
       #                     labels=Names1[panel.number()],
       #                     col.text = "black",
       #                     cex=1.5,fontface='bold')
       #        }, 
       contour=FALSE, layout=c(2, 4)) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# sd
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
sd.test <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/sd"),
                      pattern="*.tif", full.names=TRUE)
raster_sd.test<-foreach(i=1:length(sd.test), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(sd.test[[i]])

stk.sd.test<-stack(raster_sd.test[[7]],
                   raster_sd.test[[4]], 
                   raster_sd.test[[3]],
                   raster_sd.test[[1]],
                   raster_sd.test[[2]],
                   raster_sd.test[[6]],
                   raster_sd.test[[5]],
                   bands=NULL, native=FALSE, RAT=TRUE)

Names1 <- c("A", "B", "C","D", "E","F")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
my.at7 <-  seq(0, 0.2, 0.01)
cols7 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))

spplot(stk.sd.test, xlim = bbox(stk.sd.test)[1, ], 
       ylim = bbox(stk.sd.test)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols7(100),
       sp.layout = list("sp.polygons", mapaSHP, col = "grey65"),
       at=my.at7,
       strip=FALSE,
       main=list(""),
       #        panel = function(x, y, z,...) {
       #          panel.levelplot.raster(x, y, z,...)
       #          panel.text(x = -130, y = -40,
       #                     labels=Names1[panel.number()],
       #                     col.text = "black",
       #                     cex=1.5,fontface='bold')
       #        }, 
       contour=FALSE, layout=c(2, 4)) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Residual mean squared error
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rmse.test <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/rmse"),
                        pattern="*.tif", full.names=TRUE)
raster_rmse.test<-foreach(i=1:length(rmse.test), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(rmse.test[[i]])

stk.rmse.test<-stack(raster_rmse.test[[4]], 
                     raster_rmse.test[[3]],
                     raster_rmse.test[[1]],
                     raster_rmse.test[[2]],
                     raster_rmse.test[[6]],
                     raster_rmse.test[[5]],
                     bands=NULL, native=FALSE, RAT=TRUE)

Names1 <- c("A", "B", "C","D", "E","F")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
my.at4 <-  seq(0, 0.2, 0.01)
cols4 <- colorRampPalette(brewer.pal(9, 'YlOrRd'))

spplot(stk.rmse.test, xlim = bbox(stk.rmse.test)[1, ], 
       ylim = bbox(stk.rmse.test)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       col.regions = cols4(100),
       sp.layout = list("sp.polygons", mapaSHP, col = "grey65"),
       at=my.at4,
       strip=FALSE,
       main=list(""),
       #        panel = function(x, y, z,...) {
       #          panel.levelplot.raster(x, y, z,...)
       #          panel.text(x = -130, y = -40,
       #                     labels=Names1[panel.number()],
       #                     col.text = "black",
       #                     cex=1.5,fontface='bold')
       #        }, 
       contour=FALSE, layout=c(2, 3)) 

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