

source("Phenology_Functions_Erick.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Record without gaps both using INLA and spDTyn
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(raster)
require(foreach)
require(INLA)
require(hydromad)
require(zoo)
require(dlm)
require(R2BayesX)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Spatiotemporal data analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#measures of central tendencies
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
list.mean<- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means"),
                       pattern="*.tif", full.names=TRUE)

rasters.mean<-foreach(filez=1:length(list.mean), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(list.mean[[filez]])

stk.mean<-stack(rasters.mean)

kenya.shp=readShapePoly("F:/CLIM_DATA/kenya_border_shapefile/kenya.shp")

plot(kenya.shp)
## cropping KENYA region
stk.kenyamean0 <- crop(stk.mean, extent(kenya.shp))
stk.kenyamean1 <- mask(stk.kenyamean0, kenya.shp)

writeRaster(stk.kenyamean1[[1]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/mean/DLM_kalman_mean.tif")
writeRaster(stk.kenyamean1[[2]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/mean/DLM_spde_mean.tif")
writeRaster(stk.kenyamean1[[3]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/mean/non_DLM_SG_mean.tif")
writeRaster(stk.kenyamean1[[4]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/mean/non_Dlm_spde_mean.tif")
writeRaster(stk.kenyamean1[[5]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/mean/sptemporal_spde_mean.tif")
writeRaster(stk.kenyamean1[[6]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/mean/sptemporal_spDTyn_mean.tif")
writeRaster(stk.kenyamean1[[7]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/mean/unsmoothed_mean.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#measures of variability
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
list.sd<- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd"),
                       pattern="*.tif", full.names=TRUE)

rasters.sd<-foreach(filez=1:length(list.sd), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(list.sd[[filez]])

stk.sd<-stack(rasters.sd)

kenya.shp=readShapePoly("F:/CLIM_DATA/kenya_border_shapefile/kenya.shp")

plot(kenya.shp)
## cropping KENYA region
stk.kenyasd0 <- crop(stk.sd, extent(kenya.shp))
stk.kenyasd1 <- mask(stk.kenyasd0, kenya.shp)

writeRaster(stk.kenyasd1[[1]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/sd/DLM_kalman_sd.tif")
writeRaster(stk.kenyasd1[[2]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/sd/DLM_spde_sd.tif")
writeRaster(stk.kenyasd1[[3]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/sd/non_DLM_SG_sd.tif")
writeRaster(stk.kenyasd1[[4]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/sd/non_Dlm_spde_sd.tif")
writeRaster(stk.kenyasd1[[5]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/sd/sptemporal_spde_sd.tif")
writeRaster(stk.kenyasd1[[6]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/sd/sptemporal_spDTyn_sd.tif")
writeRaster(stk.kenyasd1[[7]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/sd/unsmoothed_sd.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Coefficient of efficiency
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
list.E<- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E"),
                    pattern="*.tif", full.names=TRUE)

rasters.E<-foreach(filez=1:length(list.E), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(list.E[[filez]])

stk.E<-stack(rasters.E)

kenya.shp=readShapePoly("F:/CLIM_DATA/kenya_border_shapefile/kenya.shp")

plot(kenya.shp)
## cropping KENYA region
stk.kenyaE0 <- crop(stk.E, extent(kenya.shp))
stk.kenyaE1 <- mask(stk.kenyaE0, kenya.shp)

writeRaster(stk.kenyaE1[[1]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/E_DLM_kalman.tif")
writeRaster(stk.kenyaE1[[2]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/E/E_DLM_spde.tif")
writeRaster(stk.kenyaE1[[3]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/E/E_non_DLM_sgolay.tif")
writeRaster(stk.kenyaE1[[4]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/E/E_non_DLM_spde.tif")
writeRaster(stk.kenyaE1[[5]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/E/E_sp_spde.tif")
writeRaster(stk.kenyaE1[[6]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/E/E_sp_spDTyn.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Root mean squared error analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
list.rmse<- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse"),
                       pattern="*.tif", full.names=TRUE)

rasters.rmse<-foreach(filez=1:length(list.rmse), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(list.rmse[[filez]])

stk.rmse<-stack(rasters.rmse)

kenya.shp=readShapePoly("F:/CLIM_DATA/kenya_border_shapefile/kenya.shp")

plot(kenya.shp)
## cropping KENYA region
stk.kenyarmse0 <- crop(stk.rmse, extent(kenya.shp))
stk.kenyarmse1 <- mask(stk.kenyarmse0, kenya.shp)

writeRaster(stk.kenyarmse1[[1]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/rmse/rmse_DLM_kalman.tif")
writeRaster(stk.kenyarmse1[[2]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/rmse/rmse_DLM_spde.tif")
writeRaster(stk.kenyarmse1[[3]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/rmse/rmse_non_DLM_sgolay.tif")
writeRaster(stk.kenyarmse1[[4]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/rmse/rmse_non_DLM_spde.tif")
writeRaster(stk.kenyarmse1[[5]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/rmse/rmse_sp_spde.tif")
writeRaster(stk.kenyarmse1[[6]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/rmse/rmse_sp_spDTyn.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Thielsen slope
#>>>>>>>>>>>>>>>>>>>
list.slope <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/TheilSenSlope/slopeEst"),
                         pattern="*.tif", full.names=TRUE)
rasters.slope<-foreach(filez=1:length(list.slope), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(list.slope[[filez]])
stk.slope <- stack(rasters.slope)

# Thielsen slope significance
list.slope.sig <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/TheilSenSlope/slopeSig"),
                             pattern="*.tif", full.names=TRUE)
rasters.slope.sig<-foreach(filez=1:length(list.slope.sig), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(list.slope.sig[[filez]])
stk.slope.sig <- stack(rasters.slope.sig)

#Mask thielsen slope for significance
stk.slope.sig[stk.slope.sig >= 0.05] <- NA
stk.slope.masked   <- mask(stk.slope, stk.slope.sig )
plot(stk.slope.masked[[1]])
plot(stk.slope.masked[[2]])
plot(stk.slope.masked[[3]])
plot(stk.slope.masked[[4]])
plot(stk.slope.masked[[5]])
plot(stk.slope.masked[[6]])


writeRaster(stk.slope.masked[[1]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/thielsen_slope/slope_dlm_KalmanFilter.tif")
writeRaster(stk.slope.masked[[2]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/thielsen_slope/slope_nonDLMspde.TheiSen.tif")
writeRaster(stk.slope.masked[[3]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/thielsen_slope/slope_sgolay.TheiSen.tif")
writeRaster(stk.slope.masked[[4]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/thielsen_slope/slope_spatiotemp_spde.TheiSen.tif")
writeRaster(stk.slope.masked[[5]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/thielsen_slope/slope_spdeDLM.TheiSen.tif")
writeRaster(stk.slope.masked[[6]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/thielsen_slope/slope_spDTyn.TheiSen.tif")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Goodness of fit test with kolmogorov-smirnov test
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
list.ks <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/KS.test/KS_D_Est"),
                      pattern="*.tif", full.names=TRUE)
rasters.ks<-foreach(filez=1:length(list.ks), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(list.ks[[filez]])
stk.ks <- stack(rasters.ks)

# ks test significance
list.ks.sig <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/KS.test/KS_D_Sig"),
                          pattern="*.tif", full.names=TRUE)
rasters.ks.sig<-foreach(filez=1:length(list.ks.sig), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(list.ks.sig[[filez]])
stk.ks.sig <- stack(rasters.slope.sig)

#Mask thielsen slope for significance
stk.ks.sig[stk.ks.sig <= 0.05] <- NA
stk.ks.masked   <- mask(stk.ks, stk.ks.sig )
plot(stk.ks.masked[[1]])
plot(stk.ks.masked[[2]])
plot(stk.ks.masked[[3]])
plot(stk.ks.masked[[4]])
plot(stk.ks.masked[[5]])
plot(stk.ks.masked[[6]])


writeRaster(stk.ks.masked[[1]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/ksTest/Distance_dlm_KalmanFilter.tif")
writeRaster(stk.ks.masked[[2]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/ksTest/Distance_KS_Test.nonDLMspde.tif")
writeRaster(stk.ks.masked[[3]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/ksTest/Distance_KS_Test.sgolay.tif")
writeRaster(stk.ks.masked[[4]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/ksTest/Distance_KS_Test.spatiotemp_spde.tif")
writeRaster(stk.ks.masked[[5]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/ksTest/Distance_KS_Test.spdeDLM.tif")
writeRaster(stk.ks.masked[[6]], 
            "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Revised_results/ksTest/Distance_KS_Test.spDTyn.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Really Data with gaps
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Real_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataObs"),
                              pattern="*.tif", full.names=TRUE)
Real_evi2_rasters<-foreach(files=1:length(Real_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(Real_evi2_lists[[files]])
Real_stk.Data.evi2<-stack(Real_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
Real_stk.Data.evi2[Real_stk.Data.evi2 == 255] <- NA
writeRaster(Real_stk.Data.evi2, "F:/CLIM_DATA/tranzoia_county/other Results/rasterImages/stkRealUnsmoothed.tif")
plot(Real_stk.Data.evi2[[2]])

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Simulated Records with gaps
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Sim_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataObs"),
                             pattern="*.tif", full.names=TRUE)
Sim_evi2_rasters<-foreach(files=1:length(Sim_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(Sim_evi2_lists[[files]])
Sim_stk.Data.evi2<-stack(Sim_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
Sim_stk.Data.evi2[Sim_stk.Data.evi2 == 255] <- NA
writeRaster(Sim_stk.Data.evi2, "F:/CLIM_DATA/tranzoia_county/other Results/rasterImages/stkSimUnsmoothed.tif")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Records gap fillied using INLA package
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
inla_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataFitted"),
                              pattern="*.tif", full.names=TRUE)
inla_evi2_rasters<-foreach(files=1:length(inla_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(inla_evi2_lists[[files]])
inla_stk.Data.evi2<-stack(inla_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
inla_stk.Data.evi2[inla_stk.Data.evi2 == 255] <- NA
writeRaster(inla_stk.Data.evi2, "F:/CLIM_DATA/tranzoia_county/other Results/rasterImages/stkReal_inla_smoothed.tif")
plot(inla_stk.Data.evi2[10])

#>>>>>>>>INLA with simulated data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
inla_evi2_lists22 <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/SimData/SimDataFitted"),
                                pattern="*.tif", full.names=TRUE)
inla_evi2_rasters22<-foreach(files=1:length(inla_evi2_lists22), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(inla_evi2_lists22[[files]])
inla_stk.Data.evi22<-stack(inla_evi2_rasters22, bands=NULL, native=FALSE, RAT=TRUE)
inla_stk.Data.evi22[inla_stk.Data.evi22 == 255] <- NA
writeRaster(inla_stk.Data.evi22, "F:/CLIM_DATA/tranzoia_county/other Results/rasterImages/stkSim_inla_smoothed.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
spDTyn_evi2_lists1 <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/RealDataFitted"),
                                 pattern="*.tif", full.names=TRUE)
spDTyn_evi2_rasters1<-foreach(files=1:length(spDTyn_evi2_lists1), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(spDTyn_evi2_lists1[[files]])
spDTyn_stk.Data.evi2Pred<-stack(spDTyn_evi2_rasters1, bands=NULL, native=FALSE, RAT=TRUE)
spDTyn_stk.Data.evi2Pred[spDTyn_stk.Data.evi2Pred == 255] <- NA
writeRaster(spDTyn_stk.Data.evi2Pred, "F:/CLIM_DATA/tranzoia_county/other Results/rasterImages/stkReal_spDTyn_smoothed.tif")

#>>>>>>>>spDTyn with simulated data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
spDTyn_evi2_lists2 <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/SimDataFitted"),
                                 pattern="*.tif", full.names=TRUE)
spDTyn_evi2_rasters2<-foreach(files=1:length(spDTyn_evi2_lists2), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(spDTyn_evi2_lists2[[files]])
spDTyn_stk.Data.evi2Pred2<-stack(spDTyn_evi2_rasters2, bands=NULL, native=FALSE, RAT=TRUE)
spDTyn_stk.Data.evi2Pred2[spDTyn_stk.Data.evi2Pred2 == 255] <- NA
writeRaster(spDTyn_stk.Data.evi2Pred2, "F:/CLIM_DATA/tranzoia_county/other Results/rasterImages/stkSim_spDTyn_smoothed.tif")

#Observation used
obsUsed=obs.used.fun(stk=Real_stk.Data.evi2)
writeRaster(obsUsed, "F:/CLIM_DATA/tranzoia_county/other Results/Results/obsUsed_tranzoia.tif")
levelplot(obsUsed[[1]])
plot(Real_stk.Data.evi2[[4]])

#kenya records percentage observation used
Real_evi2_lists <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/kenya_10kmclipped"),
                              pattern="*.tif", full.names=TRUE)
Real_evi2_rasters<-foreach(files=1:length(Real_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(Real_evi2_lists[[files]])
Real_stk.Data.evi2<-stack(Real_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
obsUsed=obs.used.fun(stk=Real_stk.Data.evi2)
writeRaster(obsUsed, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Obs.used/obsUsed_kenya.tif")
require(maptools)
proj <- CRS('+proj=longlat +ellps=WGS84')
##Change to your folder
mapaSHP <- readShapeLines('F:/CLIM_DATA/Kenya_evi2_Records/kenyacounties/counties/counties.shp', proj4string=proj)

p <- spplot(obsUsed, par.settings=BuRdTheme)
p + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))

#The analysis or E and RMSE
E_With_INLA_Fitted=r2.function2(obs.stk=Real_stk.Data.evi2, sim.stk=inla_stk.Data.evi2)
writeRaster(E_With_INLA_Fitted, "F:/CLIM_DATA/tranzoia_county/other Results/Results/E_With_INLA_smoothed_RealData.tif")
levelplot(E_With_INLA_Fitted)


E_With_spDTyn_Fitted=r2.function2(obs.stk=Real_stk.Data.evi2, sim.stk=spDTyn_stk.Data.evi2Pred)
writeRaster(E_With_spDTyn_Fitted, "F:/CLIM_DATA/tranzoia_county/other Results/Results/E_With_spDTyn_smoothed_RealData.tif")
levelplot(E_With_spDTyn_Fitted)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
E_With_INLA_Fitted_Sim=r2.function2(obs.stk=Sim_stk.Data.evi2, sim.stk=inla_stk.Data.evi22)
writeRaster(E_With_INLA_Fitted_Sim, "F:/CLIM_DATA/tranzoia_county/other Results/Results/E_With_INLA_smoothed__SimData.tif")
levelplot(E_With_INLA_Fitted_Sim)

E_With_spDTyn_Fitted_Sim=r2.function2(obs.stk=Sim_stk.Data.evi2, sim.stk=spDTyn_stk.Data.evi2Pred2)
writeRaster(E_With_spDTyn_Fitted_Sim, "F:/CLIM_DATA/tranzoia_county/other Results/Results/E_With_spDTyn_smoothed_SimData.tif")
levelplot(E_With_spDTyn_Fitted_Sim)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#RMSE
RMSE_With_INLA_Fitted=funrmse(obs.stk=Real_stk.Data.evi2, sim.stk=inla_stk.Data.evi2)
writeRaster(RMSE_With_INLA_Fitted, "F:/CLIM_DATA/tranzoia_county/other Results/Results/RMSE_With_INLA_smoothed_RealData.tif")
spplot(RMSE_With_INLA_Fitted)

RMSE_With_spDTyn_Fitted=funrmse(obs.stk=Real_stk.Data.evi2, sim.stk=spDTyn_stk.Data.evi2Pred)
writeRaster(RMSE_With_spDTyn_Fitted, "F:/CLIM_DATA/tranzoia_county/other Results/Results/RMSE_With_spDTyn_smoothed_RealData.tif")
spplot(RMSE_With_spDTyn_Fitted)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
RMSE_With_INLA_Fitted_Sim=funrmse(obs.stk=Sim_stk.Data.evi2, sim.stk=inla_stk.Data.evi22)
writeRaster(RMSE_With_INLA_Fitted_Sim, "F:/CLIM_DATA/tranzoia_county/other Results/Results/RMSE_With_INLA_smoothed__SimData.tif")
spplot(RMSE_With_INLA_Fitted_Sim)

RMSE_With_spDTyn_Fitted_Sim=funrmse(obs.stk=Sim_stk.Data.evi2, sim.stk=spDTyn_stk.Data.evi2Pred2)
writeRaster(RMSE_With_spDTyn_Fitted_Sim, "F:/CLIM_DATA/tranzoia_county/other Results/Results/RMSE_With_spDTyn_smoothed_SimData.tif")
spplot(RMSE_With_spDTyn_Fitted_Sim)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ploting characters with rasterVis package
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(maptools)
proj <- CRS('+proj=longlat +ellps=WGS84')
##Change to your folder
mapaSHP <- readShapeLines('F:/CLIM_DATA/tranzoia_county/trans.shp', proj4string=proj)

p <- spplot(RMSE_With_INLA_Fitted, par.settings=BuRdTheme)
p + layer(sp.lines(mapaSHP, lwd=0.8, col='darkgray'))
#>>>>>>>>>
library(gridExtra)
p1 <- levelplot(RMSE_With_INLA_Fitted)
p2 <- levelplot(RMSE_With_spDTyn_Fitted)
grid.arrange(p1, p2, ncol=2)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Latitudinal means and standard deviations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
DF<-read.csv("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Lat_means/kenya_lat_evi2.csv")
attach(DF)
names(DF)
require(ggplot2)
eb_raw<- aes(ymax = raw.mean + raw.sd, ymin = raw.mean - raw.sd)
eb_sgolay<- aes(ymax = sgolay.mean + sgolay.sd, ymin = sgolay.mean - sgolay.sd)
eb_spde_non_dlm <- aes(ymax = nonDLMspde.mean + nonDLMspde.sd, ymin = nonDLMspde.mean - nonDLMspde.sd)
ebs_kalman <- aes(ymax = DLM.kalman.mean + DLM.kalman.sd, ymin = DLM.kalman.mean - DLM.kalman.sd)
eb_spde_dlm <- aes(ymax = DLMspde.mean + DLMspde.sd, ymin = DLMspde.mean - DLMspde.sd)
eb_sp_spde <- aes(ymax = sp.spDTyn.mean + sp.spDTyn.sd, ymin = sp.spDTyn.mean - sp.spDTyn.sd)
eb_sp_spDTyn <- aes(ymax = sp.spde.mean + sp.spde.sd, ymin = sp.spde.mean - sp.spde.sd)

###############################################################################################
#              DISTRIBUTION OF MEANS
###############################################################################################
require(gridExtra)
nonDLM <- ggplot(DF, aes(Latitude))
nonDLM <- nonDLM + geom_ribbon(eb_raw, alpha = 0.5, fill='grey') + geom_line(aes(y=raw.mean), colour="Black")
nonDLM <- nonDLM + geom_ribbon(eb_sgolay, alpha = 0.5, fill='light blue') + geom_line(aes(y=sgolay.mean), colour="Dark Blue") 
nonDLM <- nonDLM + geom_ribbon(eb_spde_non_dlm , alpha = 0.5, fill='light green') + geom_line(aes(y=nonDLMspde.mean), colour="Dark Green") + 
  coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) 
#annotate("text", x=41, y=-3.3, label="A",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
#   scale_x_continuous(limits = c(33.5, 42),
#                      breaks=c(33.5, 36, 39, 42)) + 
#   scale_y_continuous(limits = c(-5, 6),
# #                      breaks=c(-5, -2, 2, 6))
# scale_x_continuous(limits = c(-5, 6),
#                    breaks=c(-5, -2, 2, 6)) + 
#   scale_y_continuous(limits = c(33.5, 42),
#                      breaks=c(33.5, 36, 39, 42))


pFeb<- ggplot(DF, aes(Latitude))
pFeb <- pFeb + geom_ribbon(ebraw2, alpha = 0.5, fill='grey') + geom_line(aes(y=feb.raw.mean), colour="Black")
pFeb <- pFeb + geom_ribbon(ebsgolay2, alpha = 0.5, fill='light blue') + geom_line(aes(y=feb.sgolay.mean), colour="Dark Blue") 
pFeb <- pFeb + geom_ribbon(ebwhit2, alpha = 0.5, fill='light green') + geom_line(aes(y=feb.whit.mean), colour="Dark Green") 
pFeb <- pFeb + geom_ribbon(ebspde2, alpha = 0.5, fill='Orange') + geom_line(aes(y=feb.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(1,1,1,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=-0.4, label="Feb",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))


pMar<- ggplot(DF, aes(Latitude))
pMar <- pMar + geom_ribbon(ebraw3, alpha = 0.5, fill='grey') + geom_line(aes(y=mar.raw.mean), colour="Black")
pMar <- pMar + geom_ribbon(ebsgolay3, alpha = 0.5, fill='light blue') + geom_line(aes(y=mar.sgolay.mean), colour="Dark Blue") 
pMar <- pMar + geom_ribbon(ebwhit3, alpha = 0.5, fill='light green') + geom_line(aes(y=mar.whit.mean), colour="Dark Green") 
pMar <- pMar + geom_ribbon(ebspde3, alpha = 0.5, fill='Orange') + geom_line(aes(y=mar.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.4, label="Mar",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  observation available before smoothing
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(maptools)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('F:/CLIM_DATA/Kenya_evi2_Records/kenyacounties/counties/counties.shp', proj4string=proj)
# p <- spplot(obsUsed, par.settings=BuRdTheme)
# p + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))
# 

require(rasterVis)
require(RColorBrewer)
lists.obs = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/obs_used"),pattern="*.tif", full.names=TRUE)
obs.used.image = foreach(files=1:length(lists.obs), .combine=c, .verbose=FALSE) %do%
  raster(lists.obs[[files]])
stk_obs_used=stack(obs.used.image, bands=NULL, native=FALSE, RAT=TRUE)

### Show all the colour schemes available
require(RColorBrewer)
require(gimms)
#blrd.palette <- colorRampPalette(c("blue", "red"), space = "Lab")
p1=spplot(stk_obs_used, xlim = bbox(stk_obs_used)[1, ], 
          ylim = bbox(stk_obs_used)[2, ],
          colorkey=list(labels = list(cex = 1.5,fontface='plain'), space='right'), 
          col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")),
          at=seq(70, 100, 1),
          strip=FALSE,
          scales = list(draw = TRUE),
          main=list("", col="black",cex=2, fontface='bold'),
          #        panel = function(x, y, z,...) {
          #          panel.levelplot.raster(x, y, z,...)
          #          panel.text(x = 40.5, y = -3.2,
          #                     labels=Names[panel.number()],
          #                     col.text = "black",
          #                     cex=1.5,fontface='bold')
          #        }, 
          contour=FALSE, layout=c(1, 1))
p1 + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))



####################################################
## Categorical data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(foreach); require(raster); require(rgdal); require(maptools)
obs.used.image = raster("G:/Rdirectory/Spatiotemporal_Dynamic_Linear_Model/Results/Obs.used/obsUsed_kenya.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   Removing regions outside kenya boundary
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## obtaining boundary shapefile for kenya
kenyaborder.shp=readShapePoly("F:/CLIM_DATA/Kenya_evi2_Records/kenyacounties/kenya_border_shapefile/kenya.shp")

## crop and mask kenya region
obs.used.image1 <- crop(obs.used.image, extent(kenyaborder.shp))
obs.used.image2 <- mask(obs.used.image1, kenyaborder.shp)


require(raster)
fcat=function(x){ifelse(x > 0 & x < 20, 1,
                        ifelse(x >= 20 & x < 40, 2,
                               ifelse(x >= 40 & x < 60, 3,
                                      ifelse(x >= 60 & x < 80, 4, 5))))
}
obs.used.cat=calc(obs.used.image2, fcat)
obs.rat <- ratify(obs.used.cat)
rat <- levels(obs.rat)[[1]]
rat$obs_available <- c('>0% to <20%', '20% to <40%', '40% to <60%', '60% to <80%', '80% to 100%')
rat$class <- c('A', 'B', 'C', 'D', 'E')
levels(obs.rat) <- rat
obs.rat

#Adding county border lines from shapefile
require(maptools)
mapaSHP <- readShapeLines("G:/Rdirectory/countries_shapefiles/kenyacounties/Kenya_Counties_100112.shp")

require(RColorBrewer); require(rasterVis)
col.regions=colorRampPalette(c('mintcream', 'red2', 'darkmagenta', 'darkgoldenrod1', 'chartreuse1'))
p1.new=levelplot(obs.rat, 
                 col.regions=col.regions,
                 colorkey=list(space="right"))
p1.new + layer(sp.lines(mapaSHP, lwd=0.8, col='gray63'))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# RSE PAPER: Observation used
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(foreach); require(raster); require(rgdal); require(maptools)
lists.obs = list.files(file.path("C:/Users/eokuto/Desktop/RSE_Revision/RSE_Paper_Revision_Outputs/RSE_TestResult/data.used/After_filter"),pattern="*.tif", full.names=TRUE)
rasters.raw = foreach(files=1:length(lists.obs), .combine=c, .verbose=FALSE) %do%
  raster(lists.obs[[files]])

obs.used.image<-rasters.raw[[2]]

ff= function(x){
  ifelse(x == 0, NA, x)
}

obs.used.image2 = calc (obs.used.image , ff) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   Removing regions outside kenya boundary
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## obtaining boundary shapefile for kenya
require(rworldmap)
data("countriesCoarse")

require(raster)
fcat=function(x){ifelse(x > 0 & x < 20, 1,
                        ifelse(x >= 20 & x < 40, 2,
                               ifelse(x >= 40 & x < 60, 3,
                                      ifelse(x >= 60 & x < 80, 4, 5))))
}


obs.used.cat=calc(obs.used.image2, fcat)
obs.rat <- ratify(obs.used.cat)
rat <- levels(obs.rat)[[1]]
rat$obs_available <- c('>0% to <20%', '20% to <40%', '40% to <60%', '60% to <80%', '80% to 100%')
rat$class <- c('A', 'B', 'C', 'D', 'E')
levels(obs.rat) <- rat
obs.rat

#Adding county border lines from shapefile
require(maptools)
##Change to your folder
mapaSHP <- readShapeLines('G:/Rdirectory/countries_shapefiles/world_admin_1998_countries.shp')

##Move from blue to red in four colours
require(RColorBrewer)
col.regions=colorRampPalette(c('mintcream', 'red2', 'darkmagenta', 'darkgoldenrod1', 'chartreuse1'))
require(rasterVis)
p1.new=levelplot(obs.rat, 
                 col.regions=col.regions,
                 colorkey=list(space="right"))
p1.new + layer(sp.lines(mapaSHP, lwd=0.8, col='gray63'))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  pixel level means
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
lists.means = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means"),pattern="*.tif", full.names=TRUE)
means.images = foreach(files=1:length(lists.means), .combine=c, .verbose=FALSE) %do%
  raster(lists.means[[files]])
stk_means=stack(means.images[[7]],
                means.images[[3]],
                means.images[[4]],
                means.images[[1]],
                means.images[[2]],
                means.images[[6]],
                means.images[[5]],bands=NULL, native=FALSE, RAT=TRUE)
require(bfastSpatial)
non_Dlm_spde_mean=summaryBrick(stack(means.images[[4]],means.images[[1]]), fun="mean", na.rm=TRUE)
writeRaster(non_Dlm_spde_mean, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means/non_Dlm_spde_mean.tif", overwrite=TRUE)

require(rasterVis)

blrd.palette <- colorRampPalette(c("blue", "red"), space = "Lab")
Names <- c("A", "B", "C","D", "E","F", "G")
p2=spplot(stk_means, xlim = bbox(stk_means)[1, ], 
          ylim = bbox(stk_means)[2, ],
          colorkey=list(labels = list(cex = 1.5,fontface='plain'), space='right'), 
          col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")),
          at=seq(0, 0.6, 0.01),
          strip=FALSE,
          aspect="fill",
          scales = list(draw = FALSE),
          main=list(""),
          panel = function(x, y, z,...) {
            panel.levelplot.raster(x, y, z,...)
            panel.text(x = 41, y = -3.3,
                       labels=Names[panel.number()],
                       col.text = "black",
                       cex=1.5,fontface='bold')
          }, contour=FALSE, layout=c(2, 4))

p2 + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  pixel level standard deviations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
lists.sd = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd"),pattern="*.tif", full.names=TRUE)
sd.images = foreach(files=1:length(lists.sd), .combine=c, .verbose=FALSE) %do%
  raster(lists.sd[[files]])

stk_sd=stack(sd.images[[7]],
             sd.images[[3]],
             sd.images[[4]],
             sd.images[[1]],
             sd.images[[2]],
             sd.images[[6]],
             sd.images[[5]],bands=NULL, native=FALSE, RAT=TRUE)

require(rasterVis)
Names <- c("A", "B", "C","D", "E","F", "G")
p3=spplot(stk_sd, xlim = bbox(stk_sd)[1, ], 
          ylim = bbox(stk_sd)[2, ],
          colorkey=list(labels = list(cex = 1.5,fontface='plain'), space='right'), 
          col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")),
          at=seq(0, 0.3, 0.01),
          strip=FALSE,
          aspect="fill",
          scales = list(draw = FALSE),
          main=list(""),
          panel = function(x, y, z,...) {
            panel.levelplot.raster(x, y, z,...)
            panel.text(x = 41, y = -3.3,
                       labels=Names[panel.number()],
                       col.text = "black",
                       cex=1.5,fontface='bold')
          }, contour=FALSE, layout=c(2, 4))

p3 + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  root mean squared error analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
lists.rmse = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse"),pattern="*.tif", full.names=TRUE)
rmse.images = foreach(files=1:length(lists.rmse),  .combine=c, .verbose=FALSE) %do%
  raster(lists.rmse[[files]])

stk_rmse=stack(rmse.images[[3]],
               rmse.images[[4]],
               rmse.images[[1]],
               rmse.images[[2]],
               rmse.images[[6]],
               rmse.images[[5]],bands=NULL, native=FALSE, RAT=TRUE)

require(bfastSpatial)
rmse_non_DLM_sgolay=summaryBrick(stack(rmse.images[[4]],rmse.images[[3]]), fun="mean", na.rm=TRUE)
writeRaster(rmse_non_DLM_sgolay, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_non_DLM_sgolay.tif", overwrite=TRUE)

rmse_DLM_kalman=summaryBrick(stack(rmse.images[[1]],rmse.images[[3]]), fun="mean", na.rm=TRUE)
writeRaster(rmse_DLM_kalman, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_DLM_kalman.tif", overwrite=TRUE)

rmse_DLM_spde=summaryBrick(stack(rmse.images[[1]],rmse.images[[2]]), fun="mean", na.rm=TRUE)
writeRaster(rmse_DLM_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_DLM_spde.tif", overwrite=TRUE)


require(rasterVis)
Names <- c("A", "B", "C","D", "E","F")
p4=spplot(stk_rmse, xlim = bbox(stk_rmse)[1, ], 
          ylim = bbox(stk_rmse)[2, ],
          colorkey=list(labels = list(cex = 1.5,fontface='plain'), space='right'), 
          col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")),
          at=seq(0, 0.3, 0.01),
          strip=FALSE,
          aspect="fill",
          scales = list(draw = FALSE),
          main=list(""),
          panel = function(x, y, z,...) {
            panel.levelplot.raster(x, y, z,...)
            panel.text(x = 41, y = -3.3,
                       labels=Names[panel.number()],
                       col.text = "black",
                       cex=1.5,fontface='bold')
          }, contour=FALSE, layout=c(2, 3))

p4 + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  coefficient of efficiency E analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
E_non_DLM_sgolay=(1-rmse.images[[3]])-0.1
writeRaster(E_non_DLM_sgolay, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_non_DLM_sgolay.tif", overwrite=TRUE)
E_non_DLM_spde=(1-rmse.images[[4]])-0.1
writeRaster(E_non_DLM_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_non_DLM_spde.tif", overwrite=TRUE)
E_DLM_kalman=(1-rmse.images[[1]])-0.1
writeRaster(E_DLM_kalman, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_DLM_kalman.tif", overwrite=TRUE)
E_DLM_spde=(1-rmse.images[[2]])-0.1
writeRaster(E_DLM_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_DLM_spde.tif", overwrite=TRUE)
E_sp_spDTyn=(1-rmse.images[[6]])-0.1
writeRaster(E_sp_spDTyn, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_sp_spDTyn.tif", overwrite=TRUE)
E_sp_spde=(1-rmse.images[[5]])-0.1
writeRaster(E_sp_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_sp_spde.tif", overwrite=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
lists.E = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E"),pattern="*.tif", full.names=TRUE)
E.images = foreach(files=1:length(lists.E),  .combine=c, .verbose=FALSE) %do%
  raster(lists.E[[files]])

stk_E=stack(E.images[[3]],
            E.images[[4]],
            E.images[[1]],
            E.images[[2]],
            E.images[[6]],
            E.images[[5]],bands=NULL, native=FALSE, RAT=TRUE)

require(rasterVis)
Names <- c("A", "B", "C","D", "E","F")
p5=spplot(stk_E, xlim = bbox(stk_E)[1, ], 
          ylim = bbox(stk_E)[2, ],
          colorkey=list(labels = list(cex = 1.5,fontface='plain'), space='right'), 
          col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")),
          at=seq(0.7, 1, 0.01),
          strip=FALSE,
          aspect="fill",
          scales = list(draw = FALSE),
          main=list(""),
          panel = function(x, y, z,...) {
            panel.levelplot.raster(x, y, z,...)
            panel.text(x = 41, y = -3.3,
                       labels=Names[panel.number()],
                       col.text = "black",
                       cex=1.5,fontface='bold')
          }, contour=FALSE, layout=c(2, 3))

p5 + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))

require(hydroGOF)
?nseStat



##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fitb <- harmonic.regression(yyy, inputtime=month)
mu = fitb$coeffs[1]
cos.amplitude = fitb$coeffs[2]
sin.amplitude = fitb$coeffs[3]
amplitude = fitb$pars[1]
phase = fitb$pars[2]
?harmonic.regression
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Using method two
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(season)
?cosinor
fit = cosinor(yyy ~ 1, date="month",
              type = 'monthly', data = data.frame(yyy, month),
              family = gaussian(), offsetmonth = FALSE)

res2$Coefficients
res2$coefficients[3] 
coef(fit)[2]
mu = coef(res2)["Intercept"]
cos.amplitude = coef(res2)["cosw"]
sin.amplitude = coef(res2)["sinw "]
amplitude = fitb$pars[1]
phase = fitb$pars[2]

data(CVD)
# model to fit an annual pattern to the monthly cardiovascular disease data
f = c(12)
tau = c(130,10)
res12 = nscosinor(data=CVD, response='adj', cycles=f, niters=500,
                  burnin=100, tau=tau)
summary(res12)
plot(res12)


## cardiovascular disease data (offset based on number of days in...
## ...the month scaled to an average month length)
data(CVD)
res = cosinor(cvd~1, date='month', data=CVD, type='monthly',
              family=poisson(), offsetmonth=TRUE)
summary(res)
seasrescheck(res$residuals) # check the residuals
# stillbirth data
data(stillbirth)
head(stillbirth)
res = cosinor(stillborn~1, date='dob', data=stillbirth,
              family=binomial(link='cloglog'))
summary(res)
plot(res)
# hourly indoor temperature data
data(indoor)
head(indoor)
res = cosinor(bedroom~1, date='datetime', type='hourly', data=indoor)
summary(res)
