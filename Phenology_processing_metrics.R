
rm(list=ls())
source("Phenology_Functions_Erick.R")
require(gimms)
require(raster)
require(raster)
require(foreach)
require(INLA)
require(hydromad)
require(hydroGOF)
require(zoo)
require(dlm)
require(R2BayesX)
require(MODIS)


install.packages("MODIS", dependencies=TRUE)
dd=read.csv("")
path="F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped"
lists = list.files(file.path(path),pattern="*.tif", full.names=TRUE)
rasters.images = foreach(files=1:length(lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists[[files]])
stkkenya = stack(rasters.images, bands=NULL, native=FALSE, RAT=TRUE)

plot(stkkenya[[1]])

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#TESTING ROWISE SMOOTHING FUNCTIONS 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# kenyadata20km <- read.csv("F:/CLIM_DATA/Kenya_evi2_Records/kenya.monthly.20KMunsmoothedEVI2.csv")
# coord=kenyadata20km[, 1:2]
# 
# kenyadata20km_records = kenyadata20km[1:5, -c(1:2)]
# #kenyadata20km_records = kenyadata20km[, -c(1:2)]
# dim(kenyadata20km)
# dim(kenyadata20km_records)

# NonDLMinlaSmoothed = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=FALSE) %do%
#   Non_DLM_inla_fun2(kenyadata20km_records[[row]])
# NonDLMbayesXSmoothed = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=FALSE) %do%
#   Non_DLM_bayesX_fun2(kenyadata20km_records[[row]])

# plot(NonDLMinlaSmoothed[1,1:12], type="l")
# plot(melt(kenyadata20km_records[1,1:12], na.rm=FALSE)$value, type="l", col="blue")

# NonDLMinlaSmoothed = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=FALSE) %do%
#   Non_DLM_inla_fun2(kenyadata20km_records[[row]])
# DLMinlaSmoothed = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=FALSE) %do%
#   DLM_inla_Fun2(kenyadata20km_records[[row]])
fun_correction <- function(x){
  xx=ifelse(x > 1, 1,
            ifelse(x < -1, -1, x))
}
#Using INLA
INLA_Non_DLM_result = Non_DLM_smoothing_Function_inla(stkkenya)
INLA_Non_DLM_result2 = foreach(i=1:nlayers(INLA_Non_DLM_result), .combine=c, .verbose=FALSE) %do%
  f(INLA_Non_DLM_result[[i]])

INLA_Non_DLM_result3=calc(INLA_Non_DLM_result, fun_correction)

writeRaster(INLA_Non_DLM_result, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/Non_DLM_stk_inla.tif")

INLA_DLM_result = DLM_Smoothing_Function_inla(stkkenya)
writeRaster(INLA_DLM_result, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/DLM_stk_inla.tif")

#Using Kalman Filter
#kalman_Non_DLM_result = calc(stk=stkkenya, fun=Non_DLM_inla_fun2, na.rm=FALSE, forcefun=FALSE, forceapply=FALSE)
#writeRaster(kalman_Non_DLM_result, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/Non_DLM_stk_kalman.tif")
kalman_DLM_result = DLM_kalman_filter(stkkenya)
writeRaster(kalman_DLM_result, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/DLM_stk_kalman.tif", overwrite=TRUE)

install.packages("signal", dependencies=TRUE)
require(signal)
savitzky_golay = funsgolaymonthly(stkkenya)
writeRaster(savitzky_golay, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/Non_DLM_stk_savitzky_golay.tif")

#Using BayesX
NonDLMbayesXSmoothed = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=FALSE) %do%
  Non_DLM_bayesX_fun2(kenyadata20km_records[[row]])
DLMbayesXSmoothed = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=FALSE) %do%
  DLM_bayesX_fun2(kenyadata20km_records[[row]])


plot(DLMinlaSmoothed[1, 1:12], type="l", col="red")
plot(DLMbayesXSmoothed[1, 1:12], type="l", col="blue")
dim(NonDLMinlaSmoothed)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#COMPUTING PHENOLOGICAL METRICS USING INLA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# res2=pheno_fun(y, nmonths=12, nyears=1, ncircle=2)
# result_stk <- phenology_DLM2(stk=stkkenya, nmonths=360, nyears=30, ncircle=30)
# result = calc(stkkenya, fun=pheno_fun)
# writeRaster(result, "F:/CLIM_DATA/Kenya_evi2_Records/phenology_metrics_kenya/stkphenMetricskenya.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#smoothing using Time series Non DLM with INLA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
kenyadata20km <- read.csv("F:/CLIM_DATA/Kenya_evi2_Records/kenya.monthly.20KMunsmoothedEVI2.csv")
coord=kenyadata20km[, 1:2]
kenyadata20km_records = kenyadata20km[, -c(1:2)]
dim(kenyadata20km_records)

fun_correction <- function(x){
  xx=ifelse(x > 1, 1,
            ifelse(x < -1, -1, x))
}


DLM_stk_inla1 = calc(DLM_stk_inla, fun=fun.correction)
Non_DLM_stk_inla1 = calc(Non_DLM_stk_inla, fun=fun.correction)
DLM_stk_kalman1 = calc(DLM_stk_kalman, fun=fun.correction)
Non_DLM_stk_SG1 = calc(Non_DLM_stk_SG, fun=fun.correction)

writeRaster(DLM_stk_inla1, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/spde/dlm_spde.tif", bylayer=TRUE)
writeRaster(DLM_stk_kalman1, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/kalman_filter/dlm_kalman.tif", bylayer=TRUE)
writeRaster(Non_DLM_stk_inla1, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Non_DLM/spde/Non_dlm_spde.tif", bylayer=TRUE)
writeRaster(Non_DLM_stk_SG1, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Non_DLM/Savitzky_golay/Non_dlm_sgolay.tif", bylayer=TRUE)

spt_stk_spde <- spde.record.function(obs.stk=stkkenya, sim.stk=Non_DLM_stk_SG1)
writeRaster(spt_stk_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Spatiotemporal_DLM/spde/spde.tif", bylayer=TRUE)

spt_stk_spDTyn <- spDTyn.record.function(sim1.stk=DLM_stk_kalman1, sim2.stk=Non_DLM_stk_SG1)
spt_stk_spDTyn2 <- spDTyn.record.function(sim1.stk=spt_stk_spDTyn, sim2.stk=Non_DLM_stk_inla1)
writeRaster(spt_stk_spDTyn2, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Spatiotemporal_DLM/spDTyn/spDTyn.tif", bylayer=TRUE)


Non_DLM_stk_inla1 = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=FALSE) %do%
  Non_DLM_inla_fun2(kenyadata20km_records[[row]])
Non_DLM_stk_inla = cbind(coord, Non_DLM_stk_inla1)
write.csv(Non_DLM_stk_inla, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Non_DLM/Non_DLM_stk_inla.csv")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#smoothing using Time series DLM with INLA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#DLM_stk_inla <- DLM_Smoothing_Function_inla(stk=stkkenya)
DLM_stk_inla1 = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=TRUE) %do%
  DLM_inla_Fun2(kenyadata20km_records[[row]])
DLM_stk_inla = cbind(coord, DLM_stk_inla1)
write.csv(DLM_stk_inla, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/DLM_stk_inla.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#smoothing using spatiotemporal DLM with INLA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Do not run
Spatiotemporal_DLM_stk_inla <- Non_DLM_smoothing_Function_inla(stk=stkkenya)
writeRaster(Spatiotemporal_DLM_stk_inla, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Spatiotemporal_DLM/spatiotemporal_DLM_stk_inla.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>...  Other methods ....>>>>>>>>>>>>>>>>>>>
#smoothing using Time series Non DLM with SARIMA model
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Non_DLM_stk_bayesX <- Non_DLM_bayesX_function(stk=stkkenya)
#writeRaster(Non_DLM_stk_bayesX, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Non_DLM/Non_DLM_stk_bayesX.tif")

Non_DLM_stk_bayesX1 = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=TRUE) %do%
  Non_DLM_bayesX_fun2(kenyadata20km_records[[row]])
Non_DLM_stk_bayesX = cbind(coord, Non_DLM_stk_bayesX1)
#Non_DLM_stk_inla <- Non_DLM_smoothing_Function_inla(stk=stkkenya)
write.csv(Non_DLM_stk_bayesX, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Non_DLM/Non_DLM_stk_bayesX.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#smoothing using Time series DLM with bayesX package:  DLM_bayesX_function
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#DLM_stk_bayesX <- DLM_bayesX_function(stk=stkkenya)
#writeRaster(DLM_stk_bayesX, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/DLM_stk_KalmanFilter.tif")

DLM_stk_bayesX1 = foreach(row=1:nrow(kenyadata20km_records), .combine=rbind, .verbose=TRUE) %do%
  DLM_bayesX_fun2(kenyadata20km_records[[row]])
DLM_stk_bayesX = cbind(coord, DLM_stk_bayesX1)
write.csv(DLM_stk_bayesX, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/DLM_stk_bayesX.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#smoothing using Time series DLM with dlm package:  DLM_bayesX_function
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
DLM_stk_kalman_filter <- DLM_kalman_filter(stk=stkkenya)
writeRaster(DLM_stk_kalman_filter, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/DLM_stk_KalmanFilter.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#smoothing using spatiotemporal DLM with spDTyn package
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
spatiotemporal_DLM_stk_spDTyn <- Non_DLM_smoothing_Function_spDTyn(stk=stkkenya)
writeRaster(spatiotemporal_DLM_stk_spDTyn, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Spatiotemporal_DLM/Spatiotemporal_DLM_stk_spDTyn.tif")


#END
#non dlm
lists.SG = list.files(file.path("F:CLIM_DATA/Kenya_evi2_Records/Results/Non_DLM/Savitzky_golay"),pattern="*.tif", full.names=TRUE)
SG.images = foreach(files=1:length(lists.SG), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.SG[[files]])

lists.nonDLM_spde = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/Non_DLM/spde"),pattern="*.tif", full.names=TRUE)
nonDLM_spde.images = foreach(files=1:length(lists.nonDLM_spde), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.nonDLM_spde[[files]])

#temporal dlm
lists.kalman = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/kalman_filter"),pattern="*.tif", full.names=TRUE)
kalman.images = foreach(files=1:length(lists.kalman), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.kalman[[files]])

lists.DLM_spde = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/spde"),pattern="*.tif", full.names=TRUE)
dlm_spde.images = foreach(files=1:length(lists.DLM_spde), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.DLM_spde[[files]])

#spatiotemporal dlm
lists.sp_spde = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/Spatiotemporal_DLM/spde"),pattern="*.tif", full.names=TRUE)
sp_spde.images = foreach(files=1:length(lists.sp_spde), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.sp_spde[[files]])

lists.spDTyn = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/Spatiotemporal_DLM/spDTyn"),pattern="*.tif", full.names=TRUE)
spDTyn.images = foreach(files=1:length(lists.spDTyn), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.spDTyn[[files]])

stk_unsmoothed=stack(stkkenya, bands=NULL, native=FALSE, RAT=TRUE)
Non_DLM_stk_SG=stack(SG.images, bands=NULL, native=FALSE, RAT=TRUE)
Non_DLM_stk_spde=stack(nonDLM_spde.images, bands=NULL, native=FALSE, RAT=TRUE)
DLM_stk_spde=stack(dlm_spde.images, bands=NULL, native=FALSE, RAT=TRUE)
DLM_stk_kalman=stack(kalman.images, bands=NULL, native=FALSE, RAT=TRUE)
sptemporal_stk_spde=stack(sp_spde.images, bands=NULL, native=FALSE, RAT=TRUE)
sptemporal_stk_spDTyn=stack(spDTyn.images, bands=NULL, native=FALSE, RAT=TRUE)


?calc
####################################
# Further data management
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#raster means
stk_unsmoothed_mean<- calc(stk_unsmoothed, fun=mean, na.rm=TRUE)
Non_DLM_stk_SG_mean<- calc(Non_DLM_stk_SG, fun=mean, na.rm=TRUE)
nonDlm_spde_mean<- calc(Non_DLM_stk_spde, fun=mean, na.rm=TRUE)
DLM_stk_spde_mean<- calc(DLM_stk_spde, fun=mean, na.rm=TRUE)
DLM_stk_kalman_mean<- calc(DLM_stk_kalman, fun=mean, na.rm=TRUE)
sptemporal_stk_spde_mean<- calc(sptemporal_stk_spde, fun=mean, na.rm=TRUE)
sptemporal_stk_spDTyn_mean<- calc(sptemporal_stk_spDTyn, fun=mean, na.rm=TRUE)

writeRaster(stk_unsmoothed_mean, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means/unsmoothed_mean.tif", overwrite=TRUE)
writeRaster(Non_DLM_stk_SG_mean, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means/non_DLM_SG_mean.tif")
writeRaster(nonDlm_spde_mean, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means/non_Dlm_spde_mean.tif")
writeRaster(DLM_stk_spde_mean, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means/DLM_spde_mean.tif")
writeRaster(DLM_stk_kalman_mean, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means/DLM_kalman_mean.tif")
writeRaster(sptemporal_stk_spde_mean, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means/sptemporal_spde_mean.tif")
writeRaster(sptemporal_stk_spDTyn_mean, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/means/sptemporal_spDTyn_mean.tif")

require(bfastSpatial)
stk_unsmoothed_sd<- summaryBrick(stk_unsmoothed, fun="sd", na.rm=TRUE)
on_DLM_stk_SG_sd<- summaryBrick(Non_DLM_stk_SG, fun="sd", na.rm=TRUE)
nonDlm_spde_sd<- summaryBrick(Non_DLM_stk_spde, fun="sd", na.rm=TRUE)
DLM_stk_spde_sd<- summaryBrick(DLM_stk_spde, fun="sd", na.rm=TRUE)
DLM_stk_kalman_sd<- summaryBrick(DLM_stk_kalman, fun="sd", na.rm=TRUE)
sptemporal_stk_spde_sd<- summaryBrick(sptemporal_stk_spde, fun="sd", na.rm=TRUE)
sptemporal_stk_spDTyn_sd<- summaryBrick(sptemporal_stk_spDTyn, fun="sd", na.rm=TRUE)

writeRaster(stk_unsmoothed_sd, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd/unsmoothed_sd.tif", overwrite=TRUE)
writeRaster(on_DLM_stk_SG_sd, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd/Non_DLM_SG_sd.tif")
writeRaster(nonDlm_spde_sd, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd/nonDlm_spde_sd.tif")
writeRaster(DLM_stk_spde_sd, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd/DLM_spde_sd.tif")
writeRaster(DLM_stk_kalman_sd, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd/DLM_kalman_sd.tif")
writeRaster(sptemporal_stk_spde_sd, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd/sptemporal_spde_sd.tif")
writeRaster(sptemporal_stk_spDTyn_sd, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/sd/sptemporal_spDTyn_sd.tif")


#Latitudinal means
require(raster)
Latitude<-yFromRow(stk_unsmoothed_mean, row=1:nrow(stk_unsmoothed_mean))

raw.mean<-apply(as.matrix(stkkenya), 1, mean, na.rm=TRUE)
raw.sd<-apply(as.matrix(stkkenya), 1, sd, na.rm=TRUE)

sgolay.mean<-apply(as.matrix(Non_DLM_stk_SG_mean), 1, mean, na.rm=TRUE)
sgolay.sd<-apply(as.matrix(Non_DLM_stk_SG_mean), 1, sd, na.rm=TRUE)
nonDLMspde.mean<-apply(as.matrix(nonDlm_spde_mean), 1, mean, na.rm=TRUE)
nonDLMspde.sd<-apply(as.matrix(nonDlm_spde_mean), 1, sd, na.rm=TRUE)

DLM.kalman.mean<-apply(as.matrix(DLM_stk_kalman_mean), 1, mean, na.rm=TRUE)
DLM.kalman.sd<-apply(as.matrix(DLM_stk_kalman_mean), 1, sd, na.rm=TRUE)
DLMspde.mean<-apply(as.matrix(DLM_stk_spde_mean), 1, mean, na.rm=TRUE)
DLMspde.sd<-apply(as.matrix(DLM_stk_spde_mean), 1, sd, na.rm=TRUE)

sp.spde.mean<-apply(as.matrix(sptemporal_stk_spde_mean), 1, mean, na.rm=TRUE)
sp.spde.sd<-apply(as.matrix(sptemporal_stk_spde_mean), 1, sd, na.rm=TRUE)
sp.spDTyn.mean<-apply(as.matrix(sptemporal_stk_spDTyn_mean), 1, mean, na.rm=TRUE)
sp.spDTyn.sd<-apply(as.matrix(sptemporal_stk_spDTyn_mean), 1, sd, na.rm=TRUE)


kenya_evi2_lat_record<-data.frame(Latitude, 
                    raw.mean, raw.sd,
                    sgolay.mean, sgolay.sd,
                    nonDLMspde.mean, nonDLMspde.sd,
                    DLM.kalman.mean, DLM.kalman.sd, 
                    DLMspde.mean, DLMspde.sd,
                    sp.spde.mean, sp.spde.sd,
                    sp.spDTyn.mean, sp.spDTyn.sd)

write.csv(kenya_evi2_lat_record, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/Lat_means/kenya_lat_evi2.csv")


#percentage of observation used
obsUsed=obs.used.fun(stk=stkkenya)
writeRaster(obsUsed, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Obs.used/obsUsed_kenya.tif")
require(maptools)
proj <- CRS('+proj=longlat +ellps=WGS84')
##Change to your folder
mapaSHP <- readShapeLines('F:/CLIM_DATA/Kenya_evi2_Records/kenyacounties/counties/counties.shp', proj4string=proj)

p <- spplot(obsUsed, par.settings=BuRdTheme)
p + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk_unsmoothed=stack(stkkenya, bands=NULL, native=FALSE, RAT=TRUE)
Non_DLM_stk_SG=stack(SG.images, bands=NULL, native=FALSE, RAT=TRUE)
Non_DLM_stk_spde=stack(nonDLM_spde.images, bands=NULL, native=FALSE, RAT=TRUE)
DLM_stk_spde=stack(dlm_spde.images, bands=NULL, native=FALSE, RAT=TRUE)
DLM_stk_kalman=stack(kalman.images, bands=NULL, native=FALSE, RAT=TRUE)
sptemporal_stk_spde=stack(sp_spde.images, bands=NULL, native=FALSE, RAT=TRUE)
sptemporal_stk_spDTyn=stack(spDTyn.images, bands=NULL, native=FALSE, RAT=TRUE)


#The analysis of the coefficient of efficiency 
E_non_DLM_sgolay=E.function(obs.stk=stkkenya, sim.stk=Non_DLM_stk_SG)
E_non_DLM_spde=E.function(obs.stk=stkkenya, sim.stk=Non_DLM_stk_spde)
E_DLM_kalman=E.function(obs.stk=stkkenya, sim.stk=DLM_stk_kalman)
E_DLM_spde=E.function(obs.stk=stkkenya, sim.stk=DLM_stk_spde)
E_sp_spde=E.function(obs.stk=stkkenya, sim.stk=sptemporal_stk_spde)
E_sp_spDTyn=E.function(obs.stk=stkkenya, sim.stk=sptemporal_stk_spDTyn)

#stkkenya
plot(stkkenya[[1]])
plot(Non_DLM_stk_SG[[1]])

writeRaster(E_non_DLM_sgolay, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_non_DLM_sgolay.tif")
writeRaster(E_non_DLM_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_non_DLM_spde.tif")
writeRaster(E_DLM_kalman, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_DLM_kalman.tif")
writeRaster(E_DLM_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_DLM_spde.tif")
writeRaster(E_sp_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_sp_spde.tif")
writeRaster(E_sp_spDTyn, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/coefficient_E/E_sp_spDTyn.tif")

#The analysis of the residual mean squared error 
rmse_non_DLM_sgolay=funrmse(obs.stk=stkkenya, sim.stk=Non_DLM_stk_SG)
rmse_non_DLM_spde=funrmse(obs.stk=stkkenya, sim.stk=Non_DLM_stk_spde)
rmse_DLM_kalman=funrmse(obs.stk=stkkenya, sim.stk=DLM_stk_kalman)
rmse_DLM_spde=funrmse(obs.stk=stkkenya, sim.stk=DLM_stk_spde)
rmse_sp_spde=funrmse(obs.stk=stkkenya, sim.stk=sptemporal_stk_spde)
rmse_sp_spDTyn=funrmse(obs.stk=stkkenya, sim.stk=sptemporal_stk_spDTyn)

writeRaster(rmse_non_DLM_sgolay, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_non_DLM_sgolay.tif")
writeRaster(rmse_non_DLM_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_non_DLM_spde.tif")
writeRaster(rmse_DLM_kalman, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_DLM_kalman.tif")
writeRaster(rmse_DLM_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_DLM_spde.tif")
writeRaster(rmse_sp_spde, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_sp_spde.tif")
writeRaster(rmse_sp_spDTyn, "F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/rmse/rmse_sp_spDTyn.tif")
