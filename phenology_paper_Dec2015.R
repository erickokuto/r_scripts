
#Phenology metrics analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Record without gaps both using INLA and spDTyn
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(raster); require(foreach); require(INLA);
require(zoo); require(dlm); require(R2BayesX); require(maptools)
require(bfastSpatial); require(bfastSpatial); require(gimms)
require(MODIS); require(foreach);

setwd("G:/Rdirectory/Phenology_metrics")
source("PhD_Processing_Functions_Erick.R") 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#           Phenological metrics
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Really Data with gaps
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
evi2_lists <- list.files(file.path("F:/EVI2_RECORDS/global_golay_50km"),
                         pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists[[files]])
stk.Data.evi<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)

## obtaining shapefile for Africa
africa.shp=readShapePoly("F:/EVI2_RECORDS/africa_border_shapefile/whole_africa.shp")
#africa.shp=readShapePoly("/media/erick/OKUTO1/EVI2_RECORDS/kenya_border_shapefile/kenya.shp")

plot(africa.shp)
## cropping africa region
stk.africa0 <- crop(stk.Data.evi, extent(africa.shp))
stk.africa <- mask(stk.africa0, africa.shp)
#stk.africa<-dropLayer(stk.africa1, c(37:360))

stk.africa2 = rasterToPoints(stk.africa)
coord = cbind(stk.africa2[, 1:2]) 
spDF_Africa = cbind(stk.africa2[, -(1:2)])      #10029 rows/pixels
dim(spDF_Africa)
plot(stk.africa[[8]])

require(reshape)
#PhenologyMetrics0 = mcparallel(PhenologyParFunctionRevised(stk.africa))
PhenologyMetrics = PhenologyParFunctionRevised5(stk=stk.africa)

#PhenologyMetrics = mc.calc(stk.africa, fun=PhenologyParFunction2,  mc.cores = 4)
#PhenologyMetrics = calc(stk.africa, fun=PhenologyParFunction4)

#?mclapply
#collect the results
#PhenologyMetrics   <- mccollect(PhenologyMetrics0)
#spDF_Africa2 = spDF_Africa[1:5, ]

PhenologyMetrics = foreach(row=1:nrow(spDF_Africa), .packages="raster", .combine=rbind, .verbose=TRUE) %do%
  PhenologyParFunctionWithDataFrame(spDF_Africa[row])

write.csv(PhenologyMetrics, 
            "/home/erick/Documents/PhD_Projects/Phenology_paper/Phenology_parameters/Phenology_Parameters.csv")

#xyz <- read.csv("/home/erick/Documents/PhD_Projects/Phenology_paper/Phenology_parameters/Phenology_Parameters.csv")
xyz = cbind(coord, PhenologyMetrics)
Phenology_Parameters_STK <- rasterFromXYZ(xyz)
writeRaster(Phenology_Parameters_STK, '/home/erick/Documents/PhD_Projects/Phenology_paper/Phenology_parameters/Phenology_Parameters_STK.tif', bylayer=FALSE)

require(rasterVis)
levelplot(Phenology_Parameters_STK[[9]])

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Remote Sensing of Environment Paper
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(bfastSpatial); require(raster);
require(mblm); require(Kendall); require(rasterVis)

#Savitzky-Golay: Trend Analysis
evi2_lists.SG <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/kalman_filter"),
                         pattern="*.tif", full.names=TRUE)
evi2_rasters.SG<-foreach(files=1:length(evi2_lists.SG), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists.SG[[files]])
stk.Data.evi.SG<-stack(evi2_rasters.SG, bands=NULL, native=FALSE, RAT=TRUE)
#stk.Data.evi.SG[stk.Data.evi.SG < -1] <- -1

trend.out.SG = Theil_Sen_slope_revised(stk.Data.evi.SG)
writeRaster(trend.out.SG, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/RSE_outputs/trend_analysis/trend.out.SG.tif")


evi2_lists.raw <- list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped"),
                            pattern="*.tif", full.names=TRUE)
evi2_rasters.raw<-foreach(files=1:length(evi2_lists.raw), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists.raw[[files]])
stk.Data.evi.raw<-stack(evi2_rasters.raw, bands=NULL, native=FALSE, RAT=TRUE)
trend.out.kenya = Kolmogorov.Smirnov.Test.Fun(s1=stk.Data.evi.raw, s2=stk.Data.evi.SG)
writeRaster(trend.out.kenya, "F:/CLIM_DATA/Kenya_evi2_Records/Results/ks.test/ks.test_kenya_dlm.tif")
?na.spline


#spde: Trend Analysis
evi2_lists.spde <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/DataWhittakerSmoothed"),
                            pattern="*.tif", full.names=TRUE)
evi2_rasters.spde<-foreach(files=1:length(evi2_lists.spde), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists.spde[[files]])
stk.Data.evi.spde<-stack(evi2_rasters.spde, bands=NULL, native=FALSE, RAT=TRUE)
stk.Data.evi.spde[stk.Data.evi.spde < -1] <- -1

trend.out.spde = calc(stk.Data.evi.spde, fun=Trend.Analysis.Fun) 
writeRaster(trend.out.spde, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/RSE_outputs/trend_analysis/trend.out.spde.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(bfastSpatial)
#output.stk = summaryBrick(stk.Data.evi2, fun=PhenologyFunction2)
PhenologyMetrics = mc.calc(stk.Data.evi2, fun=PhenologyParFunction) 
writeRaster(PhenologyMetrics, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/Phenology_parameters/spde_parameters/spdePhenoParameters.tif")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(maptools)  ## For wrld_simpl
require(raster)
require(foreach)

## Example SpatialPolygonsDataFrame
# data(wrld_simpl)
# poly_shape <- subset(wrld_simpl, NAME=="Africa")
# plot(poly_shape, add=FALSE, lwd=2)

evi2_lists <- list.files(file.path("F:/CLIM_DATA/global/global_golay_50km"),
                         pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists[[files]])
stk.Data.evi<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)


africa.shp=readShapePoly("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Africa/africa_shapefile/whole_africa.shp")
plot(africa.shp)

## crop and mask
stk.africa <- crop(stk.Data.evi[[1]], extent(africa.shp))
stk.africa <- mask(stk.africa, africa.shp)


## crop and mask
stk.africa <- crop(stk.Data.evi, extent(africa.shp))
stk.africa <- mask(stk.africa, africa.shp)
plot(stk.africa)
plot(africa.shp, add=TRUE, lwd=2)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

require(raster); require(bfastSpatial); require(snow);
require(MODIS); require(foreach); require(maptools)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Really Data with gaps
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
evi2_lists <- list.files(file.path("F:/CLIM_DATA/global/global_golay_50km"),
                         pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists[[files]])
stk.Data.evi<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)


## obtaining shapefile for Africa
africa.shp=readShapePoly("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Africa/africa_shapefile/whole_africa.shp")

## cropping africa region
raster.africa <- crop(stk.Data.evi[[1]], extent(africa.shp))

require(sp); require(maps); require(maptools); require(mapdata);
require(latticeExtra); require(fields)
#other shapefiles options
LAI.hurst <- calc.hurst(stk=LAI.monthly,begin.year, end.year)
plot(LAI.hurst)

sp <- as(raster.africa, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries <- map('worldHires', fill=TRUE,plot=F)
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS(projection(raster.africa)))

spplot(sp,col.regions=tim.colors(1001),par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols))

rasterVis::levelplot(raster.africa,col.regions=tim.colors(1001)) +
  layer(sp.polygons(bPols))














#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#install.packages("dlmodeler", dependencies=TRUE)
#Global Mann-Kendall trend based on vip NDVI3g
################################################################################
## download data
################################################################################
## download entire vip ndvi3g collection in parallel
# library(doParallel)
# cl <- makeCluster(3)
# registerDoParallel(cl)
###############################################################################
path="F:/CLIM_DATA/global/global_golay_30km"
lists = list.files(file.path(path),pattern="*.tif", full.names=TRUE)
rasters.images = foreach(files=1:length(lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists[[files]])
vip_raster_agg0 <- stack(rasters.images)

################################################################################
## remove seasonal signal
################################################################################

fun_correction <- function(x){
  xx=ifelse(x > 1, 1,
            ifelse(x < -1, -1, x))
  return(xx)
}

vip_raster_agg <- calc(vip_raster_agg0, fun = fun_correction)

## calculate long-term monthly means
vip_list_means <- foreach(i = 1:12, 
                            .packages = c("raster", "rgdal")) %dopar% {
                              
                              # layers corresponding to current period (e.g. '82jan15a')
                              id <- seq(i, nlayers(vip_raster_agg), 12)
                              vip_raster_agg_tmp <- vip_raster_agg[[id]]
                              
                              # calculate long-term mean of current period (e.g. for 1982-2013 'jan15a')
                              calc(vip_raster_agg_tmp, fun = mean, na.rm = TRUE)
                            } 

vip_raster_means <- stack(vip_list_means)


## replicate monthly 'vip_raster_means' to match up with number of layers of 
## initial 'vip_raster_agg' (as `foreach` does not support recycling!)
vip_list_means <- replicate(nlayers(vip_raster_agg) / nlayers(vip_raster_means), 
                              vip_raster_means)
vip_raster_means <- stack(vip_list_means)

## subtract long-term mean from  monthly values
files_out <- names(vip_raster_agg)
vip_list_deseason <- foreach(i = 1:nlayers(vip_raster_agg), 
                               .packages = c("raster", "rgdal")) %dopar% {
                                 
                                 rst <- vip_raster_agg[[i]] - vip_raster_means[[i]]
                                 rst <- writeRaster(rst, 
                                                    filename = paste0("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/deseasoned_rst/DSN_", names(vip_raster_agg[[i]])), 
                                                    format = "GTiff", overwrite = TRUE)
                                 
                               }

require(rasterVis)
#levelplot(vip_list_deseason[[1]])
vip_raster_deseason <- stack(vip_list_deseason)

################################################################################
## mann-kendall trend test (p < 0.001)
################################################################################

## Apply mann-kendall trend test (p < 0.001) custom function that returns significant values of tau only
require(Kendall);require(gimms)

significantTau <- function(x) {
  require(Kendall);require(gimms)
  mk <- MannKendall(x)
  # reject value of tau if p >= 0.001
  if (mk$sl >= 0.001) {
    return(NA) 
    # keep value of tau if p < 0.001
  } else {
    return(mk$tau)
  }
}

## apply custom function on a pixel basis
vip_raster_trend <- overlay(vip_raster_deseason, fun = significantTau, 
                              filename = "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/Phenology_parameters/Mann_Kandall_Trend/vip5km_mkTest001", 
                              format = "GTiff")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>END >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#Trend analysis function
Mann_Kandall_Test <- function(vip_raster_agg, outDirPath, ResultDirPath){
  require(raster); require(rgdal); require(Kendall);require(gimms);require(foreach)
  #calculate long-term monthly means
  vip_list_means <- foreach(i = 1:12, 
                            .packages = c("raster", "rgdal")) %dopar% {
                              
                              # layers corresponding to current period (e.g. '82jan15a')
                              id <- seq(i, nlayers(vip_raster_agg), 12)
                              vip_raster_agg_tmp <- vip_raster_agg[[id]]
                              
                              # calculate long-term mean of current period (e.g. for 1982-2013 'jan15a')
                              calc(vip_raster_agg_tmp, fun = mean, na.rm = TRUE)
                            } 
  
  vip_raster_means <- stack(vip_list_means)
  
  fun_correction <- function(x){
    xx=ifelse(x > 1, 1,
              ifelse(x < -1, -1, x))
    return(xx)
  }
  
  vip_raster_means2 <- calc(vip_raster_means, fun = fun_correction)
  
  #replicate monthly 'vip_raster_means' to match up with number of layers of 
  #initial 'vip_raster_agg' (as `foreach` does not support recycling!)
  vip_list_means <- replicate(nlayers(vip_raster_agg) / nlayers(vip_raster_means2), 
                              vip_raster_means2)
  vip_raster_means <- stack(vip_list_means)
  
  #subtract long-term mean from  monthly values
  files_out <- names(vip_raster_agg)
  vip_list_deseason <- foreach(i = 1:nlayers(vip_raster_agg), 
                               .packages = c("raster", "rgdal")) %dopar% {
                                 
                                 rst <- vip_raster_agg[[i]] - vip_raster_means[[i]]
                                 rst <- writeRaster(rst, 
                                                    filename = paste0("outDirPath/DSN_", names(vip_raster_agg[[i]])), 
                                                    format = "GTiff", overwrite = TRUE)
                                 
                               }
  
  vip_raster_deseason <- stack(vip_list_deseason)
  
  #Apply mann-kendall trend test (p < 0.001) custom function that 
  #returns only significant values of tau only
  require(Kendall);require(gimms)
  significantTau <- function(x) {
    require(Kendall);require(gimms)
    mk <- MannKendall(x)
    #reject value of tau if p >= 0.001
    if (mk$sl >= 0.001) {
      return(NA) 
      # keep value of tau if p < 0.001
    } else {
      return(mk$tau)
    }
  }
 
  #apply custom function on a pixel basis
  vip_raster_trend <- overlay(vip_raster_deseason, fun = significantTau, 
                              filename = paste0(ResultDirPath, "/vip5km_mkTest001"), 
                              format = "GTiff") 
  
}


#Importing the vegetation index records
path="F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyEVI2-5km/sgolay"
lists = list.files(file.path(path),pattern="*.tif", full.names=TRUE)
rasters.images = foreach(files=1:length(lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists[[files]])


#creating a raster stack
vip_raster_agg <- stack(rasters.images)
ResultDirPath = "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/Phenology_parameters/Mann_Kandall_Trend"
outDirPath = "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/deseasoned_rst"
Mann_Kandall_Test(vip_raster_agg=vip_raster_agg, outDirPath=outDirPath, ResultDirPath=ResultDirPath)

################################################################################
## visualize data
################################################################################

## complementary shapefile data
#install.packages("rworldmap", dependencies = TRUE)
library(rworldmap)
#vignette('rworldmap')
data("countriesCoarse")

## colors, see http://colorbrewer2.org/
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))

## create plot
spplot(vip_raster_trend, col.regions = cols(100), scales = list(draw = TRUE), 
       sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"), 
       at = seq(-.6, .6, .1))

spplot(vip_raster_trend2, col.regions = cols(100), scales = list(draw = TRUE), 
       sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"), 
       at = seq(-.6, .6, .1))
