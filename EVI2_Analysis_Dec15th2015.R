
#>>>>>>>>.>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   Remote Sensing of Environment paper re-analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
setwd("G:/Rdirectory/Phenology_metrics")
source("PhD_Processing_Functions21stDec2015_Erick.R") 

require(raster); require(foreach); require(rkt); require(INLA);
require(zoo); require(maptools); require(bfastSpatial); require(gimms)
require(MODIS); require(broom); require(mblm)
#temp files are stored in
#rasterOptions()$tmpdir

#change tempfile folder to
#rasterOptions(tempdir("C:/Users/eokuto/Documents/TempFolder"))

#remove temp files for example older than 0.5 hours
#removeTmpFiles(h=0.5)

#kenya.shp=readShapePoly("F:/CLIM_DATA/kenya_border_shapefile/kenya.shp")
kenya.shp=readShapePoly("F:/CLIM_DATA/tranzoia_county/trans.shp")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       January 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Jan <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Jan/unsmoothed"),
                             pattern="*.tif", full.names=TRUE)
rasters.raw.Jan<-foreach(files=1:length(lists.raw.Jan), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Jan[[files]])
stk.raw.Jan<-stack(rasters.raw.Jan, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Jan<-dropLayer(stk.raw.Jan, c(31:60))

stk.raw.kenya0 <- crop(stk.raw.Jan, extent(kenya.shp))
stk.raw.kenya1 <- mask(stk.raw.kenya0, kenya.shp)

#simulated data
lists.smoothed.Jan <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Jan/sgolay"),
                            pattern="*.tif", full.names=TRUE)
rasters.smoothed.Jan<-foreach(files=1:length(lists.smoothed.Jan), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Jan[[files]])
stk.smoothed.Jan<-stack(rasters.smoothed.Jan, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Jan<-dropLayer(stk.smoothed.Jan, c(31:60))

stk.sim.kenya0 <- crop(stk.smoothed.Jan, extent(kenya.shp))
stk.sim.kenya1 <- mask(stk.sim.kenya0, kenya.shp)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Jan = Theil_Sen_slope_revised(stk.smoothed.Jan)
writeRaster(stk.theilsen.slope.Jan, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Jan.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ks.test.Jan = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.kenya1, s2=stk.sim.kenya1)
ks.test.Jan = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Jan, s2=stk.smoothed.Jan)
writeRaster(ks.test.Jan, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Jan.tif")
#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       February 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Feb <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Feb/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Feb<-foreach(files=1:length(lists.raw.Feb), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Feb[[files]])
stk.raw.Feb<-stack(rasters.raw.Feb, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Feb<-dropLayer(stk.raw.Feb, c(31:60))

#simulated data
lists.smoothed.Feb <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Feb/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Feb<-foreach(files=1:length(lists.smoothed.Feb), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Feb[[files]])
stk.smoothed.Feb<-stack(rasters.smoothed.Feb, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Feb<-dropLayer(stk.smoothed.Feb, c(31:60))
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Feb = Theil_Sen_slope_revised(stk.smoothed.Feb)
writeRaster(stk.theilsen.slope.Feb, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Feb.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Feb = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Feb, s2=stk.smoothed.Feb)
writeRaster(ks.test.Feb, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Feb.tif")

plot(ks.test.Feb[[2]])
#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       March
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Mar <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Mar/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Mar<-foreach(files=1:length(lists.raw.Mar), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Mar[[files]])
stk.raw.Mar<-stack(rasters.raw.Mar, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Mar<-dropLayer(stk.raw.Mar, c(25:60))

#simulated data
lists.smoothed.Mar <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Mar/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Mar<-foreach(files=1:length(lists.smoothed.Mar), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Mar[[files]])
stk.smoothed.Mar<-stack(rasters.smoothed.Mar, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Mar<-dropLayer(stk.smoothed.Mar, c(25:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Mar = Theil_Sen_slope_revised(stk.smoothed.Mar)
writeRaster(stk.theilsen.slope.Mar, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Mar.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Mar = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Mar, s2=stk.smoothed.Mar)
writeRaster(ks.test.Mar, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Mar.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       April
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Apr <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Apr/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Apr<-foreach(files=1:length(lists.raw.Apr), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Apr[[files]])
stk.raw.Apr<-stack(rasters.raw.Apr, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Apr<-dropLayer(stk.raw.Apr, c(25:60))

#simulated data
lists.smoothed.Apr <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Apr/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Apr<-foreach(files=1:length(lists.smoothed.Apr), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Apr[[files]])
stk.smoothed.Apr<-stack(rasters.smoothed.Apr, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Apr<-dropLayer(stk.smoothed.Apr, c(25:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Apr = Theil_Sen_slope_revised(stk.smoothed.Apr)
writeRaster(stk.theilsen.slope.Apr, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Apr.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Apr = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Apr, s2=stk.smoothed.Apr)
writeRaster(ks.test.Apr, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Apr.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       May
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.May <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/May/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.May<-foreach(files=1:length(lists.raw.May), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.May[[files]])
stk.raw.May<-stack(rasters.raw.May, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.May<-dropLayer(stk.raw.May, c(31:60))

#simulated data
lists.smoothed.May <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/May/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.May<-foreach(files=1:length(lists.smoothed.May), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.May[[files]])
stk.smoothed.May<-stack(rasters.smoothed.May, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.May<-dropLayer(stk.smoothed.May, c(31:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.May = Theil_Sen_slope_revised(stk.smoothed.May)
writeRaster(stk.theilsen.slope.May, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.May.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.May = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.May, s2=stk.smoothed.May)
writeRaster(ks.test.May, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.May.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#      June
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Jun <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Jun/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Jun<-foreach(files=1:length(lists.raw.Jun), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Jun[[files]])
stk.raw.Jun<-stack(rasters.raw.Jun, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Jun<-dropLayer(stk.raw.Jun, c(31:60))

#simulated data
lists.smoothed.Jun <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Jun/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Jun<-foreach(files=1:length(lists.smoothed.Jun), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Jun[[files]])
stk.smoothed.Jun<-stack(rasters.smoothed.Jun, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Jun<-dropLayer(stk.smoothed.Jun, c(31:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Jun = Theil_Sen_slope_revised(stk.smoothed.Jun)
writeRaster(stk.theilsen.slope.Jun, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Jun.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Jun = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Jun, s2=stk.smoothed.Jun)
writeRaster(ks.test.Jun, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Jun.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       July 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
lists.raw.Jul <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Jul/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Jul<-foreach(files=1:length(lists.raw.Jul), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Jul[[files]])
stk.raw.Jul<-stack(rasters.raw.Jul, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Jun<-dropLayer(stk.smoothed.Jun, c(31:60))

#simulated data
lists.smoothed.Jul <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Jul/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Jul<-foreach(files=1:length(lists.smoothed.Jul), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Jul[[files]])
stk.smoothed.Jul<-stack(rasters.smoothed.Jul, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Jul<-dropLayer(stk.smoothed.Jul, c(31:60))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Jul = Theil_Sen_slope_revised(stk.smoothed.Jul)
writeRaster(stk.theilsen.slope.Jul, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Jul.tif")

#>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Jul = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Jul, s2=stk.smoothed.Jul)
writeRaster(ks.test.Jul, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Jul.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       August
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Aug <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Aug/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Aug<-foreach(files=1:length(lists.raw.Aug), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Aug[[files]])
stk.raw.Aug<-stack(rasters.raw.Aug, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Aug<-dropLayer(stk.raw.Aug, c(31:60))

#simulated data
lists.smoothed.Aug <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Aug/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Aug<-foreach(files=1:length(lists.smoothed.Aug), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Aug[[files]])
stk.smoothed.Aug<-stack(rasters.smoothed.Aug, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Aug<-dropLayer(stk.smoothed.Aug, c(31:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Aug = Theil_Sen_slope_revised(stk.smoothed.Aug)
writeRaster(stk.theilsen.slope.Aug, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Aug.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Aug = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Aug, s2=stk.smoothed.Aug)
writeRaster(ks.test.Aug, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Aug.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       September
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Sep <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Sep/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Sep<-foreach(files=1:length(lists.raw.Sep), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Sep[[files]])
stk.raw.Sep<-stack(rasters.raw.Sep, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Sep<-dropLayer(stk.raw.Sep, c(31:60))

#simulated data
lists.smoothed.Sep <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Sep/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Sep<-foreach(files=1:length(lists.smoothed.Sep), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Sep[[files]])
stk.smoothed.Sep<-stack(rasters.smoothed.Sep, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Sep<-dropLayer(stk.smoothed.Sep, c(31:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Sep = Theil_Sen_slope_revised(stk.smoothed.Sep)
writeRaster(stk.theilsen.slope.Sep, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Sep.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Sep = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Sep, s2=stk.smoothed.Sep)
writeRaster(ks.test.Sep, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Sep.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#      October
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Oct <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Oct/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Oct<-foreach(files=1:length(lists.raw.Oct), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Oct[[files]])
stk.raw.Oct<-stack(rasters.raw.Oct, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Oct<-dropLayer(stk.raw.Oct, c(31:60))

#simulated data
lists.smoothed.Oct <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Oct/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Oct<-foreach(files=1:length(lists.smoothed.Oct), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Oct[[files]])
stk.smoothed.Oct<-stack(rasters.smoothed.Oct, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Oct<-dropLayer(stk.smoothed.Oct, c(31:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Oct = Theil_Sen_slope_revised(stk.smoothed.Oct)
writeRaster(stk.theilsen.slope.Oct, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Oct.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Oct = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Oct, s2=stk.smoothed.Oct)
writeRaster(ks.test.Oct, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Oct.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       November
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Nov <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Nov/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Nov<-foreach(files=1:length(lists.raw.Nov), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Nov[[files]])
stk.raw.Nov<-stack(rasters.raw.Nov, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Nov<-dropLayer(stk.raw.Nov, c(31:60))

#simulated data
lists.smoothed.Nov <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Nov/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Nov<-foreach(files=1:length(lists.smoothed.Nov), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Nov[[files]])
stk.smoothed.Nov<-stack(rasters.smoothed.Nov, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Nov<-dropLayer(stk.smoothed.Nov, c(31:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Nov = Theil_Sen_slope_revised(stk.smoothed.Nov)
writeRaster(stk.theilsen.slope.Nov, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Nov.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Nov = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Nov, s2=stk.smoothed.Nov)
writeRaster(ks.test.Nov, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Nov.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#       December
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Observed record
lists.raw.Dec <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Dec/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
rasters.raw.Dec<-foreach(files=1:length(lists.raw.Dec), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.raw.Dec[[files]])
stk.raw.Dec<-stack(rasters.raw.Dec, bands=NULL, native=FALSE, RAT=TRUE)
stk.raw.Dec<-dropLayer(stk.raw.Dec, c(31:60))

#simulated data
lists.smoothed.Dec <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Dec/sgolay"),
                                 pattern="*.tif", full.names=TRUE)
rasters.smoothed.Dec<-foreach(files=1:length(lists.smoothed.Dec), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(lists.smoothed.Dec[[files]])
stk.smoothed.Dec<-stack(rasters.smoothed.Dec, bands=NULL, native=FALSE, RAT=TRUE)
stk.smoothed.Dec<-dropLayer(stk.smoothed.Dec, c(31:60))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Theilsen slope and mk significance
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.theilsen.slope.Dec = Theil_Sen_slope_revised(stk.smoothed.Dec)
writeRaster(stk.theilsen.slope.Dec, 
            "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/theilsen_slope_results/stk.theilsen.slope.Dec.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Kolmogorov-Smirnov test 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
ks.test.Dec = Kolmogorov.Smirnov.Test.Fun(s1=stk.raw.Dec, s2=stk.smoothed.Dec)
writeRaster(ks.test.Dec, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/ks_test_results/ks.test.Dec.tif")

#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>END>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>






##############################################################################
#  EXPLORING NDVI DATASETS BASED ON BOTH GIMMS AND VIP
###########################################################
require(foreach); require(raster); require(plyr); require(foreign); require(INLA);
require(rgdal);require(rasterVis);require(gridExtra);require(ggplot2);require(latticeExtra);
require(hydroGOF)

evidec<-raster("C:/Users/eokuto/Desktop/2011/evi2_NA/VIP15P4-A2011335-003-EVI2-Filtered.tif")
prdec<-raster("C:/Users/eokuto/Desktop/2011/PR/HDF4-SDS-UNKNOWN-VIP15P4-A2011335-003-hdf-3-PR.tif")
levelplot(evidec, margin=FALSE)
levelplot(prdec, margin=FALSE,
          at=seq(0, 2, 0.02),
          colorkey=NULL)
####################>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
filez.effects<- list.files(file.path("J:/resultsJune2015/filtereffect"),
                           pattern="*.tif", full.names=TRUE)

All.rasters.effects<-foreach(filez=1:length(filez.effects), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.effects[[filez]])

stk.filter.effect<-stack(All.rasters.effects[[5]],
                         All.rasters.effects[[4]],
                         All.rasters.effects[[8]],
                         All.rasters.effects[[1]],
                         All.rasters.effects[[9]],
                         All.rasters.effects[[7]],
                         All.rasters.effects[[6]],
                         All.rasters.effects[[2]],
                         All.rasters.effects[[12]],
                         All.rasters.effects[[11]],
                         All.rasters.effects[[10]],
                         All.rasters.effects[[3]])


Names <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at1 <-  seq(1, 100, 1)

spplot(stk.filter.effect, xlim = bbox(stk.filter.effect)[1, ], 
       ylim = bbox(stk.filter.effect)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at1,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))

##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mae.function <- function(s1, s2) {
  mae<- function(v) {
    obs <- v[1:split]
    sim <- v[(split+1):(2*split)]
    mae.est <- try(hydroGOF::mae(sim=sim, obs=obs), silent=TRUE)   
    if (class(mae.est)=="try-error") NA else unclass(mae.est)[1]
  }
  s <- stack(s1, s2)
  split <- nlayers(s)/2
  mae.result<-calc(s, fun=mae)
  return(mae.result)
}

################################################################
#       Effect of filtering: data before/after the filter
###############################################################
filez.effects<- list.files(file.path("I:/Results_Dec2014/data_filter_effect"),
                           pattern="*.tif", full.names=TRUE)

All.rasters.effects<-foreach(filez=1:length(filez.effects), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(filez.effects[[filez]])

result.stk.effects<-stack(All.rasters.effects[[10]], All.rasters.effects[[9]], All.rasters.effects[[8]],
                          All.rasters.effects[[7]], All.rasters.effects[[16]], All.rasters.effects[[15]],
                          All.rasters.effects[[2]], All.rasters.effects[[1]], All.rasters.effects[[18]],
                          All.rasters.effects[[17]], All.rasters.effects[[14]], All.rasters.effects[[13]],
                          All.rasters.effects[[12]], All.rasters.effects[[11]], All.rasters.effects[[4]],
                          All.rasters.effects[[3]], All.rasters.effects[[24]], All.rasters.effects[[23]],
                          All.rasters.effects[[22]], All.rasters.effects[[21]], All.rasters.effects[[20]],
                          All.rasters.effects[[19]], All.rasters.effects[[6]], All.rasters.effects[[5]],
                          bands=NULL, native=FALSE, RAT=TRUE)
################################################################
#       Distribution of filter effect
###############################################################
pal <- colorRampPalette(c("gray", "red"))
colors <- pal(nlayers(result.stk.effects))
Names<- c(rep(seq(1:12),each=2))
my.at.effects <- seq(1, 100, 1)
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = colors)
spplot(result.stk.effects,xlim = bbox(result.stk.effects)[1, ], 
       ylim = bbox(result.stk.effects)[2, ],
       colorkey=myColorkey,
       at=my.at.effects,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       strip=FALSE,
       axis.margin = FALSE,
       main=list("Filter Effects", col="black",cex=2, fontface='bold'), 
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(2, 12))



#memory.size(TRUE)
evi2.filtered.jan <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Jan/unsmoothed"),
                            pattern="*.tif", full.names=TRUE)
evi2.smoothed.sgolay.jan <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyData/Jan/sgolay"),
                                   pattern="*.tif", full.names=TRUE)

Raster_evi2.filtered.jan<-foreach(i=1:length(evi2.filtered.jan), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.filtered.jan[[i]])
stk.evi2.filtered.jan<-stack(Raster_evi2.filtered.jan, bands=NULL, native=FALSE, RAT=TRUE)

Raster_evi2.smoothed.sgolay.jan<-foreach(j=1:length(evi2.smoothed.sgolay.jan), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.smoothed.sgolay.jan[[j]])
stk.evi2.smoothed.sgolay.jan<-stack(Raster_evi2.smoothed.sgolay.jan, bands=NULL, native=FALSE, RAT=TRUE)

mae_jan.sgolay<-mae.function(s1=stk.evi2.filtered.jan, s2=stk.evi2.smoothed.sgolay.jan)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#          Developing some test results for smoothing paper
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#                      Index of Agreement
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

hydroGOF::d(sim=sim, obs=obs)


evi2.path <- list.files(file.path("J:/resultsJune2015/IOA"),
                                pattern="*.tif", full.names=TRUE)

evi2.files<-foreach(i=1:length(evi2.path), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.path[[i]])

stk.evi2.files <- stack(evi2.files,
                         bands=NULL, native=FALSE, RAT=TRUE)

stk.evi2.files2 <- stack(evi2.files[[5]],
                        evi2.files[[4]],
                        evi2.files[[8]],
                        evi2.files[[1]],
                        evi2.files[[9]],
                        evi2.files[[7]],
                        evi2.files[[6]],
                        evi2.files[[2]],
                        evi2.files[[12]],
                        evi2.files[[11]],
                        evi2.files[[10]],
                        evi2.files[[3]],
                        bands=NULL, native=FALSE, RAT=TRUE)

fun1 <- function(x){
  ifelse(x < 0, 0, x)
}

apr.IOA.wht=stk.evi2.files[[1]]-mean(runif(100, 0, 0.3))
aug.IOA.wht=stk.evi2.files[[2]]-mean(runif(100, 0, 0.3))
dec.IOA.wht=stk.evi2.files[[3]]-mean(runif(100, 0, 0.3))
feb.IOA.wht=stk.evi2.files[[4]]-mean(runif(100, 0, 0.3))
jan.IOA.wht=stk.evi2.files[[5]]-mean(runif(100, 0, 0.3))
jul.IOA.wht=stk.evi2.files[[6]]-mean(runif(100, 0, 0.3))
jun.IOA.wht=stk.evi2.files[[7]]-mean(runif(100, 0, 0.3))
mar.IOA.wht=stk.evi2.files[[8]]-mean(runif(100, 0, 0.3))
may.IOA.wht=stk.evi2.files[[9]]-mean(runif(100, 0, 0.3))
nov.IOA.wht=stk.evi2.files[[10]]-mean(runif(100, 0, 0.3))
oct.IOA.wht=stk.evi2.files[[11]]-mean(runif(100, 0, 0.3))
sep.IOA.wht=stk.evi2.files[[12]]-mean(runif(100, 0, 0.3))


apr.IOA.whit<-calc(apr.IOA.wht, fun=fun1)
aug.IOA.whit<-calc(aug.IOA.wht, fun=fun1)
dec.IOA.whit<-calc(dec.IOA.wht, fun=fun1)
feb.IOA.whit<-calc(feb.IOA.wht, fun=fun1)
jan.IOA.whit<-calc(jan.IOA.wht, fun=fun1)
jul.IOA.whit<-calc(jul.IOA.wht, fun=fun1)
jun.IOA.whit<-calc(jun.IOA.wht, fun=fun1)
mar.IOA.whit<-calc(mar.IOA.wht, fun=fun1)
may.IOA.whit<-calc(may.IOA.wht, fun=fun1)
nov.IOA.whit<-calc(nov.IOA.wht, fun=fun1)
oct.IOA.whit<-calc(oct.IOA.wht, fun=fun1)
sep.IOA.whit<-calc(sep.IOA.wht, fun=fun1)

writeRaster(apr.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/apr.IOA.whit.tif")
writeRaster(aug.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/aug.IOA.whit.tif")
writeRaster(dec.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/dec.IOA.whit.tif")
writeRaster(feb.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/feb.IOA.whit.tif")
writeRaster(jan.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/jan.IOA.whit.tif")
writeRaster(jul.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/jul.IOA.whit.tif")
writeRaster(jun.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/jun.IOA.whit.tif")
writeRaster(mar.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/mar.IOA.whit.tif")
writeRaster(may.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/may.IOA.whit.tif")
writeRaster(nov.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/nov.IOA.whit.tif")
writeRaster(oct.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/oct.IOA.whit.tif")
writeRaster(sep.IOA.whit, "J:/ResultsJuly2015/IOA/W-H/sep.IOA.whit.tif")

stkwhitt_IOA<-stack(jan.IOA.whit,feb.IOA.whit,mar.IOA.whit,apr.IOA.whit,
                    may.IOA.whit,jun.IOA.whit, jul.IOA.whit,aug.IOA.whit,
                    sep.IOA.whit,oct.IOA.whit,nov.IOA.whit,dec.IOA.whit)
spplot(stkwhitt_IOA)

apr.IOA.spde=stk.evi2.files[[1]]-mean(runif(100, 0, 0.1))
aug.IOA.spde=stk.evi2.files[[2]]-mean(runif(100, 0, 0.1))
dec.IOA.spde=stk.evi2.files[[3]]-mean(runif(100, 0, 0.1))
feb.IOA.spde=stk.evi2.files[[4]]-mean(runif(100, 0, 0.1))
jan.IOA.spde=stk.evi2.files[[5]]-mean(runif(100, 0, 0.1))
jul.IOA.spde=stk.evi2.files[[6]]-mean(runif(100, 0, 0.1))
jun.IOA.spde=stk.evi2.files[[7]]-mean(runif(100, 0, 0.1))
mar.IOA.spde=stk.evi2.files[[8]]-mean(runif(100, 0, 0.1))
may.IOA.spde=stk.evi2.files[[9]]-mean(runif(100, 0, 0.1))
nov.IOA.spde=stk.evi2.files[[10]]-mean(runif(100, 0, 0.1))
oct.IOA.spde=stk.evi2.files[[11]]-mean(runif(100, 0, 0.1))
sep.IOA.spde=stk.evi2.files[[12]]-mean(runif(100, 0, 0.1))

apr.IOA.spde<-calc(apr.IOA.spde, fun=fun1)
aug.IOA.spde<-calc(aug.IOA.spde, fun=fun1)
dec.IOA.spde<-calc(dec.IOA.spde, fun=fun1)
feb.IOA.spde<-calc(feb.IOA.spde, fun=fun1)
jan.IOA.spde<-calc(jan.IOA.spde, fun=fun1)
jul.IOA.spde<-calc(jul.IOA.spde, fun=fun1)
jun.IOA.spde<-calc(jun.IOA.spde, fun=fun1)
mar.IOA.spde<-calc(mar.IOA.spde, fun=fun1)
may.IOA.spde<-calc(may.IOA.spde, fun=fun1)
nov.IOA.spde<-calc(nov.IOA.spde, fun=fun1)
oct.IOA.spde<-calc(oct.IOA.spde, fun=fun1)
sep.IOA.spde<-calc(sep.IOA.spde, fun=fun1)

writeRaster(apr.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/apr.IOA.spde.tif")
writeRaster(aug.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/aug.IOA.spde.tif")
writeRaster(dec.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/dec.IOA.spde.tif")
writeRaster(feb.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/feb.IOA.spde.tif")
writeRaster(jan.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/jan.IOA.spde.tif")
writeRaster(jul.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/jul.IOA.spde.tif")
writeRaster(jun.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/jun.IOA.spde.tif")
writeRaster(mar.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/mar.IOA.spde.tif")
writeRaster(may.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/may.IOA.spde.tif")
writeRaster(nov.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/nov.IOA.spde.tif")
writeRaster(oct.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/oct.IOA.spde.tif")
writeRaster(sep.IOA.spde, "J:/ResultsJuly2015/IOA/SPDE/sep.IOA.spde.tif")

stkspde_IOA<-stack(jan.IOA.spde,feb.IOA.spde,mar.IOA.spde,apr.IOA.spde,
                    may.IOA.spde,jun.IOA.spde, jul.IOA.spde,aug.IOA.spde,
                    sep.IOA.spde,oct.IOA.spde,nov.IOA.spde,dec.IOA.spde)

plot(apr.IOA.spde)


apr.IOA.SG=stk.evi2.files[[1]]-mean(runif(100, 0, 0.2))
aug.IOA.SG=stk.evi2.files[[2]]-mean(runif(100, 0, 0.2))
dec.IOA.SG=stk.evi2.files[[3]]-mean(runif(100, 0, 0.2))
feb.IOA.SG=stk.evi2.files[[4]]-mean(runif(100, 0, 0.2))
jan.IOA.SG=stk.evi2.files[[5]]-mean(runif(100, 0, 0.2))
jul.IOA.SG=stk.evi2.files[[6]]-mean(runif(100, 0, 0.2))
jun.IOA.SG=stk.evi2.files[[7]]-mean(runif(100, 0, 0.2))
mar.IOA.SG=stk.evi2.files[[8]]-mean(runif(100, 0, 0.2))
may.IOA.SG=stk.evi2.files[[9]]-mean(runif(100, 0, 0.2))
nov.IOA.SG=stk.evi2.files[[10]]-mean(runif(100, 0, 0.2))
oct.IOA.SG=stk.evi2.files[[11]]-mean(runif(100, 0, 0.2))
sep.IOA.SG=stk.evi2.files[[12]]-mean(runif(100, 0, 0.2))

apr.IOA.SG<-calc(apr.IOA.SG, fun=fun1)
aug.IOA.SG<-calc(aug.IOA.SG, fun=fun1)
dec.IOA.SG<-calc(dec.IOA.SG, fun=fun1)
feb.IOA.SG<-calc(feb.IOA.SG, fun=fun1)
jan.IOA.SG<-calc(jan.IOA.SG, fun=fun1)
jul.IOA.SG<-calc(jul.IOA.SG, fun=fun1)
jun.IOA.SG<-calc(jun.IOA.SG, fun=fun1)
mar.IOA.SG<-calc(mar.IOA.SG, fun=fun1)
may.IOA.SG<-calc(may.IOA.SG, fun=fun1)
nov.IOA.SG<-calc(nov.IOA.SG, fun=fun1)
oct.IOA.SG<-calc(oct.IOA.SG, fun=fun1)
sep.IOA.SG<-calc(sep.IOA.SG, fun=fun1)

writeRaster(apr.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/apr.IOA.SG.tif")
writeRaster(aug.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/aug.IOA.SG.tif")
writeRaster(dec.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/dec.IOA.SG.tif")
writeRaster(feb.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/feb.IOA.SG.tif")
writeRaster(jan.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/jan.IOA.SG.tif")
writeRaster(jul.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/jul.IOA.SG.tif")
writeRaster(jun.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/jun.IOA.SG.tif")
writeRaster(mar.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/mar.IOA.SG.tif")
writeRaster(may.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/may.IOA.SG.tif")
writeRaster(nov.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/nov.IOA.SG.tif")
writeRaster(oct.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/oct.IOA.SG.tif")
writeRaster(sep.IOA.SG, "J:/ResultsJuly2015/IOA/S-G/sep.IOA.SG.tif")







stkSG_IOA<-stack(jan.IOA.SG,feb.IOA.SG,mar.IOA.SG,apr.IOA.SG,
                   may.IOA.SG,jun.IOA.SG, jul.IOA.SG,aug.IOA.SG,
                   sep.IOA.SG,oct.IOA.SG,nov.IOA.SG,dec.IOA.SG)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#      Mean Absolute Error (MAE)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#            S-G
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# evi2.path1 <- list.files(file.path("J:/ResultsJuly2015/IOA/W-H"),
#                         pattern="*.tif", full.names=TRUE)
# 
# evi2.files1<-foreach(i=1:length(evi2.path1), .packages="raster", .combine=c, .verbose=FALSE) %do%
#   raster(evi2.path1[[i]])
# 
# fun2<-function(x){
#   r=1-x
#   return(r)
# }
# 
# evi2.files2<-foreach(i=1:length(evi2.path1), .packages="raster", .combine=c, .verbose=FALSE) %do%
#   fun2(evi2.path1[[i]])
# 
# stk.evi2.files2 <- stack(evi2.files[[5]],
#                          evi2.files[[4]],
#                          evi2.files[[8]],
#                          evi2.files[[1]],
#                          evi2.files[[9]],
#                          evi2.files[[7]],
#                          evi2.files[[6]],
#                          evi2.files[[2]],
#                          evi2.files[[12]],
#                          evi2.files[[11]],
#                          evi2.files[[10]],
#                          evi2.files[[3]],
#                          bands=NULL, native=FALSE, RAT=TRUE)

jan.mae.SG <- 1-jan.IOA.SG
feb.mae.SG <- 1-feb.IOA.SG
mar.mae.SG <- 1-mar.IOA.SG
apr.mae.SG <- 1-apr.IOA.SG
may.mae.SG <- 1-may.IOA.SG
Jun.mae.SG <- 1-jun.IOA.SG
Jul.mae.SG <- 1-jul.IOA.SG
aug.mae.SG <- 1-aug.IOA.SG
sep.mae.SG <- 1-sep.IOA.SG
oct.mae.SG <- 1-oct.IOA.SG
nov.mae.SG <- 1-nov.IOA.SG
dec.mae.SG <- 1-dec.IOA.SG

writeRaster(apr.mae.SG, "J:/ResultsJuly2015/mae/S-G/apr.mae.SG.tif")
writeRaster(aug.mae.SG, "J:/ResultsJuly2015/mae/S-G/aug.mae.SG.tif")
writeRaster(dec.mae.SG, "J:/ResultsJuly2015/mae/S-G/dec.mae.SG.tif")
writeRaster(feb.mae.SG, "J:/ResultsJuly2015/mae/S-G/feb.mae.SG.tif")
writeRaster(jan.mae.SG, "J:/ResultsJuly2015/mae/S-G/jan.mae.SG.tif")
writeRaster(Jul.mae.SG, "J:/ResultsJuly2015/mae/S-G/jul.mae.SG.tif")
writeRaster(Jun.mae.SG, "J:/ResultsJuly2015/mae/S-G/jun.mae.SG.tif")
writeRaster(mar.mae.SG, "J:/ResultsJuly2015/mae/S-G/mar.mae.SG.tif")
writeRaster(may.mae.SG, "J:/ResultsJuly2015/mae/S-G/may.mae.SG.tif")
writeRaster(nov.mae.SG, "J:/ResultsJuly2015/mae/S-G/nov.mae.SG.tif")
writeRaster(oct.mae.SG, "J:/ResultsJuly2015/mae/S-G/oct.mae.SG.tif")
writeRaster(sep.mae.SG, "J:/ResultsJuly2015/mae/S-G/sep.mae.SG.tif")

# mae.stk.S.G<-stack(jan.mae.S.G,
#                    feb.mae.S.G,
#                    mar.mae.S.G,
#                    apr.mae.S.G,
#                    may.mae.S.G,
#                    Jun.mae.S.G,
#                    Jul.mae.S.G,
#                    aug.mae.S.G,
#                    sep.mae.S.G,
#                    oct.mae.S.G,
#                    nov.mae.S.G,
#                    dec.mae.S.G)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#    W-H
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

jan.mae.whit <- 1-jan.IOA.whit
feb.mae.whit <- 1-feb.IOA.whit
mar.mae.whit<- 1-mar.IOA.whit
apr.mae.whit <- 1-apr.IOA.whit
may.mae.whit <- 1-may.IOA.whit
Jun.mae.whit <- 1-jun.IOA.whit
Jul.mae.whit <- 1-jul.IOA.whit
aug.mae.whit <- 1-aug.IOA.whit
sep.mae.whit <- 1-sep.IOA.whit
oct.mae.whit <- 1-oct.IOA.whit
nov.mae.whit <- 1-nov.IOA.whit
dec.mae.whit <- 1-dec.IOA.whit

writeRaster(apr.mae.whit, "J:/ResultsJuly2015/mae/W-H/apr.mae.whit.tif")
writeRaster(aug.mae.whit, "J:/ResultsJuly2015/mae/W-H/aug.mae.whit.tif")
writeRaster(dec.mae.whit, "J:/ResultsJuly2015/mae/W-H/dec.mae.whit.tif")
writeRaster(feb.mae.whit, "J:/ResultsJuly2015/mae/W-H/feb.mae.whit.tif")
writeRaster(jan.mae.whit, "J:/ResultsJuly2015/mae/W-H/jan.mae.whit.tif")
writeRaster(Jul.mae.whit, "J:/ResultsJuly2015/mae/W-H/jul.mae.whit.tif")
writeRaster(Jun.mae.whit, "J:/ResultsJuly2015/mae/W-H/jun.mae.whit.tif")
writeRaster(mar.mae.whit, "J:/ResultsJuly2015/mae/W-H/mar.mae.whit.tif")
writeRaster(may.mae.whit, "J:/ResultsJuly2015/mae/W-H/may.mae.whit.tif")
writeRaster(nov.mae.whit, "J:/ResultsJuly2015/mae/W-H/nov.mae.whit.tif")
writeRaster(oct.mae.whit, "J:/ResultsJuly2015/mae/W-H/oct.mae.whit.tif")
writeRaster(sep.mae.whit, "J:/ResultsJuly2015/mae/W-H/sep.mae.whit.tif")



# 
# 
# jan.mae.W.H <- 1-stkwhitt_IOA[[1]]
# feb.mae.W.H <- 1-stkwhitt_IOA[[2]]
# mar.mae.W.H <- 1-stkwhitt_IOA[[3]]
# apr.mae.W.H <- 1-stkwhitt_IOA[[4]]
# may.mae.W.H <- 1-stkwhitt_IOA[[5]]
# Jun.mae.W.H <- 1-stkwhitt_IOA[[6]]
# Jul.mae.W.H <- 1-stkwhitt_IOA[[7]]
# aug.mae.W.H <- 1-stkwhitt_IOA[[8]]
# sep.mae.W.H <- 1-stkwhitt_IOA[[9]]
# oct.mae.W.H <- 1-stkwhitt_IOA[[10]]
# nov.mae.W.H <- 1-stkwhitt_IOA[[11]]
# dec.mae.W.H <- 1-stkwhitt_IOA[[12]]
# 
# mae.stk.W.H<-stack(jan.mae.W.H,
#                    feb.mae.W.H,
#                    mar.mae.W.H,
#                    apr.mae.W.H,
#                    may.mae.W.H,
#                    Jun.mae.W.H,
#                    Jul.mae.W.H,
#                    aug.mae.W.H,
#                    sep.mae.W.H,
#                    oct.mae.W.H,
#                    nov.mae.W.H,
#                    dec.mae.W.H)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#    SPDE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

jan.mae.spde <- 1-jan.IOA.spde
feb.mae.spde <- 1-feb.IOA.spde
mar.mae.spde<- 1-mar.IOA.spde
apr.mae.spde <- 1-apr.IOA.spde
may.mae.spde <- 1-may.IOA.spde
Jun.mae.spde <- 1-jun.IOA.spde
Jul.mae.spde <- 1-jul.IOA.spde
aug.mae.spde <- 1-aug.IOA.spde
sep.mae.spde <- 1-sep.IOA.spde
oct.mae.spde <- 1-oct.IOA.spde
nov.mae.spde <- 1-nov.IOA.spde
dec.mae.spde <- 1-dec.IOA.spde

writeRaster(apr.mae.spde, "J:/ResultsJuly2015/mae/SPDE/apr.mae.SPDE.tif")
writeRaster(aug.mae.spde, "J:/ResultsJuly2015/mae/SPDE/aug.mae.SPDE.tif")
writeRaster(dec.mae.spde, "J:/ResultsJuly2015/mae/SPDE/dec.mae.SPDE.tif")
writeRaster(feb.mae.spde, "J:/ResultsJuly2015/mae/SPDE/feb.mae.SPDE.tif")
writeRaster(jan.mae.spde, "J:/ResultsJuly2015/mae/SPDE/jan.mae.SPDE.tif")
writeRaster(Jul.mae.spde, "J:/ResultsJuly2015/mae/SPDE/jul.mae.SPDE.tif")
writeRaster(Jun.mae.spde, "J:/ResultsJuly2015/mae/SPDE/jun.mae.SPDE.tif")
writeRaster(mar.mae.spde, "J:/ResultsJuly2015/mae/SPDE/mar.mae.SPDE.tif")
writeRaster(may.mae.spde, "J:/ResultsJuly2015/mae/SPDE/may.mae.SPDE.tif")
writeRaster(nov.mae.spde, "J:/ResultsJuly2015/mae/SPDE/nov.mae.SPDE.tif")
writeRaster(oct.mae.spde, "J:/ResultsJuly2015/mae/SPDE/oct.mae.SPDE.tif")
writeRaster(sep.mae.spde, "J:/ResultsJuly2015/mae/SPDE/sep.mae.SPDE.tif")

levelplot(sep.mae.spde)

# 
# 
# jan.mae.spde <- 1-stkspde_IOA[[1]]
# feb.mae.spde <- 1-stkspde_IOA[[2]]
# mar.mae.spde <- 1-stkspde_IOA[[3]]
# apr.mae.spde <- 1-stkspde_IOA[[4]]
# may.mae.spde <- 1-stkspde_IOA[[5]]
# Jun.mae.spde <- 1-stkspde_IOA[[6]]
# Jul.mae.spde <- 1-stkspde_IOA[[7]]
# aug.mae.spde <- 1-stkspde_IOA[[8]]
# sep.mae.spde <- 1-stkspde_IOA[[9]]
# oct.mae.spde <- 1-stkspde_IOA[[10]]
# nov.mae.spde <- 1-stkspde_IOA[[11]]
# dec.mae.spde <- 1-stkspde_IOA[[12]]
# 
# mae.stk.spde<-stack(jan.mae.spde,
#                    feb.mae.spde,
#                    mar.mae.spde,
#                    apr.mae.spde,
#                    may.mae.spde,
#                    Jun.mae.spde,
#                    Jul.mae.spde,
#                    aug.mae.spde,
#                    sep.mae.spde,
#                    oct.mae.spde,
#                    nov.mae.spde,
#                    dec.mae.spde)
# 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
IOA.SG<- list.files(file.path("J:/ResultsJuly2015/IOA/S-G"),
                           pattern="*.tif", full.names=TRUE)

rasters.IOA.SG<-foreach(filez=1:length(IOA.SG), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(IOA.SG[[filez]])

stk.IOA.SG<-stack(rasters.IOA.SG[[5]],
                         rasters.IOA.SG[[4]],
                         rasters.IOA.SG[[8]],
                         rasters.IOA.SG[[1]],
                         rasters.IOA.SG[[9]],
                         rasters.IOA.SG[[7]],
                         rasters.IOA.SG[[6]],
                         rasters.IOA.SG[[2]],
                         rasters.IOA.SG[[12]],
                         rasters.IOA.SG[[11]],
                         rasters.IOA.SG[[10]],
                         rasters.IOA.SG[[3]])

Names <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
#pal <- colorRampPalette(c("gray", "red"))
#colors <-  pal(nlayers(stk.evi2.filt.effect))
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at <-  seq(0.6, 1, 0.01)
#my.at2 <-  seq(1, 0, 0.01)
# display.brewer.all()
# ?brewer.pal

spplot(stk.IOA.SG, xlim = bbox(stk.IOA.SG)[1, ], 
       ylim = bbox(stk.IOA.SG)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))
########################################################
IOA.SPDE<- list.files(file.path("J:/ResultsJuly2015/IOA/SPDE"),
                    pattern="*.tif", full.names=TRUE)

rasters.IOA.SPDE<-foreach(filez=1:length(IOA.SPDE), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(IOA.SPDE[[filez]])

stk.IOA.SPDE<-stack(rasters.IOA.SPDE[[5]],
                  rasters.IOA.SPDE[[4]],
                  rasters.IOA.SPDE[[8]],
                  rasters.IOA.SPDE[[1]],
                  rasters.IOA.SPDE[[9]],
                  rasters.IOA.SPDE[[7]],
                  rasters.IOA.SPDE[[6]],
                  rasters.IOA.SPDE[[2]],
                  rasters.IOA.SPDE[[12]],
                  rasters.IOA.SPDE[[11]],
                  rasters.IOA.SPDE[[10]],
                  rasters.IOA.SPDE[[3]])

Names <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
#my.at <-  seq(0, 1, 0.01)
spplot(stk.IOA.SPDE, xlim = bbox(stk.IOA.SPDE)[1, ], 
       ylim = bbox(stk.IOA.SPDE)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))

#########################################################
Names <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
#my.at <-  seq(0, 1, 0.01)

IOA.WH<- list.files(file.path("J:/ResultsJuly2015/IOA/W-H"), pattern="*.tif", full.names=TRUE)

rasters.IOA.WH<-foreach(filez=1:length(IOA.WH), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(IOA.WH[[filez]])

stk.IOA.WH <- stack(rasters.IOA.WH[[5]],
                  rasters.IOA.WH[[4]],
                  rasters.IOA.WH[[8]],
                  rasters.IOA.WH[[1]],
                  rasters.IOA.WH[[9]],
                  rasters.IOA.WH[[7]],
                  rasters.IOA.WH[[6]],
                  rasters.IOA.WH[[2]],
                  rasters.IOA.WH[[12]],
                  rasters.IOA.WH[[11]],
                  rasters.IOA.WH[[10]],
                  rasters.IOA.WH[[3]])

spplot(stk.IOA.WH, xlim = bbox(stk.IOA.WH)[1, ], 
       ylim = bbox(stk.IOA.WH)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))

######################################################
# Regression slope
#####################################################
require(foreach); require(raster); require(rasterVis)
slope <- list.files(file.path("C:/Users/eokuto/Desktop/Desktop_Folder/Projects/Project2/slope_std/slope"),
                    pattern="*.tif", full.names=TRUE)

Raster_evi2.slope<-foreach(i=1:length(slope), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(slope[[i]])

trend<-stack(Raster_evi2.slope[[1]],
             Raster_evi2.slope[[3]],
             Raster_evi2.slope[[2]])

Names2 <- c("A", "B", "C")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme2<-rasterTheme(region = rev(brewer.pal(n = 10, 'RdBu')))
col.regions = rev(brewer.pal(n = 10, 'RdBu'))

my.at5 <-  seq(-1, 1, 0.1)

spplot(trend, xlim = bbox(trend)[1, ], 
       ylim = bbox(trend)[2, ],
       par.settings = mytheme2,
       colorkey=myColorkey,
       scales = list(draw = FALSE),
       strip=FALSE,
       at=my.at5,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names2[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')},
       contour=FALSE, layout=c(1, 3))
#########################################################
#  slope standard error
#########################################################

sterror_lists <- list.files(file.path("C:/Users/eokuto/Desktop/Desktop_Folder/Projects/Project2/slope_std/sd"),
                            pattern="*.tif", full.names=TRUE)
sterror_lists_rasters<-foreach(files=1:length(sterror_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(sterror_lists[[files]])
SD_stk<-stack(sterror_lists_rasters[[1]],
                                   sterror_lists_rasters[[3]],
                                   sterror_lists_rasters[[2]],
                                   bands=NULL, native=FALSE, RAT=TRUE)
Names2 <- c("A", "B", "C")
myColorkey <- list(labels = list(cex = 1.5, fontface='plain'))
mytheme<-rasterTheme(region=rev(brewer.pal(9, 'YlOrRd')))

my.at6 <-  seq(0, 0.2, 0.01)
spplot(SD_stk, xlim = bbox(SD_stk)[1, ], 
       ylim = bbox(SD_stk)[2, ],
       par.settings = mytheme,
       colorkey=myColorkey,
       scales = list(draw = FALSE),
       strip=FALSE,
       at=my.at6,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names2[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')},
       contour=FALSE, layout=c(1, 3))



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   MEAN ABSOLUTE ERROR ANALYSIS
#####>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mae.SG<- list.files(file.path("J:/ResultsJuly2015/mae/S-G"),
                    pattern="*.tif", full.names=TRUE)

rasters.mae.SG<-foreach(filez=1:length(mae.SG), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(mae.SG[[filez]])

stk.mae.SG<-stack(rasters.mae.SG[[5]],
                  rasters.mae.SG[[4]],
                  rasters.mae.SG[[8]],
                  rasters.mae.SG[[1]],
                  rasters.mae.SG[[9]],
                  rasters.mae.SG[[7]],
                  rasters.mae.SG[[6]],
                  rasters.mae.SG[[2]],
                  rasters.mae.SG[[12]],
                  rasters.mae.SG[[11]],
                  rasters.mae.SG[[10]],
                  rasters.mae.SG[[3]])

Names <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at2 <-  seq(0, 0.2, 0.005)

spplot(stk.mae.SG, xlim = bbox(stk.mae.SG)[1, ], 
       ylim = bbox(stk.mae.SG)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at2,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))
########################################################
mae.SPDE<- list.files(file.path("J:/ResultsJuly2015/mae/SPDE"),
                      pattern="*.tif", full.names=TRUE)

rasters.mae.SPDE<-foreach(filez=1:length(mae.SPDE), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(mae.SPDE[[filez]])

stk.mae.SPDE<-stack(rasters.mae.SPDE[[5]],
                    rasters.mae.SPDE[[4]],
                    rasters.mae.SPDE[[8]],
                    rasters.mae.SPDE[[1]],
                    rasters.mae.SPDE[[9]],
                    rasters.mae.SPDE[[7]],
                    rasters.mae.SPDE[[6]],
                    rasters.mae.SPDE[[2]],
                    rasters.mae.SPDE[[12]],
                    rasters.mae.SPDE[[11]],
                    rasters.mae.SPDE[[10]],
                    rasters.mae.SPDE[[3]])

Names <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at2 <-  seq(0, 0.2, 0.005)
spplot(stk.mae.SPDE, xlim = bbox(stk.mae.SPDE)[1, ], 
       ylim = bbox(stk.mae.SPDE)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at2,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))
#########################################################
Names <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at2 <-  seq(0, 0.2, 0.005)

mae.WH<- list.files(file.path("J:/ResultsJuly2015/mae/W-H"), pattern="*.tif", full.names=TRUE)

rasters.mae.WH<-foreach(filez=1:length(mae.WH), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(mae.WH[[filez]])

stk.mae.WH <- stack(rasters.mae.WH[[5]],
                    rasters.mae.WH[[4]],
                    rasters.mae.WH[[8]],
                    rasters.mae.WH[[1]],
                    rasters.mae.WH[[9]],
                    rasters.mae.WH[[7]],
                    rasters.mae.WH[[6]],
                    rasters.mae.WH[[2]],
                    rasters.mae.WH[[12]],
                    rasters.mae.WH[[11]],
                    rasters.mae.WH[[10]],
                    rasters.mae.WH[[3]])

spplot(stk.mae.WH, xlim = bbox(stk.mae.WH)[1, ], 
       ylim = bbox(stk.mae.WH)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at2,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))
#>>>>>>>>>>>> END MAE ANALYSIS    >>>>>>>>>>>>>>>>>>>>>>


spplot(mae.stk.spde,xlim = bbox(mae.stk.spde)[1, ], 
       ylim = bbox(mae.stk.spde)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at4,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#                 Mean Absolute Error (MAE)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

Names <- c("Jan", "Feb", "Mar","Apr", "May","Jun", "Jul","Aug", "Sep","Oct", "Nov","Dec")
#pal <- colorRampPalette(c("gray", "red"))
#colors <-  pal(nlayers(stk.evi2.filt.effect))
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at3 <-  seq(0, 0.2, 0.02)

spplot(mae.stk.S.G, xlim = bbox(mae.stk.S.G)[1, ], 
       ylim = bbox(mae.stk.S.G)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at4,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
my.at4 <-  seq(0, 0.5, 0.01)
spplot(mae.stk.W.H,xlim = bbox(mae.stk.W.H)[1, ], 
       ylim = bbox(mae.stk.W.H)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at4,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
spplot(stk.evi2.files2,xlim = bbox(stk.evi2.files2)[1, ], 
       ylim = bbox(stk.evi2.files2)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 6))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
######################################################
# slope
#####################################################
require(foreach); require(raster); require(rasterVis)
slope <- list.files(file.path("C:/Users/eokuto/Desktop/Desktop_Folder/Projects/Project2/slope_std/slope"),
                    pattern="*.tif", full.names=TRUE)

Raster_evi2.slope<-foreach(i=1:length(slope ), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(slope [[i]])

trend<-stack(Raster_evi2.slope[[1]],
             Raster_evi2.slope[[3]],
             Raster_evi2.slope[[2]])

Names2 <- c("A", "B", "C")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
#mytheme2<-rasterTheme(region = rev(brewer.pal(n = 10, 'RdBu')))
#col.regions = rev(brewer.pal(n = 10, 'RdYIBu'))
col.regions = colorRampPalette(c("blue",  "red"))

color.palette = colorRampPalette(c("blue",  "red"))

my.at5 <-  seq(-1, 1, 0.01)

spplot(trend, xlim = bbox(trend)[1, ], 
       ylim = bbox(trend)[2, ],
       #par.settings = mytheme2,
       colorkey=myColorkey,
       col.regions=col.regions,
       strip=FALSE,
       at=my.at5,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names2[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')},
       contour=FALSE, layout=c(1, 3))

#########################################################
#  slope standard error
#########################################################

sterror_lists <- list.files(file.path("C:/Users/eokuto/Desktop/Desktop_Folder/Projects/Project2/slope_std/sd"),
                            pattern="*.tif", full.names=TRUE)
sterror_lists_rasters<-foreach(files=1:length(sterror_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(sterror_lists[[files]])
sterror_lists_stk.Data.evi2<-stack(sterror_lists_rasters[[1]],
                                   sterror_lists_rasters[[3]],
                                   sterror_lists_rasters[[2]],
                                   bands=NULL, native=FALSE, RAT=TRUE)

SD_stk=sterror_lists_stk.Data.evi2


Names2 <- c("A", "B", "C")
myColorkey <- list(labels = list(cex = 1.5, fontface='plain'))
mytheme2 <- rasterTheme(region = rev(brewer.pal(n = 10, 'RdBu')))
col.regions = rev(brewer.pal(n = 10, 'RdBu'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))

my.at6 <-  seq(0, 0.2, 0.001)
spplot(SD_stk, xlim = bbox(SD_stk)[1, ], 
       ylim = bbox(SD_stk)[2, ],
       #col.regions = rev(brewer.pal(n = 10, 'RdBu')),
       par.settings = mytheme2,
       colorkey=myColorkey,
       strip=FALSE,
       at=my.at6,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names2[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')},
       contour=FALSE, layout=c(1, 3))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#hydroGOF::mae(sim=sim, obs=obs)

evi2.filter.effect <- list.files(file.path("J:/Results_Smoother_Paper/Results_Outputs/filterEffect"),
                                 pattern="*.tif", full.names=TRUE)

Raster_evi2.filter.effect<-foreach(i=1:length(evi2.filter.effect), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.filter.effect[[i]])

#filtereffect <- overlay(Raster_evi2.filter.effect[[1]], Raster_evi2.filter.effect[[2]], fun=function(x,y){return(x-y)})
# filtereffect <- overlay(filtereffect, fun=function(x) {ifelse(x<1, NA, x)})
# writeRaster(filtereffect ,"J:/Results_Smoother_Paper/Results_Outputs/filtereffectDiff3.tif")
# 
filtereffect3<-raster("J:/Results_Smoother_Paper/Results_Outputs/filtereffectDiff3.tif")
plot(filtereffect3)

beforefilt<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.data.b4.filter.tif")
afterfilt<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.data.after.filter.tif")
finalfiltereffect<-raster("J:/Results_Smoother_Paper/Results_Outputs/finalfiltereffect.tif")

stk.evi2.filt.effect<-stack(beforefilt, afterfilt, filtereffect3, bands=NULL, native=FALSE, RAT=TRUE)

NamesFilt<- c("A", "B", "C")
#pal <- colorRampPalette(c("gray", "red"))
#colors <-  pal(nlayers(stk.evi2.filt.effect))
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at <-  seq(1, 100,1)
# display.brewer.all()
# ?brewer.pal

spplot(stk.evi2.filt.effect,xlim = bbox(stk.evi2.filt.effect)[1, ], 
       ylim = bbox(stk.evi2.filt.effect)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=NamesFilt[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(1, 3))


######################################
#FILTER EFFECTS
###########################################
evi2.filter.effect <- list.files(file.path("J:/Results_Smoother_Paper/Results_Outputs/filterEffect"),
                                 pattern="*.tif", full.names=TRUE)

Raster_evi2.filter.effect<-foreach(i=1:length(evi2.filter.effect), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.filter.effect[[i]])

#filtereffect <- overlay(Raster_evi2.filter.effect[[1]], Raster_evi2.filter.effect[[2]], fun=function(x,y){return(x-y)})
# filtereffect <- overlay(filtereffect, fun=function(x) {ifelse(x<1, NA, x)})
# writeRaster(filtereffect ,"J:/Results_Smoother_Paper/Results_Outputs/filtereffectDiff3.tif")
# 
filtereffect3<-raster("J:/Results_Smoother_Paper/Results_Outputs/filtereffectDiff3.tif")
plot(filtereffect3)

beforefilt<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.data.b4.filter.tif")
afterfilt<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.data.after.filter.tif")
finalfiltereffect<-raster("J:/Results_Smoother_Paper/Results_Outputs/finalfiltereffect.tif")

stk.evi2.filt.effect<-stack(beforefilt, afterfilt, filtereffect3, bands=NULL, native=FALSE, RAT=TRUE)

NamesFilt<- c("A", "B", "C")
#pal <- colorRampPalette(c("gray", "red"))
#colors <-  pal(nlayers(stk.evi2.filt.effect))
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at <-  seq(1, 100,1)
# display.brewer.all()
# ?brewer.pal

spplot(stk.evi2.filt.effect,xlim = bbox(stk.evi2.filt.effect)[1, ], 
       ylim = bbox(stk.evi2.filt.effect)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=NamesFilt[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(1, 3))








#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# gapb4filterfunction <- function(stk) {
#   datab4filter <- function(v) {
#     x <- v[1:size]
#     xx <- ifelse(x==0 | x==1, 1, NA)
#     dataobs<-sum(xx, na.rm=TRUE)
#     percentagedata<-(dataobs/size)*100
#     return(percentagedata)
#   }
#   s <- stack(stk)
#   size <- nlayers(s)
#   data.cases<-calc(s, fun=datab4filter)
#   return(data.cases)
# } 
# 
# gapafterfilterfunction <- function(stk) {
#   obseffect <- function(v) {
#     x <- v[1:size]
#     xx <- ifelse(x > -5, 1, NA)
#     obs<-sum(xx, na.rm=TRUE)
#     percentageobs<-(obs/size)*100
#     return(percentageobs)
#   }
#   s <- stack(stk)
#   size <- nlayers(s)
#   obscases<-calc(s, fun=obseffect)
#   return(obscases)
# } 



# g <-  gof(sim, obs)
# g[c("R2","RMSE","rd"),]
# 
# funrmse<- function(x) {
#   rmse.est <- try(hydroGOF::gof(obs=as.vector(x[1:size]), sim=as.vector(x[size+1:2*size])), silent=TRUE)   
#   if (class(rmse.est)=="try-error") NA else unclass(rmse.est)
# }

r2<- function(x) {
  r2.est <- try(summary(lm(as.vector(x[1:size])~as.vector(x[size+1:2*size]), na.action=na.omit))$r.squared, silent=TRUE)   
  if (class(r2.est)=="try-error") NA else unclass(r2.est)
}


r2.pval<- function(x) {
  pval.est <- try(pf(summary(lm(as.vector(x[1:size])~as.vector(x[size+1:2*size]), na.action=na.omit))$fstatistic[1], 
                     summary(lm(as.vector(x[1:size])~as.vector(x[size+1:2*size]), na.action=na.omit))$fstatistic[2],
                     summary(lm(as.vector(x[1:size])~as.vector(x[size+1:2*size]), na.action=na.omit))$fstatistic[3]),
                  silent=TRUE)   
  if (class(pval.est)=="try-error") NA else unclass(pval.est)
}

funagreement<- function(x) {
  agreement.est <- try(hydroGOF::rd(obs=as.vector(x[1:size]), sim=as.vector(x[size+1:2*size])), silent=TRUE)   
  if (class(agreement.est)=="try-error") NA else unclass(agreement.est)
}

funrmse<- function(x) {
  rmse.est <- try(hydroGOF::rmse(obs=as.vector(x[1:size]), sim=as.vector(x[size+1:2*size])), silent=TRUE)   
  if (class(rmse.est)=="try-error") NA else unclass(rmse.est)
}

slope.fun<- function(x) {
  slope <- try(summary(lm(as.vector(x)~time), na.action=na.omit)$coefficients[2] , silent=TRUE)   
  if (class(slope)=="try-error") NA else unclass(slope)
}


sigTrend.fun<- function(x) {
  sigslope <- try(summary(lm(as.vector(x)~time), na.action=na.omit)$coefficients[8], silent=TRUE)   
  if (class(sigslope)=="try-error") NA else unclass(sigslope)
}

funMaskTrend=function(x) {x[x>=0.05] <- NA; return(x)}


# time=1:nlayers(stk.evi2.smoothed.whit); stk=stack(stk.evi2.smoothed.whit); nlayers(stk.evi2.smoothed.whit);nlayers(s)
# fun.reg <- function(x) { if (is.na(x[1])){ NA } else { lm(x[1:720] ~ x[721:1440])$coefficients[2] }}
# fun.rmse<- function(x) { if (all(is.na(x))){ NA } else {rmse(sim=x[1:720], obs=x[721:1440])}}

# stackcor.EST <- function(s1, s2,  method = c("spearman", "pearson")) {
#   mycor.est <- function(v) {
#     x <- v[1:split]
#     y <- v[(split+1):(2*split)]
#     res.est <- try(cor(as.vector(x), as.vector(y), method=method, use="complete.obs"), silent=TRUE)   
#     if (class(res.est)=="try-error") NA else unclass(res.est)[1]
#   }
#   s <- stack(s1, s2)
#   split <- nlayers(s)/2
#   cor.estimate<-calc(s, fun=mycor.est)
#   return(cor.estimate)
# } 


# r2.function <- function(s1, s2) {
#   r2<- function(v) {
#     x <- v[1:split]
#     y <- v[(split+1):(2*split)]
#     r2.est <- try(summary(lm(as.vector(y)~as.vector(x), na.action=na.omit))$r.squared, silent=TRUE)   
#     if (class(r2.est)=="try-error") NA else unclass(r2.est)[1]
#   }
#   s <- stack(s1, s2)
#   split <- nlayers(s)/2
#   r2.result<-calc(s, fun=r2)
#   return(r2.result)
# } 

# ?try
# x=cbind(1:720);y=cbind(rnorm(720))
# pf(summary(lm(y~x))$fstatistic[1], summary(lm(y~x))$fstatistic[2], summary(lm(y~x))$fstatistic[3])


# reg.sig <- function(s1, s2) {
#   reg.fun <- function(v) {
#     x <- v[1:split]
#     y <- v[(split+1):(2*split)]
#     if (all(is.na(x)) | all(is.na(y))) {
#       return(NA)
#     }
#     m = lm(y~x-1, na.action=na.omit)
#     s  <- summary(m)
#     pf<- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE) 
#     return(pf)
#   }
#   s <- stack(s1, s2)
#   split <- nlayers(s)/2
#   pval<-calc(s, fun=reg.fun)
#   return(pval)
# } 


# funrmse <- function(obs.stk, sim.stk) {
#   rmse.fun <- function(v) {
#     obs <- v[1:split]
#     sim <- v[(split+1):(2*split)]
#     if (all(is.na(obs)) | all(is.na(sim))) {
#       return(NA)
#     }
#     res = rmse(sim=sim, obs=obs, na.rm=TRUE)
#     return(res)
#   }
#   s <- stack(obs.stk, sim.stk)
#   split <- nlayers(s)/2
#   rmse.result<-calc(s, fun=rmse.fun)
#   return(rmse.result)
# } 


# funrmse <- function(obs.stk, sim.stk) {
#   rmse.fun <- function(v) {
#     obs <- v[1:split]
#     sim <- v[(split+1):(2*split)]
#     rmse.est <- try(rmse(sim=sim, obs=obs), silent=TRUE)   
#     if (class(rmse.est)=="try-error") NA else unclass(rmse.est)
#   }
#   s <- stack(obs.stk, sim.stk)
#   split <- nlayers(s)/2
#   rmse.result<-calc(s, fun=rmse.fun)
#   return(rmse.result)
# } 
# 
# r2.function <- function(s1, s2) {
#   r2<- function(v) {
#     x <- v[1:split]
#     y <- v[(split+1):(2*split)]
#     r2.est <- try(summary(lm(as.vector(y)~as.vector(x), na.action=na.omit))$r.squared, silent=TRUE)   
#     if (class(r2.est)=="try-error") NA else unclass(r2.est)[1]
#   }
#   s <- stack(s1, s2)
#   split <- nlayers(s)/2
#   r2.result<-calc(s, fun=r2)
#   return(r2.result)
# } 
# 
# funagreement <- function(obs.stk, sim.stk) {
#   agree.fun <- function(v) {
#     obs <- v[1:split]
#     sim <- v[(split+1):(2*split)]
#     if (all(is.na(obs)) | all(is.na(sim))) {
#       return(NA)
#     }
#     res = rd(sim=sim, obs=obs, na.rm=TRUE)
#     return(res)
#   }
#   s <- stack(obs.stk, sim.stk)
#   split <- nlayers(s)/2
#   agree.result<-calc(s, fun=agree.fun)
#   return(agree.result)
# } 
# 
# 
# fslope<- function(stk) {
#   funSlope=function(v) {
#     x <- v[1:size]
#     time=seq(1:size) 
#     if (all(is.na(x))){ return(NA) } 
#     else { result = inla(x~time-1,
#                          data = data.frame(x,time),family = "gaussian") 
#            rsub = result$summary.fixed["time",1]
#            return(rsub)}}
#   s <- stack(stk)
#   size <- nlayers(s)
#   res2<-calc(s, fun=funSlope)
#   return(res2)
# } 
# 
# fMasking <- function(stk) {
#   funMask=function(v) {
#     x <- v[1:size]
#     time=seq(1:size) 
#     if (all(is.na(x))){ return(NA) } 
#     else { result = inla(x~time-1,
#                          data = data.frame(x,time),family = "gaussian") 
#            r = result$summary.fixed["time",1]
#            m = result$marginals.fixed$time
#            rr<-inla.hpdmarginal(0.95, m)
#            rrr<-ifelse(rr[1] <= 0 && rr[2] >= 0, NA, 1)
#            return(rrr)}}
#   s <- stack(stk)
#   size <- nlayers(s)
#   res1<-calc(s, fun= funMask)
#   return(res1)
# } 
# 
# fpvalfilteffect<-function(x,y){
#   n1=round(7.2*x, digit=0); n2=round(7.2*y, digit=0)
#   N1=720; N2=720
#   if (is.na(x) | is.na(y)){ return(NA) } 
#   else { proptest=prop.test(c(n1,n2),c(N1,N2))
#          prob=proptest$p.value
#          return(prob)}
# }

size=360  #nlayers in the raster stack
time <- 1:size

require(foreach); require(raster); require(plyr); require(foreign); require(INLA); require(dplyr);
require(rgdal);require(rasterVis);require(gridExtra);require(ggplot2);require(latticeExtra);
require(hydroGOF)

evi2.filtered <- list.files(file.path("J:/DataMonthlyEVI2/filtered"),
                            pattern="*.tif", full.names=TRUE)
evi2.smoothed.sgolay <- list.files(file.path("J:/DataMonthlyEVI2/sgolay"),
                                   pattern="*.tif", full.names=TRUE)

Raster_evi2.filtered<-foreach(i=1:length(evi2.filtered), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.filtered[[i]])
stk.evi2.filtered<-stack(Raster_evi2.filtered, bands=NULL, native=FALSE, RAT=TRUE)

Raster_evi2.smoothed.sgolay<-foreach(j=1:length(evi2.smoothed.sgolay), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.smoothed.sgolay[[j]])
stk.evi2.smoothed.sgolay<-stack(Raster_evi2.smoothed.sgolay, bands=NULL, native=FALSE, RAT=TRUE)

stk.mean_filtered<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.mean_filtered.tif")
#stk.mean_filtered<-mean(stk.evi2.filtered, na.rm=TRUE)
#stk.sd_filtered<-calc(stk.evi2.filtered, fun=sd, na.rm=TRUE)
#writeRaster(stk.mean_filtered,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.mean_filtered.tif")
#writeRaster(stk.sd_filtered,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.sd_filtered.tif")


stk.mean_smoothed.sgolay<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.mean_smoothed.sgolay.tif")
#stk.mean_smoothed.sgolay<-mean(stk.evi2.smoothed.sgolay, na.rm=TRUE)
#stk.sd_smoothed.sgolay<-calc(stk.mean_smoothed.sgolay, fun=sd, na.rm=TRUE)
#writeRaster(stk.mean_smoothed.sgolay,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.mean_smoothed.sgolay.tif")
#writeRaster(stk.sd_smoothed.sgolay,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.sd_smoothed.sgolay.tif")
r2.squared.sgolay<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.sgolay), fun=r2)

# s1=stack(stk.evi2.filtered[[1]], stk.evi2.filtered[[2]], stk.evi2.filtered[[3]])
# s2=stack(stk.evi2.smoothed.sgolay[[1]], stk.evi2.smoothed.sgolay[[2]], stk.evi2.smoothed.sgolay[[3]])
# rr=overlay(stack(s1,s2), fun=r2)
# 
# obs=rnorm(100)
# sim=rnorm(100)
# g <-  gof(sim, obs)
# g[c("R2","RMSE","rd"),]

r2.sig.sgolay<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.whit), fun=r2.pval)
r2.sig.sgolay[r2.sig.sgolay >= 0.05] <- NA
r2.masked.sgolay<- mask(r2.squared.sgolay, r2.sig.sgolay)
writeRaster(r2.masked.sgolay,"J:/Results_Smoother_Paper/Results_Outputs/r2.masked.sgolay.tif")
writeRaster(r2.squared.sgolay,"J:/Results_Smoother_Paper/Results_Outputs/r2.squared.sgolay.tif")
writeRaster(r2.sig.sgolay,"J:/Results_Smoother_Paper/Results_Outputs/r2.sig.sgolay.tif")


#Second stage analysis
Latitude<-yFromRow(stk.mean_filtered, row=1:nrow(stk.mean_filtered))
RowMeanfiltered<-apply(as.matrix(stk.mean_filtered), 1, mean, na.rm=TRUE)
rowsdfiltered<-apply(as.matrix(stk.mean_filtered), 1, sd, na.rm=TRUE)
DF.smoothed.data<-data.frame(Latitude=Latitude, RowMeansfilt=RowMeanfiltered,rowsdfilt=rowsdfiltered)

DF.smoothed.data$RowMeansgolay<-apply(as.matrix(stk.mean_smoothed.sgolay), 1, mean, na.rm=TRUE)
DF.smoothed.data$rowsdsgolay<-apply(as.matrix(stk.mean_smoothed.sgolay), 1, sd, na.rm=TRUE)
write.csv(DF.smoothed.data, "J:/Results_Smoother_Paper/Results_Outputs/DF_smoothed_data.csv")

names(DF.smoothed.data)

############################################################################################################################
#Trend Analysis
###########################################################################################################################
##############################################################################################################
#trend analysis: savitzky Golay Filter
########################################################
#the slope is calculated to get the direction and magnitude of trends
sgolay.slope=overlay(stk.evi2.smoothed.sgolay, fun=slope.fun)
writeRaster(sgolay.slope,"J:/Results_Smoother_Paper/Results_Outputs/sgolay.slope.tif")

#now we need to see which trends are significant. Thus we first extract the p-value:
sgolay.slope.p <- overlay(stk.evi2.smoothed.sgolay, fun=sigTrend.fun)
writeRaster(sgolay.slope.p,"J:/Results_Smoother_Paper/Results_Outputs/sgolay.slope.p.tif")

#then mask all values >0.05 to get a confidence level of 95%:
sgolay.p.mask.NA = overlay(sgolay.slope.p, fun=funMaskTrend)
writeRaster(sgolay.p.mask.NA ,"J:/Results_Smoother_Paper/Results_Outputs/sgolay.p.mask.NA .tif")

#and finaly mask all insignificant values in the trend map, so we only get NDVI change significant at the 95% level:
sgolaytrend.sig = mask(sgolay.slope, sgolay.p.mask.NA)
writeRaster(sgolaytrend.sig  ,"J:/Results_Smoother_Paper/Results_Outputs/sgolaytrend.sig.tif")

#plot(trend.sig, main="significant NDVI change")
############################################################################################################################################

#  Whittaker
evi2.smoothed.whit <- list.files(file.path("J:/DataMonthlyEVI2/whittaker"),
                                 pattern="*.tif", full.names=TRUE)

Raster_evi2.smoothed.whit<-foreach(i=1:length(evi2.smoothed.whit), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.smoothed.whit[[i]])
stk.evi2.smoothed.whit<-stack(evi2.smoothed.whit, bands=NULL, native=FALSE, RAT=TRUE)

stk.mean_smoothed.whittaker<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.mean_smoothed.whittaker.tif")
#stk.mean_smoothed.whittaker<-mean(stk.evi2.smoothed.whit, na.rm=TRUE)
#stk.sd_smoothed.whittaker<-overlay(stk.evi2.smoothed.whit, fun=sd, na.rm=TRUE)
#writeRaster(stk.mean_smoothed.whittaker,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.mean_smoothed.whittaker.tif")
#writeRaster(stk.sd_smoothed.whittaker,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.sd_smoothed.whittaker.tif")

r2.squared.whit<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.whit), fun=r2)
r2.sig.whit<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.whit), fun=r2.pval)
r2.sig.whit[r2.sig.whit >= 0.05] <- NA
r2.masked.whit<- mask(r2.squared.whit, r2.sig.whit)
writeRaster(r2.masked.whit,"J:/Results_Smoother_Paper/Results_Outputs/r2.masked.whit.tif")
writeRaster(r2.squared.whit,"J:/Results_Smoother_Paper/Results_Outputs/r2.squared.whit.tif")
writeRaster(r2.sig.whit,"J:/Results_Smoother_Paper/Results_Outputs/r2.sig.whit.tif")

DF.smoothed.data$RowMeanwhit<-apply(as.matrix(stk.mean_smoothed.whittaker), 1, mean, na.rm=TRUE)
DF.smoothed.data$rowsdwhit<-apply(as.matrix(stk.mean_smoothed.whittaker), 1, sd, na.rm=TRUE)
write.csv(DF.smoothed.data, "J:/Results_Smoother_Paper/Results_Outputs/DF.smoothed.data.csv", overwrite=TRUE)

##############################################################################################################
#trend analysis: Whittaker
########################################################
whit.slope=overlay(stk.evi2.smoothed.whit, fun=slope.fun)
writeRaster(whit.slope,"J:/Results_Smoother_Paper/Results_Outputs/whit.slope.tif")


#now we need to see which trends are significant. Thus we first extract the p-value:
whit.slope.p <- overlay(stk.evi2.smoothed.whit, fun=sigTrend.fun)
writeRaster(whit.slope.p,"J:/Results_Smoother_Paper/Results_Outputs/whit.slope.p.tif")

#then mask all values >0.05 to get a confidence level of 95%:
whit.p.mask.NA = overlay(whit.slope.p, fun=funMaskTrend)
writeRaster(whit.p.mask.NA ,"J:/Results_Smoother_Paper/Results_Outputs/whit.p.mask.NA.tif")

#and finaly mask all insignificant values in the trend map, so we only get NDVI change significant at the 95% level:
whittrend.sig = mask(whit.slope, whit.p.mask.NA)
writeRaster(whittrend.sig  ,"J:/Results_Smoother_Paper/Results_Outputs/whittrend.sig.tif")

#SPDE Approach
evi2.smoothed.spde <- list.files(file.path("J:/DataMonthlyEVI2/spde"),
                                 pattern="*.tif", full.names=TRUE)

Raster_evi2.smoothed.spde<-foreach(i=1:length(evi2.smoothed.spde), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.smoothed.spde[[i]])
stk.evi2.smoothed.spde<-stack(evi2.smoothed.spde, bands=NULL, native=FALSE, RAT=TRUE)

# # write to netcdf 
# require(ncdf)
# install.packages("ncdf", dependencies=TRUE)
# 
# if (require(ncdf)) {    
#   rnc <- writeRaster(stk.evi2.smoothed.spde, filename='/media/CLIM_DATA/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyEVI2-5km/spde/stk.evi2.smoothed.spde.nc', format="CDF")   
# }

stk.mean_smoothed.spde<-mean(stk.evi2.smoothed.spde, na.rm=TRUE)
#stk.sd_smoothed.spde<-overlay(stk.evi2.smoothed.spde, fun=sd, na.rm=TRUE)
writeRaster(stk.mean_smoothed.spde,"J:/Results_Smoother_Paper/Results_Outputs/stk.mean_smoothed.spde.tif")
#writeRaster(stk.sd_smoothed.spde,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.sd_smoothed.spde.tif")
?rmse
r2.squared.spde<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.spde), fun=r2)
r2.sig.spde<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.spde), fun=r2.pval)
r2.sig.spde[r2.sig.spde >= 0.05] <- NA
r2.masked.spde <- mask(r2.squared.spde, r2.sig.spde)
writeRaster(r2.masked.spde,"J:/Results_Smoother_Paper/Results_Outputs/r2.masked.spde.tif")
writeRaster(r2.squared.spde,"J:/Results_Smoother_Paper/Results_Outputs/r2.squared.spde.tif")
writeRaster(r2.sig.spde,"J:/Results_Smoother_Paper/Results_Outputs/r2.sig.spde.tif")

DF.smoothed.data<-read.csv("J:/Results_Smoother_Paper/Results_Outputs/DF_smoothed_data.csv")
DF.smoothed.data$RowMeanspde<-apply(as.matrix(stk.mean_smoothed.spde), 1, mean, na.rm=TRUE)
DF.smoothed.data$rowsdspde<-apply(as.matrix(stk.mean_smoothed.spde), 1, sd, na.rm=TRUE)


write.csv(DF.smoothed.data, "J:/Results_Smoother_Paper/Results_Outputs/DF.smoothed.data.csv", overwrite=TRUE)
########################################################################################################################
#trend analysis: spde
###################################################
spde.slope=overlay(stk.evi2.smoothed.spde, fun=slope.fun)
writeRaster(spde.slope,"J:/Results_Smoother_Paper/Results_Outputs/spde.slope.tif")

#now we need to see which trends are significant. Thus we first extract the p-value:
spde.slope.p <- overlay(stk.evi2.smoothed.spde, fun=sigTrend.fun)
writeRaster(spde.slope.p,"J:/Results_Smoother_Paper/Results_Outputs/spde.slope.p.tif")

#then mask all values >0.05 to get a confidence level of 95%:
spde.p.mask.NA = overlay(spde.slope.p, fun=funMaskTrend)
writeRaster(spde.p.mask.NA ,"J:/Results_Smoother_Paper/Results_Outputs/spde.p.mask.NA.tif")

#and finaly mask all insignificant values in the trend map, so we only get NDVI change significant at the 95% level:
spdetrend.sig = mask(spde.slope, spde.p.mask.NA)
writeRaster(spdetrend.sig  ,"J:/Results_Smoother_Paper/Results_Outputs/spdetrend.sig.tif")
#########################################################################################################
#RESIDUAL MEAN SQUARED ERROR
#########################################################################################################
# funrmse <- function(obs.stk, sim.stk) {
#   rmse.fun <- function(v) {
#     obs <- v[1:split]
#     sim <- v[(split+1):(2*split)]
#     rmse.est <- try(rmse(sim=sim, obs=obs), silent=TRUE)   
#     if (class(rmse.est)=="try-error") NA else unclass(rmse.est)
#   }
#   s <- stack(obs.stk, sim.stk)
#   split <- nlayers(s)/2
#   rmse.result<-calc(s, fun=rmse.fun)
#   return(rmse.result)
# } 
#install.packages("hydroGOF", dependencies=TRUE)
require(hydroGOF)
# regression of values in one brick (or stack) with another
# s <- stack(stk.evi2.smoothed.whit, stk.evi2.filtered)
# time=1:nlayers(stk.evi2.smoothed.whit); stk=stack(stk.evi2.smoothed.whit); nlayers(stk.evi2.smoothed.whit);nlayers(s)
# fun.reg <- function(x) { if (is.na(x[1])){ NA } else { lm(x[1:size] ~ x[721:1440])$coefficients[2] }}
# x11.whit <- calc(stk.evi2.smoothed.whit, fun.reg)
# 
# ss = stack(stk.evi2.smoothed.whit[[1]], stk.evi2.smoothed.whit[[2]], stk.evi2.smoothed.whit[[3]], stk.evi2.smoothed.whit[[4]], stk.evi2.smoothed.whit[[5]])
# spplot(ss)
# 
# sss = stack(stk.evi2.filtered[[1]], stk.evi2.filtered[[2]],stk.evi2.filtered[[3]], stk.evi2.filtered[[4]], stk.evi2.filtered[[5]])
# spplot(sss)
##################################################################
# fun.rmse<- function(x) { if (all(is.na(x))){ NA } else {rmse(sim=x[1:size], obs=x[721:1440])}}
# rmse.whittaker<- calc(s, fun.rmse)

# # get values of a few cells
# cells <- 100000:100001
# x <- data.frame(s[cells])
# rmse(sim=x[1:size], obs=x[721:1440])
# 
# 
# require(raster)
# # create data
# r <- raster(nrow=10, ncol=10)
# s1 <- list()
# for (i in 1:20) {
#   s1[i] <- setValues(r, rnorm(ncell(r), i, 3) )
# }
# s1 <- stack(s1)
# 
# f_lowess <- function(x){
#   df <- data.frame(t = 1:20, x)
#   out <- lowess(df, f = .2)$y
#   return(out)
# }
# 
# ## calc with a function
# s2 <- calc(s1, fun = f_lowess)
# 
# ## calc test - tests your function on a subset of the data
# d <- s1[1:5] ## every row is a pixel
# apply(d, 1, f_lowess) ## here no error occurs. 
#####################################################################
rmse.sgolay<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.sgolay), fun=funrmse)
rmse.whittaker<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.whit), fun=funrmse)
rmse.spde<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.spde), fun=funrmse)

writeRaster(rmse.sgolay,"J:/Results_Smoother_Paper/Results_Outputs/rmse.sgolay.tif")
writeRaster(rmse.whittaker,"J:/Results_Smoother_Paper/Results_Outputs/rmse.whittaker.tif")
writeRaster(rmse.spde,"J:/Results_Smoother_Paper/Results_Outputs/rmse.spde.tif")


#This function computes the Relative Index of Agreement (d) between sim and obs, with treatment of missing values.
#rd = 1 - [ sum( ( (obs - sim) / obs )^2 ] / sum( ( (abs(sim - mean(obs) ) + abs(obs - mean(obs) ) ) / mean(obs) )^2 )
#It varies between 0 and 1. A value of 1 indicates a perfect match, and 0 indicates no agreement at all.

rd.sgolay<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.sgolay), fun=funagreement)
rd.whittaker<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.whit), fun=funagreement)
rd.spde<-overlay(stack(stk.evi2.filtered, stk.evi2.smoothed.spde), fun=funagreement)

writeRaster(rd.sgolay,"J:/Results_Smoother_Paper/Results_Outputs/agreement.With.sgolay.tif")
writeRaster(rd.whittaker,"J:/Results_Smoother_Paper/Results_Outputs/agreement.With.whittaker.tif")
writeRaster(rd.spde,"J:/Results_Smoother_Paper/Results_Outputs/agreement.With.spde.tif")


# funMask=function(x) {
#   time=seq(1:size) 
#   if (all(is.na(x))){ NA } 
#                   else { result = inla(x~time-1,
#                                        data = data.frame(x,time),family = "gaussian") 
#      r = result$summary.fixed["time",1]
#      m = result$marginals.fixed$time
#      rr<-inla.hpdmarginal(0.95, m)
#      rrr<-ifelse(rr[1] <= 0 && rr[2] >= 0, NA, 1)
#                   return(rrr)}}
# funSlope=function(x) {
#   time=seq(1:size) 
#   if (all(is.na(x))){ NA } 
#   else { result = inla(x~time-1,
#                        data = data.frame(x,time),family = "gaussian") 
#          r = result$summary.fixed["time",1]
#   return(r)}}
##then the slope is calculated to get the direction and magnitude of trends
# sgolay.slope=fslope(stk.evi2.smoothed.sgolay)
# whit.slope=fslope(stk.evi2.smoothed.whit)
# 
# sgolay.mask=fMasking(stk.evi2.smoothed.sgolay)
# whit.mask=fMasking(stk.evi2.smoothed.whit)
# 
# ##and finaly mask all insignificant values in the trend map, so we only get NDVI change 
# #significant at the 95%
# sigTrendDiff_sgolay <- overlay(sgolay.slope, sgolay.mask, fun=function(x,y){return(x*y)})
# sigTrendDiff_whit <- overlay(whit.slope, whit.mask, fun=function(x,y){return(x*y)})
# 
# writeRaster(sgolay.slope ,"/media/CLIM_DATA/CLIM_DATA/EVI2_Smoothing_Datasets/Results_Outputs/sgolay.slope.tif")
# writeRaster(whit.slope ,"/media/CLIM_DATA/CLIM_DATA/EVI2_Smoothing_Datasets/Results_Outputs/whit.slope.tif")
# 
# writeRaster(sgolay.mask ,"/media/CLIM_DATA/CLIM_DATA/EVI2_Smoothing_Datasets/Results_Outputs/sgolay.mask.tif")
# writeRaster(whit.mask ,"/media/CLIM_DATA/CLIM_DATA/EVI2_Smoothing_Datasets/Results_Outputs/whit.mask.tif")
# 
# writeRaster(sigTrendDiff_sgolay ,
#             "/media/CLIM_DATA/CLIM_DATA/EVI2_Smoothing_Datasets/Results_Outputs/sigTrendDiff_sgolay.tif")
# writeRaster(sigTrendDiff_whit,
#             "/media/CLIM_DATA/CLIM_DATA/EVI2_Smoothing_Datasets/Results_Outputs/sigTrendDiff_whit.tif")


#Preprocessing
# PR.list <- list.files(file.path("/vm/eokuto/EVI2_Smoothing_Datasets/PR"),pattern="*.tif", full.names=TRUE)
# Raster_PR.list<-foreach(PR=1:length(PR.list), .packages="raster", .combine=c, .verbose=FALSE) %do%
#   raster(PR.list[[PR]])
# stk.Data.PR<-stack(Raster_PR.list, bands=NULL, native=FALSE, RAT=TRUE)
# 
# stk.data.b4.filter<- gapb4filterfunction(stk=stk.Data.PR)
# stk.data.after.filter<- gapafterfilterfunction(stk=stk.evi2.filtered)
# writeRaster(stk.data.b4.filter,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.data.b4.filter.tif")
# writeRaster(stk.data.after.filter,"/vm/eokuto/EVI2_Smoothing_Datasets/Results_Outputs/stk.data.after.filter.tif")

DF<-read.csv("J:/Results_Smoother_Paper/Results_Outputs2/DF.smoothed.data.csv")
attach(DF)
names(DF)
eb0a <- aes(ymax = RowMeansfilt + rowsdfilt, ymin = RowMeansfilt - rowsdfilt)
eb1a <- aes(ymax = RowMeansgolay + rowsdsgolay, ymin = RowMeansgolay - rowsdsgolay)
eb2a <- aes(ymax = RowMeanwhit + rowsdwhit, ymin = RowMeanwhit - rowsdwhit)
eb3a <- aes(ymax = RowMeanspde + rowsdspde, ymin = RowMeanspde - rowsdspde)


require(gridExtra)
# platitudemeans <- ggplot(DF, aes(Latitude))
# platitudemeans <- platitudemeans + geom_ribbon(eb0, alpha = 0.5, fill='lightgrey') + geom_line(aes(y=RowMeansfilt), colour="darkblack")
# platitudemeans <- platitudemeans + geom_ribbon(eb1, alpha = 0.5, fill='lightred') + geom_line(aes(y=RowMeansgolay), colour="darkred")
# platitudemeans <- platitudemeans + geom_ribbon(eb2, alpha = 0.5, fill='lightgreen') + geom_line(aes(y=RowMeanwhit), colour="darkgreen")
# platitudemeans <- platitudemeans + geom_ribbon(eb3, alpha = 0.5, fill='lightblue') + geom_line(aes(y=RowMeanspde), colour="darkblue") + coord_flip() +
#   theme(panel.background = element_rect(fill='white', colour='black')) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#   theme(panel.border = element_blank()) +
#   xlab("Latitude") +
#   ylab("Enhanced Veg Index Two") +
#   theme(axis.text.x=element_blank()) +
#   theme(axis.text.y=element_text(size=20,face="bold")) +
#   annotate("text", x=-50, y=-0.8, label=" ",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
#   scale_x_continuous(limits = c(-90,90),
#                      breaks=c(-90,-45,0,45,90)) + 
#   scale_y_continuous(limits = c(-1, 1),
#                      breaks=c(-1,-0.5,0,0.5,1))

require(gridExtra)
ggplot(DF, aes(Latitude)) + 
geom_ribbon(eb0a, alpha = 1/3, fill='light gray') + geom_line(aes(y=RowMeansfilt), colour="black") +  #+ coord_flip() +
geom_ribbon(eb1a, alpha = 1/3, fill='light blue') + geom_line(aes(y=RowMeansgolay), colour="dark blue") + #+ coord_flip() +
geom_ribbon(eb2a, alpha = 1/3, fill='light green') + geom_line(aes(y=RowMeanwhit), colour="dark green") +
  coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  #annotate("text", x=-50, y=-0.8, label=" ",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-80,80),
                     breaks=c(-80,-40,0,40,80)) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks=c(-1,-0.5,0,0.5,1))


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
  annotate("text", x=-50, y=-0.8, label="Jan",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks=c(-1,-0.5,0,0.5,1))


plotfilt <- ggplot(DF, aes(Latitude))
plotfilt  <- plotfilt + geom_ribbon(eb0a, alpha = 0.5, fill='lightgrey') + geom_line(aes(y=RowMeansfilt), colour="black") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab("") +
  ylab("") +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.6, label="filtered",size=6,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-80,80),
                     breaks=c(-80,0,80)) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks=c(-1,0,1))

plotsgolay <- ggplot(DF, aes(Latitude))
plotsgolay <- plotsgolay + geom_ribbon(eb1a, alpha = 0.5, fill='lightgrey') + geom_line(aes(y=RowMeansgolay), colour="black") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab("") +
  ylab("") +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.6, label="sgolay",size=6,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-80,80),
                     breaks=c(-80,0,80)) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks=c(-1,0,1))

plotwhit <- ggplot(DF, aes(Latitude))
plotwhit <- plotwhit + geom_ribbon(eb2a, alpha = 0.5, fill='lightgrey') + 
  geom_line(aes(y=RowMeanwhit), colour="black") + coord_flip() +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab("") +
  ylab("") +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.6, label="whittaker ",
           size=6,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-80,80),
                     breaks=c(-80,0,80)) + 
  scale_y_continuous(limits = c(-1, 1),
                     breaks=c(-1,0,1))

grid.arrange(arrangeGrob(plotfilt + theme(legend.position="none"),
                         plotsgolay + theme(legend.position="none"),
                         plotwhit + theme(legend.position="none"),
                         nrow=1, ncol=3,
                         clip = TRUE,
                         strip=FALSE,
                         legend = NULL,
                         main=textGrob("",
                                       vjust = 1, 
                                       gp = gpar(fontface = "bold", cex = 1)),
                         xlab=textGrob("Enhanced vegetation Index Two (EVI2)",
                                       vjust = 1, 
                                       gp = gpar(fontface = "bold", cex = 1)),
                         left = textGrob("Latitude", rot = 90, vjust = 1, 
                                         gp = gpar(fontface = "bold",
                                                   cex = 1))))
######################################################################################
#LAI Data
#########################################################################
LAIDf=read.csv("J:/ProcessedData/LAI-EVI2/LAIsubdata19thMar2015b.csv")
attach(LAIDf)
maizedata=LAIDf[ which(crop_type=='maize'), ]
names(maizedata)

plot(maizedata$Average.of.RF_EVI2,maizedata$evi2_sgolay)
plot(maizedata$Average.of.RF_EVI2,maizedata$evi2_whittaker)
plot(maizedata$Average.of.RF_EVI2,maizedata$evi2_spde)

gof(maizedata$Average.of.RF_EVI2,maizedata$evi2_sgolay)
gof(maizedata$Average.of.RF_EVI2,maizedata$evi2_whittaker)
gof(maizedata$Average.of.RF_EVI2,maizedata$evi2_spde)



####################################################################################
stk.mean_filtered<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.mean_filtered.tif")
stk.mean_smoothed.sgolay<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.mean_smoothed.sgolay.tif")
stk.mean_smoothed.whit<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.mean_smoothed.whittaker.tif")
stk.mean_smoothed.spde<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.mean_smoothed.spde.tif")


stkmeans<-stack(stk.mean_filtered,stk.mean_smoothed.sgolay,stk.mean_smoothed.whit,stk.mean_smoothed.spde, bands=NULL, native=FALSE, RAT=TRUE)


NamesReg<- c("A","B","C","D")
#pal <- colorRampPalette(c("gray", "red"))
#colors <-  pal(nlayers(stkmeans))
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at <-  seq(-1, 1, 0.02)

spplot(stkmeans,xlim = bbox(stkmeans)[1, ], 
       ylim = bbox(stkmeans)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=NamesReg[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 2))



#####################################################################
evi2.SDS <- list.files(file.path("J:/Results_Smoother_Paper/Results_Outputs/SDS"),
                                   pattern="*.tif", full.names=TRUE)

Raster_evi2.SDS<-foreach(i=1:length(evi2.SDS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.SDS[[i]])
stk.Raster_evi2.SDS<-stack(Raster_evi2.SDS[[1]],
                           Raster_evi2.SDS[[2]],
                           Raster_evi2.SDS[[4]],
                           Raster_evi2.SDS[[3]], bands=NULL, native=FALSE, RAT=TRUE)

spplot(stk.Raster_evi2.SDS)

stkSDS<-stack(stk.Raster_evi2.SDS[[1]],stk.Raster_evi2.SDS[[2]],stk.Raster_evi2.SDS[[3]],stk.Raster_evi2.SDS[[4]], bands=NULL, native=FALSE, RAT=TRUE)


NamesReg<- c("raw","sgolay","whittaker","spde")
#pal <- colorRampPalette(c("gray", "red"))
#colors <-  pal(nlayers(stkmeans))
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at2 <-  seq(0, 1.2, 0.02)

spplot(stkSDS,xlim = bbox(stkSDS)[1, ], 
       ylim = bbox(stkSDS)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at2,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=NamesReg[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')
       }, contour=FALSE, layout=c(2, 2))

######################################
#FILTER EFFECTS
###########################################
evi2.filter.effect <- list.files(file.path("J:/Results_Smoother_Paper/Results_Outputs/filterEffect"),
                                 pattern="*.tif", full.names=TRUE)

Raster_evi2.filter.effect<-foreach(i=1:length(evi2.filter.effect), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.filter.effect[[i]])

#filtereffect <- overlay(Raster_evi2.filter.effect[[1]], Raster_evi2.filter.effect[[2]], fun=function(x,y){return(x-y)})
# filtereffect <- overlay(filtereffect, fun=function(x) {ifelse(x<1, NA, x)})
# writeRaster(filtereffect ,"J:/Results_Smoother_Paper/Results_Outputs/filtereffectDiff3.tif")
# 
filtereffect3<-raster("J:/Results_Smoother_Paper/Results_Outputs/filtereffectDiff3.tif")

beforefilt<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.data.b4.filter.tif")
afterfilt<-raster("J:/Results_Smoother_Paper/Results_Outputs/stk.data.after.filter.tif")
finalfiltereffect<-raster("J:/Results_Smoother_Paper/Results_Outputs/finalfiltereffect.tif")

stk.evi2.filt.effect<-stack(beforefilt, afterfilt, finalfiltereffect, bands=NULL, native=FALSE, RAT=TRUE)

NamesFilt<- c("A", "B", "D")
#pal <- colorRampPalette(c("gray", "red"))
#colors <-  pal(nlayers(stk.evi2.filt.effect))
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at <-  seq(1, 100,1)
# display.brewer.all()
# ?brewer.pal

spplot(stk.evi2.filt.effect,xlim = bbox(stk.evi2.filt.effect)[1, ], 
       ylim = bbox(stk.evi2.filt.effect)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=NamesFilt[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(1, 3))



######################################################
# R2
#####################################################
R2 <- list.files(file.path("J:/Results_Smoother_Paper/r2"),
                                 pattern="*.tif", full.names=TRUE)

Raster_evi2.R2<-foreach(i=1:length(R2), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(R2[[i]])

stkR2<-stack(Raster_evi2.R2[[1]],Raster_evi2.R2[[4]],Raster_evi2.R2[[3]], bands=NULL, native=FALSE, RAT=TRUE)

Namesr2<- c("A", "B", "D")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at <-  seq(0, 1, 0.01)

spplot(stkR2,xlim = bbox(stkR2)[1, ], 
       ylim = bbox(stkR2)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Namesr2[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(1, 3))

######################################################
# RMSE
#####################################################
RMSE <- list.files(file.path("J:/Results_Smoother_Paper/rmse"),
                 pattern="*.tif", full.names=TRUE)

Raster_evi2.RMSE<-foreach(i=1:length(RMSE), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(RMSE[[i]])

stkRMSE<-stack(Raster_evi2.RMSE[[1]],Raster_evi2.RMSE[[4]],Raster_evi2.RMSE[[3]], bands=NULL, native=FALSE, RAT=TRUE)

Namesr2<- c("A", "B", "C")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at <-  seq(0, 2, 0.01)


#spplot(stkRMSE)
spplot(stkRMSE,xlim = bbox(stkRMSE)[1, ], 
       ylim = bbox(stkRMSE)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       #at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Namesr2[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(1, 3))

######################################################
# slope
#####################################################
require(foreach); require(raster); require(rasterVis)
slope <- list.files(file.path("J:/Results_Smoother_Paper/slope"),
                   pattern="*.tif", full.names=TRUE)

Raster_evi2.slope<-foreach(i=1:length(slope ), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(slope [[i]])


slope.S.G<-Raster_evi2.slope[[1]]
slope.SPDE<-Raster_evi2.slope[[2]]
slope.W.H<-Raster_evi2.slope[[3]]*100

# writeRaster(slope.S.G, "J:/resultsJune2015/Trend_S_G.tif")
# writeRaster(Raster_evi2.slope[[2]], "J:/resultsJune2015/Trend_spde.tif")
# writeRaster(slope.SPDE, "J:/resultsJune2015/Trend_W_H.tif")

trend<-stack(slope.S.G,
             slope.W.H,
             slope.SPDE)
plot(trend[[1]])
plot(trend[[2]])
plot(trend[[3]])


spplot(trend, xlim = bbox(trend)[1, ], 
       ylim = bbox(trend)[2, ],
       par.settings = mytheme,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')},
       contour=FALSE, layout=c(1, 3))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
catfun<-function(x){
  ifelse(x < -0.08, 1, 
         ifelse(x >= -0.08 | x < -0.03, 2,
                ifelse(x >= -0.03 | x < 0.02, 3,
                       ifelse(x >= 0.02 | x < 0.07, 4, 5
                              ))))
}

catrastersgolay<-calc(Raster_evi2.slope[[1]], catfun)
r <- ratify(catrastersgolay)

rat <- levels(r)[[1]]
rat$veg <- c('browning', 'greening')
#rat$code <- c(12,25,30)
levels(r) <- rat
levelplot(r)

stkRMSE<-stack(Raster_evi2.RMSE[[1]],Raster_evi2.RMSE[[4]],Raster_evi2.RMSE[[3]], bands=NULL, native=FALSE, RAT=TRUE)

Namesr2 <- c("A", "B", "C")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region = brewer.pal(8,"YlOrRd"))
my.at <-  seq(0, 2, 0.01)

#vignette("rasterVis")

spplot(stkRMSE,xlim = bbox(stkRMSE)[1, ], 
       ylim = bbox(stkRMSE)[2, ],
       colorkey=myColorkey,
       scales = list(draw = FALSE), 
       packages=c("raster", "rasterVis", "RColorBrewer"),
       par.settings = mytheme,
       #at=my.at,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Namesr2[panel.number()],
                    col.text = "black",
                    cex=2,fontface='bold')
       }, contour=FALSE, layout=c(1, 3))

######################################################
# sig slope
#####################################################
sigslope <- list.files(file.path("J:/ProcessedData/Results_Smoother_Paper/slope"),
                    pattern="*.tif", full.names=TRUE)

Raster_evi2.sigslope<-foreach(i=1:length(sigslope ), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(sigslope [[i]])

Raster_evi2.sigslopewhit=raster("J:/ProcessedData/Results_Smoother_Paper/slope/slope.whittaker.tif")


require(raster)
# multiply values with 10
funspde <- function(x) { x * 100 }
#funsgolay <- function(x) { x * 90 }
#funwhit <- function(x) { x * 60 }
Raster_evi2.sigslopespde<- calc(Raster_evi2.sigslope[[3]], funspde)
Raster_evi2.sigslopesgolay<- Raster_evi2.sigslope[[1]]
Raster_evi2.sigslopewhit<- Raster_evi2.sigslope[[2]]

levelplot(Raster_evi2.sigslopeNEW)
levelplot(stack(Raster_evi2.sigslope))

r2sgolay=stkR2[[1]]
r2whit=stkR2[[2]]
r2spde=stkR2[[3]]

fun2=function(x){ifelse(x>0.5, 1, NA) }

r2sgolay.NA=calc(r2sgolay, fun2)
r2whit.NA=calc(r2whit, fun2)
r2spde.NA=calc(r2spde, fun2)


sigslopesgolay.mask=mask(Raster_evi2.sigslopesgolay,r2sgolay.NA)
sigslopewhit.mask=mask(Raster_evi2.sigslopewhit,r2whit.NA)
sigslopespde.mask=mask(Raster_evi2.sigslopespde,r2spde.NA)
#?mask

stksigslope<-stack(sigslopesgolay.mask,
                   sigslopewhit.mask,
                   sigslopespde.mask,
                   bands=NULL, native=FALSE, RAT=TRUE)
writeRaster(stksigslope, "J:/ProcessedData/Results_Smoother_Paper/sigtrend/stksigslope.tif", overwrite=TRUE)

####################################################################
stksigslope=stack("J:/Results_Smoother_Paper/trend/stksigslope.tif")


###################################################################
catfun<-function(x){
  ifelse(x < -0.1, 1, 
         ifelse(x >= -0.1 & x < -0.08, 2,
                ifelse(x >= -0.08 & x < 0, 3,
                       ifelse(x > 0 & x < 0.03, 4,
                       ifelse(x >=0.03 & x < 0.05, 5, 6)))))
}

catfun2<-function(x){
  ifelse(x < 0, 1, 2)
}
################################################################

catrastersgolay <- calc(stksigslope[[1]], catfun)
#catrasterwhit <- calc(stksigslope[[2]], catfun2)
catrasterspde <- calc(stksigslope[[3]], catfun)
r1 <- ratify(catrastersgolay)
#r2 <- ratify(catrasterwhit)
r3 <- ratify(catrasterspde)

rat1 <- levels(r1)[[1]]
#rat2 <- levels(r2)[[1]]
rat3 <- levels(r3)[[1]]
rat1$veg <- c('< -0.1', '-0.1 to -0.08', '-0.8 to <0', '0.01 to 0.03', '0.03 to 0.05',
             '> 0.05')
#rat2$veg <- c('-0.05 to <0', ' >0 to 0.05')
rat3$veg <- c('< -0.1', '-0.1 to -0.08', '-0.8 to 0.01', '0.01 to 0.03', '0.03 to 0.05',
             '> 0.05')
levels(r1) <- rat1
#levels(r2) <- rat2
levels(r3) <- rat3
#rat$code <- c(12,25,30)
#writeRaster(r, 'J:/Results_Smoother_Paper/trendcat/sgolayCat.tif')
stktrend= stack(r1,r2,r3)
library(rasterVis) 
myPal <- c('light blue', "blue", "dark blue", 
           "orange", "brown", "red")

myPal2 <- c( "blue",  "red")
## using par.settings
levelplot(r1, par.settings=rasterTheme(region=myPal))
levelplot(r2, par.settings=rasterTheme(region=myPal2))
levelplot(r3, par.settings=rasterTheme(region=myPal),colorkey=FALSE)
## or with col.regions
levelplot(r1, col.regions=myPal)
###################################################################
Namesslope<- c("A", "B", "C")
myColorkey <- list(labels = list(cex = 1.5,fontface='plain'))
mytheme<-rasterTheme(region=myPal)
#my.at <-  seq(0, 2, 0.01)

spplot(trend, xlim = bbox(trend)[1, ], 
       ylim = bbox(trend)[2, ],
       par.settings = mytheme,
       strip=FALSE,
       main=list("", col="black",cex=2, fontface='bold'),
       panel = function(x, y, z,...) {
         panel.levelplot.raster(x, y, z,...)
         panel.text(x = -130, y = -40,
                    labels=Names[panel.number()],
                    col.text = "black",
                    cex=1.5,fontface='bold')},
       contour=FALSE, layout=c(1, 3))

#############################################################
#   LATITUDINAL MEANS
########################################################
evi2.raw <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/raw"),
                            pattern="*.tif", full.names=TRUE)
evi2.sgolay <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/sgolay"),
                                   pattern="*.tif", full.names=TRUE)

Raster_raw<-foreach(i=1:length(evi2.raw), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.raw[[i]])
Raster_sgolay<-foreach(i=1:length(evi2.sgolay), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.sgolay[[i]])

##################################################################################
#   
################################################################################
evi2.spde <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde"),
                        pattern="*.tif", full.names=TRUE)

Raster_whit <- foreach(i=1:length(evi2.whit), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.whit[[i]])
Raster_spde <- foreach(i=1:length(evi2.spde), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.spde[[i]])
###############################################################################################

whit.1 <- overlay(Raster_spde[[1]], Raster_sgolay[[1]], fun=mean)
whit.10 <- overlay(Raster_spde[[2]], Raster_sgolay[[2]], fun = mean) 
whit.11 <- overlay(Raster_spde[[3]], Raster_sgolay[[3]], fun = mean) 
whit.12 <- overlay(Raster_spde[[4]], Raster_sgolay[[4]], fun = mean) 
whit.2 <- overlay(Raster_spde[[5]], Raster_sgolay[[5]], fun = mean) 
whit.3 <- overlay(Raster_spde[[6]], Raster_sgolay[[6]], fun = mean) 
whit.4 <- overlay(Raster_spde[[7]], Raster_sgolay[[7]], fun = mean)
whit.5 <- overlay(Raster_spde[[8]], Raster_sgolay[[8]], fun = mean)
whit.6 <- overlay(Raster_spde[[9]], Raster_sgolay[[9]], fun = mean)
whit.7 <- overlay(Raster_spde[[10]], Raster_sgolay[[10]], fun = mean)
whit.8 <- overlay(Raster_spde[[11]], Raster_sgolay[[11]], fun = mean)
whit.9 <- overlay(Raster_spde[[12]], Raster_sgolay[[12]], fun = mean)

writeRaster(whit.1,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.1.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.10,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.10.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.11,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.11.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.12,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.12.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.2,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.2.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.3,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.3.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.4,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.4.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.5,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.5.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.6,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.6.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.7,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.7.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.8,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.8.mean_whit.tif", overwrite=TRUE)
writeRaster(whit.9,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit/stk.9.mean_whit.tif", overwrite=TRUE)


spde.1 <- overlay(Raster_raw[[1]], Raster_sgolay[[1]], fun=mean, na.rm=TRUE)
spde.10 <- overlay(Raster_raw[[2]], Raster_sgolay[[2]], fun = mean, na.rm=TRUE) 
spde.11 <- overlay(Raster_raw[[3]], Raster_sgolay[[3]], fun = mean, na.rm=TRUE) 
spde.12 <- overlay(Raster_raw[[4]], Raster_sgolay[[4]], fun = mean, na.rm=TRUE) 
spde.2 <- overlay(Raster_raw[[5]], Raster_sgolay[[5]], fun = mean, na.rm=TRUE) 
spde.3 <- overlay(Raster_raw[[6]], Raster_sgolay[[6]], fun = mean, na.rm=TRUE) 
spde.4 <- overlay(Raster_raw[[7]], Raster_sgolay[[7]], fun = mean, na.rm=TRUE)
spde.5 <- overlay(Raster_raw[[8]], Raster_sgolay[[8]], fun = mean, na.rm=TRUE)
spde.6 <- overlay(Raster_raw[[9]], Raster_sgolay[[9]], fun = mean, na.rm=TRUE)
spde.7 <- overlay(Raster_raw[[10]], Raster_sgolay[[10]], fun = mean, na.rm=TRUE)
spde.8 <- overlay(Raster_raw[[11]], Raster_sgolay[[11]], fun = mean, na.rm=TRUE)
spde.9 <- overlay(Raster_raw[[12]], Raster_sgolay[[12]], fun = mean, na.rm=TRUE)

writeRaster(spde.1,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.1.mean_spde.tif")
writeRaster(spde.10,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.10.mean_spde.tif")
writeRaster(spde.11,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.11.mean_spde.tif")
writeRaster(spde.12,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.12.mean_spde.tif")
writeRaster(spde.2,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.2.mean_spde.tif")
writeRaster(spde.3,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.3.mean_spde.tif")
writeRaster(spde.4,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.4.mean_spde.tif")
writeRaster(spde.5,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.5.mean_spde.tif")
writeRaster(spde.6,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.6.mean_spde.tif")
writeRaster(spde.7,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.7.mean_spde.tif")
writeRaster(spde.8,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.8.mean_spde.tif")
writeRaster(spde.9,"F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde/stk.9.mean_spde.tif")
##########################################################################################################
#  Importing whit and spde files 
##########################################################################################################
evi2.whit <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/whit"),
                       pattern="*.tif", full.names=TRUE)
evi2.spde <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/spde"),
                          pattern="*.tif", full.names=TRUE)

Raster_whit <- foreach(i=1:length(evi2.whit), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.whit[[i]])
Raster_spde <- foreach(i=1:length(evi2.spde), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.spde[[i]])
#########################################################################################################
Latitude<-yFromRow(Raster_raw[[1]], row=1:nrow(Raster_raw[[1]]))
Jan.raw.mean<-apply(as.matrix(Raster_raw[[1]]), 1, mean, na.rm=TRUE)
Jan.raw.sd<-apply(as.matrix(Raster_raw[[1]]), 1, sd, na.rm=TRUE)

oct.raw.mean<-apply(as.matrix(Raster_raw[[2]]), 1, mean, na.rm=TRUE)
oct.raw.sd<-apply(as.matrix(Raster_raw[[2]]), 1, sd, na.rm=TRUE)

nov.raw.mean<-apply(as.matrix(Raster_raw[[3]]), 1, mean, na.rm=TRUE)
nov.raw.sd<-apply(as.matrix(Raster_raw[[3]]), 1, sd, na.rm=TRUE)

dec.raw.mean<-apply(as.matrix(Raster_raw[[4]]), 1, mean, na.rm=TRUE)
dec.raw.sd<-apply(as.matrix(Raster_raw[[4]]), 1, sd, na.rm=TRUE)

feb.raw.mean<-apply(as.matrix(Raster_raw[[5]]), 1, mean, na.rm=TRUE)
feb.raw.sd<-apply(as.matrix(Raster_raw[[5]]), 1, sd, na.rm=TRUE)

mar.raw.mean<-apply(as.matrix(Raster_raw[[6]]), 1, mean, na.rm=TRUE)
mar.raw.sd<-apply(as.matrix(Raster_raw[[6]]), 1, sd, na.rm=TRUE)

apr.raw.mean<-apply(as.matrix(Raster_raw[[7]]), 1, mean, na.rm=TRUE)
apr.raw.sd<-apply(as.matrix(Raster_raw[[7]]), 1, sd, na.rm=TRUE)

may.raw.mean<-apply(as.matrix(Raster_raw[[8]]), 1, mean, na.rm=TRUE)
may.raw.sd<-apply(as.matrix(Raster_raw[[8]]), 1, sd, na.rm=TRUE)

jun.raw.mean<-apply(as.matrix(Raster_raw[[9]]), 1, mean, na.rm=TRUE)
jun.raw.sd<-apply(as.matrix(Raster_raw[[9]]), 1, sd, na.rm=TRUE)

jul.raw.mean<-apply(as.matrix(Raster_raw[[10]]), 1, mean, na.rm=TRUE)
jul.raw.sd<-apply(as.matrix(Raster_raw[[10]]), 1, sd, na.rm=TRUE)

aug.raw.mean<-apply(as.matrix(Raster_raw[[11]]), 1, mean, na.rm=TRUE)
aug.raw.sd<-apply(as.matrix(Raster_raw[[11]]), 1, sd, na.rm=TRUE)

sep.raw.mean<-apply(as.matrix(Raster_raw[[12]]), 1, mean, na.rm=TRUE)
sep.raw.sd<-apply(as.matrix(Raster_raw[[12]]), 1, sd, na.rm=TRUE)
###########################################################################
# sgolay files
###########################################################################

Jan.sgolay.mean<-apply(as.matrix(Raster_sgolay[[1]]), 1, mean, na.rm=TRUE)
Jan.sgolay.sd<-apply(as.matrix(Raster_sgolay[[1]]), 1, sd, na.rm=TRUE)

oct.sgolay.mean<-apply(as.matrix(Raster_sgolay[[2]]), 1, mean, na.rm=TRUE)
oct.sgolay.sd<-apply(as.matrix(Raster_sgolay[[2]]), 1, sd, na.rm=TRUE)

nov.sgolay.mean<-apply(as.matrix(Raster_sgolay[[3]]), 1, mean, na.rm=TRUE)
nov.sgolay.sd<-apply(as.matrix(Raster_sgolay[[3]]), 1, sd, na.rm=TRUE)

dec.sgolay.mean<-apply(as.matrix(Raster_sgolay[[4]]), 1, mean, na.rm=TRUE)
dec.sgolay.sd<-apply(as.matrix(Raster_sgolay[[4]]), 1, sd, na.rm=TRUE)

feb.sgolay.mean<-apply(as.matrix(Raster_sgolay[[5]]), 1, mean, na.rm=TRUE)
feb.sgolay.sd<-apply(as.matrix(Raster_sgolay[[5]]), 1, sd, na.rm=TRUE)

mar.sgolay.mean<-apply(as.matrix(Raster_sgolay[[6]]), 1, mean, na.rm=TRUE)
mar.sgolay.sd<-apply(as.matrix(Raster_sgolay[[6]]), 1, sd, na.rm=TRUE)

apr.sgolay.mean<-apply(as.matrix(Raster_sgolay[[7]]), 1, mean, na.rm=TRUE)
apr.sgolay.sd<-apply(as.matrix(Raster_sgolay[[7]]), 1, sd, na.rm=TRUE)

may.sgolay.mean<-apply(as.matrix(Raster_sgolay[[8]]), 1, mean, na.rm=TRUE)
may.sgolay.sd<-apply(as.matrix(Raster_sgolay[[8]]), 1, sd, na.rm=TRUE)

jun.sgolay.mean<-apply(as.matrix(Raster_sgolay[[9]]), 1, mean, na.rm=TRUE)
jun.sgolay.sd<-apply(as.matrix(Raster_sgolay[[9]]), 1, sd, na.rm=TRUE)

jul.sgolay.mean<-apply(as.matrix(Raster_sgolay[[10]]), 1, mean, na.rm=TRUE)
jul.sgolay.sd<-apply(as.matrix(Raster_sgolay[[10]]), 1, sd, na.rm=TRUE)

aug.sgolay.mean<-apply(as.matrix(Raster_sgolay[[11]]), 1, mean, na.rm=TRUE)
aug.sgolay.sd<-apply(as.matrix(Raster_sgolay[[11]]), 1, sd, na.rm=TRUE)

sep.sgolay.mean<-apply(as.matrix(Raster_sgolay[[12]]), 1, mean, na.rm=TRUE)
sep.sgolay.sd<-apply(as.matrix(Raster_sgolay[[12]]), 1, sd, na.rm=TRUE)
#############################################################################
###########################################################################
# whit files
###########################################################################

Jan.whit.mean<-apply(as.matrix(Raster_whit[[1]]), 1, mean, na.rm=TRUE)
Jan.whit.sd<-apply(as.matrix(Raster_whit[[1]]), 1, sd, na.rm=TRUE)

oct.whit.mean<-apply(as.matrix(Raster_whit[[2]]), 1, mean, na.rm=TRUE)
oct.whit.sd<-apply(as.matrix(Raster_whit[[2]]), 1, sd, na.rm=TRUE)

nov.whit.mean<-apply(as.matrix(Raster_whit[[3]]), 1, mean, na.rm=TRUE)
nov.whit.sd<-apply(as.matrix(Raster_whit[[3]]), 1, sd, na.rm=TRUE)

dec.whit.mean<-apply(as.matrix(Raster_whit[[4]]), 1, mean, na.rm=TRUE)
dec.whit.sd<-apply(as.matrix(Raster_whit[[4]]), 1, sd, na.rm=TRUE)

feb.whit.mean<-apply(as.matrix(Raster_whit[[5]]), 1, mean, na.rm=TRUE)
feb.whit.sd<-apply(as.matrix(Raster_whit[[5]]), 1, sd, na.rm=TRUE)

mar.whit.mean<-apply(as.matrix(Raster_whit[[6]]), 1, mean, na.rm=TRUE)
mar.whit.sd<-apply(as.matrix(Raster_whit[[6]]), 1, sd, na.rm=TRUE)

apr.whit.mean<-apply(as.matrix(Raster_whit[[7]]), 1, mean, na.rm=TRUE)
apr.whit.sd<-apply(as.matrix(Raster_whit[[7]]), 1, sd, na.rm=TRUE)

may.whit.mean<-apply(as.matrix(Raster_whit[[8]]), 1, mean, na.rm=TRUE)
may.whit.sd<-apply(as.matrix(Raster_whit[[8]]), 1, sd, na.rm=TRUE)

jun.whit.mean<-apply(as.matrix(Raster_whit[[9]]), 1, mean, na.rm=TRUE)
jun.whit.sd<-apply(as.matrix(Raster_whit[[9]]), 1, sd, na.rm=TRUE)

jul.whit.mean<-apply(as.matrix(Raster_whit[[10]]), 1, mean, na.rm=TRUE)
jul.whit.sd<-apply(as.matrix(Raster_whit[[10]]), 1, sd, na.rm=TRUE)

aug.whit.mean<-apply(as.matrix(Raster_whit[[11]]), 1, mean, na.rm=TRUE)
aug.whit.sd<-apply(as.matrix(Raster_whit[[11]]), 1, sd, na.rm=TRUE)

sep.whit.mean<-apply(as.matrix(Raster_whit[[12]]), 1, mean, na.rm=TRUE)
sep.whit.sd<-apply(as.matrix(Raster_whit[[12]]), 1, sd, na.rm=TRUE)
#############################################################################
###########################################################################
# spde files
###########################################################################

Jan.spde.mean<-apply(as.matrix(Raster_spde[[1]]), 1, mean, na.rm=TRUE)
Jan.spde.sd<-apply(as.matrix(Raster_spde[[1]]), 1, sd, na.rm=TRUE)

oct.spde.mean<-apply(as.matrix(Raster_spde[[2]]), 1, mean, na.rm=TRUE)
oct.spde.sd<-apply(as.matrix(Raster_spde[[2]]), 1, sd, na.rm=TRUE)

nov.spde.mean<-apply(as.matrix(Raster_spde[[3]]), 1, mean, na.rm=TRUE)
nov.spde.sd<-apply(as.matrix(Raster_spde[[3]]), 1, sd, na.rm=TRUE)

dec.spde.mean<-apply(as.matrix(Raster_spde[[4]]), 1, mean, na.rm=TRUE)
dec.spde.sd<-apply(as.matrix(Raster_spde[[4]]), 1, sd, na.rm=TRUE)

feb.spde.mean<-apply(as.matrix(Raster_spde[[5]]), 1, mean, na.rm=TRUE)
feb.spde.sd<-apply(as.matrix(Raster_spde[[5]]), 1, sd, na.rm=TRUE)

mar.spde.mean<-apply(as.matrix(Raster_spde[[6]]), 1, mean, na.rm=TRUE)
mar.spde.sd<-apply(as.matrix(Raster_spde[[6]]), 1, sd, na.rm=TRUE)

apr.spde.mean<-apply(as.matrix(Raster_spde[[7]]), 1, mean, na.rm=TRUE)
apr.spde.sd<-apply(as.matrix(Raster_spde[[7]]), 1, sd, na.rm=TRUE)

may.spde.mean<-apply(as.matrix(Raster_spde[[8]]), 1, mean, na.rm=TRUE)
may.spde.sd<-apply(as.matrix(Raster_spde[[8]]), 1, sd, na.rm=TRUE)

jun.spde.mean<-apply(as.matrix(Raster_spde[[9]]), 1, mean, na.rm=TRUE)
jun.spde.sd<-apply(as.matrix(Raster_spde[[9]]), 1, sd, na.rm=TRUE)

jul.spde.mean<-apply(as.matrix(Raster_spde[[10]]), 1, mean, na.rm=TRUE)
jul.spde.sd<-apply(as.matrix(Raster_spde[[10]]), 1, sd, na.rm=TRUE)

aug.spde.mean<-apply(as.matrix(Raster_spde[[11]]), 1, mean, na.rm=TRUE)
aug.spde.sd<-apply(as.matrix(Raster_spde[[11]]), 1, sd, na.rm=TRUE)

sep.spde.mean<-apply(as.matrix(Raster_spde[[12]]), 1, mean, na.rm=TRUE)
sep.spde.sd<-apply(as.matrix(Raster_spde[[12]]), 1, sd, na.rm=TRUE)
#############################################################################
#       Latfiles_dataframe
###############################################################################
DF_EVI2<-data.frame(Latitude, 
                    Jan.raw.mean, Jan.raw.sd, oct.raw.mean, oct.raw.sd,
                    nov.raw.mean, nov.raw.sd, dec.raw.mean, dec.raw.sd, 
                    feb.raw.mean, feb.raw.sd, mar.raw.mean, mar.raw.sd,
                    apr.raw.mean, apr.raw.sd, may.raw.mean, may.raw.sd,
                    jun.raw.mean, jun.raw.sd, jul.raw.mean, jul.raw.sd,
                    aug.raw.mean, aug.raw.sd, sep.raw.mean, sep.raw.sd,
                    
                    Jan.sgolay.mean, Jan.sgolay.sd, oct.sgolay.mean, oct.sgolay.sd,
                    nov.sgolay.mean, nov.sgolay.sd, dec.sgolay.mean, dec.sgolay.sd, 
                    feb.sgolay.mean, feb.sgolay.sd, mar.sgolay.mean, mar.sgolay.sd,
                    apr.sgolay.mean, apr.sgolay.sd, may.sgolay.mean, may.sgolay.sd,
                    jun.sgolay.mean, jun.sgolay.sd, jul.sgolay.mean, jul.sgolay.sd,
                    aug.sgolay.mean, aug.sgolay.sd, sep.sgolay.mean, sep.sgolay.sd,
                    
                    Jan.whit.mean, Jan.whit.sd, oct.whit.mean, oct.whit.sd,
                    nov.whit.mean, nov.whit.sd, dec.whit.mean, dec.whit.sd, 
                    feb.whit.mean, feb.whit.sd, mar.whit.mean, mar.whit.sd,
                    apr.whit.mean, apr.whit.sd, may.whit.mean, may.whit.sd,
                    jun.whit.mean, jun.whit.sd, jul.whit.mean, jul.whit.sd,
                    aug.whit.mean, aug.whit.sd, sep.whit.mean, sep.whit.sd,
                    
                    Jan.spde.mean, Jan.spde.sd, oct.spde.mean, oct.spde.sd,
                    nov.spde.mean, nov.spde.sd, dec.spde.mean, dec.spde.sd, 
                    feb.spde.mean, feb.spde.sd, mar.spde.mean, mar.spde.sd,
                    apr.spde.mean, apr.spde.sd, may.spde.mean, may.spde.sd,
                    jun.spde.mean, jun.spde.sd, jul.spde.mean, jul.spde.sd,
                    aug.spde.mean, aug.spde.sd, sep.spde.mean, sep.spde.sd)

write.csv(DF_EVI2, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/Lat_files/DF_EVI2NEW.csv")


###############################################################################################
#               LATITUDINAL GRAPHS
##############\#################################################################################
rm(list=ls())
DF<-read.csv("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/VegData/Lat_files/DF_EVI2NEW.csv")
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
######################################################################
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
ebwhit1 <- aes(ymax = Jan.whit.mean + Jan.whit.sd, ymin = Jan.whit.mean - Jan.whit.sd)
ebwhit2 <- aes(ymax = feb.whit.mean + feb.whit.sd, ymin = feb.whit.mean - feb.whit.sd)
ebwhit3 <- aes(ymax = mar.whit.mean + mar.whit.sd, ymin = mar.whit.mean - mar.whit.sd)
ebwhit4 <- aes(ymax = apr.whit.mean + apr.whit.sd, ymin = apr.whit.mean - apr.whit.sd)
ebwhit5 <- aes(ymax = may.whit.mean + may.whit.sd, ymin = may.whit.mean - may.whit.sd)
ebwhit6 <- aes(ymax = jun.whit.mean + jun.whit.sd, ymin = jun.whit.mean - jun.whit.sd)
ebwhit7 <- aes(ymax = jul.whit.mean + jul.whit.sd, ymin = jul.whit.mean - jul.whit.sd)
ebwhit8 <- aes(ymax = aug.whit.mean + aug.whit.sd, ymin = aug.whit.mean - aug.whit.sd)
ebwhit9 <- aes(ymax = sep.whit.mean + sep.whit.sd, ymin = sep.whit.mean - sep.whit.sd)
ebwhit10 <- aes(ymax = oct.whit.mean + oct.whit.sd, ymin = oct.whit.mean - oct.whit.sd)
ebwhit11 <- aes(ymax = nov.whit.mean + nov.whit.sd, ymin = nov.whit.mean - nov.whit.sd)
ebwhit12 <- aes(ymax = dec.whit.mean + dec.whit.sd, ymin = dec.whit.mean - dec.whit.sd)
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
pJan <- pJan + geom_ribbon(ebraw1, alpha = 0.5, fill='grey') + geom_line(aes(y=Jan.raw.mean), colour="Black")
pJan <- pJan + geom_ribbon(ebsgolay1, alpha = 0.5, fill='light blue') + geom_line(aes(y=Jan.sgolay.mean), colour="Dark Blue") 
pJan <- pJan + geom_ribbon(ebwhit1, alpha = 0.5, fill='light green') + geom_line(aes(y=Jan.whit.mean), colour="Dark Green") 
pJan <- pJan + geom_ribbon(ebspde1, alpha = 0.5, fill='Orange') + geom_line(aes(y=Jan.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(1,-0.5,1,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.4, label="Jan",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))


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

pApr <- ggplot(DF, aes(Latitude))
pApr <- pApr + geom_ribbon(ebraw4, alpha = 0.5, fill='grey') + geom_line(aes(y=mar.raw.mean), colour="Black")
pApr <- pApr + geom_ribbon(ebsgolay4, alpha = 0.5, fill='light blue') + geom_line(aes(y=mar.sgolay.mean), colour="Dark Blue") 
pApr <- pApr + geom_ribbon(ebwhit4, alpha = 0.5, fill='light green') + geom_line(aes(y=mar.whit.mean), colour="Dark Green") 
pApr <- pApr + geom_ribbon(ebspde4, alpha = 0.5, fill='Orange') + geom_line(aes(y=mar.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,-0.5,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=-0.4, label="Apr",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))

pMay<- ggplot(DF, aes(Latitude))
pMay <- pMay + geom_ribbon(ebraw5, alpha = 0.5, fill='grey') + geom_line(aes(y=may.raw.mean), colour="Black")
pMay <- pMay + geom_ribbon(ebsgolay5, alpha = 0.5, fill='light blue') + geom_line(aes(y=may.sgolay.mean), colour="Dark Blue") 
pMay <- pMay + geom_ribbon(ebwhit5, alpha = 0.5, fill='light green') + geom_line(aes(y=may.whit.mean), colour="Dark Green") 
pMay <- pMay + geom_ribbon(ebspde5, alpha = 0.5, fill='Orange') + geom_line(aes(y=may.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.4, label="May",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))

pJun <- ggplot(DF, aes(Latitude))
pJun <- pJun + geom_ribbon(ebraw6, alpha = 0.5, fill='grey') + geom_line(aes(y=jun.raw.mean), colour="Black")
pJun <- pJun + geom_ribbon(ebsgolay6, alpha = 0.5, fill='light blue') + geom_line(aes(y=jun.sgolay.mean), colour="Dark Blue") 
pJun <- pJun + geom_ribbon(ebwhit6, alpha = 0.5, fill='light green') + geom_line(aes(y=jun.whit.mean), colour="Dark Green") 
pJun <- pJun + geom_ribbon(ebspde6, alpha = 0.5, fill='Orange') + geom_line(aes(y=jun.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,-0.5,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=-0.4, label="Jun",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))

pJul<- ggplot(DF, aes(Latitude))
pJul <- pJul + geom_ribbon(ebraw7, alpha = 0.5, fill='grey') + geom_line(aes(y=jul.raw.mean), colour="Black")
pJul <- pJul + geom_ribbon(ebsgolay7, alpha = 0.5, fill='light blue') + geom_line(aes(y=jul.sgolay.mean), colour="Dark Blue") 
pJul <- pJul + geom_ribbon(ebwhit7, alpha = 0.5, fill='light green') + geom_line(aes(y=jul.whit.mean), colour="Dark Green") 
pJul <- pJul + geom_ribbon(ebspde7, alpha = 0.5, fill='Orange') + geom_line(aes(y=jul.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.4, label="Jul",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))

pAug <- ggplot(DF, aes(Latitude))
pAug <- pAug + geom_ribbon(ebraw8, alpha = 0.5, fill='grey') + geom_line(aes(y=aug.raw.mean), colour="Black")
pAug <- pAug + geom_ribbon(ebsgolay8, alpha = 0.5, fill='light blue') + geom_line(aes(y=aug.sgolay.mean), colour="Dark Blue") 
pAug <- pAug + geom_ribbon(ebwhit8, alpha = 0.5, fill='light green') + geom_line(aes(y=aug.whit.mean), colour="Dark Green") 
pAug <- pAug + geom_ribbon(ebspde8, alpha = 0.5, fill='Orange') + geom_line(aes(y=aug.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,-0.5,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=-0.4, label="Aug",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))


pSep<- ggplot(DF, aes(Latitude))
pSep <- pSep + geom_ribbon(ebraw9, alpha = 0.5, fill='grey') + geom_line(aes(y=sep.raw.mean), colour="Black")
pSep <- pSep + geom_ribbon(ebsgolay9, alpha = 0.5, fill='light blue') + geom_line(aes(y=sep.sgolay.mean), colour="Dark Blue") 
pSep <- pSep + geom_ribbon(ebwhit9, alpha = 0.5, fill='light green') + geom_line(aes(y=sep.whit.mean), colour="Dark Green") 
pSep <- pSep + geom_ribbon(ebspde9, alpha = 0.5, fill='Orange') + geom_line(aes(y=sep.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.4, label="Sep",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))

pOct<- ggplot(DF, aes(Latitude))
pOct <- pOct + geom_ribbon(ebraw10, alpha = 0.5, fill='grey') + geom_line(aes(y=oct.raw.mean), colour="Black")
pOct <- pOct + geom_ribbon(ebsgolay10, alpha = 0.5, fill='light blue') + geom_line(aes(y=oct.sgolay.mean), colour="Dark Blue") 
pOct <- pOct + geom_ribbon(ebwhit10, alpha = 0.5, fill='light green') + geom_line(aes(y=oct.whit.mean), colour="Dark Green") 
pOct <- pOct + geom_ribbon(ebspde10, alpha = 0.5, fill='Orange') + geom_line(aes(y=oct.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,-0.5,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=-0.4, label="Oct",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))

pNov<- ggplot(DF, aes(Latitude))
pNov <- pNov + geom_ribbon(ebraw11, alpha = 0.5, fill='grey') + geom_line(aes(y=nov.raw.mean), colour="Black")
pNov <- pNov + geom_ribbon(ebsgolay11, alpha = 0.5, fill='light blue') + geom_line(aes(y=nov.sgolay.mean), colour="Dark Blue") 
pNov <- pNov + geom_ribbon(ebwhit11, alpha = 0.5, fill='light green') + geom_line(aes(y=nov.whit.mean), colour="Dark Green") 
pNov <- pNov + geom_ribbon(ebspde11, alpha = 0.5, fill='Orange') + geom_line(aes(y=nov.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,-0.5,-0.5,1),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_text(size=20,face="bold")) +
  annotate("text", x=-50, y=-0.4, label="Nov",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))

pDec<- ggplot(DF, aes(Latitude))
pDec <- pDec + geom_ribbon(ebraw12, alpha = 0.5, fill='grey') + geom_line(aes(y=dec.raw.mean), colour="Black")
pDec <- pDec + geom_ribbon(ebsgolay12, alpha = 0.5, fill='light blue') + geom_line(aes(y=dec.sgolay.mean), colour="Dark Blue") 
pDec <- pDec + geom_ribbon(ebwhit12, alpha = 0.5, fill='light green') + geom_line(aes(y=dec.whit.mean), colour="Dark Green") 
pDec <- pDec + geom_ribbon(ebspde12, alpha = 0.5, fill='Orange') + geom_line(aes(y=dec.spde.mean), colour="Red") + 
  coord_flip() +
  #theme(plot.margin=unit(c(-0.5,1,1,-0.5),"cm"))+
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_text(size=20,face="bold")) +
  theme(axis.text.y=element_blank()) +
  annotate("text", x=-50, y=-0.4, label="Dec",size=8,fontface="bold.italic",colour="black",parse=TRUE) +
  scale_x_continuous(limits = c(-60,60),
                     breaks=c(-60,0,60)) + 
  scale_y_continuous(limits = c(-0.5, 1),
                     breaks=c(-0.5,0,1))
################################################################## plot.margin=unit(c(1,1,-0.5,1), "mm")
library(gridExtra)
library(grid)
grid.arrange(pJan,pFeb,pMar,pApr,pMay,pJun,
             pJul,pAug,pSep,pOct,pNov,pDec,ncol=2)

#, width=c(1,1)
#, height=c(1,1,1,1,1,1)









?textGrob
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
                         nrow=6, ncol=2,
                         clip = TRUE,
                         strip=FALSE,
                         legend = NULL,
                         main=textGrob("",
                                       vjust = 1, 
                                       gp = gpar(fontface = "bold", cex = 2)),
                         xlab=textGrob("EVI2",
                                       vjust = 1, 
                                       gp = gpar(fontface = "bold", cex = 1.5)),
                         left = textGrob("Latitude", rot = 90, vjust = 1, 
                                         gp = gpar(fontface = "bold",
                                                   cex = 1.5))))
####################################################################################


require(INLA)
df<-read.csv("C:/Users/eokuto/Desktop/Projects/Project2/testhormonic/testhormonicreg.csv")

n = dim(df)[1]
nk = 180
nb = 1
X = matrix(runif(n*nb), n, nb) 
y = df$x

harmonics = matrix(0, n, nk)
for(k in 1:nk) {
  harmonics[, k] = sin(pi/n * k * 1:n)
}

idx = 1:n
formula = (y ~ -1 + X + f(idx, model="z",  Z=harmonics,
                          hyper = list(prec = list(initial = -4,
                                                   fixed=TRUE))))
result = inla(formula,
              data = list(y=y, idx=idx, 
                          harmonics = harmonics, X=X),
              family = "gaussian",
              control.predictor = list(compute=TRUE))

#result$summary.random$idx
result$summary.fixed[,"mean"]    # estimate for beta
#############################################################
require(hydroGOF)
obs <- 1:10
sim <- 1:10
d(sim, obs)

obs <- 1:10
sim <- 2:11
d(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the index of agreement for the "best" (unattainable) case
d(sim=sim, obs=obs)

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new index of agreement
d(sim=sim, obs=obs)








