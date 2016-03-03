

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Installing MODIS package
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
install.packages("MODIS", repos="http://R-Forge.R-project.org")
install.packages('mapdata',dependencies=TRUE)
require(MODIS);require(raster);require(foreach)
MODISoptions()
setRepositories() 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Importing raster images in .tif* format and developing a stack
#Monthly Records between 1981 and 2011
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
files = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped"),pattern="*.tif", full.names=TRUE)
rasters.files = foreach(i=1:length(files), .combine=c, .verbose=FALSE) %do%
  raster(files[[i]])
stk.rasters = stack(rasters.files, bands=NULL, native=FALSE, RAT=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#checking the date information in an input file (ie. from MODIS filenames)
#and generates an output date information
#used for the creation of a regular intervalled result.
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vi.files <- stack(files)
timeInfo <- orgTime(vi.files, nDays="1 month",
                    begin="1982001", end="2011335",
                    pillow=12, pos1=10, pos2=16, format="%Y%j")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#multi-core computing with'raster' functions
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(snow) # Might require snow package
beginCluster(type="SOCK",exclude="MODIS") # See: ?beginCluster

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Applying the filter and exporting yearly stacks to the specified directory
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
system.time(whittaker.raster(stk.rasters,
                             w=NULL, t=NULL,
                             timeInfo=timeInfo,
                             lambda = 5000,
                             nIter= 3,
                             outDirPath = "F:/CLIM_DATA/Kenya_evi2_Records/Adaptive_Smoothers_folder",
                             overwrite=TRUE))
endCluster()

s=stack("F:/CLIM_DATA/Kenya_evi2_Records/Adaptive_Smoothers_folder/NDVI_YearlyLambda5000_year1982.tif")

