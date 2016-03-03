
require(MODIS);require(raster);require(foreach)
## Not run: 
# The full capacity of the following functions is currently avaliable only with M*D13 data.
# !! The function is very new, double check the result !!

# You need to extract: 'vegetation index', 'VI_Quality layer', and 'composite day of the year'.
# runGdal(product="MOD13A2",begin="2004340",extent="sicily",end="2006070",
# job="fullCapa",SDSstring="101000000010")
# You can download this dataset from (2.6 MB): 
# https://ivfl-rio.boku.ac.at/owncloud/public.php?service=files&t=2fdac3598dba8f5bd865b9dadd715e22&download
# copy it to: options("MODIS_outDirPath")
# Extract: 
# unzip(paste0(options("MODIS_outDirPath"),"fullCapa.zip"),exdir=options("MODIS_outDirPath"))
# delete the zip file:
# unlink(paste0(options("MODIS_outDirPath"),"fullCapa.zip"))
path <- paste0(options("MODIS_outDirPath"),"fullCapa")

# the only obligatory dataset is the vegetatino index 
# get the 'vi' data in the source directory: 
vi <- preStack(path=path, pattern="*_NDVI.tif$")

# "orgTime" detects timing information of the input data and generates based on the arguments
# the output date information. 
# For spline functions (in general) the beginning and the end of the time series
# is always problematic. So there is the argument "pillow" (default 75 days) that adds
# (if available) some more layers on the two endings.

timeInfo <- orgTime(vi,nDays=16,begin="2005001",end="2005365",pillow=40)

# now re-run "preStack" with two differences, 'files' (output of the first 'preStack' call)
# and the 'timeInfo'
# Here only the data needed for the filtering is extracted:
vi <- preStack(files=vi,timeInfo=timeInfo)

# For speedup try (On some Win7 problematic): 
# beginCluster() # See: ?beginCluster
system.time(whittaker.raster(vi,timeInfo=timeInfo,lambda=5000))

# if the files are M*D13 you can use also Quality layers and the composite day of the year:
wt <- preStack(path=path, pattern="*_VI_Quality.tif$", timeInfo=timeInfo)
# can also be already stacked:
inT <- preStack(path=path, pattern="*_composite_day_of_the_year.tif$", timeInfo=timeInfo)

# beginCluster() # See: ?beginCluster 
system.time(whittaker.raster(vi=vi, wt=wt, inT=inT, timeInfo=timeInfo, lambda=5000, overwrite=TRUE))