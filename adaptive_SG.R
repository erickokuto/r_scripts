
install.packages("MODIS", repos="http://R-Forge.R-project.org")
require(MODIS)
MODISoptions()
setRepositories() # activate CRAN, R-forge, and Omegahat and then: 
install.packages('mapdata',dependencies=TRUE))

setwd(paste0(options()$MODIS_outDirPath,"/testJob"))
dir() # your data

# look for data
vi     <- preStack(pattern="*_NDVI.tif$")
qa   <- preStack(pattern="*_VI_Quality.tif$")
time <- preStack(pattern="*_composite_day_of_the_year.tif$")

crono <- orgTime(vi) # here you decide the time steps for the output

# order data by time, and use only what is defined by orgTime
vi      <- stack(preStack(files=vi,timeInfo=crono))
time <- stack(preStack(files=time,timeInfo=crono))
qa    <- stack(preStack(files=qa,timeInfo=crono))

#the weighting is generated internally in whittaker.raster but you
#could also do it separately for more control see ?makeWeights
#qa <-  makeWeights(qa, bitShift=2, bitMask=15, threshold=6) # for
#bitShift and bitMask info on other products run
detectBitInfo("MOD13Q1")

#filtering
whittaker.raster(vi=vi, wt=qa, inT=time, timeInfo=crono, lambda=1000, overwrite=TRUE) 

?whittaker.raster

#Without weighting function
#Example (in ?whittaker.raster):
path <- 'dir to your data'
#with the first run of preStack all files in 'path' with the ending *_NDVI.tif$ are listened

vi <- preStack(path=path, pattern="*_NDVI.tif$")
vi

#with orgTime you say: retrieve and sort ascending by time, all
#filenames (vi), that are needed to process the period begin to end,
#adding (_if available_) a pillow of data on the begin and on the end you
#your time serie to ensure a high quality filtering also on the temporal borders.

timeInfo <- orgTime(vi, nDays=16, begin="2005001", end="2005365", pillow=40)
timeInfo

#now just get the data in vi, selected and sorted by timeInfo
vi <- preStack(files=vi, timeInfo=timeInfo)
vi

# now the simples run of whittaker.raster
# 'vi' is a vector of filenames, but you can also stack it:
vi <- stack(vi)

whittaker.raster(vi=vi,timeInfo=timeInfo) # the result is written to a
                                          #file in the current dir as specified by the default argument in
                                          #whittaker.raster outDirPath="./"

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Importing raster images in .tif* format and developing a stack
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
files = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped"),pattern="*.tif", full.names=TRUE)
rasters.files = foreach(i=1:length(files), .combine=c, .verbose=FALSE) %do%
  raster(files[[i]])
stk.rasters = stack(rasters.files, bands=NULL, native=FALSE, RAT=TRUE)


#The raster (.tif) files are on monthly basis ascending order, not sure whether begin and end are correct
#timeInfo <- orgTime(stk.rasters, nDays=8, begin=1981, end=2011, pillow=75)
#timeInfo

#This function checks the date information in an 
#input file (ie. from MODIS filenames) and generates an output date information used
#for the creation of a regular intervalled result.

days=rep(rep(c(8, 16, 24), times=12),times=30)
months=rep(rep(1:12, each=3), times=30)
years=rep(1982:2011, each=36)
A=rep("A", times=360)



#VIP15P4-A1982001-003-EVI2-Filtered

# months=rep(c("001", "032","060","091","121","152","182","213","244","275","305","335"), times=30)
# years=rep(1982:2011, each=12)
# #A=rep("VIP15P4-A", times=360)
# A=rep("A", times=360)
# B=rep("-003-EVI2-Filtered", times=360) 
# #data=data.frame(A, years, months, B)
# data=data.frame(A, years, months)


require(plyr)
# ldply(apply(data, 1, function(x) data.frame(
#   vi = paste(x[c("years", "months", "B")], collapse=""))))

# ss=ldply(apply(data, 1, function(x) data.frame(
#    vi=paste(x[c("years", "months")], collapse=""))))
# sss=as.vector(ss)
vi3 <- stack(files)

# xxx=as.character(ldply(apply(data, 1, function(x) data.frame(
#   paste(x[c("A", "years", "months")], collapse="")))))

# cc=format(as.Date("2005135","%Y%j"),"%Y/%b/%d")
# dates.record = foreach(i=1:dim(ss)[1], .combine=c, .verbose=FALSE) %do%
#   format(as.Date(ss[i],"%Y%j"),"%Y/%b/%d")

timeInfo <- orgTime(vi3, nDays="1 month", begin="1982001", end="2011335", pillow=40)
timeInfo

#Adaptive Savitzky-Golay filter
# whittaker.raster(vi=stk.rasters, timeInfo=timeInfo, groupYears=FALSE, timeInfo = orgTime(vi),
#                                    outDirPath = "F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped2")

whittaker.raster(vi=stk.rasters, groupYears=TRUE, timeInfo=timeInfo,
                  outDirPath = "F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped2", overwrite=TRUE)

#plot(raster("F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped2/NDVI_YearlyLambda500_year1982.tif"))
