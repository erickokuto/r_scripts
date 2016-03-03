

require(INLA)
require(raster)
require(rasterVis)
require(foreach)

DRCevi2_lists <- list.files(file.path("F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/EVI2_Records"),
                         pattern="*.tif", full.names=TRUE)
DRCevi2_rasters<-foreach(files=1:length(DRCevi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(DRCevi2_lists[[files]])
DRC_stk.Data.evi2<-stack(DRCevi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)

obs.used <- function(x) {
  x=as.vector(x)
 xsum=sum(!is.na(x))
 percent=(xsum/720)*100
 return(percent)
} 


prection.images.fun<- function(x) {
  xx=ifelse(!is.na(x), NA, x)
  return(xx)
} 

years<-rep(2012:2016,each=24)
size<-120
days<-rep(c("001", "016", "032", "047", "060", "075", 
               "091", "106", "121", "136", "152", "167", "182",
               "197", "213", "228", "244", "259", "274", "289",
               "305", "320", "335", "350"), times=5)

pred.stack<-subset(stk.Data.evi2, 1:120)
pred.raster<-unstack(pred.stack)

pred.raster2<-foreach(files=1:length(pred.raster), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(pred.raster[[files]], fun=prection.images.fun)


for (i in 1:length(pred.raster2)) {
  writeRaster(pred.raster2[[i]],
              filename=paste('F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/EVI2_Records/', 
                             'VIP15P4.A', 
                             years[i],
                             days[i], '.003.EVI2.Filtered', 
                             pattern=".tif",
                             sep=''))
}

DRC_obs_used<-calc(stk.Data.evi2, fun=obs.used)
writeRaster(DRC_obs_used,
    "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/Results/Obs_used/DRC_obs_used.tif")
levelplot(DRC_obs_used)
levelplot(stk.Data.evi2[[1]])

# mat <- extract(stk.Data.evi2, 1:ncell(stk.Data.evi2) )
# head(mat)
coord<-xyFromCell(stk.Data.evi2,1:ncell(stk.Data.evi2))
# combine<-cbind(coord, mat)

require(foreign)
DRC_Data<-rasterToPoints(DRC_stk.Data.evi2)
DRC_Data<-as.data.frame(DRC_Data)

head(DRC_Data[ ,1:10])
write.dta(DRC_Data, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI2_Data_Wide.dta")
write.csv(DRC_Data, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI2_Data_Wide.csv")

require(reshape2)
#Importing the EVI2 records with 842 variables
DRC_EVI2<-read.dta("F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI2_Data_Wide.dta")

#Droping the coordinate x and y variables to enhance dropping of 15b columns
DRC_EVI3<-DRC_EVI2[, -(1:2)]
dim(DRC_EVI3)

#Removing 15b columns from the data
DRC_EVI4<-DRC_EVI3[, -(seq(from=2, to=840, by=2))]

#Adding the coordinates to the new EVI2 monthly records
DRC_EVI_Monthly_Data<-data.frame(DRC_EVI2[,1:2], DRC_EVI4)
dim(DRC_EVI_Monthly_Data)
names(DRC_EVI_Monthly_Data)

#Exporting the database records to other formats (stata and csv)
require(foreign)
write.dta(DRC_EVI_Monthly_Data, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI_Monthly_Data_Wide.dta")
write.csv(DRC_EVI_Monthly_Data, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI_Monthly_Data_Wide.csv")

#Preparing to reshape from wide to long format
require("plyr")
require("reshape2")
require("dplyr")
require("tidyr")
#####################################################
#Reshaping using a test sub records: Works fine
#######################################################
# subdata_long<-reshape(subdata, direction="long",
#                        varying=list(names(subdata)[3:dim(subdata)[2]]),
#                        v.names="Value", idvar=c("x","y"),
#                        timevar="Year", times=rep(1982, times=3), sep="")
##############################################################################
DRC_EVI_Monthly_Data_long<-reshape(DRC_EVI_Monthly_Data, direction="long",
                       varying=list(names(DRC_EVI_Monthly_Data)[3:dim(DRC_EVI_Monthly_Data)[2]]),
                       v.names="Value", idvar=c("x","y"),
                       timevar="Year", times=rep(1982:2016, each=12), sep="")
##############################################################################
#    Exporting the reshaped record database in long format to .dta and ,csv formats
#####################################################################################
write.dta(DRC_EVI_Monthly_Data_long,
          "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI_Monthly_Data_long.dta")
write.csv(DRC_EVI_Monthly_Data_long,
          "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI_Monthly_Data_long.csv")

