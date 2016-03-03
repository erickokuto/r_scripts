
require(spTDyn)
require(INLA)
require(spate)
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
write.dta(DRC_Data, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI2_Data_Wide.dta")
write.csv(DRC_Data, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI2_Data_Wide.csv")

DRC_Data_long<-reshape(DRC_Data, direction="long", varying=list(names(DRC_Data)[3:dim(DRC_Data)[2]]),
                       v.names="EVI2_Value", idvar=c("x","y"),
                       timevar="time_var", times=rep(1982:2016), each=24)
write.dta(DRC_Data_long, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI2_Data_long.dta")
write.csv(DRC_Data_long, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/DRC_EVI2_Data_long.csv")


