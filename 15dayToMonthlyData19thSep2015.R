

require(foreach); require(raster)

# fun.flag<-function(x){
#   y=ifelse(x > 1, 1,
#            ifelse(x < -1, -1, x))
#   y[is.na(y)] <- NA
#   y[is.nan(y) ] <- NA
#   return(y)
# }

# based on variable values
# newdata <- mydata[ which(mydata$gender=='F'
#                          & mydata$age > 65), ]
# 
##############################################################################
#    EVI2
##############################################################################
sgolayDataEVI <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Africa/africa_yearly_evi2/1982"),
                      pattern="*.tif", full.names=TRUE)
Raster_EVI1982<-foreach(i=1:length(sgolayDataEVI), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(sgolayDataEVI[[i]])
stk.evi2.1982<-stack(Raster_EVI1982, bands=NULL, native=FALSE, RAT=TRUE)

require(rasterVis)
spplot(stk.evi2.1982[[1]])

# vals<-extract(stk.evi2.1982,1:ncell(stk.evi2.1982))
# coord<-xyFromCell(stk.evi2.1982,1:ncell(stk.evi2.1982))
# combine<-cbind(coord,vals)
# write.table(combine,"xyvalues.txt")


vals<-as.vector(extract(stk.evi2.1982,19590))
coord<-xyFromCell(stk.evi2.1982,9590)
combine<-cbind(coord,vals)
#write.table(combine,"xyvalues.txt")

















#############################################################
sgolayDataEVI <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Africa_EVI2_Records/AfricaMonthlyevi"),
                            pattern="*.tif", full.names=TRUE)
Raster_sgolayDataEVI<-foreach(i=1:length(sgolayDataEVI), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(sgolayDataEVI[[i]])

length(Raster_sgolayDataEVI)
plot(Raster_sgolayDataEVI[[1]])
###########
mat <- extract( s , 1:ncell(s) )
head( mat )
head( rasterToPoints( s ) )


vals<-extract(s,1:ncell(s))
coord<-xyFromCell(s,1:ncell(s))
combine<-cbind(coord,vals)
write.table(combine,"xyvalues.txt")

xyfromraster<-xyFromCell(stack.sgolay[[1]], cell.nos)
combine.coord<-cbind(points2,xyfromraster)











###################################################################
# NDVI
#################################################################
africaDataNDVI <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Africa/AfricaBiMonthlyndvi"),
                            pattern="*.tif", full.names=TRUE)
Raster_africaDataNDVI<-foreach(i=1:length(africaDataNDVI), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(africaDataNDVI[[i]])

