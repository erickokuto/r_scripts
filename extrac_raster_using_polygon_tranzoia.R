
require(maptools)  ## For wrld_simpl
require(raster)
require(foreach)

## Example SpatialPolygonsDataFrame
data(wrld_simpl)
poly_shape <- subset(wrld_simpl, NAME=="Rwanda")
plot(poly_shape, add=FALSE, lwd=2)

evi2_lists <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Kenya_evi2_Records/kenya_10kmclipped"),
                            pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists[[files]])
stk.Data.evi<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)

nlayers(stk.Data.evi)

trans.shp=readShapePoly("F:/CLIM_DATA/tranzoia_county/trans.shp")
plot(trans.shp)

## crop and mask
r1.tranz <- crop(stk.Data.evi[[5]], extent(trans.shp))
r2.tranz <- mask(r1.tranz, trans.shp)

## Check that it worked
plot(r2.tranz)
plot(trans.shp, add=TRUE, lwd=2)

# vals<-extract(stk.Data.evi[[1]],1:ncell(stk.Data.evi[[1]]))
# coord<-xyFromCell(stk.Data.evi[[1]],1:ncell(stk.Data.evi[[1]]))
# combine<-cbind(coord,vals)
# write.table(combine,"xyvalues.txt") 

#time variables
month=rep(1:12, times=31)
year=rep(1982:2012, each=12)

coord.tranz<-xyFromCell(r1.tranz,1:ncell(r1.tranz))
vals.tranz<-extract(stk.Data.evi,coord.tranz)
dim(vals.tranz)

combine.tranz=data.frame(coord.tranz,vals.tranz)
write.csv(combine.tranz, "F:/CLIM_DATA/tranzoia_county/combine.tranz.10KMunsmoothed.csv")
dim(combine.tranz)
plot(vals.tranz[1, 120:132], type="l")
plot(vals.tranz[120, ], type="l")

####################################################
#f(location, model="bym", group=hourindex, 
#   control.group=list(model="somecyclicalmodel"))
####################################################
require(reshape2)
w=pi/6
longcombine.tranz<- melt(combine.tranz, id.vars=c("x", "y"))
longcombine.tranz$month=rep(rep(1:12, each=150), times=31)
longcombine.tranz$year=rep(1982:2012, each=150*12)
longcombine.tranz$time=rep(1:372, each=150)
longcombine.tranz[, "amplitude.cos"] <- cos(w*longcombine.tranz[, "time"])
longcombine.tranz[, "amplitude.sine"] <- sin(w*longcombine.tranz[, "time"])
longcombine.tranz[, "weight"] <- w*longcombine.tranz[, "time"]


dim(longcombine.tranz)
head(longcombine.tranz)
tail(longcombine.tranz)
write.csv(longcombine.tranz, "F:/CLIM_DATA/tranzoia_county/longcombine.tranz3.csv")

#longcombine.tranz[1:200, ]
