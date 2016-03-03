
require(maptools)  ## For wrld_simpl
require(raster)
require(foreach)

## Example SpatialPolygonsDataFrame
data(wrld_simpl)
KEpoly_shape <- subset(wrld_simpl, NAME=="Kenya")
plot(KEpoly_shape, add=FALSE, lwd=2)


evi2_lists20km <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Kenya_evi2_Records/kenya_20kmclipped"),
                         pattern="*.tif", full.names=TRUE)
evi2_lists10km <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Kenya_evi2_Records/kenya_10kmclipped"),
                             pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists20km), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists20km[[files]])
stk.Data.evi<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)

nlayers(stk.Data.evi)

X11()
plot(stk.Data.evi[[1]])

# trans.shp=readShapePoly("F:/CLIM_DATA/tranzoia_county/trans.shp")
# plot(trans.shp)

## crop and mask
r1.kenya <- crop(stk.Data.evi[[1]], extent(KEpoly_shape))
r2.kenya <- mask(r1.kenya, KEpoly_shape)

## Check that it worked
plot(r2.kenya)
plot(KEpoly_shape, add=TRUE, lwd=2)

# vals<-extract(stk.Data.evi[[1]],1:ncell(stk.Data.evi[[1]]))
# coord<-xyFromCell(stk.Data.evi[[1]],1:ncell(stk.Data.evi[[1]]))
# combine<-cbind(coord,vals)
# write.table(combine,"xyvalues.txt") 

#time variables
month=rep(1:12, times=30)
year=rep(1982:2011, each=24)

coord.kenya<-xyFromCell(r1.kenya, 1:ncell(r1.kenya))
vals.kenya<-extract(stk.Data.evi, coord.kenya)
dim(vals.kenya)

combine.kenya=data.frame(coord.kenya,vals.kenya)
write.csv(combine.kenya, "F:/CLIM_DATA/EVI2_Smoothing_Datasets/Kenya_evi2_Records/kenya.monthly.20KMunsmoothedEVI2.csv")
dim(combine.kenya)
####################################################
