
require("INLA"); require(foreach); require(raster)

GIMMS.list <- list.files(file.path("J:/Gimms15Day_Data_Smoothing"),
                         pattern="*.tif", full.names=TRUE)

GIMMS.rasters<-foreach(all=1:length(GIMMS.list), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.list[[all]])

days.15<-rep(c("15a", "15b"), times=366)
months<-c(rep(c(4,4,8,8,12,12,2,2,1,1,7,7,6,6,3,3,5,5,11,11,10,10,9,9), times=12 ),
          rep(c(8,8,12,12,7,7,11,11,10,10,9,9), times=1),
          rep(c(4,4,8,8,12,12,2,2,1,1,7,7,6,6,3,3,5,5,11,11,10,10,9,9), times=18))
years<-c(rep(2000:2011, each=24),
         rep(1981, times=12),
         rep(1982:1999, each=24))

for (i in 1:length(GIMMS.rasters)) {
  writeRaster(GIMMS.rasters[[i]],
              filename=paste('J:/Gimms15Day_Data_Renamed2/',
                             years[i],"_", months[i],"_", days.15[i],"_",'GIMMS' , pattern=".tif", sep=''),
              overwrite=TRUE)
}


# Extract data using renamed gimms data
GIMMS.renamed <- list.files(file.path("/vm/eokuto/Gimms15Day_Data_Renamed2"),
                            pattern="*.tif", full.names=TRUE)

GIMMS.rasters2<-foreach(all=1:length(GIMMS.renamed), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.renamed[[all]])

stack.GIMMS<-stack(GIMMS.rasters2, bands=NULL, native=FALSE, RAT=TRUE)
points<-rbind(c(15.78,47.86), c(-1.29,36.82), c(0,25), c(12.65,-8))
NDVI<-extract(stack.GIMMS, points)

days.15N<-rep(c("15a", "15b"), times=366)
yearsN<-c(rep(c(1981), times=12),
          rep(c(1982:2011), each=24))
monthsN<-c(rep(c(7:12), each=2 ),
           rep(rep(c(1:12), each=2), times=30))



df<-data.frame(brazil=as.vector(NDVI[1,]),kenya=as.vector(NDVI[2,]),
               congo_forest=as.vector(NDVI[3,]),mali=as.vector(NDVI[4,]),day=days.15N,
               month=monthsN,year=yearsN)
write.csv(df,"/vm/eokuto/hormonic.csv")

