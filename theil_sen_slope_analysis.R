
require(raster); require(foreach); require(INLA);
require(zoo); require(dlm); require(R2BayesX); require(maptools)
require(bfastSpatial); require(bfastSpatial); require(gimms)
require(MODIS); require(foreach);
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Really Data with gaps
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
evi2_lists <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyEVI2-5km/sgolay"),
                           pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
    raster(evi2_lists[[files]])
stk.Data.evi<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)

stk.Data.evi2<-dropLayer(stk.Data.evi, c(121:360))
  
## obtaining shapefile for Africa
africa.shp=readShapePoly("F:/CLIM_DATA/kenya_border_shapefile/kenya.shp")
#africa.shp=readShapePoly("/media/erick/OKUTO1/EVI2_RECORDS/kenya_border_shapefile/kenya.shp")
  
plot(africa.shp)
## cropping africa region
stk.africa0 <- crop(stk.Data.evi, extent(africa.shp))
stk.africa <- mask(stk.africa0, africa.shp)
#stk.africa<-dropLayer(stk.africa1, c(37:360))
  
stk.africa2 = rasterToPoints(stk.africa)
coord = cbind(stk.africa2[, 1:2]) 
spDF_Africa = cbind(stk.africa2[, -(1:2)])      #10029 rows/pixels
dim(spDF_Africa)
plot(stk.Data.evi[[8]])
  
require(reshape)
spDF_Africa3 = spDF_Africa[1:5, ]


Kolmogorov.Smirnov.Test.Fun <- function(s1, s2) {
  fun.KolmogorovTest <- function(v) {
    x <- v[1:split]
    y <- v[(split+1):(2*split)]
    
    if (all(is.na(x)) | all(is.na(y))) {
      D = NA; p.value = NA
      result = cbind(D, p.value)
      return(result)
    }
    ks = ks.test(x, y)
    require(broom)   # converting the summary() object to dataframe
    D = tidy(ks)[1, 1]  # extracting distance statistic
    p.value = tidy(ks)[1, 2]  # extracting pvalue from the dataframe
    result = cbind(D, p.value)
    return(result)
  }
  require(raster); require(bfastSpatial)
  s <- stack(s1, s2)
  split <- nlayers(s)/2
  ks.result <- mc.calc(s, fun=fun.KolmogorovTest, mc.cores = 1)
  return(ks.result)
}

fun.KolmogorovTest <- function(v) {
  x <- v[1:split]
  y <- v[(split+1):(2*split)]
  
  if (all(is.na(x)) | all(is.na(y))) {
    D = NA; p.value = NA
    result = cbind(D, p.value)
    return(result)
  }
  ks = ks.test(x, y)
  require(broom)   # converting the summary() object to dataframe
  D = tidy(ks)[1, 1]
  p.value = tidy(ks)[1, 2]
  result = cbind(D, p.value)
  return(result)
}


fun.theilsenslope <- function(x) {
  if (all(is.na(x))) {
    B = NA; sl = NA; AIC = NA
    result = cbind(B, sl, AIC)
    return(cbind(result))
  }
  time = 1:length(x)
  lmfit = lm(x ~ time)
  require(broom)   # converting the summary() object to dataframe
  B = tidy(lmfit)[2, 2]  # thielSen slope estimates
  sl = tidy(lmfit)[2, 5]  # MannKendal significance
  AIC = AIC(lmfit)
  result = cbind(B, sl, AIC)
  return(result)
}
require(broom); require(mblm)

#thielsen slope based on the sgolay records
evi2_lists <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyEVI2-5km/sgolay"),
                         pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists[[files]])
stk.Data.evi<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
stk.Data.evi2<-dropLayer(stk.Data.evi, c(61:360))
theilsen_stk_result_sgolay <- calc(stk.Data.evi2, fun.theilsenslope)
writeRaster(theilsen_stk_result_sgolay, "F:/Results_Dec2015/ThielSenOutputs/golbal_theilsen_stk_result_sgolay.tif")
plot(theilsen_stk_result_sgolay[[2]])
#remove temp files for example older than 0.5 hours
removeTmpFiles(h=0.5)

#thielsen slope based on the whittaker records
evi2_lists2 <- list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyEVI2-5km/whittaker"),
                         pattern="*.tif", full.names=TRUE)
evi2_rasters2<-foreach(files=1:length(evi2_lists2), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists2[[files]])
stk.Data.evi2<-stack(evi2_rasters2, bands=NULL, native=FALSE, RAT=TRUE)
stk.Data.evi22<-dropLayer(stk.Data.evi2, c(61:360))
theilsen_stk_result_whit <- calc(stk.Data.evi22, fun.theilsenslope)
writeRaster(theilsen_stk_result_whit, "F:/Results_Dec2015/ThielSenOutputs/golbal_theilsen_stk_result_whittaker.tif")





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>......

#Create an image such that when MK.sl <= 0.01 it takes value 1 else 0 to "mask" significance
#theil-sen slope estimates

require(pracma)
## Not run:
# *** Sinosoid test function ***
ts <- sin(2*pi*(1:1000)/200)
t1 <- ts + rnorm(1000)/10
t2 <- savgol(t1, 51)
plot( 1:1000, t1, col = "grey")
lines(1:1000, ts, col = "blue")
lines(1:1000, t2, col = "red")
# t3 <- whittaker(t1, lambda = 1600)
# lines(1:1000, t3, col = "darkgreen", lwd = 2)


lm1 <- lm(Fertility ~ . , data = swiss)
AIC(lm1)
stopifnot(all.equal(AIC(lm1),
                    AIC(logLik(lm1))))
BIC(lm1)

lm2 <- update(lm1, . ~ . -Examination)
AIC(lm1, lm2)
BIC(lm1, lm2)
#-----------------------------------------------------------------------
#   Whittacker Smoothing
#   Paul Eilers, 2002 (Matlab) and Nicholas Lewin-Koh, 2004 (R/S)
#-----------------------------------------------------------------------
whittaker <- function(y, lambda, d = 2){
  #   Smoothing with a finite difference penalty
  #   y:      signal to be smoothed
  #   lambda: smoothing parameter (rough 50..1e4 smooth)
  #   d:      order of differences in penalty (generally 2)
  require(SparseM, warn.conflicts = FALSE)
  m <- length(y)
  E <- as(m, "matrix.diag.csr")
  class(E) <- "matrix.csr"
  Dmat <- diff(E, differences = d)
  B <- E + (lambda * t(Dmat) %*% Dmat)
  z <- solve(B, y)
  return(z)
}
## End(Not run)

AIC = AIC(t2)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Color pellates based on colorspace package
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(colorspace)
## convenience demo functions
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...) 

pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

## qualitative palette
wheel(rainbow_hcl(12))

## a few useful diverging HCL palettes
par(mar = rep(0, 4), mfrow = c(4, 1))
pal(diverge_hcl(7))
pal(diverge_hcl(7, h = c(246, 40), c = 96, l = c(65, 90)))
pal(diverge_hcl(7, h = c(130, 43), c = 100, l = c(70, 90)))
pal(diverge_hcl(7, h = c(180, 70), c = 70, l = c(90, 95)))
pal(diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95)))
pal(diverge_hcl(7, h = c(128, 330), c = 98, l = c(65, 90)))
pal(diverge_hcl(7, h = c(255, 330), l = c(40, 90)))
pal(diverge_hcl(7, c = 100, l = c(50, 90), power = 1))

## sequential palettes
pal(sequential_hcl(12))
pal(heat_hcl(12, h = c(0, -100), l = c(75, 40), c = c(40, 80), power = 1))
pal(terrain_hcl(12, c = c(65, 0), l = c(45, 95), power = c(1/3, 1.5)))
pal(heat_hcl(12, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))

## compare base and colorspace palettes
## (in color and desaturated)
par(mar = rep(0, 4), mfrow = c(2, 2))
## rainbow color wheel
wheel(rainbow_hcl(12))
wheel(rainbow(12))
wheel(desaturate(rainbow_hcl(12)))
wheel(desaturate(rainbow(12)))

## diverging red-blue colors
pal(diverge_hsv(7))
pal(diverge_hcl(7, c = 100, l = c(50, 90)))
pal(desaturate(diverge_hsv(7)))
pal(desaturate(diverge_hcl(7, c = 100, l = c(50, 90))))

## diverging cyan-magenta colors
pal(cm.colors(7))
pal(diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95)))
pal(desaturate(cm.colors(7)))
pal(desaturate(diverge_hcl(7, h = c(180, 330), c = 59, l = c(75, 95))))

## heat colors
pal(heat.colors(12))
pal(heat_hcl(12))
pal(desaturate(heat.colors(12)))
pal(desaturate(heat_hcl(12)))

## terrain colors
pal(terrain.colors(12))
pal(terrain_hcl(12))
pal(desaturate(terrain.colors(12)))
pal(desaturate(terrain_hcl(12)))

######################################
new.y = rnorm(720)
y.ts <- ts(new.y, frequency=24, start=c(1982,1), end=c(2011, 24))
mySTLdata <- stl(y.ts, s.window="periodic")



#Two sample Kolmogorov-Smornov Test
Kolmogorov.Smirnov.Test.Fun <- function(s1, s2) {
  ks_test<- function(v) {
    require(stats)
    obs <- v[1:split]
    sim <- v[(split+1):(2*split)]
    ks.est <- try(ks.test(sim=sim, obs=obs), silent=TRUE) 
    
    D.statistic=if(class(ks.est)=="try-error") NA 
    else unclass(round(ks.est$statistic["D"], digits = 4))
    D.statistic.sig=if(class(ks.est)=="try-error") NA 
    else unclass(round(ks.est$p.value, digits = 4))
    ks.result = cbind(D.statistic, D.statistic.sig)
    return(ks.result)
  }
  require(raster)
  s <- stack(s1, s2)
  split <- nlayers(s)/2
  ks.result <- calc(s, fun=ks_test)
  return(ks.result)
}



Theil_Sen_slope <- function(stk) {
    funTheilSen=function(v) {
      new.y <- v[1:size]
      
      require(stats)
      #seasonal decomposition of time series by Loess (into trend, season, remainder)
      y.ts <- ts(new.y, frequency=24, start=c(1982,1), end=c(2011, 24))
      mySTLdata <- stl(y.ts, s.window="periodic")
      
      #coersing the stl object into a dataframe
      mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
      y <- as.vector(mySTLdata.DF1[ ,"trend"])
      x = 1:size/size
      
      #The formula take all these things into account
      #compute slope with theilSen and sig with Mankendall
      require(rkt)
      fit.slope = try(rkt(date=x, y=y), silent=TRUE)
      
      theils.slope = if(class(fit.slope)=="try-error") NA 
      else unclass(round(fit.slope$B, digits = 4))
      theils.slope.sl = if(class(fit.slope)=="try-error") NA 
      else unclass(round(fit.slope$sl, digits = 4))
      
      TheilSen.result = cbind(theils.slope, theils.slope.sl)
      return(TheilSen.result)
      
}    
    s <- stack(stk)
    size <- nlayers(s)
    Final.result <- calc(s, fun=funTheilSen)
    return(Final.result)
  } 





