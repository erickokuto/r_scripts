
DLM_kalman_filter <- function(stk, begin.year=1981, end.year=2011, freq=12) {
  
  kalman_DLM_Fun <- function(v) {
    
    y1=as.vector(v[1:size])
    
    if (all(is.na(y1))) {
      
      return(rep(NA, times = size))
      
    } else {
      require(dlm)
      require(zoo)
      
      y=na.spline(y1)
      y.ts <- as.ts(y, start=begin.year, end=end.year, frequency = freq)
      
      build.1<-function(theta){
        dlmModPoly(order=1,dV=exp(theta[1]),dW=exp(theta[2]))
      }
      
      fit.1<-dlmMLE(y.ts, parm=c(1, 1), build.1)
      updated.build <- build.1(fit.1$par)
      kalman.Filt.model<-dlmFilter(y.ts, updated.build)
      y.pred=as.numeric(kalman.Filt.model$m)
      y.pred.corrected=y.pred[-(1)]
      return(y.pred.corrected)
    } 
  }
  
  size = nlayers(stk)
  result = calc(stk, fun=kalman_DLM_Fun)
  return(result)
  
} 


#Savitzky Golay smoother
funsgolayFilter<- function(stk, begin.year=1982, end.year=2011, freq=12, year.ts=30, p.order=1, w.length=3) {
  fun.smoother.sgolay<- function(v) {
    x <- v[1:size]
    x.vector=as.vector(x)
    begin.year=begin.year
    end.year=end.year
    lowest=1
    freq=freq
    if (all(is.na(x.vector))) {
      return(rep(NA, times = size))
      
    } else {
      
      require(zoo)
      y.vector=na.spline(x.vector)
      y.ts = stats::ts(y.vector,
                           start=c(begin.year,lowest),
                           end=c(end.year,freq),
                           frequency=freq)
      require(signal)
      return(signal::sgolayfilt(y.ts,
                                p=p.order, n=w.length, ts=year.ts))
    }
  }
  require(raster)
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=fun.smoother.sgolay)
  return(result)
}

require(raster);require(foreach);require(zoo);require(dlm);require(signal)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Importing raster images in .tif* format and developing a stack
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
lists = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped"),pattern="*.tif", full.names=TRUE)
rasters.images = foreach(files=1:length(lists), .combine=c, .verbose=FALSE) %do%
  raster(lists[[files]])
stk = stack(rasters.images, bands=NULL, native=FALSE, RAT=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#smoothing Time series record with Kalman filtering algorithm:  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk_kalman_filter <- DLM_kalman_filter(stk=stk)
writeRaster(stk_kalman_filter, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/stk_kalman_filter.tif",
            bylayer=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#smoothing Time series record with Savitzky-Golay filtering algorithm:  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk_SGolay_filter <- funsgolayFilter(stk=stk)
writeRaster(stk_SGolay_filter, "F:/CLIM_DATA/Kenya_evi2_Records/Results/Temporal_DLM/SGolay_filter.tif",
            bylayer=TRUE)
