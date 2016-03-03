
#Theil-sen trend slope estimate
require(mblm); require(rkt)
set.seed(1234)
x <- 1:720+rnorm(720)
y <- x+rnorm(720)
time = 1:720/720
fit <- mblm(y ~ time)
fit$coefficients["time"]

require(Kendall) #use trend component extracted from an stl function
x<-1:10
MK1 = MannKendall(as.ts(x))
MK.sl = as.numeric(MK1$sl)


#Create an image such that when MK.sl <= 0.01 it takes value 1 else 0 to "mask" significance
#theil-sen slope estimates


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





