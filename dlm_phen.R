##################################################################################################
#
# script with R  code to implement all the examples at the paper entitled
# "Direct Fitting of dynamic models using integrated nested Laplace approximations - INLA"
# 
# We highly recommend to read the paper before running the examples.
#
# This is not intended to be an INLA tutorial. For that purpose, readers should first access the 
# introductory material at <http://www.r-inla.org/>
#
# Authors: Ramiro Ruiz C?rdenas <ramiro@est.ufmg.br> ; Elias Teixeira Krainski <eliaskr.ufpr.br>
#
# last update: 2011-10-23
##################################################################################################
# an alternative way to fit the same model using and RW1 process
# --------------------------------------------------------------
require(raster)
require(foreach)
require(INLA)
DRCevi2_lists <- list.files(file.path("F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/EVI2_Records"),
                            pattern="*.tif", full.names=TRUE)
DRCevi2_rasters<-foreach(files=1:length(DRCevi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(DRCevi2_lists[[files]])
DRC_stk.Data.evi2<-stack(DRCevi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
nlayers(DRC_stk.Data.evi2)

#############################################################################################
# Nonlinear regression curve fitting in R:
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
install.packages("minpack.lm")
require(minpack.lm)
nlsLM(responseY~MODEL, start=list(starting values for model parameters))

#Akaike’s Information Criterion in R to determine best model:
AIC(nlsLM(responseY~MODEL1,start=list(starting values)))
AIC(nlsLM(responseY~MODEL2, start=list(starting values)))
AIC(nlsLM(responseY~MODEL3,start=list(starting values)))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Test code for triagonometric functions for DLM
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
harmonic.test <- read.csv("G:/Rdirectory/Phenology_metrics/hormonic2.csv")
attach(harmonic.test)
require(dlm)
require(zoo)
df=data.frame(y=brz.veg.index[1:720], indx=rep(1:360, each=2))
new.y <- as.vector(tapply(df$y, df$indx, mean))
y.ts <- ts(new.y, frequency=12, start=c(1982,1), end=c(2011,12))
mySTLdata <- stl(y.ts, s.window="periodic")

#coersing and stl object to dataframe
mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
season.vector1 <- mySTLdata.DF1[,"seasonal"]
plot(season.vector1, type="l")
#if you nead the dates
library(xts)
mySTLdata.DF2<-as.data.frame(as.xts(mySTLdata$time.series))
season.vector2 <- mySTLdata.DF2[,"seasonal"]

par(mfrow=c(1,1))
plot(season.vector1[1:12], type="l")
plot(season.vector2, type="l")

require(HarmonicRegression)
?harmonic.regression 
fit = harmonic.regression(season.vector1[25:36], inputtime=c(1:12), Tau = 12, normalize = FALSE) 
fit$ci
fit$pars
fit$means










#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(minpack.lm)
?nls.lm


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
build.1<-function(theta){
  dlmModPoly(order=1,dV=exp(theta[1]),dW=exp(theta[2]))
}

?dlmModTrig

init=c(1,1)
fit.1<-dlmMLE(y.ts, parm=init, build.1)
updated.build <- build.1(fit.1$par)
kalman.Filt.model<-dlmFilter(y.ts, updated.build)
#kalman.Filt.model<-dlmBSample(y.ts, updated.build)
y.pred=as.numeric(kalman.Filt.model$m)
y.pred.corrected=y.pred[-(1)]
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Harmonic Regression
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## Not run:
require(dlmodeler)
# generate some data
N <- 365*5
t <- c(1:N,rep(NA,365))
a <- rnorm(N+365,0,.5)
y <- pi + cos(2*pi*t/365.25) + .25*sin(2*pi*t/365.25*3) +
  exp(1)*a + rnorm(N+365,0,.5)

# build a model for this data
m <- dlmodeler.build.polynomial(0,sigmaH=.5,name='level') +
  dlmodeler.build.dseasonal(7,sigmaH=0,name='week')
dlmodeler.build.tseasonal(365.25,3,sigmaH=0,name='year')
dlmodeler.build.regression(a,sigmaH=0,name='reg')
m$name <- 'mymodel'
system.time(f <- dlmodeler.filter(y, m, raw.result=TRUE))

# extract all the components
?dlmodeler.extract
m.state.mean <- dlmodeler.extract(f,m,type="state",
                                  value="mean")

m.state.cov <- dlmodeler.extract(f,m,type="state",
                                 value="covariance")

m.obs.mean <- dlmodeler.extract(f,m,type="observation",
                                value="mean")

m.obs.cov <- dlmodeler.extract(f,m,type="observation",
                               value="covariance")

m.obs.int <- dlmodeler.extract(f,m,type="observation",
                               value="interval",prob=.99)
par(mfrow=c(2,1))

# show the one step ahead forecasts & 99% prediction intervals
plot(y,xlim=c(N-10,N+30))
lines(m.obs.int$mymodel$upper[1,],col='light grey')
lines(m.obs.int$mymodel$lower[1,],col='light grey')
lines(m.obs.int$mymodel$mean[1,],col=2)

# see to which values the filter has converged:
m.state.mean$level[,N] # should be close to pi
mean(abs(m.state.mean$week[,N])) # should be close to 0
m.state.mean$year[1,N] # should be close to 1
m.state.mean$year[6,N] # should be close to .25
m.state.mean$reg[,N] # should be close to e
# show the filtered level+year components
plot(m.obs.mean$level[1,]+m.obs.mean$year[1,],
     type='l',ylim=c(pi-2,pi+2),col='light green',
     ylab="smoothed & filtered level+year")
system.time(s <- dlmodeler.smooth(f,m))

# show the smoothed level+year components
s.obs.mean <- dlmodeler.extract(s,m,type="observation",
                                value="mean")
lines(s.obs.mean$level[1,]+s.obs.mean$year[1,],type='l',
      ylim=c(pi-2,pi+2),col='dark green')
## End(Not run)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Harmonic Regression with Harmonics
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
t <- c(1:N,rep(NA,365))
a <- rnorm(N+365,0,.5)
y <- pi + cos(2*pi*t/365.25) + .25*sin(2*pi*t/365.25*3) +
  exp(1)*a + rnorm(N+365,0,.5)

# build a model for this data
m <- dlmodeler.build.polynomial(0,sigmaH=.5,name='level') +
  dlmodeler.build.dseasonal(7,sigmaH=0,name='week')
dlmodeler.build.tseasonal(365.25,3,sigmaH=0,name='year')
dlmodeler.build.regression(a,sigmaH=0,name='reg')
m$name <- 'mymodel'
system.time(f <- dlmodeler.filter(y, m, raw.result=TRUE))

# extract all the components
m.state.mean <- dlmodeler.extract(f,m,type="state",
                                  value="mean")
m.state.cov <- dlmodeler.extract(f,m,type="state",
                                 value="covariance")
m.obs.mean <- dlmodeler.extract(f,m,type="observation",
                                value="mean")
m.obs.cov <- dlmodeler.extract(f,m,type="observation",
                               value="covariance")
m.obs.int <- dlmodeler.extract(f,m,type="observation",
                               value="interval",prob=.99)
par(mfrow=c(2,1))

# show the one step ahead forecasts & 99% prediction intervals
plot(y,xlim=c(N-10,N+30))
lines(m.obs.int$mymodel$upper[1,],col='light grey')
lines(m.obs.int$mymodel$lower[1,],col='light grey')
lines(m.obs.int$mymodel$mean[1,],col=2)

# see to which values the filter has converged:
m.state.mean$level[,N] # should be close to pi
mean(abs(m.state.mean$week[,N])) # should be close to 0
m.state.mean$year[1,N] # should be close to 1
m.state.mean$year[6,N] # should be close to .25
m.state.mean$reg[,N] # should be close to e

# show the filtered level+year components
plot(m.obs.mean$level[1,]+m.obs.mean$year[1,],
     type='l',ylim=c(pi-2,pi+2),col='light green',
     ylab="smoothed & filtered level+year")
system.time(s <- dlmodeler.smooth(f,m))

# show the smoothed level+year components
s.obs.mean <- dlmodeler.extract(s,m,type="observation",
                                value="mean")
lines(s.obs.mean$level[1,]+s.obs.mean$year[1,],type='l',
      ylim=c(pi-2,pi+2),col='dark green')
## End(Not run)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>