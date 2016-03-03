
rm(list=ls())
source("Phenology_Functions_Erick.R")

require(raster)
require(foreach)
require(INLA)
require(hydromad)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Phenology function using SPDE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
month<-rep(1:12)

    #Just one year
k <- 360      #Twelve months


plot(Q)
plot(X)

#Time varrying covariates
k=360
sin.amplitude <- as.vector(rep(sin(30 * (2*pi) * (1:k/k)))) ### just 1 cycles
cos.amplitude <- as.vector(rep(cos(30 * (2*pi) * (1:k/k)))) ### just 1 cycles
mu=rep(1, times=360)
idxmu=rep(1:360)
idxsin=rep(1:360)
idxcos=rep(1:360)

plot(sin.amplitude, type="l")
plot(cos.amplitude, type="l")
#idx <- seq(as.Date('1982-01-01'), as.Date('1982-12-31'), 'month')
sin <- sin.amplitude
cos <- cos.amplitude

# mu <- zoo(mu, time(sin))
# idxmu <- zoo(mu0, time(sin))
# idxsin <- zoo(mu1, time(sin))
# idxcos <- zoo(mu2, time(cos))

require(INLA)
#install.packages("wq", dependencies=TRUE)
tranzoiadata=read.csv("F:/CLIM_DATA/Kenya_evi2_Records/kenya.monthly.20KMunsmoothedEVI2.csv")
require(reshape)

y=melt(tranzoiadata[2,3:362], na.rm=FALSE)$value


res <- inla(y~mu+sin+cos+
              f(idxmu, mu, model="ar1",constr=TRUE) +
              f(idxsin, sin,  model="ar1", cyclic=FALSE, constr=TRUE)+
              f(idxcos, cos,  model="ar1", cyclic=FALSE, constr=TRUE)-1,
            family="gaussian",
            data=data.frame(y=y, mu=mu, idxmu=idxmu,
                            idxsin=idxsin, idxcos=idxcos, sin=sin, cos=cos),
            control.predictor=list(compute=TRUE),
            control.compute=list(config=FALSE),
            verbose=TRUE)
summary(res)

ypred=res$summary.fitted.values[, "mean"]



unclass (ifelse(res$summary.fix[3, 1] < 0,
                (atan(res$summary.fix[2, 1]/res$summary.fix[3, 1])+pi),
                atan(res$summary.fix[2, 1]/res$summary.fix[3, 1])))

atan((res$summary.fix[2, 1]/res$summary.fix[3, 1])+pi)




sampl=inla.posterior.sample(n=1000, result=fit)
mu1.pred=sampl$mu1
hh1.pred=sampl$hh1
hh2.pred=sampl$hh2

hyper=rbind(fit$summary.hyperpar[c(3, 5, 7), ])
yp<-zoo(fit$summary.fitted.values[1:12,1], idx)

plot(y, type="l")
lines(yp, col="blue")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
xyz1 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/WideObsMeans10KM.csv")
xyz11=xyz1[5, -(1:2)]
prices=melt(xyz11, na.rm=FALSE)$value

#prices <- nikkei225[1:14, 2]
head(prices)
length(prices)
print(length(prices)) #ts(y, start=1982, end=2011, frequency = 12)
prices.ts <- as.ts(prices, start=1982, end=2011, frequency = 12)

# class(prices.ts)
# plot(prices.ts,type="o",col="black", ylab="",main="Nikkei225")

#install.packages("dlm")
library(dlm)
#Define a function (a dlm model) function name: build.1()
build.1<-function(theta){
  dlmModPoly(order=1,dV=exp(theta[1]),dW=exp(theta[2]))
}

fit.1<-dlmMLE(prices.ts,parm=c(1,1),build.1)
str(fit.1)
print(fit.1)

#Re-define the function "build.1()" by inputting estimated 2 
#parameters to "build.1()"
mod.Nikkei.225 <- build.1(fit.1$par)
print(mod.Nikkei.225)

#Execute Kalman-Filter caluculation by "dlmFilter()" by inputting Nikkei225
#dateset and the defined model "mod.Nikkei.225
Nikkei225.Filt<-dlmFilter(prices.ts, mod.Nikkei.225)
print(Nikkei225.Filt$m)
YPRED=as.numeric(Nikkei225.Filt$m)
YPRED2=YPRED[-(1)]

?dlmFilter
plot(prices.ts,type="o",col="black",ylab="",main="Nikkei225 Filtering")
lines(dropFirst(Nikkei225.Filt$m),col="red",lwd=2)

plot(prices.ts,type="o",col="black",ylab="",main="Nikkei225 Filtering & Smoothing")
lines(dropFirst(Nikkei225.Filt$m),col="red",lwd=2)
lines(dropFirst(Nikkei225.Smooth$s),col="blue",lwd=2)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#R2BayesX
require(R2BayesX)
## estimate model
b1 <- bayesx(y ~ sx(x) + z + w, data = dat)
## extract fitted values
fit <- fitted(b1)

require(R2BayesX)
require(BayesX)
require(zoo)
yy=na.spline(y)
res2 = bayesx(yy~sin1+cos1+
                 sx(idxmu, bs = "rw1")+
                 sx(idxsin, by=sin1, bs = "rw1")+
                 sx(idxcos, by=cos1,   bs = "rw1"),
            family="gaussian",
            data=data.frame(y=yy, idxmu=idxmu,
                            idxsin=idxsin, idxcos=idxcos, 
                            sin1=sin, cos1=cos),
            verbose=TRUE)

summary(res2)

idxmu=idxmu
idxsin=idxsin
idxcos=idxcos
sin1=sin
cos1=cos
nd2=data.frame(idxmu,
              idxsin, idxcos, 
              sin1, cos1)


## prediction model from refitting with weights
nd3 <- unclass(round(predict(res2, type = "response"), digits = 4))
 
length(nd3)
cbind(yy, y)
plot(nd3[1:12], type="l")


require(R2BayesX)
#Non DLM with bayesX
res3 = bayesx(y~sin1+cos1+
                sx(idx, bs = "rw1"),
              family="gaussian",
              data=data.frame(y=y, idx=idxmu, 
                              sin1=sin, cos1=cos),
              verbose=TRUE)

summary(res3)
## extract fitted values
fit2 <- fitted(res3)
plot(fit2[1:12], type="l")
lines(y[1:12], col="blue")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#method two
require(wq)
phe=phenoPhase(y, mon.range = c(1, 12))
summary(phe)

require(HarmonicRegression)
fitb <- harmonic.regression(y, inputtime=1:12)
mu = fitb$coeffs[1]
cos.amplitude = fitb$coeffs[2]
sin.amplitude = fitb$coeffs[3]
amplitude = fitb$pars[1]
phase = fitb$pars[2]


require(season)
fit = cosinor(y ~ 1, date="month",
              type = 'monthly', data = data.frame(yobs, month=1:12),
              family = gaussian(), offsetmonth = FALSE)
fit

require(wq)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
tranzoiadata=read.csv("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/WideObsMeans10KM.csv")
tranzoiadata2=tranzoiadata[,3:ncol(tranzoiadata)]
require(reshape)
obs.percent=apply(tranzoiadata2, 1,  obs)
obs.result=cbind(x=tranzoiadata[,1], y=tranzoiadata[,2], obs.percent=obs.percent)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


obs <- 1:10
sim <- 1:10
NSE(sim, obs)

obs <- 1:10
sim <- 2:11
NSE(sim, obs)

##################
# Loading daily streamflows of the Ega River (Spain), from 1961 to 1970
data(EgaEnEstellaQts)
obs <- EgaEnEstellaQts
x=rnorm(100)
xx=x+rnorm(100)
NSE(sim=x, obs=xx)

# Generating a simulated daily time series, initially equal to the observed series
sim <- obs 

# Computing the 'NSE' for the "best" (unattainable) case
gof(sim=x, obs=xx)[17]

# Randomly changing the first 2000 elements of 'sim', by using a normal distribution 
# with mean 10 and standard deviation equal to 1 (default of 'rnorm').
sim[1:2000] <- obs[1:2000] + rnorm(2000, mean=10)

# Computing the new 'NSE'
NSE(sim=sim, obs=obs)
