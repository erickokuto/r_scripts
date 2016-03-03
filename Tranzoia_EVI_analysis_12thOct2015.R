
funrmse <- function(obs.stk, sim.stk) {
  rmse.fun <- function(v) {
    obs <- v[1:split]
    sim <- v[(split+1):(2*split)]
    rmse.est <- try(mse(sim=sim, obs=obs), silent=TRUE)   
    if (class(rmse.est)=="try-error") NA else unclass(rmse.est)
  }
  s <- stack(obs.stk, sim.stk)
  split <- nlayers(s)/2
  rmse.result<-calc(s, fun=rmse.fun)
  return(rmse.result)
} 

r2.function <- function(obs.stk, sim.stk) {
  r2<- function(v) {
    x <- v[1:split]
    y <- v[(split+1):(2*split)]
    xx=ts(x, start=1982, end=2011, frequency = 12)
    yy=ts(y, start=1982, end=2011, frequency = 12)
    idx <- seq(as.Date('1982-01-01'), as.Date('2011-12-31'), 'month')
    #Q <- zoo(xx, as.Date("1982-01-01") + 1:360)
    Q <- zoo(xx, idx)
    X <- zoo(yy, time(Q))
    r2.est <- try(nseStat(Q, X, complete.cases=TRUE, ref = ave(Q, months(time(Q)))))   
    if (class(r2.est)=="try-error") NA else unclass(r2.est)[1]
  }
  s <- stack(obs.stk, sim.stk)
  split <- nlayers(s)/2
  r2.result<-calc(s, fun=r2)
  return(r2.result)
} 


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Record without gaps both using INLA and spDTyn
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(foreach)
require(raster)
require(hydromad)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Really Data with gaps
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Real_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataObs"),
                              pattern="*.tif", full.names=TRUE)
Real_evi2_rasters<-foreach(files=1:length(Real_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(Real_evi2_lists[[files]])
Real_stk.Data.evi2<-stack(Real_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
plot(Real_stk.Data.evi2[[10]])

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Simulated Records with gaps
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Sim_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataObs"),
                             pattern="*.tif", full.names=TRUE)
Sim_evi2_rasters<-foreach(files=1:length(Sim_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(Sim_evi2_lists[[files]])
Sim_stk.Data.evi2<-stack(Sim_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Records gap fillied using INLA package
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
inla_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataFitted"),
                              pattern="*.tif", full.names=TRUE)
inla_evi2_rasters<-foreach(files=1:length(inla_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(inla_evi2_lists[[files]])
inla_stk.Data.evi2<-stack(inla_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
plot(inla_stk.Data.evi2[10])

#>>>>>>>>INLA with simulated data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
inla_evi2_lists22 <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/SimData/SimDataFitted"),
                              pattern="*.tif", full.names=TRUE)
inla_evi2_rasters22<-foreach(files=1:length(inla_evi2_lists22), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(inla_evi2_lists22[[files]])
inla_stk.Data.evi22<-stack(inla_evi2_rasters22, bands=NULL, native=FALSE, RAT=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
spDTyn_evi2_lists1 <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/RealDataFitted"),
                                 pattern="*.tif", full.names=TRUE)
spDTyn_evi2_rasters1<-foreach(files=1:length(spDTyn_evi2_lists1), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(spDTyn_evi2_lists1[[files]])
spDTyn_stk.Data.evi2Pred<-stack(spDTyn_evi2_rasters1, bands=NULL, native=FALSE, RAT=TRUE)


#>>>>>>>>spDTyn with simulated data >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
spDTyn_evi2_lists2 <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/SimDataFitted"),
                                 pattern="*.tif", full.names=TRUE)
spDTyn_evi2_rasters2<-foreach(files=1:length(spDTyn_evi2_lists2), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(spDTyn_evi2_lists2[[files]])
spDTyn_stk.Data.evi2Pred2<-stack(spDTyn_evi2_rasters2, bands=NULL, native=FALSE, RAT=TRUE)

#The analysis or E and RMSE
E_With_INLA_Fitted=r2.function(obs.stk=Real_stk.Data.evi2, sim.stk=inla_stk.Data.evi2)
writeRaster(E_With_INLA_Fitted, "F:/CLIM_DATA/tranzoia_county/other Results/Results/E_With_INLA_Fitted_RealData.tif")

E_With_spDTyn_Fitted=r2.function(obs.stk=Real_stk.Data.evi2, sim.stk=spDTyn_stk.Data.evi2Pred)
writeRaster(E_With_spDTyn_Fitted, "F:/CLIM_DATA/tranzoia_county/other Results/Results/E_With_spDTyn_Fitted_RealData.tif")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
E_With_INLA_Fitted_Sim=r2.function(obs.stk=Sim_stk.Data.evi2, sim.stk=inla_stk.Data.evi22)
writeRaster(E_With_INLA_Fitted_Sim, "F:/CLIM_DATA/tranzoia_county/other Results/Results/E_With_INLA_Fitted__SimData.tif")

E_With_spDTyn_Fitted_Sim=r2.function(obs.stk=Sim_stk.Data.evi2, sim.stk=spDTyn_stk.Data.evi2Pred2)
writeRaster(E_With_spDTyn_Fitted_Sim, "F:/CLIM_DATA/tranzoia_county/other Results/Results/E_With_spDTyn_Fitted_SimData.tif")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>








##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fitb <- harmonic.regression(yyy, inputtime=month)
mu = fitb$coeffs[1]
cos.amplitude = fitb$coeffs[2]
sin.amplitude = fitb$coeffs[3]
amplitude = fitb$pars[1]
phase = fitb$pars[2]
?harmonic.regression
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Using method two
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(season)
?cosinor
fit = cosinor(yyy ~ 1, date="month",
              type = 'monthly', data = data.frame(yyy, month),
              family = gaussian(), offsetmonth = FALSE)

res2$Coefficients
res2$coefficients[3] 
coef(fit)[2]
mu = coef(res2)["Intercept"]
cos.amplitude = coef(res2)["cosw"]
sin.amplitude = coef(res2)["sinw "]
amplitude = fitb$pars[1]
phase = fitb$pars[2]

data(CVD)
# model to fit an annual pattern to the monthly cardiovascular disease data
f = c(12)
tau = c(130,10)
res12 = nscosinor(data=CVD, response='adj', cycles=f, niters=500,
                  burnin=100, tau=tau)
summary(res12)
plot(res12)


## cardiovascular disease data (offset based on number of days in...
## ...the month scaled to an average month length)
data(CVD)
res = cosinor(cvd~1, date='month', data=CVD, type='monthly',
              family=poisson(), offsetmonth=TRUE)
summary(res)
seasrescheck(res$residuals) # check the residuals
# stillbirth data
data(stillbirth)
head(stillbirth)
res = cosinor(stillborn~1, date='dob', data=stillbirth,
              family=binomial(link='cloglog'))
summary(res)
plot(res)
# hourly indoor temperature data
data(indoor)
head(indoor)
res = cosinor(bedroom~1, date='datetime', type='hourly', data=indoor)
summary(res)
