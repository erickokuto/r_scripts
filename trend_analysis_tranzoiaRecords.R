
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Data both real and simulated with gaps
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Real_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataObs"),
                              pattern="*.tif", full.names=TRUE)
Real_evi2_rasters<-foreach(files=1:length(Real_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(Real_evi2_lists[[files]])
Real_stk.Data.evi2<-stack(Real_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)

Sim_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataObs"),
                              pattern="*.tif", full.names=TRUE)
Sim_evi2_rasters<-foreach(files=1:length(Sim_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(Sim_evi2_lists[[files]])
Sim_stk.Data.evi2<-stack(Sim_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(HarmonicRegression)
require(foreach)
require(raster)
inla_evi2_lists <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataFitted"),
                            pattern="*.tif", full.names=TRUE)
inla_evi2_rasters<-foreach(files=1:length(inla_evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(inla_evi2_lists[[files]])
inla_stk.Data.evi2<-stack(inla_evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
spDTyn_evi2_lists1 <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/RealDataFitted"),
                              pattern="*.tif", full.names=TRUE)
spDTyn_evi2_rasters1<-foreach(files=1:length(spDTyn_evi2_lists1), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(spDTyn_evi2_lists1[[files]])
spDTyn_stk.Data.evi2Pred<-stack(spDTyn_evi2_rasters1, bands=NULL, native=FALSE, RAT=TRUE)


spDTyn_evi2_lists2 <- list.files(file.path("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/RealDataFitted"),
                                 pattern="*.tif", full.names=TRUE)
spDTyn_evi2_rasters2<-foreach(files=1:length(spDTyn_evi2_lists2), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(spDTyn_evi2_lists2[[files]])
spDTyn_stk.Data.evi2Obs<-stack(spDTyn_evi2_rasters2, bands=NULL, native=FALSE, RAT=TRUE)




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
