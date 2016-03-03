
###########################################################################
#       APPLICATION OF S-GOLAY FILTER
###########################################################################
require(raster);require(rgdal);require(signal);require(zoo);require(ptw);require(foreach); require(INLA); require(plyr)

evi2_lists <- list.files(file.path("/media/CLIM_DATA/CLIM_DATA/EVI2_Smoothing_Datasets/EVI2_Timeseries_Data"),
                         pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists[[files]])
stk.Data.evi2<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#        SMOOTHING USING SAVITZKY-GOLAY APPROACH
#
##############################################################################################

funsgolay<- function(stk) {
  fun.smoother.sgolay<- function(v) {
    x <- v[1:size]
    x.vector=as.vector(x)
    x.vector[is.na(x.vector)] <- mean(x.vector,
                                      na.rm=TRUE)
    begin.year=1982
    end.year=2011
    lowest=1
    freq=24
    if (all(is.na(x.vector))) {
      return(rep(NA, times = size))
    } else {
      evi2.ts2 = stats::ts(x.vector, 
                           start=c(begin.year,lowest),
                           end=c(end.year,freq),
                           frequency=freq)
      return(signal::sgolayfilt(evi2.ts2, 
                                p=1, n=3, ts=30))
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=fun.smoother.sgolay)
  return(result)
} 

evi2.filtered.sgolay <- funsgolay(stk=stk.Data.evi2)
evi2.filtered.sgolay2<-unstack(evi2.filtered.sgolay)

years<-rep(1982:2011,each=24)
length(years)
size<-720
days.15<-rep(c("15a", "15b"), times=size/2)
months<-rep(rep(c(1:12), each=2), times=size/24)

for (i in 1:length(evi2.filtered.sgolay2)) {
  writeRaster(evi2.filtered.sgolay2[[i]],
  filename=paste('/home/eric/Documents/EVI2_Smoothing_Datasets/smoothed_using_sgolay/', 
  'evi2', '_', years[i],'_',
  months[i], '_',
  days.15[i], '_', 
  'sgolay',
  pattern=".tif",
  sep=''))
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#        SMOOTHING USING WHITTAKER-HENDERSON APPROACH
#
##############################################################################################

funwhit<- function(stk) {
  fun.smoother.whit <- function(v) {
    x <- v[1:size]
    x.vector=as.vector(x)
    x.vector[is.na(x.vector)] <- mean(x.vector, 
                                      na.rm=TRUE)
    begin.year=1982
    end.year=2011
    lowest=1
    freq=24
    if (all(is.na(x.vector))) {
      return(rep(NA, times = size))
    } else {
      evi2.ts2 = stats::ts(x.vector, 
                           start=c(begin.year,lowest),
                           end=c(end.year,freq), frequency=freq)
      return(ptw::whit2(evi2.ts2, lambda = 1e5))
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s,
               fun=fun.smoother.whit)
  return(result)
} 

evi2.filtered.whit <- funwhit(stk.Data.evi2) 
evi2.filtered.whit2 <-unstack(evi2.filtered.whit)

years<-rep(1982:2011,each=24)
length(years)
size<-length(evi2.filtered.sgolay2)
days.15<-rep(c("15a", "15b"), times=size/2)
months<-rep(rep(c(1:12), each=2), times=size/24)

for (i in 1:length(evi2.filtered.whit2)) {
  writeRaster(evi2.filtered.whit2[[i]],
              filename=paste('/home/eric/Documents/EVI2_Smoothing_Datasets/smoothed_using_whittaker/', 
                                                        'evi2', '_',
                             years[i],'_',
                             months[i], '_',
                             days.15[i], '_', 
                             'whittaker', 
                             pattern=".tif",
                                sep=''))
}

require(raster)
plot(raster("F:\\CLIM_DATA\\EVI2_Smoothing_Datasets\\ProcessedData\\DataSPDESmoothed\\VIP15P4-A1982001-003-evi2-spde.tif"))
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#        SMOOTHING USING BAYESIAN-SPDE APPROACH
#
##############################################################################################
funspde <- function(stk) {
  fun.smoother.spde <- function(v) {
    y = v[1:size];
    y = as.vector(y);
    sigma0 = 1; kappa0 = 1e-3; tau0 = 1/(4*kappa0^3*sigma0^2)^0.5;
    no.months=size/2; 
    no.years = no.months/12
    knots.seasonal = seq(1, 12, by = 3);
    knots.trend = seq(1, no.years, 
                              length=8);
    year=rep(1:no.years, each=24);
    month=rep(rep(1:12, each=2), 
              times=no.years)
    mesh.seasonal = INLA::inla.mesh.1d(knots.seasonal, 
                                       interval=c(0, 12), 
                                       degree=2,
                                       boundary="cyclic"); 
    mesh.trend = INLA::inla.mesh.1d(knots.trend,
                                    interval=c(0, no.years), 
                                    degree=2, 
                                    boundary="free")
    spde.seasonal = INLA::inla.spde2.matern(mesh.seasonal,
                                            constr=TRUE, 
                                            B.tau = cbind(log(tau0), 1),
                                            B.kappa = cbind(log(kappa0), 0),
                                            theta.prior.prec = 1e-4); 
    spde.trend = INLA::inla.spde2.matern(mesh.trend, 
                                         constr=TRUE, 
                                         B.tau = cbind(log(tau0), 1),
                                         B.kappa = cbind(log(kappa0), 0),
                                         theta.prior.prec = 1e-4)
    A.season = INLA::inla.spde.make.A(mesh.seasonal,
                                      loc = month);
    A.trend = INLA::inla.spde.make.A(mesh.trend,
                                     loc = year); 
    index.year = INLA::inla.spde.make.index("year", 
                                            n.spde=spde.trend$n.spde)
    index.month = INLA::inla.spde.make.index("month",
                                             n.spde=spde.seasonal$n.spde); 
    intercept=rep(1, times=size)
    stk= INLA::inla.stack(data=list(y=y), 
                          A=list(A.trend, A.season, 1), 
                          effects=list(index.year, index.month, 
                                       intercept=intercept),
                          tag="est")
    data.stk = INLA::inla.stack.data(stk); 
    ind = inla.stack.index(stk,
                           tag='est')$data
    formula = y ~ 0 + intercept + f(year, 
                                    model=spde.trend) + f(month,
                                                          model=spde.seasonal)
    if (all(is.na(y))) {
      return(rep(NA, times=size))
    } else {
      result = INLA::inla(formula,
                          family="gaussian", 
                          data=data.stk,
                          quantiles=NULL, 
                          control.compute=list(cpo=FALSE, config=FALSE),
                          control.predictor=list(A=inla.stack.A(stk),
                                                 compute=TRUE),
                          control.inla = list(strategy = "gaussian"),
                          verbose=FALSE)
      xx = unclass(result)$summary.linear.pred[ind, "mean"]
      #xxx = unclass(result)$summary.linear.pred[ind, "sd"]
      return(xx)
    } 
  }
  s = stack(stk)
  size = nlayers(s)
  result = calc(s, fun=fun.smoother.spde)
  return(result)
} 

evi2.filtered.spde <- funspde(stk.Data.evi2) 
writeRaster(evi2.filtered.spde,
            "/home/eric/Documents/EVI2_Smoothing_Datasets/smoothed_using_spde/evi2.filtered.spde .tif")

################################################################################
#      function for trend analysis
#############################################################################
require(INLA)
TrendFun<- function(stk) {
  dynamic_model<- function(v) {
    x <- v[1:size]
    x.vector=as.vector(x)
    
    n = length(x.vector)
    nk = 180
    nb = 1
    X = matrix(runif(n*nb), n, nb) 
    y = x.vector
    harmonics = matrix(0, n, nk)
    for(k in 1:nk) {
      harmonics[, k] = sin(pi/n * k * 1:n)
    }
    idx = 1:n
    if (all(is.na(x.vector))) {
      return(NA)
    } else {
      formula = (y ~ -1 + X + f(idx, model="z",  Z=harmonics,
                                hyper = list(prec = list(initial = -4,
                                                         fixed=TRUE))))
      result = inla(formula,
                    data = list(y=y, idx=idx, 
                                harmonics = harmonics, X=X),
                    family = "gaussian",
                    control.predictor = list(compute=FALSE))
     return(result$summary.fixed[,"mean"])
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=dynamic_model)
  return(result)
} 

evi2.trends <- TrendFun(stk=stk.Data.evi2)
evi2.evi2.trends2<-unstack(evi2.trends)
##########################################################################################
df<-read.csv("C:/Users/eokuto/Desktop/Projects/Project2/testhormonic/testhormonicreg.csv")

n = dim(df)[1]
nk = 180
nb = 1
X = matrix(runif(n*nb), n, nb) 
y = df$x

harmonics = matrix(0, n, nk)
for(k in 1:nk) {
  harmonics[, k] = sin(pi/n * k * 1:n)
}

idx = 1:n
formula = (y ~ -1 + X + f(idx, model="z",  Z=harmonics,
                          hyper = list(prec = list(initial = -4,
                                                   fixed=TRUE))))
result = inla(formula,
              data = list(y=y, idx=idx, 
                          harmonics = harmonics, X=X),
              family = "gaussian",
              control.predictor = list(compute=TRUE))

#result$summary.random$idx
result$summary.fixed[,"mean"]    #estimate for beta

#############################################################
