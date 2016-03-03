
require(MODIS);require(raster);require(foreach)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Installing MODIS package
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#install.packages("MODIS", repos="http://R-Forge.R-project.org")
#install.packages('mapdata',dependencies=TRUE)
require(MODIS);require(raster);require(foreach); require(mapdata)
#MODISoptions()
#setRepositories() 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Importing raster images in .tif* format and developing a stack
#Monthly Records between 1981 and 2011
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
files = list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/DataFilteredEvi2"),pattern="*.tif", full.names=TRUE)
#files = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped"),pattern="*.tif", full.names=TRUE)
rasters.files = foreach(i=1:length(files), .combine=c, .verbose=FALSE) %do%
  raster(files[[i]])
stk.rasters = stack(rasters.files, bands=NULL, native=FALSE, RAT=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#checking the date information in an input file (ie. from MODIS filenames)
#and generates an output date information
#used for the creation of a regular intervalled result.
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
vi.files <- stack(files)
timeInfo <- orgTime(vi.files, nDays=15,
                    begin="1982001", end="2011350",
                    pillow=12, pos1=10, pos2=16, format="%Y%j")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#multi-core computing with'raster' functions
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(snow) # Might require snow package
beginCluster(type="SOCK",exclude="MODIS") # See: ?beginCluster

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Applying the filter and exporting yearly stacks to the specified directory
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
outDirPath = "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/Data_WH_RevisedDec2015"
whittaker.raster(stk.rasters,
                             w=NULL, t=NULL,
                             timeInfo=timeInfo,
                             lambda = 500,
                             nIter= 3,
                             outDirPath = outDirPath)
endCluster()

#s=stack("F:/CLIM_DATA/Kenya_evi2_Records/Adaptive_Smoothers_folder/NDVI_YearlyLambda5000_year1982.tif")


########function for optimization smoothing method
require(raster);require(foreach)
files = list.files(file.path("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/DataFilteredEvi2"),pattern="*.tif", full.names=TRUE)
rasters.files = foreach(i=1:length(files), .combine=c, .verbose=FALSE) %do%
  raster(files[[i]])
stk.rasters = stack(rasters.files, bands=NULL, native=FALSE, RAT=TRUE)
stk.rasters[stk.rasters >1] <- 1
stk.rasters[stk.rasters <1] <- -1
outDirPath = "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/Data_WH_RevisedDec2015"

require(MODIS)
optimization_function <- function(files, outDirPath, nDays="15days", lambda = 500, stk.rasters, overwrite=c(TRUE,FALSE)){
  require(MODIS);require(raster);require(foreach) 
  vi.files <- stack(files) #list.files object with pattern="*.tif"
  timeInfo <- orgTime(vi.files, nDays="15days",
                      begin="1982001", end="2011350",
                      pillow=12, pos1=10, pos2=16, format="%Y%j")
  whittaker.raster(stk.rasters,
                   w=NULL, t=NULL,
                   timeInfo=timeInfo,
                   lambda = 500,
                   nIter= 3,
                   outDirPath = outDirPath,
                   overwrite=FALSE)
  
}

res = optimization_function(files=files, 
                            outDirPath=outDirPath, 
                            nDays="15days", 
                            lambda = 500, 
                            stk.rasters=stk.rasters, 
                            overwrite = TRUE)
##############################################################################
# SPDE smoother revised
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
funspdeRevisedDec2015 <- function(stk) {
  fun.smoother.spde <- function(v) {
    require(INLA)
    y = v[1:size];
    y = as.vector(y);
    sigma0 = 1; kappa0 = 1e-3; tau0 = 1/(4*kappa0^3*sigma0^2)^0.5;
    no.months=size/2; 
    no.years = no.months/12
    n = size
    knots.seasonal = seq(1, 12, by = 3);
    knots.trend = seq(1, n,length=30);
    knots.cyclic = seq(1, no.years, length=8);
    idx=seq(1:n)
    year=rep(1:no.years, each=24);
    month=rep(rep(1:12, each=2), times=no.years)
    mesh.seasonal = inla.mesh.1d(knots.seasonal, 
                                       interval=c(0, 12), 
                                       degree=2,
                                       boundary="cyclic"); 
    mesh.trend = inla.mesh.1d(knots.trend,
                                    interval=c(0, n), 
                                    degree=2, 
                                    boundary="free");
    mesh.cyclic = inla.mesh.1d(knots.cyclic,
                                    interval=c(0, no.years), 
                                    degree=2, 
                                    boundary="cyclic")
    spde.seasonal = inla.spde2.matern(mesh.seasonal,
                                            constr=TRUE, 
                                            B.tau = cbind(log(tau0), 1),
                                            B.kappa = cbind(log(kappa0), 0),
                                            theta.prior.prec = 1e-4); 
    spde.trend = inla.spde2.matern(mesh.trend, 
                                         constr=TRUE, 
                                         B.tau = cbind(log(tau0), 1),
                                         B.kappa = cbind(log(kappa0), 0),
                                         theta.prior.prec = 1e-4)
    spde.cyclic = inla.spde2.matern(mesh.cyclic, 
                                         constr=TRUE, 
                                         B.tau = cbind(log(tau0), 1),
                                         B.kappa = cbind(log(kappa0), 0),
                                         theta.prior.prec = 1e-4)
    A.season = inla.spde.make.A(mesh.seasonal,  loc = month);
    A.trend = inla.spde.make.A(mesh.trend, loc = idx); 
    A.cyclic = inla.spde.make.A(mesh.cyclic, loc = year); 
    index.idx = inla.spde.make.index("idx",  n.spde=spde.trend$n.spde)
    index.year = inla.spde.make.index("year",  n.spde=spde.cyclic$n.spde)
    index.month = inla.spde.make.index("month", n.spde=spde.seasonal$n.spde); 
    intercept=rep(1, times=n)
    stk= inla.stack(data=list(y=y), 
                          A=list(A.trend, 
                                 A.season, 
                                 A.cyclic, 1), 
                          effects=list(index.idx,
                                       index.month, 
                                       index.year, 
                                       intercept=intercept),
                          tag="est")
    data.stk = inla.stack.data(stk); 
    ind = inla.stack.index(stk, tag='est')$data
    formula = y ~ 0 + intercept + 
      f(idx, model=spde.trend) + 
      f(month, model=spde.seasonal) +
      f(year, model=spde.cyclic)
    
    if (all(is.na(y))) {
      
      return(rep(NA, times=size))
      
    } else {
      
      result = inla(formula,
                          family="gaussian", 
                          data=data.stk,
                          quantiles=NULL, 
                          control.compute=list(cpo=FALSE, config=FALSE),
                          control.predictor=list(A=inla.stack.A(stk), compute=TRUE),
                          control.inla = list(strategy = "gaussian"),
                          verbose=TRUE)
      
      xx = unclass(result)$summary.linear.pred[ind, "mode"]
      
      return(xx)
    } 
  }
  s = stack(stk)
  size = nlayers(s)
  result = calc(s, fun=fun.smoother.spde)
  return(result)
} 

