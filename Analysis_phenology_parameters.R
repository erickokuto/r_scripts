

#Trend analysis
require(rasterVis)
vip_raster_trend=raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

require(rworldmap)
data("countriesCoarse")

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols <- colorRampPalette(brewer.pal(11, "BrBG"))

## create plot
spplot(vip_raster_trend, col.regions = cols(100), scales = list(draw = TRUE), 
       sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"), 
       at = seq(-.6, .6, .1))



require(raster); require(bfastSpatial); require(snow);
require(rworldmap); require(rasterVis);
require(MODIS);require(foreach);  require(RColorBrewer)
require(sp); require(maps); require(maptools); require(mapdata);
require(latticeExtra); require(fields)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Trend Analysis using Mann-Kandall
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## obtaining shapefile for Africa
africa.shp=readShapePoly("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Africa/africa_shapefile/whole_africa.shp")


raster.MannKandall <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

## cropping africa region: returns either raster or a stack depending on the input
raster.MannKandall2 <- crop(raster.MannKandall, extent(africa.shp))

sp.MannKandall <- as(raster.MannKandall2, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries.MannKandall <- map('worldHires', fill=TRUE,plot=F)
IDs.MannKandall <- sapply(strsplit(boundaries.MannKandall$names, ":"), function(x) x[1])
bPols.MannKandall <- map2SpatialPolygons(boundaries.MannKandall, IDs=IDs.MannKandall,
                             proj4string=CRS(projection(raster.MannKandall2)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols.MannKandall <- colorRampPalette(brewer.pal(11, "BrBG"))
at.MannKandall = seq(-.6, .6, .1)
spplot(sp.MannKandall,col.regions=cols.MannKandall(100), scales = list(draw = TRUE), at = at.MannKandall,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols.MannKandall))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# beta0: annual mean
#>>>>>>>>>>>>>>>>>>>>>
raster.phen.betas0 <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp0 <- as(raster.phen.betas0, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries0 <- map('worldHires', fill=TRUE,plot=F)
IDs0 <- sapply(strsplit(boundaries0$names, ":"), function(x) x[1])
bPols0 <- map2SpatialPolygons(boundaries0, IDs=IDs0,
                             proj4string=CRS(projection(raster.phen.betas0)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols0 <- colorRampPalette(brewer.pal(11, "BrBG"))
at0 = seq(-.6, .6, .1)
spplot(sp0,col.regions=cols0(100), scales = list(draw = TRUE), at = at0,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols0))

#>>>>>>>>>>>>>>>>>>>>>>>
#beta1: sine amplitude
#>>>>>>>>>>>>>>>>>>>>>>>>>>>
raster.phen.betas1 <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp1 <- as(raster.phen.betas1, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries1 <- map('worldHires', fill=TRUE,plot=F)
IDs1 <- sapply(strsplit(boundaries1$names, ":"), function(x) x[1])
bPols1 <- map2SpatialPolygons(boundaries1, IDs=IDs1,
                              proj4string=CRS(projection(raster.phen.betas1)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols1 <- colorRampPalette(brewer.pal(11, "BrBG"))
at1 = seq(-.6, .6, .1)
spplot(sp1,col.regions=cols1(100), scales = list(draw = TRUE), at = at1,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols1))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>
#beta2: cos amplitude
#>>>>>>>>>>>>>>>>>>>>>>>>>>
raster.phen.betas2 <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp2 <- as(raster.phen.betas2, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries2 <- map('worldHires', fill=TRUE,plot=F)
IDs2 <- sapply(strsplit(boundaries2$names, ":"), function(x) x[1])
bPols2 <- map2SpatialPolygons(boundaries2, IDs=IDs2,
                              proj4string=CRS(projection(raster.phen.betas2)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols2 <- colorRampPalette(brewer.pal(11, "BrBG"))
at2 = seq(-.6, .6, .1)
spplot(sp2,col.regions=cols2(100), scales = list(draw = TRUE), at = at2,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols2))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#autocorrelation rho parameters
# beta0: annual mean
#>>>>>>>>>>>>>>>>>>>>>
raster.phen.betas0.rho <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp0.rho <- as(raster.phen.betas0.rho, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries0.rho <- map('worldHires', fill=TRUE,plot=F)
IDs0.rho <- sapply(strsplit(boundaries0.rho$names, ":"), function(x) x[1])
bPols0.rho <- map2SpatialPolygons(boundaries0.rho, IDs=IDs0.rho,
                              proj4string=CRS(projection(raster.phen.betas0.rho)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols0.rho <- colorRampPalette(brewer.pal(11, "BrBG"))
at0.rho = seq(-.6, .6, .1)
spplot(sp0.rho,col.regions=cols0.rho(100), scales = list(draw = TRUE), at = at0.rho,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols0.rho))

#>>>>>>>>>>>>>>>>>>>>>>>
#beta1: sine amplitude
#>>>>>>>>>>>>>>>>>>>>>>>>>>>
raster.phen.betas1.rho <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp1.rho <- as(raster.phen.betas1.rho, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries1.rho <- map('worldHires', fill=TRUE,plot=F)
IDs1.rho <- sapply(strsplit(boundaries1.rho$names, ":"), function(x) x[1])
bPols1.rho <- map2SpatialPolygons(boundaries1.rho, IDs=IDs1.rho,
                              proj4string=CRS(projection(raster.phen.betas1.rho)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols1.rho <- colorRampPalette(brewer.pal(11, "BrBG"))
at1.rho = seq(-.6, .6, .1)
spplot(sp1.rho,col.regions=cols1.rho(100), scales = list(draw = TRUE), at = at1.rho,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols1.rho))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>
#beta2: cos amplitude
#>>>>>>>>>>>>>>>>>>>>>>>>>>
raster.phen.betas2.rho <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp2.rho <- as(raster.phen.betas2.rho, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries2.rho <- map('worldHires', fill=TRUE,plot=F)
IDs2.rho <- sapply(strsplit(boundaries2.rho$names, ":"), function(x) x[1])
bPols2.rho <- map2SpatialPolygons(boundaries2.rho, IDs=IDs2.rho,
                              proj4string=CRS(projection(raster.phen.betas2.rho)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols2.rho <- colorRampPalette(brewer.pal(11, "BrBG"))
at2.rho = seq(-.6, .6, .1)
spplot(sp2.rho,col.regions=cols2.rho(100), scales = list(draw = TRUE), at = at2.rho,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols2.rho))

#>>>>>>>>>>>>>>>>>>>>>>>
#    Annual Phase
#>>>>>>>>>>>>>>>>>>>>>>>>>>>
raster.Phase <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp.Phase <- as(raster.Phase, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries.Phase <- map('worldHires', fill=TRUE,plot=F)
IDs.Phase <- sapply(strsplit(boundaries.Phase$names, ":"), function(x) x[1])
bPols.Phase <- map2SpatialPolygons(boundaries.Phase, IDs=IDs.Phase,
                             proj4string=CRS(projection(raster.Phase)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols.Phase <- colorRampPalette(brewer.pal(11, "BrBG"))
at.Phase = seq(-.6, .6, .1)
spplot(sp.Phase,col.regions=cols.Phase(100), scales = list(draw = TRUE), at = at.Phase,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols.Phase))


#>>>>>>>>>>>>>>>>>>>>>>>
#    Annual Amplitude
#>>>>>>>>>>>>>>>>>>>>>>>>>>>
raster.Amplitude <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp.Amplitude <- as(raster.Amplitude, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries.Amplitude <- map('worldHires', fill=TRUE,plot=F)
IDs.Amplitude <- sapply(strsplit(boundaries.Amplitude$names, ":"), function(x) x[1])
bPols.Amplitude <- map2SpatialPolygons(boundaries.Amplitude, IDs=IDs.Amplitude,
                                   proj4string=CRS(projection(raster.Amplitude)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols.Amplitude <- colorRampPalette(brewer.pal(11, "BrBG"))
at.Amplitude = seq(-.6, .6, .1)
spplot(sp.Amplitude,col.regions=cols.Amplitude(100), scales = list(draw = TRUE), at = at.Amplitude,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols.Amplitude))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#CPO and Logarithmic Score
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
raster.logscore <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp.logscore <- as(raster.logscore, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries.logscore <- map('worldHires', fill=TRUE,plot=F)
IDs.logscore <- sapply(strsplit(boundaries.logscore$names, ":"), function(x) x[1])
bPols.logscore <- map2SpatialPolygons(boundaries.logscore, IDs=IDs.logscore,
                                       proj4string=CRS(projection(raster.logscore)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols.logscore <- colorRampPalette(brewer.pal(11, "BrBG"))
at.logscore = seq(-.6, .6, .1)
spplot(sp.logscore,col.regions=cols.logscore(100), scales = list(draw = TRUE), at = at.logscore,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols.logscore))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Deviance Information Criterion (DIC)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
raster.dic <- raster("F:/CLIM_DATA/global/viptrend/vip_mk001_8213.tif")

sp.dic <- as(raster.dic, 'SpatialGridDataFrame')
trellis.par.set(sp.theme())
boundaries.dic <- map('worldHires', fill=TRUE,plot=F)
IDs.dic <- sapply(strsplit(boundaries.dic$names, ":"), function(x) x[1])
bPols.dic <- map2SpatialPolygons(boundaries.dic, IDs=IDs.dic,
                                       proj4string=CRS(projection(raster.dic)))

## colors, see http://colorbrewer2.org/
require(RColorBrewer)
cols.dic <- colorRampPalette(brewer.pal(11, "BrBG"))
at.dic = seq(-.6, .6, .1)
spplot(sp.dic,col.regions=cols.dic(100), scales = list(draw = TRUE), at = at.dic,
       par.settings=list(fontsize=list(text=20))) + layer(sp.polygons(bPols.dic))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>










## colors, see http://colorbrewer2.org/
# require(RColorBrewer)
# cols <- colorRampPalette(brewer.pal(11, "BrBG"))
#
# ## create plot
# spplot(raster.trend.mk, col.regions = cols(100), scales = list(draw = TRUE), 
#        sp.layout = list("sp.polygons", countriesCoarse, col = "grey65"), 
#        at = seq(-.6, .6, .1))
# 
# rasterVis::levelplot(raster.africa,col.regions=tim.colors(1001)) +
#   layer(sp.polygons(bPols))











#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Test spde function
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
y <- mySTLdata.DF1[,"seasonal"]

k = 360; nyears=30; npars = 11

#Time varrying covariates
sin.amplitude <- as.vector(rep(sin(30 * (2*pi) * (1:k/k)))) ##30 cycles
cos.amplitude <- as.vector(rep(cos(30 * (2*pi) * (1:k/k)))) 

length(sin.amplitude)
plot(sin.amplitude, type="l")
plot(cos.amplitude, type="l")

require(INLA)

#developing the spde model functionality
sigma0 = 1; kappa0 = 1e-3; tau0 = 1/(4*kappa0^3*sigma0^2)^0.5;
knots = seq(1, 360, by = 12); 
month=rep(rep(1:12), times=nyears)

#creating 1D mesh for temporal spde
mesh = inla.mesh.1d(knots, 
                    interval=c(0, 360), 
                    degree=2,
                    boundary="cyclic");

#With mesh, we define the SPDE model using the function inla.spde2.matern()
spde = inla.spde2.matern(mesh,
                          constr=TRUE, 
                          B.tau = cbind(log(tau0), 1),
                          B.kappa = cbind(log(kappa0), 0),
                          theta.prior.prec = 1e-4);

#setting of index for each call of the f() function
i0 = inla.spde.make.index("i0", n.spde=spde$n.spde, n.group=nyears);
i1 = inla.spde.make.index("i1", n.spde=spde$n.spde, n.group=nyears);
i2 = inla.spde.make.index("i2", n.spde=spde$n.spde, n.group=nyears);


#time projector matrix A is defined as follows
A0 <- inla.spde.make.A(mesh, loc = month, group=rep(1:nyears, each=12))
A1 <- inla.spde.make.A(mesh, loc = month, group=rep(1:nyears, each=12), weights=sin.amplitude)
A2 <- inla.spde.make.A(mesh, loc = month, group=rep(1:nyears, each=12), weights=cos.amplitude)

#putting all these together using stack functionality, we have 
stk <- inla.stack(data=list(y=as.vector(y)), tag='y',
                  A=list(A0, A1, A2, 1),
                  effects=list(i0, i1, i2,
                               data.frame(mu1=1, 
                                          sin.amplitude=sin.amplitude,
                                          cos.amplitude=cos.amplitude)))

#The formula take all these things into account
formula <- y ~ 0 + mu1 + sin.amplitude + cos.amplitude + ### to fit mu_beta
  f(i0, model=spde, group=i0.group, control.group=list(model='rw1')) +
  f(i1, model=spde, group=i1.group, control.group=list(model='ar1')) +
  f(i2, model=spde, group=i2.group, control.group=list(model='ar1'))


#inla call: 
result = try(inla(formula,
                    family="gaussian", 
                    data=inla.stack.data(stk), 
                    control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
                    control.predictor=list(A=inla.stack.A(stk), compute=TRUE),
                    control.inla = list(strategy = "gaussian"),
                    verbose=TRUE), silent=TRUE)

#The estimated phenology parameter posterior mean estimates
beta0=if(class(result)=="try-error") NA else unclass(result$summary.fix[1,1])
sin_beta1=if(class(result)=="try-error") NA else unclass(result$summary.fix[2,1])
cos_beta2=if(class(result)=="try-error") NA else unclass(result$summary.fix[3,1])
Amplitude=if(class(result)=="try-error") NA else unclass (sqrt(sin_beta1^2 + cos_beta2^2))
Phase=if(class(result)=="try-error") NA else unclass (asin(sin_beta1/Amplitude))

#The estimated phenology parameter posterior sd estimates
beta0.sd=if(class(result)=="try-error") NA else unclass(result$summary.fix[1,2])
sin_beta1.sd=if(class(result)=="try-error") NA else unclass(result$summary.fix[2,2])
cos_beta2.sd=if(class(result)=="try-error") NA else unclass(result$summary.fix[3,2])

#Model validation parameter estimates
dic=if(class(result)=="try-error") NA else unclass (result$dic$dic)
waic=if(class(result)=="try-error") NA else unclass (result$waic$waic)
logCPO=if(class(result)=="try-error") NA else unclass (sum(-log(result$cpo$cpo), na.rm=TRUE)) 

#transformed spde parameters
post.se = if(class(result)=="try-error") NA else unclass(inla.tmarginal(function(x) sqrt(1/x),
                                                                    result$marginals.hy[[1]]))
tau_field.x = if(class(post.se)=="try-error") NA else unclass(inla.emarginal(function(x) x, post.se))

result.i0.x <- if(class(result)=="try-error") NA else unclass(inla.spde2.result(result, 'i0', spde, do.transf=TRUE))
tau.i0.x <- if(class(result)=="try-error") NA else unclass(sqrt(inla.emarginal(function(x) x, result.i0.x$marginals.variance.nominal[[1]])))

result.i1.x <- if(class(result)=="try-error") NA else unclass(inla.spde2.result(result, 'i1', spde, do.transf=TRUE))
tau.i1.x <- if(class(result)=="try-error") NA else unclass(sqrt(inla.emarginal(function(x) x, result.i1.x$marginals.variance.nominal[[1]])))

result.i2.x <- if(class(result)=="try-error") NA else unclass(inla.spde2.result(result, 'i2', spde, do.transf=TRUE))
tau.i2.x <- if(class(result)=="try-error") NA else unclass(sqrt(inla.emarginal(function(x) x, result.i2.x$marginals.variance.nominal[[1]])))

#non transformed spde parameters (theta1) posterior mean
theta1.i0.mean=if(class(result)=="try-error") NA else unclass(result$summary.hy[2,1])
theta1.i1.mean=if(class(result)=="try-error") NA else unclass(result$summary.hy[3,1])
theta1.i2.mean=if(class(result)=="try-error") NA else unclass(result$summary.hy[5,1])

#(theta1) posterior sd
theta1.i0.sd=if(class(result)=="try-error") NA else unclass(result$summary.hy[2,2])
theta1.i1.sd=if(class(result)=="try-error") NA else unclass(result$summary.hy[3,2])
theta1.i2.sd=if(class(result)=="try-error") NA else unclass(result$summary.hy[5,2])

#Temporal autocorrelation posterior mean
rho.beta1=if(class(result)=="try-error") NA else unclass(result$summary.hy[4,1])
rho.beta2=if(class(result)=="try-error") NA else unclass(result$summary.hy[6,1])

#Temporal autocorrelation posterior sd
rho.beta1.sd=if(class(result)=="try-error") NA else unclass(result$summary.hy[4,2])
rho.beta2.sd=if(class(result)=="try-error") NA else unclass(result$summary.hy[6,2])

#Organizing the outputs in matrix for raster stacking  
results.matrix=cbind(beta0=beta0, 
                     sin_beta1=sin_beta1, 
                     cos_beta2=cos_beta2,
                     beta0.sd=beta0.sd,
                     sin_beta1.sd=sin_beta1.sd,
                     cos_beta2.sd=cos_beta2.sd,
                     Amplitude=Amplitude,
                     Phase=Phase,
                     rho.beta1=rho.beta1,
                     rho.beta2=rho.beta2,
                     rho.beta1.sd=rho.beta1.sd,
                     rho.beta2.sd=rho.beta2.sd,
                     dic=dic, waic=waic, logCPO=logCPO, 
                     tau_field.x=tau_field.x,
                     tau.i0.x=tau.i0.x,
                     tau.i1.x=tau.i1.x,
                     tau.i2.x=tau.i2.x,
                     theta1.i0.sd=theta1.i0.sd,
                     theta1.i1.sd=theta1.i1.sd,
                     theta1.i2.sd=theta1.i2.sd) 

dim(results.matrix)  

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Non spde phenology function
#seasonal decomposition of time series by Loess (into trend, season, remainder)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(stats); require(zoo) 
y.ts <- ts(new.y, frequency=12, start=c(1982,1), end=c(2011,12))
mySTLdata <- stl(y.ts, s.window="periodic")

#coersing the stl object into a dataframe
mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
y <- mySTLdata.DF1[ ,"seasonal"]

mu1=rep(1, times=k)
idxmu = rep(rep(1:12), times=nyears);
idxsin = rep(rep(1:12), times=nyears); 
idxcos = rep(rep(1:12), times=nyears);
size = length(y)

#Time varrying covariates
sin.amplitude <- as.vector(rep(sin(30 * (2*pi) * (1:k/k)))) ##30 cycles
cos.amplitude <- as.vector(rep(cos(30 * (2*pi) * (1:k/k)))) 

require(INLA); require(raster)

#The formula take all these things into account
formula <- y ~ mu1 + sin.amplitude + cos.amplitude +
  f(idxmu, mu1, model="ar1",cyclic=TRUE, constr=TRUE) +
  f(idxsin, sin.amplitude,  model="ar1", cyclic=TRUE, constr=TRUE)+
  f(idxcos, cos.amplitude,  model="ar1", cyclic=TRUE, constr=TRUE)-1

#inla call:
result = try(inla(formula,
               family="gaussian",
               data=data.frame(y=y, mu1=mu1, idxmu=idxmu,
                               idxsin=idxsin, idxcos=idxcos, 
                               sin.amplitude=sin.amplitude,
                               cos.amplitude=cos.amplitude),
               control.predictor=list(compute=TRUE),
               control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
               control.inla = list(strategy = "gaussian"),
               verbose=TRUE), silent=TRUE)

#The estimated phenology parameter posterior mean estimates
beta0=if(class(result)=="try-error") NA else unclass(result$summary.fix[1,1])
sin_beta1=if(class(result)=="try-error") NA else unclass(result$summary.fix[2,1])
cos_beta2=if(class(result)=="try-error") NA else unclass(result$summary.fix[3,1])
Amplitude=if(class(result)=="try-error") NA else unclass (sqrt(sin_beta1^2 + cos_beta2^2))
Phase=if(class(result)=="try-error") NA else unclass (asin(sin_beta1/Amplitude))

#The estimated phenology parameter posterior sd estimates
beta0.sd=if(class(result)=="try-error") NA else unclass(result$summary.fix[1,2])
sin_beta1.sd=if(class(result)=="try-error") NA else unclass(result$summary.fix[2,2])
cos_beta2.sd=if(class(result)=="try-error") NA else unclass(result$summary.fix[3,2])

#Model validation parameter estimates
dic=if(class(result)=="try-error") NA else unclass (result$dic$dic)
waic=if(class(result)=="try-error") NA else unclass (result$waic$waic)
logCPO=if(class(result)=="try-error") NA else unclass (sum(-log(result$cpo$cpo), na.rm=TRUE)) 

#non transformed spde parameters (theta1) posterior mean
rho.beta0.postmean=if(class(result)=="try-error") NA else unclass(result$summary.hy[3,1])
rho.beta1.postmean=if(class(result)=="try-error") NA else unclass(result$summary.hy[5,1])
rho.beta2.postmean=if(class(result)=="try-error") NA else unclass(result$summary.hy[7,1])

#rho posterior sd
rho.beta0.postsd=if(class(result)=="try-error") NA else unclass(result$summary.hy[3,2])
rho.beta1.postsd=if(class(result)=="try-error") NA else unclass(result$summary.hy[5,2])
rho.beta2.postsd=if(class(result)=="try-error") NA else unclass(result$summary.hy[7,2])

#Organizing the outputs in matrix for raster stacking  
results.matrix=cbind(beta0=beta0, 
                     sin_beta1=sin_beta1, 
                     cos_beta2=cos_beta2,
                     beta0.sd=beta0.sd,
                     sin_beta1.sd=sin_beta1.sd,
                     cos_beta2.sd=cos_beta2.sd,
                     Amplitude=Amplitude,
                     Phase=Phase,
                     rho.beta0.postmean=rho.beta0.postmean,
                     rho.beta1.postmean=rho.beta1.postmean,
                     rho.beta2.postmean=rho.beta2.postmean,
                     rho.beta0.postsd=rho.beta0.postsd,
                     rho.beta1.postsd=rho.beta1.postsd,
                     rho.beta2.postsd=rho.beta2.postsd,
                     dic=dic, waic=waic, logCPO=logCPO)  
results.matrix 
dim(results.matrix)




######### END>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(raster)
fcat=function(x){ifelse(x < -0.3, 1,
                        ifelse(x >= -0.3 & x < 0, 2,
                               ifelse(x >= 0 & x < 0.3, 3,
                                      ifelse(x >= 0.3, 4, NA))))
}

trend.cat=calc(vip_raster_trend, fcat)
trend.rat <- ratify(trend.cat)
rat <- levels(trend.rat)[[1]]
rat$Trend <- c('< -0.3', '-0.3 to <0', '0 to <0.3', '>0.3')
rat$class <- c('A', 'B', 'C', 'D')
levels(trend.rat) <- rat
trend.rat

p1.new=levelplot(trend.rat, col.regions=c('midnightblue', 'brown', 'indianred1', 'palegreen'))
p1.new + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))




















#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  observation available before smoothing
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(maptools)
proj <- CRS('+proj=longlat +ellps=WGS84')
mapaSHP <- readShapeLines('F:/CLIM_DATA/Kenya_evi2_Records/kenyacounties/counties/counties.shp', proj4string=proj)
# p <- spplot(obsUsed, par.settings=BuRdTheme)
# p + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))
# 

require(rasterVis)
require(RColorBrewer)
lists.obs = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/obs_used"),pattern="*.tif", full.names=TRUE)
obs.used.image = foreach(files=1:length(lists.obs), .combine=c, .verbose=FALSE) %do%
  raster(lists.obs[[files]])
stk_obs_used=stack(obs.used.image, bands=NULL, native=FALSE, RAT=TRUE)

### Show all the colour schemes available
require(RColorBrewer)
require(gimms)
#blrd.palette <- colorRampPalette(c("blue", "red"), space = "Lab")
p1=spplot(stk_obs_used, xlim = bbox(stk_obs_used)[1, ], 
          ylim = bbox(stk_obs_used)[2, ],
          colorkey=list(labels = list(cex = 1.5,fontface='plain'), space='right'), 
          col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")),
          at=seq(70, 100, 1),
          strip=FALSE,
          scales = list(draw = TRUE),
          main=list("", col="black",cex=2, fontface='bold'),
          #        panel = function(x, y, z,...) {
          #          panel.levelplot.raster(x, y, z,...)
          #          panel.text(x = 40.5, y = -3.2,
          #                     labels=Names[panel.number()],
          #                     col.text = "black",
          #                     cex=1.5,fontface='bold')
          #        }, 
          contour=FALSE, layout=c(1, 1))
p1 + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))
####################################################
## Categorical data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
lists.obs = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/Results/outputs/obs_used"),pattern="*.tif", full.names=TRUE)
obs.used.image = foreach(files=1:length(lists.obs), .combine=c, .verbose=FALSE) %do%
  raster(lists.obs[[files]])
rr=obs.used.image 
require(raster)
fcat=function(x){ifelse(x < 50, 1,
                        ifelse(x >= 50 & x < 70, 2,
                               ifelse(x >= 70 & x < 90, 3,
                                      ifelse(x >= 90, 4))))
}

obs.used.cat=calc(rr, fcat)
obs.rat <- ratify(obs.used.cat)
rat <- levels(obs.rat)[[1]]
rat$obs_available <- c('< 40%', '40-60%', '60-80%', '> 80%')
rat$class <- c('A', 'B', 'C', 'D')
levels(obs.rat) <- rat
obs.rat

p1.new=levelplot(obs.rat, col.regions=c('midnightblue', 'brown', 'indianred1', 'palegreen'))
p1.new + layer(sp.lines(mapaSHP, lwd=0.8, col='black'))
## with 'att' you can choose another variable from the RAT
# levelplot(r, att=2, col.regions=c('palegreen', 'midnightblue', 'indianred1'))
# levelplot(r, att='class', col.regions=c('palegreen', 'midnightblue', 'indianred1'))

