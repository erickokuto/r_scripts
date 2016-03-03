

require(raster)
require(foreach)
setwd("F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyEVI2-5km")
evi2_lists <- list.files(file.path("sgolay"),
                            pattern="*.tif", full.names=TRUE)
evi2_rasters<-foreach(files=1:length(evi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2_lists[[files]])
stk.Data.evi2<-stack(evi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)

nlayers=nlayers(stk.Data.evi2)
nyears=nlayers/12


plot(stk.Data.evi2[[3]])

trend.time<-rep(1:nlayers(stk.Data.evi2))
seasonal1.time<-rep(rep(1:12), times=nyears)
seasonal2.time<-rep(rep(1:6, times=2), times=nyears)
cyclical.time<-rep(1982:2011, each=12)


######################################################
#  Scaling time variables
#####################################################
?scale
#scale(x, center = FALSE, scale = apply(x, 2, sd, na.rm = TRUE)).)
trend.ctime<-scale(rep(1:nlayers(DRC_stk.Data.evi2)), center = FALSE)
seasonal.ctime<-scale(rep(rep(1:12, each=2), times=nyears), center = FALSE)
cyclical.ctime<-scale(rep(1982:2016, each=24), center = FALSE)

#Centering time variable
inla_df$ctime<-scale(inla_df$time,scale=F)


########################################################
#
######################################################
require(gam)
require(mgcv)
gam(formula,family=gaussian(),data=list(),weights=NULL,subset=NULL,
    na.action,offset=NULL,method="GCV.Cp",
    optimizer=c("outer","newton"),control=list(),scale=0,
    select=FALSE,knots=NULL,sp=NULL,min.sp=NULL,H=NULL,gamma=1,
    fit=TRUE,paraPen=NULL,G=NULL,in.out,drop.unused.levels=TRUE,...)


#####################################
# Smoothing using INLA Approach
######################################
inlafunction<-function(x){
  
}

#####################################
#  Smoothing using BayesX approach
######################################
bayesXfunction<-function(x){
  
}

#####################################
#Smoothing using mixed effect (frequentist) method
######################################
gamfunction<-function(x){
  
}

funsgolay<- function(stk) {
  fun.smoother.sgolay<- function(v) {
    x <- v[1:size]
    x.vector=as.vector(x)
    begin.year=1982
    end.year=2016
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









###################################################
### Using one pixel from Brazil as an example
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
require("INLA")

df0<-read.csv("F:/final_5km_15Days/brazil_pixel.csv")
year<-rep(1:32, each=24)
month<-rep(rep(1:12, each=2), times=32)
names(df0); dim(df0)[1]

plot(df0$NDVI, type="b", lwd=2,
     ylim=c(0,1), col="black",
     xlab="Year", ylab="NDVI Index",
     main="Monthly NDVI Index for One pixel")

##############################################################################
#    INLA model development
############################################################################
#knots for the seasonal component
###################################################
knots.seasonal = seq(1, 12, by = 2)

################################################
#knots for the trend component
###############################################
Knots.trend = seq(1, 32, length=8)

#############################################################################
# mesh construction
#######################################################################
mesh.seasonal = inla.mesh.1d(knots.seasonal, interval=c(0, 12), degree=2, boundary="cyclic")

mesh.trend = inla.mesh.1d(Knots.trend, interval=c(0, 32), degree=2, boundary="free")

###################################################
#    spde formulation
###################################################
sigma0 = 1
kappa0 = 1e-3
tau0 = 1/(4*kappa0^3*sigma0^2)^0.5

########################################################
# Seasonal coomponent
########################################################
spde.seasonal = inla.spde2.matern(mesh.seasonal, constr=TRUE,
                         B.tau = cbind(log(tau0), 1),
                         B.kappa = cbind(log(kappa0), 0),
                         theta.prior.prec = 1e-4)

###########################################################
#Trend component
############################################################
spde.trend = inla.spde2.matern(mesh.trend, constr=TRUE,
                         B.tau = cbind(log(tau0), 1),
                         B.kappa = cbind(log(kappa0), 0),
                         theta.prior.prec = 1e-4)

##########################################################
#        
################################################################
#Q = inla.spde2.precision(seasonal.spde, theta=c(log(3), 0))



########################################################
#  constructing observation matrix
#######################################################
A.season = inla.spde.make.A(mesh.seasonal,
                     loc = month)

A.trend = inla.spde.make.A(mesh.trend,
                     loc = year)

index.year = inla.spde.make.index("year", n.spde=spde.trend$n.spde)
index.month = inla.spde.make.index("month", n.spde=spde.seasonal$n.spde)

################################################################
#  Stack 
###############################################################

#Your A parameter to inla.stack should be in the 
#same order as the effects, list(A.trend,A.season,1)

mesh.seasonal$n; mesh.trend$n

stack = inla.stack(data=list(y = df0$NDVI), 
                   A=list(A.trend, A.season, 1),
                   effects=list(index.year, 
                                index.month,
                                intercept=rep(1, times=768)),
                                tag="est")

dim(A.seasonal); dim(A.trend); length(index.year); length(index.month)

####################################################################
# INLA model formula
######################################################################
formula = y ~ 0 + intercept + f(month, model=spde.seasonal) + f(year, model=spde.trend)
data = inla.stack.data(stack)
result = inla(formula, family="gaussian", data=data,
              control.compute=list(cpo=TRUE, config=FALSE),
              control.predictor=list(A=inla.stack.A(stack), compute=TRUE),
              control.inla=list(int.strategy = "grid", diff.logdens = 4),
              verbose=TRUE)
summary(result)

plot(result, single=TRUE)

####################################################################
# Sampling from the posterior distribution
######################################################################
#samples = inla.posterior.sample(2,result)

########################################################################
pp.rf1 <- inla.spde2.result(result , 'month', seasonal.spde, do.transf=TRUE)
pp.rf2 <- inla.spde2.result(result , 'year', trend.spde, do.transf=TRUE)

############################################################
#  Estimated SPDE parameters for seasonal component
########################################################
par(mfrow=c(1,3), mar=c(3, 3, 0.3, 0.3), mgp=c(2,1,0))
plot(pp.rf1$marginals.variance.nominal[[1]], type='l',
     xlab=expression(sigma^2), ylab='Density')
abline(v=sigma2x, col=2)
plot(pp.rf1$marginals.kappa[[1]], type='l',
     xlab=expression(kappa), ylab='Density')
abline(v=kappa, col=2)
plot(pp.rf1$marginals.range.nominal[[1]], type='l',
     xlab='Nominal range', ylab='Density')
abline(v=sqrt(8*1)/kappa, col=2)

############################################################
#   Estimated SPDE parameters for trend component
########################################################
par(mfrow=c(1,3), mar=c(3, 3, 0.3, 0.3), mgp=c(2,1,0))
plot(pp.rf2$marginals.variance.nominal[[1]], type='l',
     xlab=expression(sigma^2), ylab='Density')
abline(v=sigma2x, col=2)
plot(pp.rf2$marginals.kappa[[1]], type='l',
     xlab=expression(kappa), ylab='Density')
abline(v=kappa, col=2)
plot(pp.rf2$marginals.range.nominal[[1]], type='l',
     xlab='Nominal range', ylab='Density')
abline(v=sqrt(8*1)/kappa, col=2)

#########################################################################
#      compute cross-validation or predictive measures
##########################################################################
result$cpo$cpo
result$cpo$pit
result$cpo$failure
result$cpo$failure[i] > 0

improved.result = inla.cpo(result)

#####################################################################
#     Extracting predicted values from the indices
##################################################################
(ind <- inla.stack.index(stack, tag='est')$data)
ObsPred<-cbind(round(result$summary.linear.pred[ind,], 4), ObservedNDVI=df0$NDVI)
ObsPred[ , c(8,1,2,3,4,5,6,7)]

df<-data.frame(observed=df0$NDVI, projMean=ObsPred$mean,
                      sd=ObsPred$sd, time, year=rep(1981:2012, each=24),
               Composite=rep(c("15a","15b"), times=32))

##########################################################################
# Computing for the Goodness of fit
#########################################################################
require(hydroTSM);require(hydroGOF)
ggof(sim=df$projMean, obs=df$observed)
cor.test(df$projMean, df$observed, complete.cases=TRUE)

###################################################################
# Alternatively
#################################################################
require(ggplot2)
eb <- aes(ymax = df$projMean + df$sd, ymin = df$projMean - df$sd)
g <- ggplot(df, aes(time)) +
  theme(panel.background = element_rect(fill='white', colour='black')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) 
g <- g + geom_line(aes(y=observed), colour="blue")
g <- g + geom_ribbon(eb, alpha = 0.5,fill='gray') + geom_line(aes(y=projMean), colour="black")
g <- g + ylab("Y") + xlab("X")
g
##########################################################################

