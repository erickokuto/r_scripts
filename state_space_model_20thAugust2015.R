##################################################################################################
#
# script with R code to implement all the examples at the paper entitled
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
#
##################################################################################################

require(INLA)

# an alternative way to fit the same model using and RW1 process
# --------------------------------------------------------------
i = 1:n    # indices of the RW1 prior for the INLA library
formula1 = y ~ f(i, model="rw1", constr=F) -1
r = inla(formula1, data = data.frame(i,y),
         quantiles=c(0.025, 0.05, 0.5, 0.95, 0.975),
         control.predictor=list(compute=TRUE))

# elapsed time (seconds)
r$cpu.used
r2$cpu.used
r3$cpu.used

# hyperparamerter estimation values
# ---------------------------------
r$summary.hyperpar
r2$summary.hyperpar
r3$summary.hyperpar

# graph for observations (y)
rang <- range(r$summary.fitted.values[1:n, 3:5], y)
plot(r$summary.fitted.values[1:n,1], type="l", 
     ylim=rang, col="red", xlim=c(1,n),ylab="y",xlab="time")
lines(r$summary.fitted.values[1:n,3], col="blue", lty=3)
lines(r$summary.fitted.values[1:n,5], col="blue", lty=3)
lines(y[1:n])
legend("topright", legend=c("simulated y_t","posterior mean","95% CI"), col=c("black", "red","blue"),lty=c(1,1,2),bty="n")
#title("a toy example")

# graph for states (x)
rang <- range(r$summary.random[[1]][1:n, 4:6], x)
plot(r$summary.random[[1]][1:n,2], type="l", 
     ylim=rang, col="red", xlim=c(1,n),ylab="X_t",xlab="time")
lines(r$summary.random[[1]][1:n,4], col="blue", lty=3)
lines(r$summary.random[[1]][1:n,6], col="blue", lty=3)
lines(x[1:n])
legend("topright", legend=c("simulated X_t","posterior mean","95% CI"), col=c("black", "red","blue"),lty=c(1,1,2),bty="n")
#title("a toy example")


#################################################################
#
##################################################################
require(raster)
require(foreach)
DRCevi2_lists <- list.files(file.path("F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/EVI2_Records"),
                            pattern="*.tif", full.names=TRUE)
DRCevi2_rasters<-foreach(files=1:length(DRCevi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(DRCevi2_lists[[files]])
DRC_stk.Data.evi2<-stack(DRCevi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
test_obs<-extract(DRC_stk.Data.evi2, y=cbind(27,-9))
test_obs<-as.vector(test_obs)
write.csv(test_obs, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/test_obs.csv")

X11()
plot(DRC_stk.Data.evi2[[1]])
plot(test_obs)

nyears=nlayers(DRC_stk.Data.evi2)/24
trend.effect <- seq(1:nlayers(DRC_stk.Data.evi2))
seasonal.effect<-rep(rep(1:12, each=2), times=nyears)
cyclical.effect<-rep(1982:2016, each=24)

inla.setOption(scale.model.default = TRUE)
## Parameters for the loggamma prior
prior = c(1, 0.001)
## A small constant we add to the diagonal to prevent the model to be
## intrinsic.
d = 1e-8

n.seas=12
hyper.prec = list(prec = list(param = c(1, 0.01)))



df<-cbind(test_obs, trend.effect, seasonal.effect,cyclical.effect)

formula1 = test_obs ~ f(trend.effect, model = "rw1", diagonal=d, constr=TRUE) +
  f(cyclical.effect, model = "rw1", diagonal=d, constr=TRUE) +
  f(seasonal.effect, model="seasonal", 
    season.length=n.seas, constr=TRUE, diagonal=d) -1

r = inla(formula1, family="gaussian", data = as.data.frame(df),
         quantiles=c(0.5),
         control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
         control.predictor=list(compute=TRUE))
summary(r)

#r$cpo$cpo
r$dic
r$waic

r$summary.fitted.val[, c("mean", "sd") ]
r$summary.random$trend.effect$mean
r$summary.random$seasonal.effect$mean
r$summary.random$cyclical.effect$mean
r$summary.random$trend.effect$sd
r$summary.random$seasonal.effect$sd
r$summary.random$cyclical.effect$sd
plot(r$summary.random$trend.effect$mean)
plot(r$summary.random$seasonal.effect$mean, lty=1)
plot(r$summary.random$cyclical.effect$mean)
plot(trend.effect[-(720:840)], mean.e[-(720:840)], lty=2)

