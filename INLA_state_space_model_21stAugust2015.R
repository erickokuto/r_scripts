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

# nyears=dim(df)[1]/24
# trend.effect <- seq(1:nlayers(DRC_stk.Data.evi2))
# seasonal.effect<-rep(rep(1:12, each=2), times=nyears)
# cyclical.effect<-rep(1982:2016, each=24)


X11()
plot(DRC_stk.Data.evi2[[1]])
plot(test_obs)



inla.setOption(scale.model.default = TRUE)
## Parameters for the loggamma prior
prior = c(1, 0.001)
## A small constant we add to the diagonal to prevent the model to be
## intrinsic.
d = 1e-8

n.seas=12
hyper.prec = list(prec = list(param = c(1, 0.01)))



# df<-data.frame(test_obs, trend.effect, seasonal.effect,cyclical.effect)
# write.csv(df, "F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/test_obs2.csv")
#df<-read.csv("F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/test_obs2.csv")

#################################################################################
#   Univariate model of density of vegetation
################################################################################
df<-read.csv("F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/DRC_Database/test_obs2.csv")
#df$occurence<-ifelse(is.na(df$test_obs), 0, 1))
df<-df[1:720,]

nyears=dim(df)[1]/24
trend.effect <- seq(1:dim(df)[1])
seasonal.effect<-rep(rep(1:12, each=2), times=nyears)
cyclical.effect<-rep(1982:2011, each=24)

############################################################################
#   The NULL model (Univariate model of density of vegetation record without any effect)
############################################################################
formula1 = test_obs ~ 1

r1 = inla(formula1, family="gaussian", data = df,
          quantiles=c(0.5),
          control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
          control.inla=list(strategy="laplace", int.strategy = "grid", diff.logdens = 4),
          control.predictor=list(compute=TRUE))
r1$ok

summary(r1)
r1$cpu.used

r1$waic$waic
r1$cpo$cpo

#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
#prediction quality of the model
LCPO=sum(-log(r1$cpo$cpo), na.rm=TRUE)

r1$dic
r1$waic
r1$cpo$cpo
r1$cpo$pit
r1$cpo$failure
r1$cpo$failure[i] > 0
r1$cpo$failure[i] == 0
############################################################################
#   Univariate model of density of vegetation record
############################################################################
formula1 = test_obs ~ f(trend.effect, model = "rw2", diagonal=1e-8, scale.model=TRUE, constr=TRUE) +
  f(cyclical.effect, model = "rw1", diagonal=1e-8, cyclic=TRUE, constr=TRUE, scale.model=TRUE) +
  f(seasonal.effect, model="seasonal", 
  season.length=n.seas, constr=TRUE, 
    diagonal=1e-8) -1

r1 = inla(formula1, family="gaussian", data = df,
         quantiles=NULL,
         control.compute=list(cpo=TRUE, po=FALSE, dic = FALSE, waic = TRUE),
         control.predictor=list(compute=TRUE), verbose=TRUE)
r1$ok
summary(r1)
r1$waic$waic
r1$cpo$cpo


#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
#prediction quality of the model
LCPO=sum(-log(r1$cpo$cpo), na.rm=TRUE)

#Fitted values
round(r1$summary.fitted.values[1:720,], 4)

#compare Precisions
round(rbind(rw1=r1$summary.hy[2,2],rw2=r1$summary.hy[3,2]),4)

#Based on the CPO-values, we can calculate the logarithmic score
# round(rbind(DIC=c(rw1=r1$dic$dic,rw2=r2$dic$dic),
 LCPO1=sum(-log(r1$cpo$cpo), na.rm=TRUE) #A smaller value of the logarithmic score indicates a better
                                         #prediction quality of the model
# ############################################################################
#   Univariate model for occurence of vegetation record
############################################################################
# formula2 = occurence ~ f(trend.effect, model = "rw1", diagonal=1e-8, constr=TRUE) +
#   f(cyclical.effect, model = "rw1", diagonal=1e-8, constr=TRUE) +
#   f(seasonal.effect, model="seasonal", 
#     season.length=n.seas, constr=TRUE, diagonal=1e-8) -1
# 
# r2 = inla(formula2, family="binomial", Ntrials=rep(1, times=dim(df)[1]), data = df,
#          quantiles=c(0.5),
#          control.compute=list(cpo=TRUE, dic = FALSE, waic = TRUE),
#          control.predictor=list(compute=TRUE), verbose=TRUE)
# 
# r2$ok
# summary(r2)
# r2$waic$waic

#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
#prediction quality of the model
#LCPO2=sum(-log(r2$cpo$cpo), na.rm=TRUE)

############################################################################
#   Joint model of occurence and density of vegetation record
############################################################################
N=2*dim(df)[1]
n=dim(df)[1]
Y = matrix(NA, N, 2)
Y[1:n, 1] = df$test_obs
Y[1:n + n, 2] = df$occurence


## Ntrials for the Binomial
size = rep(1, times=n)
Ntrials = numeric(N)
Ntrials[1:n] = NA
Ntrials[1:n + n] = size

## Duplicate the covariate/random effects which is/are shared
trend2 = numeric(N)
trend2[1:n] = trend.effect
trend2[1:n + n] = trend.effect

cyclical2 = numeric(N)
cyclical2[1:n] = cyclical.effect
cyclical2[1:n + n] = cyclical.effect

seasonal2 = numeric(N)
seasonal2[1:n] = seasonal.effect
seasonal2[1:n + n] = seasonal.effect

#################################################################
# If the covariate 'x' is different for the two
# families, 'x' and 'xx', say, then we only need to
# make the following changes
# X = numeric(N)
# X[1:n] = x
# X[1:n + n] = NA
# 
# XX = numeric(N)
# XX[1:n] = NA
# XX[1:n + n] = xx
###########################################################################

formula3 = Y ~ f(trend2, model = "rw2", diagonal=1e-8, constr=TRUE, scale.model=TRUE) +
  f(cyclical2, model = "rw1", diagonal=1e-8, constr=TRUE, cyclic=TRUE, scale.model=TRUE) +
  f(seasonal2, model="seasonal", 
    season.length=12, constr=TRUE, diagonal=1e-8) -1

r3 = inla(formula3, family=c("gaussian", "binomial"),
          data = data.frame(Y, trend2, cyclical2, seasonal2, Ntrials),
         quantiles=c(0.5), Ntrials=Ntrials,
         control.compute=list(cpo=TRUE, dic = FALSE, waic = TRUE),
         control.predictor=list(compute=TRUE), verbose=TRUE)

# r33 = inla(formula3, family=c("gaussian", "zeroinflatedbinomial0"),
#           data = data.frame(Y, trend2, cyclical2, seasonal2, Ntrials),
#           quantiles=c(0.5), Ntrials=Ntrials,
#           control.compute=list(cpo=TRUE, dic = TRUE),
#           control.predictor=list(compute=TRUE), verbose=TRUE)
r3$ok
summary(r3)

#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
#prediction quality of the model
LCPO3=sum(-log(r3$cpo$cpo), na.rm=TRUE)
##############################################################################
#   Preliminary results for Model 1 and model 2
##############################################################################
summary(r2)

#r$cpo$cpo
r$dic
r$waic

r1$summary.fitted.val[, c("mean", "sd")]
r1$summary.random$trend.effect$mean
r1$summary.random$seasonal.effect$mean
r1$summary.random$cyclical.effect$mean
r1$summary.random$trend.effect$sd
r1$summary.random$seasonal.effect$sd
r1$summary.random$cyclical.effect$sd
plot(r1$summary.random$trend.effect$mean, type="l")
plot(r1$summary.random$seasonal.effect$mean, type="l")
plot(r1$summary.random$cyclical.effect$mean, type="l")

r2$summary.fitted.val[, c("mean", "sd")]
r2$summary.random$trend.effect$mean
r2$summary.random$seasonal.effect$mean
r2$summary.random$cyclical.effect$mean
r2$summary.random$trend.effect$sd
r2$summary.random$seasonal.effect$sd
r2$summary.random$cyclical.effect$sd
plot(r2$summary.random$trend.effect$mean, type="l")
plot(r2$summary.random$seasonal.effect$mean, type="l")
plot(r2$summary.random$cyclical.effect$mean, type="l")


r3$summary.fitted.val[, c("mean", "sd")]
r3$summary.random$trend2$mean
r3$summary.random$seasonal2$mean
r3$summary.random$cyclical2$mean
r3$summary.random$trend2$sd
r3$summary.random$seasonal2$sd
r3$summary.random$cyclical2$sd
plot(r3$summary.random$trend2$mean, type="l")
plot(r3$summary.random$seasonal2$mean, type="l")
plot(r3$summary.random$cyclical2$mean, type="l")
####################################################################################
#    END
####################################################################################
install.packages("excursions", dependencies=TRUE)

require(excursions)
plot(obs.loc,Y,xlab="",ylab="")
lines(X)
lines(mu.post,col=2)
lines(result$summary.linear.predictor$mean[iprd],col=3)

p.marginal = 1- result$summary.linear.predictor$"0 cdf"