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
#   MODEL ONE (MODEL WITH ONLY SEASONAL EFFECT)
############################################################################
formula.model1 = test_obs ~ f(seasonal.effect, model="seasonal", season.length=n.seas, constr=TRUE, 
                              diagonal=1e-8) -1

result.model1 = inla(formula.model1, family="gaussian", data = df,
         quantiles=NULL,
         control.compute=list(cpo=TRUE, po=FALSE, dic = TRUE, waic = TRUE),
         control.predictor=list(compute=TRUE), verbose=TRUE)


result.model1 = try(inla(formula.model1, family="gaussian", data = df,
                     quantiles=NULL,
                     control.compute=list(cpo=TRUE, po=FALSE, dic = TRUE, waic = TRUE),
                     control.predictor=list(compute=TRUE), verbose=TRUE), silent=TRUE)

dic.univariate=if(class(result.model1)=="try-error") NA else unclass(result.model1)$dic$dic
waic.univariate=if(class(result.model1)=="try-error") NA else unclass(result.model1)$waic$waic
LCPO.univariate=if(class(result.model1)=="try-error") NA else unclass(sum(-log(result.model1$cpo$cpo), na.rm=TRUE))
result.model1.fit=if(class(result.model1)=="try-error") NA else unclass(round(result.model1$summary.fitted.values[1:720, "mean"], 4))




result.model1$ok
summary(result.model1)
waic.model1<-result.model1$waic$waic
CPU.model1<-result.model1$cpu.used
LCPO.model1=sum(-log(result.model1$cpo$cpo), na.rm=TRUE)

#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
#prediction quality of the model
# LCPO.model1=sum(-log(result.model1$cpo$cpo), na.rm=TRUE)
# result.model1$cpo$cpo
#####################################################################
# MODEL TWO (A model with both trend and seasonal effects)
####################################################################
formula.model2 = test_obs ~ f(cyclical.effect, model = "rw2",
                              diagonal=1e-8, cyclic=TRUE, constr=TRUE, 
                              scale.model=TRUE) +
  f(seasonal.time, model="seasonal", season.length=n.seas, constr=TRUE, 
    diagonal=1e-8) -1

result.model2 = inla(formula.model2, family="gaussian", data = df,
          quantiles=NULL,
          control.compute=list(cpo=TRUE, po=FALSE, dic = FALSE, waic = TRUE),
          control.predictor=list(compute=TRUE), verbose=TRUE)

result.model2$ok
summary(result.model2)
waic.model2<-result.model2$waic$waic
CPU.model2<-result.model2$cpu.used
LCPO.model2=sum(-log(result.model2$cpo$cpo), na.rm=TRUE)

#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
#prediction quality of the model
# LCPO.model2=sum(-log(result.model2$cpo$cpo), na.rm=TRUE)
# result.model2$cpo$cpo

########################################################################
# MODEL THREE (A UNIVARIATE MODEL WITH ALL THE THREE EFFECTS)
##########################################################################
formula.model3 = test_obs ~ f(trend.effect, model = "rw2", diagonal=1e-8, scale.model=TRUE, constr=TRUE) +
  f(cyclical.effect, model = "rw2", diagonal=1e-8, cyclic=TRUE, constr=TRUE, scale.model=TRUE) +
  f(seasonal.effect, model="seasonal", 
    season.length=12, constr=TRUE, 
    diagonal=1e-8) -1

result.model3 = inla(formula.model3, family="gaussian", data = df,
          quantiles=NULL,
          control.compute=list(cpo=TRUE, dic = FALSE, waic = TRUE),
          control.predictor=list(compute=TRUE), verbose=TRUE)

result.model3$ok
summary(result.model3)
waic.model3<-result.model3$waic$waic
CPU.model3<-result.model3$cpu.used
LCPO.model3=sum(-log(result.model3$cpo$cpo), na.rm=TRUE)

#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
# #prediction quality of the model
# LCPO.model3=sum(-log(result.model3$cpo$cpo), na.rm=TRUE)
# result.model3$cpo$cpo
# 
# ############################################################################
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
###########################################################################

formula.joint = Y ~ f(trend2, model = "rw2", diagonal=1e-8, constr=TRUE, scale.model=TRUE) +
  f(cyclical2, model = "rw2", diagonal=1e-8, constr=TRUE, cyclic=TRUE, scale.model=TRUE) +
  f(seasonal2, model="seasonal", season.length=12, constr=TRUE, diagonal=1e-8) -1

result.joint = inla(formula.joint, family=c("gaussian", "binomial"),
          data = data.frame(Y, trend2, cyclical2, seasonal2, Ntrials),
         quantiles=c(0.5), Ntrials=Ntrials,
         control.compute=list(cpo=TRUE, dic = FALSE, waic = TRUE),
         control.predictor=list(compute=TRUE), verbose=TRUE)

result.joint$ok
summary(result.joint)
waic.joint<-result.joint$waic$waic
CPU.joint<-result.joint$cpu.used
LCPO.joint=sum(-log(result.joint$cpo$cpo), na.rm=TRUE)

#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
# #prediction quality of the model
# LCPO.joint.model=sum(-log(result.joint$cpo$cpo), na.rm=TRUE)
# 
#######################################################################
waic<-c(
  Model1=waic.model1,
  Model2= waic.model2,
  Model3= waic.model3,
  Joint=waic.joint )

LCPO<-c(
      Model1=LCPO.model1,
      Model2= LCPO.model2,
      Model3= LCPO.model3,
      Joint=LCPO.joint )

##############################################################################
#   Fittend values (posterior means and sds)
##############################################################################
#Fitted values (posterior mean and posterior sd)
round(result.model1$summary.fitted.values[1:720, c("mean", "sd")], 4)
round(result.model2$summary.fitted.values[1:720, c("mean", "sd")], 4)
round(result.model3$summary.fitted.values[1:720, c("mean", "sd")], 4)
round(result.joint$summary.fitted.values[1:720, c("mean", "sd")], 4)

#######################################
# seasonal random effects
#######################################
round(result.model1$summary.random$seasonal.effect$mean, 4)
round(result.model2$summary.random$seasonal.effect$mean, 4)
round(result.model3$summary.random$seasonal.effect$mean, 4)
round(result.joint$summary.random$seasonal2$mean, 4)

#seasonal sds estimates
round(result.model1$summary.random$seasonal.effect$sd, 4)
round(result.model2$summary.random$seasonal.effect$sd, 4)
round(result.model3$summary.random$seasonal.effect$sd, 4)
round(result.joint$summary.random$seasonal2$sd, 4)

#Cyclical random effect posterior mean estimates
round(result.model2$summary.random$cyclical.effect$mean, 4)
round(result.model3$summary.random$cyclical.effect$mean, 4)
round(result.joint$summary.random$cyclical2$mean, 4)

#cynical random effect posterior sds estimates
round(result.model2$summary.random$cyclical.effect$sd, 4)
round(result.model3$summary.random$cyclical.effect$sd, 4)
round(result.joint$summary.random$cyclical2$sd, 4)

#Trend random effect posterior mean estimates
round(result.model2$summary.random$trend.effect$mean, 4)
round(result.model3$summary.random$trend.effect$mean, 4)
round(result.joint$summary.random$trend2$mean, 4)

#Trend random effect posterior sds estimates
round(result.model2$summary.random$trend.effect$sd, 4)
round(result.model3$summary.random$trend.effect$sd, 4)
round(result.joint$summary.random$trend2$sd, 4)


t1<-round(result.model1$summary.fitted.values[1:720, c("mean")], 4)
t2<-round(result.model2$summary.fitted.values[1:720, c("mean")], 4)
t3<-round(result.model3$summary.fitted.values[1:720, c("mean")], 4)
t4<-round(result.joint$summary.fitted.values[1:720, c("mean")], 4)

require(zoo)
# Panels
plot.zoo(cbind(t1, t2, t3, t4))

# Overplotted
# plot.zoo(cbind(t1, t2, t3, t4), 
#          plot.type = "single", 
#          col = c("red", "blue", "black", "green"))

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