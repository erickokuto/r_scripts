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



# an alternative way to fit the same model using and RW1 process
# --------------------------------------------------------------
require(raster)
require(foreach)
require(INLA)
DRCevi2_lists <- list.files(file.path("F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/EVI2_Records"),
                            pattern="*.tif", full.names=TRUE)
DRCevi2_rasters<-foreach(files=1:length(DRCevi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(DRCevi2_lists[[files]])
DRC_stk.Data.evi2<-stack(DRCevi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
nlayers(DRC_stk.Data.evi2)

#########################################################################################
#   Test pixel
#########################################################################################
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


########################################################################
# MODEL 1 (A UNIVARIATE MODEL WITH ALL THE THREE EFFECTS)
##########################################################################
formula.univariate = test_obs ~ f(trend.effect, model = "rw2", diagonal=1e-8, scale.model=TRUE, constr=TRUE) +
  f(cyclical.effect, model = "rw2", diagonal=1e-8, cyclic=TRUE, constr=TRUE, scale.model=TRUE) +
  f(seasonal.effect, model="seasonal", 
    season.length=12, constr=TRUE, 
    diagonal=1e-8) -1

result.univariate = inla(formula.univariate, family="gaussian", data = df,
                     quantiles=NULL,
                     control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
                     control.predictor=list(compute=TRUE), verbose=TRUE)

result.univariate$ok
summary(result.univariate)
dic.univariate<-result.univariate$dic$dic
waic.univariate<-result.univariate$waic$waic
CPU.univariate<-result.univariate$cpu.used
LCPO.univariate=sum(-log(result.univariate$cpo$cpo), na.rm=TRUE)

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
                    control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
                    control.predictor=list(compute=TRUE), verbose=TRUE)

result.joint$ok
summary(result.joint)
waic.joint<-result.joint$waic$waic
dic.joint<-result.joint$dic$dic
CPU.joint<-result.joint$cpu.used
LCPO.joint=sum(-log(result.joint$cpo$cpo), na.rm=TRUE)

#Based on the CPO-values, we can calculate the logarithmic score
#A smaller value of the logarithmic score indicates a better
# #prediction quality of the model
# LCPO.joint.model=sum(-log(result.joint$cpo$cpo), na.rm=TRUE)
#######################################################################
dic <- c(univariate = dic.univariate, Joint=dic.joint)
waic <- c(univariate = waic.univariate, Joint=waic.joint)
LCPO <- c(univariate = LCPO.univariate, Joint=LCPO.joint)

##############################################################################
#   Fittend values (posterior means and sds)
##############################################################################
#Fitted values (posterior mean and posterior sd)
round(result.univariate$summary.fitted.values[1:720, c("mean", "sd")], 4)
round(result.joint$summary.fitted.values[1:720, c("mean", "sd")], 4)

#######################################
# seasonal random effects
#######################################
round(result.univariate$summary.random$seasonal.effect$mean, 4)
round(result.joint$summary.random$seasonal2$mean, 4)

#seasonal sds estimates
round(result.univariate$summary.random$seasonal.effect$sd, 4)
round(result.joint$summary.random$seasonal2$sd, 4)

#Cyclical random effect posterior mean estimates
round(result.univariate$summary.random$cyclical.effect$mean, 4)
round(result.joint$summary.random$cyclical2$mean, 4)

#cynical random effect posterior sds estimates
round(result.univariate$summary.random$cyclical.effect$sd, 4)
round(result.joint$summary.random$cyclical2$sd, 4)

#Trend random effect posterior mean estimates
round(result.univariate$summary.random$trend.effect$mean, 4)
round(result.joint$summary.random$trend2$mean, 4)

#Trend random effect posterior sds estimates
round(result.univariate$summary.random$trend.effect$sd, 4)
round(result.joint$summary.random$trend2$sd, 4)

separate_model<-round(result.univariate$summary.fitted.values[1:720, c("mean")], 4)
joint_model<-round(result.joint$summary.fitted.values[1:720, c("mean")], 4)

sd.separeate<-round(result.univariate$summary.fitted.values[1:720, c("sd")], 4)
sd.joint<-round(result.joint$summary.fitted.values[1:720, c("sd")], 4)

#  PREDICTED VALUES
separate.model.pred<-round(result.univariate$summary.fitted.values[721:722, c("mean")], 4)
joint.model.pred<-round(result.joint$summary.fitted.values[721:722, c("mean")], 4)

sd.separeate.pred<-round(result.univariate$summary.fitted.values[721:722, c("sd")], 4)
sd.joint.pred<-round(result.joint$summary.fitted.values[721:722, c("sd")], 4)

require(zoo)
# Panels
plot.zoo(cbind(separate_model, joint_model))

# Overplotted
plot.zoo(cbind(separate_model, joint_model), 
         plot.type = "single", 
         col = c("red", "blue"))

plot.zoo(cbind(sd.separeate, sd.joint), 
         plot.type = "single", 
         col = c("red", "blue"))
legend("top", col=c("red","blue"), lwd=2,legend=c("separate","joint"))

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
#    Univariate Bayesian Time series model
###################################################################################
univariate.model<- function(stk) {
  univariate.smoother<- function(v) {
    x <- v[1:size]
    y=as.vector(x)
    n=length(y)
    nyears=n/24
    trend.effect <- seq(1:n)
    seasonal.effect<-rep(rep(1:12, each=2), times=nyears)
    cyclical.effect<-rep(1982:2012, each=24)
    if (all(is.na(y))) {
      return(rep(NA, times = size))
    } else {
      formula.univariate = y ~ f(trend.effect, model = "rw2",
                                        diagonal=1e-8, scale.model=FALSE, constr=TRUE) +
        f(cyclical.effect, model = "rw2", 
          diagonal=1e-8, cyclic=TRUE, constr=TRUE, scale.model=TRUE) +
        f(seasonal.effect, model="seasonal", 
          season.length=12, constr=TRUE, 
          diagonal=1e-8) -1
      result.univariate = inla(formula.univariate,
                               family="gaussian",
                               data = data.frame(y,seasonal.effect,cyclical.effect,trend.effect),
                               quantiles=NULL,
                              control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
                            control.predictor=list(compute=TRUE), verbose=TRUE)
      
      dic.univariate=result.univariate$dic$dic
      waic.univariate=result.univariate$waic$waic
      LCPO.univariate=sum(-log(result.univariate$cpo$cpo), na.rm=TRUE) 
      model.val=cbind(dic.univariate, waic.univariate, LCPO.univariate)
      
      #Fitted values (posterior mean and posterior sd)
      overall.post.mean=cbind(round(result.univariate$summary.fitted.values[1:744, c("mean")], 4))
      overall.post.sd=cbind(round(result.univariate$summary.fitted.values[1:744, c("sd")], 4))
      
      #seasonal random effects
      seasonal.post.mean=cbind(round(result.univariate$summary.random$seasonal.effect$mean, 4))
      seasonal.post.sd=cbind(round(result.univariate$summary.random$seasonal.effect$sd, 4))
      
      #Cyclical random effect posterior mean estimates
      cyclical.post.mean=cbind(round(result.univariate$summary.random$cyclical.effect$mean, 4))
      cyclical.post.sd=cbind(round(result.univariate$summary.random$cyclical.effect$sd, 4))
      
      #Trend random effect posterior mean estimates
      trend.post.mean=cbind(round(result.univariate$summary.random$trend.effect$mean, 4))
      trend.post.sd=cbind(round(result.univariate$summary.random$trend.effect$sd, 4))
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 


####################################################################################
#    Joint Bayesian Time series model
###################################################################################
joint.model<- function(stk) {
  joint.smoother<- function(v) {
    x <- v[1:size]
    y=as.vector(x)
    n=length(y)
    nyears=n/24
    trend.effect <- seq(1:n)
    seasonal.effect<-rep(rep(1:12, each=2), times=nyears)
    cyclical.effect<-rep(1982:2012, each=24)
    occurence<-ifelse(is.na(y), 0, 1)
    
    N=2*n
    Y = matrix(NA, N, 2)
    Y[1:n, 1] = y
    Y[1:n + n, 2] = occurence
    
    ## Ntrials for the Binomial
    size2 = rep(1, times=n)
    Ntrials = numeric(N)
    Ntrials[1:n] = NA
    Ntrials[1:n + n] = size2
    
    ## Duplicate the covariate/random effects which is/are shared
    trend2 = numeric(N)
    trend2[1:n] = trend.effect
    trend2[1:n + n] = trend.effect
    
    #cyclical effects
    cyclical2 = numeric(N)
    cyclical2[1:n] = cyclical.effect
    cyclical2[1:n + n] = cyclical.effect
    
    #seasonal effect
    seasonal2 = numeric(N)
    seasonal2[1:n] = seasonal.effect
    seasonal2[1:n + n] = seasonal.effect
   
    if (all(is.na(y))) {
      return(rep(NA, times = size))
    } else {
      formula.joint = Y ~ f(trend2, model = "rw2", diagonal=1e-8, constr=TRUE, scale.model=TRUE) +
        f(cyclical2, model = "rw2", diagonal=1e-8, constr=TRUE, cyclic=TRUE, scale.model=TRUE) +
        f(seasonal2, model="seasonal", season.length=12, constr=TRUE, diagonal=1e-8) -1
      
      result.joint = inla(formula.joint, family=c("gaussian", "binomial"),
                          data = data.frame(Y, trend2, cyclical2, seasonal2, Ntrials),
                          quantiles=c(0.5), Ntrials=Ntrials,
                          control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      dic.joint=result.joint$dic$dic
      waic.joint=result.joint$waic$waic
      LCPO.joint=sum(-log(result.joint$cpo$cpo), na.rm=TRUE) 
      
      #model validation metrics
      model.val=cbind(dic.joint, waic.joint, LCPO.joint)
      
      #Fitted values (posterior mean and posterior sd)
      overall.post.mean=cbind(round(result.joint$summary.fitted.values[1:744, "mean"], 4))
      overall.post.sd=cbind(round(result.joint$summary.fitted.values[1:744, "sd"], 4))
      
      #seasonal random effects
      seasonal.post.mean=cbind(round(result.joint$summary.random$seasonal2$mean, 4))
      seasonal.post.sd=cbind(round(result.joint$summary.random$seasonal2$sd, 4))
      
      #Cyclical random effect posterior mean estimates
      cyclical.post.mean=cbind(round(result.joint$summary.random$cyclical2$mean, 4))
      cyclical.post.sd=cbind(round(result.joint$summary.random$cyclical2$sd, 4))
      
      #Trend random effect posterior mean estimates
      trend.post.mean=cbind(round(result.joint$summary.random$trend2$mean, 4))
      trend.post.sd=cbind(round(result.joint$summary.random$trend2$sd, 4))
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 

#############################################################################
#   Application of joint and univariate models
##########################################################################

#univariate model
evi2.filtered.univariate <- univariate.model(stk=DRC_stk.Data.evi2)

#joint model
evi2.filtered.joint <- joint.model(stk=DRC_stk.Data.evi2)

