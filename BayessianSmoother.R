
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
      result.univariate = try(inla(formula.univariate,
                               family="gaussian",
                               data = data.frame(y,seasonal.effect,cyclical.effect,trend.effect),
                               quantiles=NULL,
                              control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
                            control.predictor=list(compute=TRUE), verbose=TRUE), silent=TRUE)
      
      dic.univariate=if(class(result.univariate)=="try-error") NA else unclass(result.univariate)$dic$dic
      waic.univariate=if(class(result.univariate)=="try-error") NA else unclass(result.univariate)$waic$waic
      LCPO.univariate=if(class(result.univariate)=="try-error") NA else unclass(sum(-log(result.univariate$cpo$cpo),
                                                                                    na.rm=TRUE))
      post.means=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.fitted.values[1:n, "mean"], 4))
      post.sd=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.fitted.values[1:n, "sd"], 4))
      
      post.seas.mean=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.random$seasonal.effect$mean, 4))
      post.seas.sd=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.random$seasonal.effect$sd, 4))
      
      post.cyc.mean=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.random$cyclical.effect$mean, 4))
      post.cyc.sd=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.random$cyclical.effect$sd, 4))
      
      post.trend.mean=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.random$trend.effect$mean, 4))
      post.trend.sd=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.random$trend.effect$sd, 4))
      
      
      #OUTPUTS
      results.vector=c(dic.univariate, waic.univariate, LCPO.univariate,
                       post.means,
                       post.sd,
                       post.seas.mean,
                       post.seas.sd,
                       post.cyc.mean,
                       post.cyc.sd,
                       post.trend.mean,
                       post.trend.sd)
      results.matrix=cbind(results.vector)
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
      
      result.joint = try(inla(formula.joint, family=c("gaussian", "binomial"),
                          data = data.frame(Y, trend2, cyclical2, seasonal2, Ntrials),
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
                          control.predictor=list(compute=TRUE), verbose=TRUE), silent=TRUE)
      
      dic.joint=result.joint$dic$dic
      waic.joint=result.joint$waic$waic
      LCPO.joint=sum(-log(result.joint$cpo$cpo), na.rm=TRUE) 
      
      
      dic.joint=if(class(result.joint)=="try-error") NA else unclass(result.joint)$dic$dic
      waic.joint=if(class(result.joint)=="try-error") NA else unclass(result.joint)$waic$waic
      LCPO.joint=if(class(result.joint)=="try-error") NA else unclass(sum(-log(result.joint$cpo$cpo),
                                                                                    na.rm=TRUE))
      post.means=if(class(result.joint)=="try-error") NA 
      else unclass(round(result.joint$summary.fitted.values[1:n, "mean"], 4))
      post.sd=if(class(result.joint)=="try-error") NA 
      else unclass(round(result.joint$summary.fitted.values[1:n, "sd"], 4))
      
      post.seas.mean=if(class(result.joint)=="try-error") NA 
      else unclass(round(result.joint$summary.random$seasonal2$mean, 4))
      post.seas.sd=if(class(result.joint)=="try-error") NA 
      else unclass(round(result.joint$summary.random$seasonal2$sd, 4))
      
      post.cyc.mean=if(class(result.joint)=="try-error") NA 
      else unclass(round(result.joint$summary.random$cyclical2$mean, 4))
      post.cyc.sd=if(class(result.joint)=="try-error") NA 
      else unclass(round(result.joint$summary.random$cyclical2$sd, 4))
      
      post.trend.mean=if(class(result.joint)=="try-error") NA 
      else unclass(round(result.joint$summary.random$trend2$mean, 4))
      post.trend.sd=if(class(result.joint)=="try-error") NA 
      else unclass(round(result.joint$summary.random$trend2$sd, 4))
      
      
      #OUTPUTS
      results.vector=c(dic.joint, waic.joint, LCPO.joint,
                       post.means,
                       post.sd,
                       post.seas.mean,
                       post.seas.sd,
                       post.cyc.mean,
                       post.cyc.sd,
                       post.trend.mean,
                       post.trend.sd)
      result.matrix=cbind(result.vector)
      return(result.matrix)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 

