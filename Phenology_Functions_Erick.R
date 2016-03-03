
#Two sample Kolmogorov-Smornov Test
Kolmogorov.Smirnov.Test.Fun <- function(s1, s2) {
  ks_test<- function(v) {
    require(stats)
    obs <- v[1:split]
    sim <- v[(split+1):(2*split)]
    ks.est <- try(ks.test(sim=sim, obs=obs), silent=TRUE) 
    
    D.statistic=if(class(ks.est)=="try-error") NA 
    else unclass(round(ks.est$statistic["D"], digits = 4))
    D.statistic.sig=if(class(ks.est)=="try-error") NA 
    else unclass(round(ks.est$p.value, digits = 4))
    ks.result = cbind(D.statistic, D.statistic.sig)
    return(ks.result)
  }
  require(raster)
  s <- stack(s1, s2)
  split <- nlayers(s)/2
  ks.result <- calc(s, fun=ks_test)
  return(ks.result)
}

Theil_Sen_slope <- function(stk) {
  funTheilSen=function(v) {
    new.y <- v[1:size]
    
    require(stats)
    #seasonal decomposition of time series by Loess (into trend, season, remainder)
    y.ts <- ts(new.y, frequency=24, start=c(1982,1), end=c(2011, 24))
    mySTLdata <- stl(y.ts, s.window="periodic")
    
    #coersing the stl object into a dataframe
    mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
    y <- as.vector(mySTLdata.DF1[ ,"trend"])
    x = 1:size/size
    
    #The formula take all these things into account
    #compute slope with theilSen and sig with Mankendall
    require(rkt)
    fit.slope = try(rkt(date=x, y=y), silent=TRUE)
    
    theils.slope = if(class(fit.slope)=="try-error") NA 
    else unclass(round(fit.slope$B, digits = 4))
    theils.slope.sl = if(class(fit.slope)=="try-error") NA 
    else unclass(round(fit.slope$sl, digits = 4))
    
    TheilSen.result = cbind(theils.slope, theils.slope.sl)
    return(TheilSen.result)
    
  }    
  s <- stack(stk)
  size <- nlayers(s)
  Final.result <- calc(s, fun=funTheilSen)
  return(Final.result)
} 

#Works but too slow
PhenologyParFunction2 <- function(x) {
  k = 360; nyears=30; npars = 17
  new.y=as.vector(x)
  
  if (is.na(new.y[1])) {
    
    return(rep(NA, times = npars))
    
  } else {
    
    #seasonal decomposition of time series by Loess (into trend, season, remainder)
    require(stats); require(zoo) 
    y.ts <- ts(new.y, frequency=12, start=c(1982,1), end=c(2011,12))
    mySTLdata <- stl(y.ts, s.window="periodic")
    
    #coersing the stl object into a dataframe
    mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
    y <- as.vector(mySTLdata.DF1[ ,"seasonal"])
    
    mu1=rep(1, times=k)
    idxmu = rep(rep(1:12), times=nyears);
    idxsin = rep(rep(1:12), times=nyears); 
    idxcos = rep(rep(1:12), times=nyears);
    
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
    return(results.matrix) 
    
  }
}


PhenologyParFunction <- function(x) {
      k = 360; nyears=30; npars = 17
      new.y=as.vector(x)
  
      if (is.na(new.y[1])) {
    
    return(rep(NA, times = npars))
    
  } else {
    
    #seasonal decomposition of time series by Loess (into trend, season, remainder)
    require(stats); require(zoo) 
    y.ts <- ts(new.y, frequency=12, start=c(1982,1), end=c(2011,12))
    mySTLdata <- stl(y.ts, s.window="periodic")
    
    #coersing the stl object into a dataframe
    mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
    y <- as.vector(mySTLdata.DF1[ ,"seasonal"])
    
    mu1=rep(1, times=k)
    idxmu = rep(rep(1:12), times=nyears);
    idxsin = rep(rep(1:12), times=nyears); 
    idxcos = rep(rep(1:12), times=nyears);
    
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
    return(results.matrix) 
    
  }
}


PhenologyFunctionRevised <- function(stk) {
  PhenologyFunction <- function(v) {
    yy <- v[1:size]
    new.y <- as.vector(yy)
    k = 360; nyears=30; npars = 17
    
    if (is.na(new.y[1])) {
      
      return(rep(NA, times = npars))
      
    } else {
      
      #seasonal decomposition of time series by Loess (into trend, season, remainder)
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
      return(results.matrix) 
      
    }
  }
  s = stack(stk)
  size = nlayers(s)
  output.stk = calc(s, fun=PhenologyFunction)  
  return(output.stk)
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   SPDE model for computing Phenology parameters
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

PhenologyFunction <- function(v) {
  #old.y = v[1:size];
  new.y = as.vector(v)
  k = 360; nyears=30; npars = 22
  
  if (is.na(new.y[1])) {
    
    return(rep(NA, times = npars))
    
  } else {
    
    #seasonal decomposition of time series by Loess (into trend, season, remainder)
    require(stats) 
    y.ts <- ts(new.y, frequency=12, start=c(1982,1), end=c(2011,12))
    mySTLdata <- stl(y.ts, s.window="periodic")
    
    #coersing the stl object into a dataframe
    mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
    y <- mySTLdata.DF1[ ,"seasonal"]
    
    #Time varrying covariates
    sin.amplitude <- as.vector(rep(sin(30 * (2*pi) * (1:k/k)))) ##30 cycles
    cos.amplitude <- as.vector(rep(cos(30 * (2*pi) * (1:k/k)))) 
    
    require(INLA); require(raster)
    
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
    #require(MODIS)
    #require(snow) # Might require snow package
    #beginCluster(type="SOCK",exclude="MODIS") # See: ?beginCluster
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
    return(results.matrix) 
    
  }
}

spdePhenologyFunction <- function(stk) {
  PhenologyFunction <- function(v) {
    require(reshape)
    new.y <- melt(v[1:size], na.rm=FALSE)$value
    k = 360; nyears=30; npars = 22
    
    if (is.na(new.y[1])) {
      
      return(rep(NA, times = npars))
      
    } else {
      
      #seasonal decomposition of time series by Loess (into trend, season, remainder)
      require(stats); require(zoo) 
      y.ts <- ts(new.y, frequency=12, start=c(1982,1), end=c(2011,12))
      mySTLdata <- stl(y.ts, s.window="periodic")
      
      #coersing the stl object into a dataframe
      mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
      y <- mySTLdata.DF1[ ,"seasonal"]
      
     #Time varrying covariates
      sin.amplitude <- as.vector(rep(sin(30 * (2*pi) * (1:k/k)))) ##30 cycles
      cos.amplitude <- as.vector(rep(cos(30 * (2*pi) * (1:k/k)))) 
      
      require(INLA); require(raster)
      
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
      return(results.matrix) 
      
   }
  }
  s = stack(stk)
  size = nlayers(s)
  output.stk = calc(s, fun=PhenologyFunction) 
  #require(bfastSpatial)
  #output.stk = mc.calc(s, fun=PhenologyFunction, mc.cores = 1) 
  return(output.stk)
}


spdePhenologyFunctionOld <- function(stk, ncircle=30) {
  require(INLA); require(raster)
  PhenologyFunction <- function(v) {
    old.y = v[1:size];
    new.y = as.vector(old.y);
    
    #seasonal decomposition of time series by Loess (into trend, season, remainder)
    require(stats) 
    y.ts <- ts(new.y, frequency=12, start=c(1982,1), end=c(2011,12))
    mySTLdata <- stl(y.ts, s.window="periodic")
    
    #coersing the stl object into a dataframe
    mySTLdata.DF1 <- as.data.frame(mySTLdata$time.series)
    y <- mySTLdata.DF1[ ,"seasonal"]
    
    #Time varrying covariates
    sin.amplitude <- as.vector(rep(sin(ncircle * (2*pi) * (1:k/k)))) ##30 cycles
    cos.amplitude <- as.vector(rep(cos(ncircle * (2*pi) * (1:k/k)))) 
    
    k = ncircle; npars = 11
    
    require(INLA)
    
    #developing the spde model functionality
    sigma0 = 1; kappa0 = 1e-3; tau0 = 1/(4*kappa0^3*sigma0^2)^0.5;
    knots.seasonal = seq(1, 12, by = 4); 
    month=rep(rep(1:12), times=k)
    
    #creating 1D mesh for temporal spde
    mesh.seasonal = inla.mesh.1d(knots.seasonal, 
                                 interval=c(0, 12), 
                                 degree=2,
                                 boundary="cyclic");
    
    #With mesh, we define the SPDE model using the function inla.spde2.matern()
    spde.seasonal = inla.spde2.matern(mesh.seasonal,
                                      constr=TRUE, 
                                      B.tau = cbind(log(tau0), 1),
                                      B.kappa = cbind(log(kappa0), 0),
                                      theta.prior.prec = 1e-4);
    
    #setting of index for each call of the f() function
    i0 = inla.spde.make.index("i0", n.spde=spde.seasonal$n.spde, n.group=k);
    i1 = inla.spde.make.index("i1", n.spde=spde.seasonal$n.spde, n.group=k);
    i2 = inla.spde.make.index("i2", n.spde=spde.seasonal$n.spde, n.group=k);
    
    
    #time projector matrix A is defined as follows
    A0 <- inla.spde.make.A(mesh, loc = month, group=rep(1:k, each=12))
    A1 <- inla.spde.make.A(mesh, loc = month, group=rep(1:k, each=12), weights=sin.amplitude)
    A2 <- inla.spde.make.A(mesh, loc = month, group=rep(1:k, each=12), weights=cos.amplitude)
    
    #putting all these together using stack functionality, we have 
    stk <- inla.stack(data=list(y=as.vector(y)), tag='y',
                      A=list(A0, A1, A2, 1),
                      effects=list(i0, i1, i2,
                                   data.frame(mu1=1, 
                                              sin.amplitude=sin.amplitude,
                                              cos.amplitude=cos.amplitude)))
    
    #The formula take all these things into account
    formula <- y ~ 0 + mu1 + sin.amplitude + cos.amplitude + ### to fit mu_beta
      f(i0, model=spde, group=i0.group, control.group=list(model='ar1')) +
      f(i1, model=spde, group=i1.group, control.group=list(model='ar1')) +
      f(i2, model=spde, group=i2.group, control.group=list(model='ar1'))
    
    if (all(is.na(y))) {
      
      return(rep(NA, times = npars))
      
    } else {
      
      #inla call:
      result = INLA::inla(formula,
                          family="gaussian", 
                          data=inla.stack.data(stk), 
                          control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
                          control.predictor=list(A=inla.stack.A(stk), compute=TRUE),
                          control.inla = list(strategy = "gaussian"),
                          verbose=TRUE)
      
      
      #Summary of the posterior marginal density for the temporal correlations
      rho_beta0=if(class(result)=="try-error") NA 
      else unclass(result$summary.hy[3,1])
      rho_beta1=if(class(result)=="try-error") NA 
      else unclass(result$summary.hy[5,1])
      rho_beta2=if(class(result)=="try-error") NA 
      else unclass(result$summary.hy[7,1])
      
      #The estimated phenology parameter
      beta0=if(class(result)=="try-error") NA 
      else unclass(result$summary.fix[1,1])
      sin_beta1=if(class(result)=="try-error") NA 
      else unclass(result$summary.fix[2,1])
      cos_beta2=if(class(result)=="try-error") NA
      else unclass(result$summary.fix[3,1])
      Amplitude=if(class(result)=="try-error") NA 
      else unclass (sqrt(sin_beta1^2 + cos_beta2^2))
      Phase=if(class(result)=="try-error") NA 
      else unclass (asin(sin_beta1/Amplitude))
      
      #Model validation parameter estimates
      dic=if(class(result)=="try-error") NA 
      else unclass (result$dic$dic)
      waic=if(class(result)=="try-error") NA 
      else unclass (result$waic$waic)
      logCPO=if(class(result)=="try-error") NA 
      else unclass (sum(-log(result$cpo$cpo), na.rm=TRUE))
      
      #Organizing the outputs in matrix for raster stacking  
      results.matrix=cbind(beta0=beta0, sin_beta1=sin_beta1, 
                           cos_beta2=cos_beta2, Amplitude=Amplitude,
                           Phase=Phase, rho_beta0=rho_beta0,
                           rho_beta1=rho_beta1, rho_beta2=rho_beta2,
                           dic=dic, waic=waic, logCPO=logCPO) 
      return(results.matrix) 
      
      
    }
  }
  s = stack(stk)
  size = nlayers(s)
  output.stk = calc(s, fun=PhenologyFunction)
  return(output.stk)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Phenology metrics computed using Harmonic Dynamic
#linear model based on the SPDE approach
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
phenology_metrics_func <- function(stk, sin.amplitude, cos.amplitude, spde,
                                   A0, A1, A2, i0, i1, i2) {
  pheno_fun<- function(v) {
    require(INLA)
    x <- v[1:size]
    y=as.vector(x)
    stk.y <- inla.stack(data=list(y=y), tag='y',
                        A=list(A0, A1, A2, 1),
                        effects=list(i0, i1, i2,
                                     data.frame(mu1=1, sine=sin.amplitude, 
                                                cos=cos.amplitude)))
    
    if (all(is.na(y))) {
      
      return(rep(NA, times = npars))
      
    } else {
      
      formula <- y ~ 0 + mu1 + sine + cos + 
        f(i0, model=spde, group=i0.group, control.group=list(model='ar1')) +
        f(i1, model=spde, group=i1.group, control.group=list(model='ar1')) +
        f(i2, model=spde, group=i2.group, control.group=list(model='ar1'))
      res = try(inla(formula, family='gaussian', data=inla.stack.data(stk.y),
                     control.predictor=list(A=inla.stack.A(stk.y), compute=TRUE),
                     control.compute = list(config=FALSE, dic=FALSE, waic=FALSE),
                     verbose=TRUE), silent=TRUE)
      
      #Summary of the posterior marginal density for the temporal correlations
      rho_annual_mean=if(class(res)=="try-error") NA else unclass(res$summary.hy[3,1])
      rho_sin_amplitude=if(class(res)=="try-error") NA else unclass(res$summary.hy[5,1])
      rho_cos_amplitude=if(class(res)=="try-error") NA else unclass(res$summary.hy[7,1])
      
      #The estimated phenology parameter
      annual_mean=if(class(res)=="try-error") NA else unclass(res$summary.fix[1,1])
      sin_amplitude=if(class(res)=="try-error") NA else unclass(res$summary.fix[2,1])
      cos_amplitude=if(class(res)=="try-error") NA else unclass(res$summary.fix[3,1])
      annual_amplitude=if(class(res)=="try-error") NA else unclass (sqrt(res$summary.fix[2, 1]^2 + res$summary.fix[3, 1]^2))
      annual_phase=if(class(res)=="try-error") NA else unclass (atan(-(res$summary.fix[2, 1]/res$summary.fix[3, 1])))
      
      results.matrix=cbind(annual_mean, sin_amplitude, cos_amplitude, annual_amplitude, annual_phase, 
                           rho_annual_mean, rho_sin_amplitude, rho_cos_amplitude)
      return(results.matrix)
    } 
  }
  
  size = nlayers(stk)
  npars = 8                  #the number of require metrics to be computed
  result = calc(stk, fun=pheno_fun)
  return(result)
} 

r2.function.old <- function(obs.stk, sim.stk) {
  r2<- function(v) {
    require(hydromad); require(zoo)
    x <- v[1:split]
    y <- v[(split+1):(2*split)]
    xx=ts(x, start=1982, end=2011, frequency = 12)
    yy=ts(y, start=1982, end=2011, frequency = 12)
    idx <- seq(as.Date('1982-01-01'), as.Date('2011-12-31'), 'month')
    Q <- zoo(xx, idx)
    X <- zoo(yy, time(Q))
    r2.est <- try(nseStat(Q, X, complete.cases=TRUE, ref = ave(Q, months(time(Q)))))   
    if (class(r2.est)=="try-error") NA else unclass(r2.est)[1]
  }
  s <- stack(obs.stk, sim.stk)
  split <- nlayers(s)/2
  r2.result<-calc(s, fun=r2)
  return(r2.result)
} 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Bayesian Dynamic linear model with harmonics Function
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#Do not run this
phenology_DLM <- function(stk, nmonths=12, nyears=1, ncircle=2) {
  pheno_fun<- function(v) {
    y1=as.vector(v[1:size])
    
    if (all(is.na(y1))) {
      
      return(rep(NA, times = 8))
      
    } else {
      require(INLA)
      res = try(inla(y~mu1+sin1+cos1+
                       f(idxmu, mu, model="ar1",constr=TRUE) +
                       f(idxsin, sin,  model="ar1", cyclic=FALSE, constr=TRUE)+
                       f(idxcos, cos,  model="ar1", cyclic=FALSE, constr=TRUE)-1,
                     family="gaussian",
                     data=data.frame(y=y1, mu1=mu, idxmu=idxmu,
                                     idxsin=idxsin, idxcos=idxcos, sin1=sin, cos1=cos),
                     control.predictor=list(compute=TRUE),
                     control.compute=list(config=TRUE),
                     verbose=TRUE), silent=TRUE)
      
      #Summary of the posterior marginal density for the temporal correlations
      rho_annual_mean=if(class(res)=="try-error") NA 
      else unclass(res$summary.hy[3,1])
      rho_cos_amplitude=if(class(res)=="try-error") NA 
      else unclass(res$summary.hy[5,1])
      rho_sin_amplitude=if(class(res)=="try-error") NA 
      else unclass(res$summary.hy[7,1])
      
      #The estimated phenology parameter
      annual_mean=if(class(res)=="try-error") NA 
      else unclass(res$summary.fix[1,1])
      sin_amplitude=if(class(res)=="try-error") NA 
      else unclass(res$summary.fix[2,1])
      cos_amplitude=if(class(res)=="try-error") NA
      else unclass(res$summary.fix[3,1])
      annual_amplitude=if(class(res)=="try-error") NA 
      else unclass (sqrt(res$summary.fix[2, 1]^2 + res$summary.fix[3, 1]^2))
      annual_phase=if(class(res)=="try-error") NA 
      else unclass (ifelse(res$summary.fix[3, 1] < 0,
                           (atan(res$summary.fix[2, 1]/res$summary.fix[3, 1])+1),
                           atan(res$summary.fix[2, 1]/res$summary.fix[3, 1])))
      
      results.matrix=cbind(annual_mean, sin_amplitude, cos_amplitude, annual_amplitude, annual_phase, 
                           rho_annual_mean, rho_sin_amplitude, rho_cos_amplitude)
      return(results.matrix)
    } 
  }
  
  #Time varrying covariates
  sin.amplitude <- as.vector(rep(sin(ncircle * (2*pi) * (1:k/k)))) ### just 2 cycles
  cos.amplitude <- as.vector(rep(cos(ncircle * (2*pi) * (1:k/k)))) ### just 2 cycles
  n <- nyears    
  k <- nmonths 
  mu=rep(1, times=k)
  mu0=rep(1:k)
  mu1=rep(1:k)
  mu2=rep(1:k)
  idx <- seq(as.Date('1982-01-01'), as.Date('1982-12-31'), 'month')
  sin <- zoo(sin.amplitude, idx)
  cos <- zoo(cos.amplitude, time(sin))
  mu <- zoo(mu, time(sin))
  idxmu <- zoo(mu0, time(sin))
  idxsin <- zoo(mu1, time(sin))
  idxcos <- zoo(mu2, time(cos))
  size = nlayers(stk)
  npars = 8                  
  result = calc(stk, fun=pheno_fun)
  return(result)
} 

#Run This
phenology_DLM2 <- function(stk, nmonths=360, nyears=30, ncircle=30) {
  pheno_fun<- function(v) {
    y1=as.vector(v[1:size])
    
    if (all(is.na(y1)) | length(y1[!is.na(y1)]) <= 1 ) {
      
      return(rep(NA, times = 8))
      
    } else {
      require(INLA)
      res = try(inla(y~mu1+sin1+cos1+
                       f(idxmu, mu1, model="ar1",constr=TRUE) +
                       f(idxsin, sin1,  model="ar1", cyclic=FALSE, constr=TRUE)+
                       f(idxcos, cos1,  model="ar1", cyclic=FALSE, constr=TRUE)-1,
                     family="gaussian",
                     data=data.frame(y=y1, mu1=mu1, idxmu=idxmu,
                                     idxsin=idxsin, idxcos=idxcos, sin1=sin.amplitude1, cos1=cos.amplitude1),
                     control.predictor=list(compute=TRUE),
                     control.compute=list(config=FALSE),
                     verbose=TRUE), silent=TRUE)
      
      #Summary of the posterior marginal density for the temporal correlations
      rho_annual_mean=if(class(res)=="try-error") NA 
      else unclass(res$summary.hy[3,1])
      rho_cos_amplitude=if(class(res)=="try-error") NA 
      else unclass(res$summary.hy[5,1])
      rho_sin_amplitude=if(class(res)=="try-error") NA 
      else unclass(res$summary.hy[7,1])
      #The estimated phenology parameter
      annual_mean=if(class(res)=="try-error") NA 
      else unclass(res$summary.fix[1,1])
      sin_amplitude=if(class(res)=="try-error") NA 
      else unclass(res$summary.fix[2,1])
      cos_amplitude=if(class(res)=="try-error") NA
      else unclass(res$summary.fix[3,1])
      annual_amplitude=if(class(res)=="try-error") NA 
      else unclass (sqrt(res$summary.fix[2, 1]^2 + res$summary.fix[3, 1]^2))
      annual_phase=if(class(res)=="try-error") NA 
      else unclass (ifelse(res$summary.fix[3, 1] < 0,
                           (atan(res$summary.fix[2, 1]/res$summary.fix[3, 1])+1),
                           atan(res$summary.fix[2, 1]/res$summary.fix[3, 1])))
      
      results.matrix=c(annual_mean, sin_amplitude, cos_amplitude, annual_amplitude, annual_phase, 
                       rho_annual_mean, rho_sin_amplitude, rho_cos_amplitude)
      return(results.matrix)
    } 
  }
  
  #Time varrying covariates
  sin.amplitude1 <- as.vector(rep(sin(ncircle * (2*pi) * (1:k/k)))) ### just 2 cycles
  cos.amplitude1 <- as.vector(rep(cos(ncircle * (2*pi) * (1:k/k)))) ### just 2 cycles
  n = nyears; k = nmonths 
  mu1=rep(1, times=k)
  idxmu = rep(1:k); idxsin = rep(1:k); idxcos = rep(1:k); size = nlayers(stk)
  npars = 8 
  result = calc(stk, fun=pheno_fun, na.rm=FALSE, forcefun=FALSE, forceapply=FALSE)
  return(result)
} 

#Run this function with calc function in raster package: tested and works
pheno_fun<- function(x, nmonths=360, nyears=30, ncircle=30) {
  y1=as.vector(x)
  
  #Time varrying covariates
  sin.amplitude1 <- as.vector(rep(sin(ncircle * (2*pi) * (1:k/k)))) ### just 2 cycles
  cos.amplitude1 <- as.vector(rep(cos(ncircle * (2*pi) * (1:k/k)))) ### just 2 cycles
  n = nyears; k = nmonths 
  mu1=rep(1, times=k)
  idxmu = rep(1:k); idxsin = rep(1:k); idxcos = rep(1:k); size = length(y1)
  npars = 8 
  if (all(is.na(y1)) | length(y1[!is.na(y1)]) <= 1 ) {
    
    return(rep(NA, times = 8))
    
  } else {
    require(INLA)
    res = try(inla(y~mu1+sin1+cos1+
                     f(idxmu, mu1, model="ar1",constr=TRUE) +
                     f(idxsin, sin1,  model="ar1", cyclic=FALSE, constr=TRUE)+
                     f(idxcos, cos1,  model="ar1", cyclic=FALSE, constr=TRUE)-1,
                   family="gaussian",
                   data=data.frame(y=y1, mu1=mu1, idxmu=idxmu,
                                   idxsin=idxsin, idxcos=idxcos, 
                                   sin1=sin.amplitude1,
                                   cos1=cos.amplitude1),
                   control.predictor=list(compute=TRUE),
                   control.compute=list(config=FALSE),
                   verbose=TRUE), silent=TRUE)
    
    #Summary of the posterior marginal density for the temporal correlations
    rho_annual_mean=if(class(res)=="try-error") NA 
    else unclass(res$summary.hy[3,1])
    rho_cos_amplitude=if(class(res)=="try-error") NA 
    else unclass(res$summary.hy[5,1])
    rho_sin_amplitude=if(class(res)=="try-error") NA 
    else unclass(res$summary.hy[7,1])
    
    #The estimated phenology parameter
    annual_mean=if(class(res)=="try-error") NA 
    else unclass(res$summary.fix[1,1])
    sin_amplitude=if(class(res)=="try-error") NA 
    else unclass(res$summary.fix[2,1])
    cos_amplitude=if(class(res)=="try-error") NA
    else unclass(res$summary.fix[3,1])
    annual_amplitude=if(class(res)=="try-error") NA 
    else unclass (sqrt(res$summary.fix[2, 1]^2 + res$summary.fix[3, 1]^2))
    annual_phase=if(class(res)=="try-error") NA 
    else unclass (ifelse(res$summary.fix[3, 1] < 0,
                         (atan(res$summary.fix[2, 1]/res$summary.fix[3, 1])+1),
                         atan(res$summary.fix[2, 1]/res$summary.fix[3, 1])))
    
    results.matrix=cbind(annual_mean=annual_mean, sin_amplitude=sin_amplitude, 
                         cos_amplitude=cos_amplitude, annual_amplitude=annual_amplitude,
                         annual_phase=annual_phase,rho_annual_mean=rho_annual_mean,
                         rho_sin_amplitude=rho_sin_amplitude, rho_cos_amplitude=rho_cos_amplitude)
    return(results.matrix)
  } 
}

#Dynamic linear model with harmonics for smoothing using INLA
DLM_Smoothing_Function_inla <- function(stk) {
  DLM_inla_Fun<- function(v) {
    y1 <- as.vector(v[1:size])
    k=360
    sin <- as.vector(rep(sin(30 * (2*pi) * (1:360/360)))) ### just 2 cycles
    cos <- as.vector(rep(cos(30 * (2*pi) * (1:360/360)))) ### just 2 cycle
    mu1=rep(1, times=size)
    idxmu = rep(1:360); idxsin = rep(1:360); idxcos = rep(1:360)
    
    if (all(is.na(y1))) {
      
      return(rep(NA, times = 360))
      
    } else {
      
      require(INLA)
      require(zoo)
      
      y=na.spline(y1)
      res = try(inla(y ~ mu1 + sin + cos +
                       f(idxmu, mu1, model="ar1",constr=TRUE) +
                       f(idxsin, sin,  model="ar1", cyclic=FALSE, constr=TRUE)+
                       f(idxcos, cos,  model="ar1", cyclic=FALSE, constr=TRUE)-1,
                     family="gaussian",
                     data=data.frame(y, mu1, idxmu, idxsin, idxcos, sin, cos),
                     control.predictor=list(compute=TRUE),
                     control.compute=list(config=FALSE),
                     verbose=TRUE), silent=TRUE)
      
      predicted_means = if(class(res)=="try-error") rep(NA, times=size)
      else unclass(res$summary.fitted.values[, "mean"])
      return(predicted_means)
    } 
  }
  require(raster)
  size = nlayers(stk)
  result = calc(stk, fun=DLM_inla_Fun)
  return(result)
  
} 


# #INLA DLM with apply function rather than raster calc function
# DLM_Smoothing_Function_inla <- function(df_wide, k=360) {
#   inla_DLM_Fun<- function(y) {
#     y1=melt(y, na.rm=FALSE)$value
#     
#     if (all(is.na(y1))) {
#       
#       return(rep(NA, times = length(y1)))
#       
#     } else {
#       require(INLA)
#       res = try(inla(y~mu1+sin1+cos1+
#                        f(idxmu, mu1, model="ar1",constr=TRUE) +
#                        f(idxsin, sin1,  model="ar1", cyclic=FALSE, constr=TRUE)+
#                        f(idxcos, cos1,  model="ar1", cyclic=FALSE, constr=TRUE)-1,
#                      family="gaussian",
#                      data=data.frame(y=y1, mu1=mu1, idxmu=idxmu,
#                                      idxsin=idxsin, idxcos=idxcos,
#                                      sin1=sin.amplitude1, cos1=cos.amplitude1),
#                      control.predictor=list(compute=TRUE),
#                      control.compute=list(config=FALSE),
#                      verbose=TRUE), silent=TRUE)
#       
#       predicted_means = if(class(res)=="try-error") NA 
#       else unclass(res$summary.fitted.values[, "mean"])
#       return(predicted_means)
#     } 
#   }
#   
#   #Time varrying covariates
#   size = nrow(df_wide)
#   sin.amplitude1 <- as.vector(rep(sin(30 * (2*pi) * (1:k/k)))) ### just 2 cycles
#   cos.amplitude1 <- as.vector(rep(cos(30 * (2*pi) * (1:k/k)))) ### just 2 cycle
#   mu1=rep(1, times=size)
#   idxmu = rep(1:k); idxsin = rep(1:k); idxcos = rep(1:k)
#   result = foreach(row=1:nrow(df_wide), .combine=rbind, .verbose=TRUE) %do%
#     inla_DLM_Fun(df_wide[[row]])
#   return(result)
#   
# } 

#Non DLM smoothing function
Non_DLM_smoothing_Function_inla <- function(stk) {
  
  Non_DLM_inla_fun<- function(v) {
    
    y1 <- as.vector(v[1:size])
    k=360
    mu1=rep(1, times=360)
    sin <- as.vector(rep(sin(30 * (2*pi) * (1:360/360)))) ### just 2 cycles
    cos <- as.vector(rep(cos(30 * (2*pi) * (1:360/360)))) ### just 2 cycle
    idx = rep(1:360) 
    
    if (all(is.na(y1))) {
      
      return(rep(NA, times = 360))
      
    } else {
      
      require(INLA)
      require(zoo)
      
      y=na.spline(y1)
      
      res = try(inla(y ~ sin + cos +
                       f(idx, model="ar1",constr=TRUE)-1,
                     family="gaussian",
                     data=data.frame(y, idx, sin, cos),
                     control.predictor=list(compute=TRUE),
                     control.compute=list(config=FALSE),
                     verbose=TRUE), silent=TRUE)
      
      predicted_means=if(class(res)=="try-error") rep(NA, times=size) 
      else unclass(res$summary.fitted.values[, "mean"])
      return(predicted_means)
    } 
  }
  require(raster)
  size = nlayers(stk)
  result = calc(stk, fun=Non_DLM_inla_fun)
  return(result)
} 

DLM_kalman_filter <- function(stk) {
  
  kalman_DLM_Fun <- function(v) {
    
    y1=as.vector(v[1:size])
    
    if (all(is.na(y1))) {
      
      return(rep(NA, times = 360))
      
    } else {
      require(dlm)
      require(zoo)
      
      y=na.spline(y1)
      y.ts <- as.ts(y, start=1982, end=2011, frequency = 12)
      
      build.1<-function(theta){
        dlmModPoly(order=1,dV=exp(theta[1]),dW=exp(theta[2]))
      }
      
      fit.1<-dlmMLE(y.ts, parm=c(1, 1), build.1)
      updated.build <- build.1(fit.1$par)
      kalman.Filt.model<-dlmFilter(y.ts, updated.build)
      y.pred=as.numeric(kalman.Filt.model$m)
      y.pred.corrected=y.pred[-(1)]
      return(y.pred.corrected)
    } 
  }
  
  size = nlayers(stk)
  result = calc(stk, fun=kalman_DLM_Fun)
  return(result)
  
} 

DLM_kalman_filter2 <- function(stk, begin.year=1981, end.year=2011, freq=12) {
  
  kalman_DLM_Fun <- function(v) {
    
    y1=as.vector(v[1:size])
    
    if (all(is.na(y1))) {
      
      return(rep(NA, times = size))
      
    } else {
      require(dlm)
      require(zoo)
      
      y=na.spline(y1)
      y.ts <- as.ts(y, start=begin.year, end=end.year, frequency = freq)
      
      build.1<-function(theta){
        dlmModPoly(order=1,dV=exp(theta[1]),dW=exp(theta[2]))
      }
      
      fit.1<-dlmMLE(y.ts, parm=c(1, 1), build.1)
      updated.build <- build.1(fit.1$par)
      kalman.Filt.model<-dlmFilter(y.ts, updated.build)
      y.pred=as.numeric(kalman.Filt.model$m)
      y.pred.corrected=y.pred[-(1)]
      return(y.pred.corrected)
    } 
  }
  
  size = nlayers(stk)
  result = calc(stk, fun=kalman_DLM_Fun)
  return(result)
  
} 

#Non DLM with bayesX package
Non_DLM_bayesX_function <-  function(stk, k=360) {
  bayesX_fun<- function(v) {
    y1=as.vector(v[1:size])
    
    if (all(is.na(y1))) {
      
      return(rep(NA, times = length(y1)))
      
    } else {
      require(R2BayesX)
      require(zoo)
      y1=na.spline(y1)
      res = try(bayesx(y~sin1+cos1+
                         sx(idx, bs = "rw1"),
                       family="gaussian",
                       data=data.frame(y=y1, idx=idx, 
                                       sin1=sin, cos1=cos),
                       verbose=TRUE), silent=TRUE)
      
      predicted_means=if(class(res)=="try-error") NA 
      else unclass(predict(res, type = "response", digits = 4))
      return(predicted_means)
    } 
  }
  size = nlayers(stk)
  sin = as.vector(rep(sin(30 * (2*pi) * (1:k/k)))) ### just 2 cycles
  cos = as.vector(rep(cos(30 * (2*pi) * (1:k/k)))) ### just 2 cycle
  idx = rep(1:size) 
  result = calc(stk, fun=bayesX_fun, na.rm=FALSE, forcefun=FALSE, forceapply=FALSE)
  return(result)
} 

#DLM with bayesX package
DLM_bayesX_function <-  function(stk, k=360) {
  bayesX_fun<- function(v) {
    y1=as.vector(v[1:size])
    
    if (all(is.na(y1))) {
      
      return(rep(NA, times = length(y1)))
      
    } else {
      require(R2BayesX)
      require(zoo)
      y1=na.spline(y1)
      res = try(bayesx(y~sin1+cos1+
                         sx(idxmu, bs = "rw1") +
                         sx(idxsin, by=sin1, bs = "rw1") +
                         sx(idxcos, by=cos1,   bs = "rw1"),
                       family="gaussian",
                       data=data.frame(y=y1, idxmu=idxmu,
                                       idxsin=idxsin, idxcos=idxcos, 
                                       sin1=sin, cos1=cos),
                       verbose=TRUE), silent=TRUE)
      
      predicted_means=if(class(res)=="try-error") NA 
      else unclass(predict(res, type = "response", digits = 4))
      return(predicted_means)
    } 
  }
  size = nlayers(stk)
  sin.amplitude1 <- as.vector(rep(sin(30 * (2*pi) * (1:k/k)))) ### just 2 cycles
  cos.amplitude1 <- as.vector(rep(cos(30 * (2*pi) * (1:k/k)))) ### just 2 cycle
  mu1=rep(1, times=size)
  idxmu = rep(1:k); idxsin = rep(1:k); idxcos = rep(1:k) 
  result = calc(stk, fun=bayesX_fun, na.rm=FALSE, forcefun=FALSE, forceapply=FALSE)
  return(result)
} 


#Savitzky Golay smoother
funsgolaymonthly<- function(stk) {
  fun.smoother.sgolay<- function(v) {
    x <- v[1:size]
    x.vector=as.vector(x)
    begin.year=1982
    end.year=2011
    lowest=1
    freq=12
    if (all(is.na(x.vector))) {
      return(rep(NA, times = size))
      
    } else {
      
      require(zoo)
      x.vector=na.spline(x.vector)
      evi2.ts2 = stats::ts(x.vector,
                           start=c(begin.year,lowest),
                           end=c(end.year,freq),
                           frequency=freq)
      require(signal)
      return(signal::sgolayfilt(evi2.ts2,
                                p=1, n=3, ts=30))
    }
  }
  require(raster)
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=fun.smoother.sgolay)
  return(result)
}

#percentage of non missing records
obs<-function(x){
  no.available=length(x[!is.na(x)]) 
  percent.available=(no.available/length(x)) *100
  return(percent.available)
}


#RMSE using hydroGOF package
funrmse <- function(obs.stk, sim.stk) {
  rmse.fun <- function(v) {
    
    require(hydroGOF); require(raster)
    obs <- as.vector(v[1:split])
    sim <- as.vector(v[(split+1):(2*split)])
    
    if (is.na(obs)) {
      
      return(NA)
      
    } else {
    
    rmse.est <- try(mse(obs=obs, sim=sim), silent=TRUE)   
    if (class(rmse.est)=="try-error") NA else unclass(rmse.est)
    return(rmse.est)
    }
  }
  s <- stack(obs.stk, sim.stk)
  split <- nlayers(s)/2
  rmse.result<-calc(s, fun=rmse.fun)
  return(rmse.result)
} 

#Coefficient of Efficiency using hydromad package
E.function <- function(obs.stk, sim.stk) {
  E<- function(v) {
    require(hydroGOF)
    obs1 <- v[1:split]
    sim1 <- v[(split+1):(2*split)]
    
    if (all(is.na(obs1))) {
      return(rep(NA, times = 1))
      
    } else {
    
    obs = ifelse(obs1 > 1, 1, 
                 ifelse(obs1 < -1, -1, obs1))
    sim = ifelse(sim1 > 1, 1, 
                 ifelse(sim1 < -1, -1, sim1))
    require(zoo)
    idx <- seq(as.Date('1982-01-01'), as.Date('2011-12-31'), 'month')
    obs <- zoo(obs, idx)
    sim <- zoo(sim, idx)
    est <- try(d(obs=obs, sim=sim, na.rm=TRUE))   
    if (class(est)=="try-error") NA else unclass(est)
    return(est)
    }
  }
  s <- stack(obs.stk, sim.stk)
  split <- nlayers(s)/2
  result<-calc(s, fun=E)
  return(result)
} 

#percentage Number of observations available before smoothing
obs.used.fun<-function(stk){
  obs.used<- function(v) {
    require(raster)
    x <- v[1:size]
    no.available=length(x[!is.na(x)])
    result1 <- (no.available/length(x)) *100   
    return(result1)
  }
  size <- nlayers(stk)
  percent.available<-calc(stk, fun=obs.used)
  return(percent.available)
}
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Modified functions to use apply function
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
DLM_inla_Fun2<- function(y) {
  require(reshape)
  y1 <- as.vector(y[1:size])
  #y1=melt(y, na.rm=FALSE)$value
  
  size = length(y1)
  k=360
  sin <- as.vector(rep(sin(30 * (2*pi) * (1:360/360)))) ### just 2 cycles
  cos <- as.vector(rep(cos(30 * (2*pi) * (1:360/360)))) ### just 2 cycle
  mu1=rep(1, times=size)
  idxmu = rep(1:360); idxsin = rep(1:360); idxcos = rep(1:360)
  
  if (all(is.na(y1))) {
    
    return(rep(NA, times = 360))
    
  } else {
    
    require(INLA)
    require(zoo)
    
    yy=na.spline(y1)
    res = try(inla(y~mu1+sin1+cos1+
                     f(idxmu, mu1, model="ar1",constr=TRUE) +
                     f(idxsin, sin1,  model="ar1", cyclic=FALSE, constr=TRUE)+
                     f(idxcos, cos1,  model="ar1", cyclic=FALSE, constr=TRUE)-1,
                   family="gaussian",
                   data=data.frame(y=yy, mu1=mu1, idxmu=idxmu,
                                   idxsin=idxsin, idxcos=idxcos,
                                   sin1=sin, cos1=cos),
                   control.predictor=list(compute=TRUE),
                   control.compute=list(config=FALSE),
                   verbose=TRUE), silent=TRUE)
    
    predicted_means = if(class(res)=="try-error") NA 
    else unclass(res$summary.fitted.values[, "mean"])
    return(predicted_means)
  } 
}

#DLM with bayesX package
DLM_bayesX_fun2<- function(y) {
  require(reshape)
  y1 <- as.vector(y[1:size])
  #y1=melt(y, na.rm=FALSE)$value
  size = length(y1)
  k=360
  sin <- as.vector(rep(sin(30 * (2*pi) * (1:360/360)))) ### just 2 cycles
  cos <- as.vector(rep(cos(30 * (2*pi) * (1:360/360)))) ### just 2 cycle
  mu1=rep(1, times=360)
  idxmu = rep(1:360); idxsin = rep(1:360); idxcos = rep(1:360)
  
  if (all(is.na(y1))) {
    
    return(rep(NA, times = 360))
    
  } else {
    
    require(R2BayesX)
    require(zoo)
    
    yy=na.spline(y1)
    
    res = try(bayesx(y~sin1+cos1+
                       sx(idxmu, bs = "rw1") +
                       sx(idxsin, by=sin1, bs = "rw1") +
                       sx(idxcos, by=cos1,   bs = "rw1"),
                     family="gaussian",
                     data=data.frame(y=yy, idxmu=idxmu,
                                     idxsin=idxsin, idxcos=idxcos, 
                                     sin1=sin, cos1=cos),
                     verbose=TRUE), silent=TRUE)
    
    predicted_means=if(class(res)=="try-error") NA
    else unclass(predict(res, type = "response"))
    return(predicted_means)
  } 
}

Non_DLM_bayesX_fun2<- function(y) {
  require(reshape)
  y1 <- as.vector(y[1:size])
  #y1=melt(y, na.rm=FALSE)$value
  size = length(y1)
  k=360
  sin <- as.vector(rep(sin(30 * (2*pi) * (1:360/360)))) ### just 2 cycles
  cos <- as.vector(rep(cos(30 * (2*pi) * (1:360/360)))) ### just 2 cycle
  idx = rep(1:360) 
  
  if (all(is.na(y1))) {
    
    return(rep(NA, times = 360))
    
  } else {
    
    require(R2BayesX)
    require(zoo)
    
    yy=na.spline(y1)
    
    res = try(bayesx(y~sin1+cos1+
                       sx(idx, bs = "rw1"),
                     family="gaussian",
                     data=data.frame(y=yy, idx=idx, 
                                     sin1=sin, cos1=cos),
                     verbose=TRUE), silent=TRUE)
    
    predicted_means=if(class(res)=="try-error") NA
    else unclass(predict(res, type = "response"))
    return(predicted_means)
  } 
}

Non_DLM_inla_fun2<- function(y) {
  require(reshape)
  y1 <- as.vector(y[1:size])
  #y1=melt(y, na.rm=FALSE)$value
  size = length(y1)
  k=360
  mu1=rep(1, times=360)
  sin <- as.vector(rep(sin(30 * (2*pi) * (1:360/360)))) ### just 2 cycles
  cos <- as.vector(rep(cos(30 * (2*pi) * (1:360/360)))) ### just 2 cycle
  idx = rep(1:360) 
  
  if (all(is.na(y1))) {
    
    return(rep(NA, times = 360))
    
  } else {
    
    require(INLA)
    require(zoo)
    
    yy=na.spline(y1)
    
    res = try(inla(y~mu1+sin1+cos1+
                     f(idx, model="ar1",constr=TRUE)-1,
                   family="gaussian",
                   data=data.frame(y=yy, mu1=mu1, idx=idx, 
                                   sin1=sin, cos1=cos),
                   control.predictor=list(compute=TRUE),
                   control.compute=list(config=FALSE),
                   verbose=TRUE), silent=TRUE)
    
    predicted_means=if(class(res)=="try-error") NA 
    else unclass(res$summary.fitted.values[, "mean"])
    return(predicted_means)
  } 
}

fun.correction <- function(x){
  xx=ifelse(x > 1, 1,
            ifelse(x < -1, -1, x))
}


spde.record.function <- function(obs.stk, sim.stk) {
  f<- function(v) {
    obs0 <- as.vector(v[1:split])
    sim0 <- as.vector(v[(split+1):(2*split)])
    
    obs = ifelse(obs0 > 1, 1,
                   ifelse(obs0 < -1, -1, obs0))
    sim  = ifelse(sim0 > 1, 1,
                   ifelse(sim0 < -1, -1, sim0))
    
    if (all(is.na(obs))) {
      
      return(rep(NA, times = 360))
      
    } else {
    
    est <- try(apply(data.frame(obs, sim), 1, mean, na.rm=TRUE) , silent=TRUE)   
    if (class(est)=="try-error") NA else unclass(est)
    est1 = na.spline(est)
    est2  = ifelse(est1 > 1, 1,
                  ifelse(est1 < -1, -1, est1))
    return(round(est2, 4))
    }
  }
  require(raster)
  s <- stack(obs.stk, sim.stk)
  split <- nlayers(s)/2
  result<-calc(s, fun=f)
  return(result)
}

spDTyn.record.function <- function(sim1.stk, sim2.stk) {
  f2<- function(v) {
    obs0 <- as.vector(v[1:split])
    sim0 <- as.vector(v[(split+1):(2*split)])
    
    obs = ifelse(obs0 > 1, 1,
                 ifelse(obs0 < -1, -1, obs0))
    sim  = ifelse(sim0 > 1, 1,
                  ifelse(sim0 < -1, -1, sim0))
    
    if (all(is.na(obs))) {
      
      return(rep(NA, times = 360))
      
    } else {
      
      est <- try(apply(data.frame(obs, sim), 1, mean, na.rm=TRUE) , silent=TRUE)   
      if (class(est)=="try-error") NA else unclass(est)
      est1 = na.spline(est)
      est2  = ifelse(est1 > 1, 1,
                     ifelse(est1 < -1, -1, est1))
      return(round(est2, 4))
    }
  }
  require(raster)
  s <- stack(sim1.stk, sim2.stk)
  split <- nlayers(s)/2
  result <- calc(s, fun=f2)
  return(result)
} 

funrmse <- function(obs.stk, sim.stk) {
  rmse.fun <- function(v) {
    require(reshape)
    x <- v[1:split]
    y <- v[(split+1):(2*split)]
    obs <- melt(x, na.rm=FALSE)$value
    sim <- melt(y, na.rm=FALSE)$value
    Q=ifelse(obs > 1 | obs < -1, NA, obs)
    X=ifelse(sim > 1 | sim < -1, NA, sim)
    require(hydromad)
    rmse.est <- try(mse(obs=Q, sim=X), silent=TRUE)   
    if (class(rmse.est)=="try-error") NA else unclass(rmse.est)
  }
  s <- stack(obs.stk, sim.stk)
  split <- nlayers(s)/2
  rmse.result<-calc(s, fun=rmse.fun)
  return(rmse.result)
} 

r2.function2 <- function(obs.stk, sim.stk) {
  r2<- function(v) {
    require(reshape)
    x <- v[1:split]
    y <- v[(split+1):(2*split)]
    obs <- melt(x, na.rm=FALSE)$value
    sim <- melt(y, na.rm=FALSE)$value
    Q=ifelse(obs > 1 | obs < -1, NA, obs)
    X=ifelse(sim > 1 | sim < -1, NA, sim)
    require(hydromad)
    r2.est <- try(nseStat(obs=Q, mod=X,  p = 2, na.action = na.pass))   
    if (class(r2.est)=="try-error") NA else unclass(r2.est)[1]
  }
  s <- stack(obs.stk, sim.stk)
  split <- nlayers(s)/2
  r2.result<-calc(s, fun=r2)
  return(r2.result)
} 

obs.used.fun<-function(stk){
  obs.used<- function(v) {
    x <- v[1:size]
    no.available=length(x[!is.na(x)])
    result1 <- (no.available/length(x)) *100   
    return(result1)
  }
  size <- nlayers(stk)
  percent.available<-calc(stk, fun=obs.used)
  return(percent.available)
}


####################################################################################
#    Univariate Bayesian Time series model
###################################################################################
univariate.model<- function(stk) {
  require(INLA);require(raster)
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
      #mean(log(inla.result$cpo))
      
      #OUTPUTS
      results.vector=c(dic.univariate, waic.univariate, LCPO.univariate,
                       round(result.univariate$summary.fitted.values[1:744, "mean"], 4),
                       round(result.univariate$summary.fitted.values[1:744, "sd"], 4),
                       round(result.univariate$summary.random$seasonal.effect$mean, 4),
                       round(result.univariate$summary.random$seasonal.effect$sd, 4),
                       round(result.univariate$summary.random$cyclical.effect$mean, 4),
                       round(result.univariate$summary.random$cyclical.effect$sd, 4),
                       round(result.univariate$summary.random$trend.effect$mean, 4),
                       round(result.univariate$summary.random$trend.effect$sd, 4))
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
  require(INLA);require(raster)
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
      
      #OUTPUTS
      result.vector=c(dic.joint, waic.joint, LCPO.joint,
                      round(result.joint$summary.fitted.values[1:744, "mean"], 4),
                      round(result.joint$summary.fitted.values[1:744, "sd"], 4),
                      round(result.joint$summary.random$seasonal2$mean, 4),
                      round(result.joint$summary.random$seasonal2$sd, 4),
                      round(result.joint$summary.random$cyclical2$mean, 4),
                      round(result.joint$summary.random$cyclical2$sd, 4),
                      round(result.joint$summary.random$trend2$mean, 4),
                      round(result.joint$summary.random$trend2$sd, 4))
      result.matrix=cbind(result.vector)
      return(result.matrix)
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

#####univariate model
# evi2.filtered.univariate <- univariate.model(stk=DRC_stk.Data.evi2)
# writeRaster(evi2.filtered.univariate, 
#             file="F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/Results/Univariate_model/result_stk_univariate_model/evi2.filtered.univariate.tif") 
# 
# #####joint model
# evi2.filtered.joint <- joint.model(stk=DRC_stk.Data.evi2)
# writeRaster(evi2.filtered.joint, 
#             file="F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/Results/Univariate_model/result_stk_univariate_model/evi2.filtered.joint.tif") 

#########################################      END  ##########################

####################################################################################
#    Univariate Bayesian Time series model
###################################################################################
model.val<- function(stk) {
  require(INLA);require(raster)
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
      return(model.val)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 

##########################################################################################
overall.post.mean<- function(stk) {
  require(INLA);require(raster)
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
                               control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                               control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Fitted values (posterior mean and posterior sd)
      overall.post.mean=cbind(round(result.univariate$summary.fitted.values[1:744, c("mean")], 4))
      return(overall.post.mean)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
###############################################################################################
overall.post.sd<- function(stk) {
  require(INLA);require(raster)
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
                               control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                               control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Fitted values (posterior mean and posterior sd)
      overall.post.sd=cbind(round(result.univariate$summary.fitted.values[1:744, c("sd")], 4))
      return(overall.post.sd)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
###################################################################################
seasonal.post.mean<- function(stk) {
  require(INLA);require(raster)
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
                               control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                               control.predictor=list(compute=TRUE), verbose=TRUE)
      #seasonal random effects
      seasonal.post.mean=cbind(round(result.univariate$summary.random$seasonal.effect$mean, 4))
      return(seasonal.post.mean)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
####################################################################################################
seasonal.post.sd<- function(stk) {
  require(INLA);require(raster)
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
                               control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                               control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #seasonal random effects
      seasonal.post.sd=cbind(round(result.univariate$summary.random$seasonal.effect$sd, 4))
      return(seasonal.post.sd)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
###############################################################################################
cyclical.post.mean<- function(stk) {
  require(INLA);require(raster)
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
                               control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                               control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Cyclical random effect posterior mean estimates
      cyclical.post.mean=cbind(round(result.univariate$summary.random$cyclical.effect$mean, 4))
      return(cyclical.post.mean)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
################################################################################################
cyclical.post.sd<- function(stk) {
  require(INLA);require(raster)
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
                               control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                               control.predictor=list(compute=TRUE), verbose=TRUE)
      
      
      #Cyclical random effect posterior mean estimates
      cyclical.post.sd=cbind(round(result.univariate$summary.random$cyclical.effect$sd, 4))
      return(cyclical.post.sd)
      
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
#######################################################################################################
trend.post.mean<- function(stk) {
  require(INLA);require(raster)
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
                               control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                               control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Trend random effect posterior mean estimates
      trend.post.mean=cbind(round(result.univariate$summary.random$trend.effect$mean, 4))
      return(trend.post.mean)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
###########################################################################################################
trend.post.sd<- function(stk) {
  require(INLA);require(raster)
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
                               control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                               control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Trend random effect posterior mean estimates
      trend.post.sd=cbind(round(result.univariate$summary.random$trend.effect$sd, 4))
      return(trend.post.sd)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
#################################################################################################
#   Joint model
####################################################################################################
joint.val<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      dic.joint=result.joint$dic$dic
      waic.joint=result.joint$waic$waic
      LCPO.joint=sum(-log(result.joint$cpo$cpo), na.rm=TRUE) 
      
      #model validation metrics
      model.val=cbind(dic.joint, waic.joint, LCPO.joint)
      return(model.val)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 
#############################################################################################
joint.post.mean<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Fitted values (posterior mean and posterior sd)
      overall.post.mean=cbind(round(result.joint$summary.fitted.values[1:744, "mean"], 4))
      return(overall.post.mean)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 
##########################################################################################
joint.post.sd<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Fitted values (posterior mean and posterior sd)
      overall.post.sd=cbind(round(result.joint$summary.fitted.values[1:744, "sd"], 4))
      return(overall.post.sd)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 
#######################################################################################
seasonal.post.mean2<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #seasonal random effects
      seasonal.post.mean=cbind(round(result.joint$summary.random$seasonal2$mean, 4))
      return(seasonal.post.mean)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 
#########################################################################################
seasonal.post.sd2<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #seasonal random effects
      seasonal.post.sd=cbind(round(result.joint$summary.random$seasonal2$sd, 4))
      return(seasonal.post.sd)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 
################################################################################################
cyclical.post.mean2<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Cyclical random effect posterior mean estimates
      cyclical.post.mean=cbind(round(result.joint$summary.random$cyclical2$mean, 4))
      return(cyclical.post.mean)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 
##############################################################################################
cyclical.post.sd2<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Cyclical random effect posterior mean estimates
      cyclical.post.sd=cbind(round(result.joint$summary.random$cyclical2$sd, 4))
      return(cyclical.post.sd)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 
##########################################################################################
trend.post.mean2<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Trend random effect posterior mean estimates
      trend.post.mean=cbind(round(result.joint$summary.random$trend2$mean, 4))
      return(trend.post.mean)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 
##############################################################################
trend.post.sd2<- function(stk) {
  require(INLA);require(raster)
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
                          quantiles=NULL, Ntrials=Ntrials,
                          control.compute=list(cpo=FALSE, dic = FALSE, waic = FALSE),
                          control.predictor=list(compute=TRUE), verbose=TRUE)
      
      #Trend random effect posterior mean estimates
      trend.post.sd=cbind(round(result.joint$summary.random$trend2$sd, 4))
      return(trend.post.sd)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=joint.smoother)
  return(result)
} 

#Smoothing using optimization method
require(raster);require(foreach) 
# files = list.files(file.path("F:/CLIM_DATA/Kenya_evi2_Records/kenya_20kmclipped"),pattern="*.tif", full.names=TRUE)
# rasters.files = foreach(i=1:length(files), .combine=c, .verbose=FALSE) %do%
#   raster(files[[i]])
# stk.rasters = stack(rasters.files, bands=NULL, native=FALSE, RAT=TRUE)
# outDirPath = "F:/CLIM_DATA/Kenya_evi2_Records/Adaptive_Smoothers_FutherTest_folder"

optimization_function <- function(files, outDirPath, nDays="1 month", lambda = 500, stk.rasters, overwrite=c(TRUE,FALSE)){
  require(MODIS);require(raster);require(foreach) 
  vi.files <- stack(files) #list.files object with pattern="*.tif"
  timeInfo <- orgTime(vi.files, nDays="1 month",
                      begin="1982001", end="2011335",
                      pillow=12, pos1=10, pos2=16, format="%Y%j")
  whittaker.raster(stk.rasters,
                   w=NULL, t=NULL,
                   timeInfo=timeInfo,
                   lambda = 500,
                   nIter= 3,
                   outDirPath = outDirPath,
                   overwrite=FALSE)
  
}

# res = optimization_function(files=files, 
#                             outDirPath=outDirPath, 
#                             nDays="1 month", 
#                             lambda = 500, 
#                             stk.rasters=stk.rasters, 
#                             overwrite = TRUE)
# 



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Trend analysis using Mann-Kandall
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Mann_Kandall_Test <- function(vip_raster_agg0, outDirPath, ResultDirPath){
  require(raster); require(rgdal); require(Kendall);require(gimms);require(foreach)
  
  
  fun_correction <- function(x){
    xx=ifelse(x > 1, 1,
              ifelse(x < -1, -1, x))
    return(xx)
  }
  
  vip_raster_agg <- calc(vip_raster_agg0, fun = fun_correction)
  
  #calculate long-term monthly means
  vip_list_means <- foreach(i = 1:12, 
                            .packages = c("raster", "rgdal")) %dopar% {
                              
                              # layers corresponding to current period (e.g. '82jan15a')
                              id <- seq(i, nlayers(vip_raster_agg), 12)
                              vip_raster_agg_tmp <- vip_raster_agg[[id]]
                              
                              # calculate long-term mean of current period (e.g. for 1982-2013 'jan15a')
                              calc(vip_raster_agg_tmp, fun = mean, na.rm = TRUE)
                            } 
  
  vip_raster_means <- stack(vip_list_means)
  
  
  #replicate monthly 'vip_raster_means' to match up with number of layers of 
  #initial 'vip_raster_agg' (as `foreach` does not support recycling!)
  vip_list_means <- replicate(nlayers(vip_raster_agg) / nlayers(vip_raster_means), 
                              vip_raster_means)
  vip_raster_means <- stack(vip_list_means)
  
  #subtract long-term mean from  monthly values
  files_out <- names(vip_raster_agg)
  vip_list_deseason <- foreach(i = 1:nlayers(vip_raster_agg), 
                               .packages = c("raster", "rgdal")) %dopar% {
                                 
                                 rst <- vip_raster_agg[[i]] - vip_raster_means[[i]]
                                 rst <- writeRaster(rst, 
                                                    filename = paste0("outDirPath/DSN_", names(vip_raster_agg[[i]])), 
                                                    format = "GTiff", overwrite = TRUE)
                                 
                               }
  
  vip_raster_deseason <- stack(vip_list_deseason)
  
  #Apply mann-kendall trend test (p < 0.001) custom function that 
  #returns only significant values of tau only
  require(Kendall);require(gimms)
  significantTau <- function(x) {
    require(Kendall);require(gimms)
    mk <- MannKendall(x)
    #reject value of tau if p >= 0.001
    if (mk$sl >= 0.001) {
      return(NA) 
      # keep value of tau if p < 0.001
    } else {
      return(mk$tau)
    }
  }
  
  #apply custom function on a pixel basis
  vip_raster_trend <- overlay(vip_raster_deseason, fun = significantTau, 
                              filename = paste0(ResultDirPath, "/vip5km_mkTest001"), 
                              format = "GTiff") 
  
}


#####Importing the vegetation index records
# path="F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/MonthlyEVI2-5km/sgolay"
# lists = list.files(file.path(path),pattern="*.tif", full.names=TRUE)
# rasters.images = foreach(files=1:length(lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
#   raster(lists[[files]])


#####creating a raster stack
# vip_raster_agg <- stack(rasters.images)
# ResultDirPath = "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/Phenology_parameters/Mann_Kandall_Trend"
# outDirPath = "F:/CLIM_DATA/EVI2_Smoothing_Datasets/ProcessedData/deseasoned_rst"
# Mann_Kandall_Test(vip_raster_agg=vip_raster_agg, outDirPath=outDirPath, ResultDirPath=ResultDirPath)

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
      return(xx)
    } 
  }
  s = stack(stk)
  size = nlayers(s)
  result = calc(s, fun=fun.smoother.spde)
  return(result)
}

#Revised to include trend, seasonal and cyclic components
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


#correcting records to be withing vegetation index values
fun_correction <- function(x){
  xx=ifelse(x > 1, 1,
            ifelse(x < -1, -1, x))
  return(xx)
}