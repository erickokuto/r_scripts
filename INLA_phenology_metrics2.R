
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
require("INLA")

#cyclic dynamic model with harmonics
yp=c(0.2243,0.2727,0.342133333,0.339433333,0.4415,0.414766667,
    0.373166667,0.359566667,0.316733333,0.3049,0.2611,0.2415)
n=12
t=1:n
w=pi/6
#w=pi
amplitude.cos = cos(w*t)
amplitude.sine = sin(w*t)
y=yp

df=data.frame(y,amplitude.cos,amplitude.sine)
formula=y~amplitude.cos+amplitude.sine
r = inla(formula, data = df,
         family = "gaussian",
         quantiles=c(0.025, 0.05, 0.5, 0.95, 0.975),
         control.predictor=list(compute=TRUE),
         verbose=TRUE)
summary(r)

# Plotting the seasonal DLM
par(mfrow=c(1,1))
rang <- range(r$summary.fitted.values[1:n, 3:7], y)
plot(1:n,r$summary.fitted.values[1:n,1], type="l", ylim=rang, col="blue", xlim=c(1,n),ylab=expression(y[t]),xlab="time")
lines(1:n,r$summary.fitted.values[1:n,3], col="blue", lty=3)
lines(1:n,r$summary.fitted.values[1:n,7], col="blue", lty=3)
lines(y[1:n], col="red")
legend("bottom",legend=c("forecasted mean","90% CI","obs y"), col=c("blue","blue","red"),lty=c(1,2,1),bty="n",cex=1.5)


#########################################################
#cyclic dynamic model with harmonics
###########################################################
tranzoiadata=read.csv("F:/CLIM_DATA/tranzoia_county/combine.tranz.csv")
dim(tranzoiadata)
names(tranzoiadata)

#######################
#some data management
########################
require(reshape2)

w=pi/6
nloc=150    #number of pixels
nyears=31   #number of years
nmonths=372 #number of months from 1982:2012 (12*nyears)

loc=cbind(melt(tranzoiadata, id.vars=c("x", "y"))[1:150,c("x","y")]) #pixel center coordinates
yp=melt(tranzoiadata, id.vars=c("x", "y"))[ , "value"] # reshaped evi values
month=rep(rep(1:12, each=150), times=31)
year=rep(1982:2012, each=nloc*12) 

amplitude.cos <- cos(w*month)
amplitude.sine <- sin(w*month)
weight <- w*month

plot(amplitude.cos, type="l")
plot(amplitude.sine, type="l")
plot(yp, type="l")

###########################################
# SPDE model functionality using INLA
###########################################
require(INLA)
kappa <- 5

mesh = inla.mesh.2d(loc=loc, max.edge=c(0.5, 1)/kappa, cutoff=0.1/kappa)
plot(mesh)

spde = inla.spde2.matern(mesh, alpha = 2)
index = inla.spde.make.index("space", n.spde = spde$n.spde, group=year, n.group = nyears)

A = inla.spde.make.A(mesh, loc = loc,
                        index = seq_len(nrow(loc)),
                        weights=weight,
                        group = year,
                        n.group= nyears)

intercept=rep(1, times=nloc*nmonths)
stack = inla.stack(data = list(y = yp),
                   A = list(A, 1, 1, 1), 
                   effects = list(index, amplitude.cos, 
                                  amplitude.sine, 
                                  intercept),
                   tag='est')

data.stk = inla.stack.data(stack) 
ind = inla.stack.index(stack, tag='est')$data

formula = y ~ 0 + intercept + 
  f(space, weight=amplitude.cos, model=spde, group = space.group,
    control.group = list(model = "ar1")) + 
  f(space, weight=amplitude.sine, model=spde, group = space.group,
    control.group = list(model = "ar1")) 

result = inla(formula,
                    family="gaussian", 
                    data=data.stk,
                    quantiles=NULL, 
                    control.compute=list(cpo=FALSE, config=FALSE),
                    control.predictor=list(A=inla.stack.A(stk),
                                           compute=TRUE),
                    control.inla = list(strategy = "gaussian"),
                    verbose=FALSE)
#########################################################################


knots.seasonal = seq(1, n, by = 3)
sigma0 = 1; kappa0 = 1e-3; tau0 = 1/(4*kappa0^3*sigma0^2)^0.5
mesh.seasonal = INLA::inla.mesh.1d(knots.seasonal, 
                                   interval=c(0, n), 
                                   degree=2,
                                   boundary="cyclic")

spde.seasonal = INLA::inla.spde2.matern(mesh.seasonal,
                                        constr=TRUE, 
                                        B.tau = cbind(log(tau0), 1),
                                        B.kappa = cbind(log(kappa0), 0),
                                        theta.prior.prec = 1e-4)
A.season = INLA::inla.spde.make.A(mesh.seasonal,loc = t)

index.month = INLA::inla.spde.make.index("t",n.spde=spde.seasonal$n.spde); 
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


result = INLA::inla(formula,
                    family="gaussian", 
                    data=data.stk,
                    quantiles=NULL, 
                    control.compute=list(cpo=FALSE, config=FALSE),
                    control.predictor=list(A=inla.stack.A(stk),
                                           compute=TRUE),
                    control.inla = list(strategy = "gaussian"),
                    verbose=FALSE)
#######################################################
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

#extracting space varying coefficients
result$summary.random$region.x$mean 
