require(dlmodeler)

# generate some quarterly data
n <- 80
level <- 12
sigma <- .75
season <- c(5,6,8,2)
y <- level + 3*sin((1:n)/10) + rep(season,n/4) + rnorm(n, 0, sigma)
y <- matrix(y,nrow=1)

# fit a stochastic level + quarterly seasonal model to the data by
# maximum likelihood estimation
build.fun <- function(p) {
  sigmaH <- exp(p[1])
  sigmaQ <- exp(p[2])*sigmaH
  mod <- dlmodeler.build.polynomial(0,sigmaH=sigmaH,sigmaQ=sigmaQ) +
    dlmodeler.build.dseasonal(4,sigmaH=0)
  return(mod)
}
fit <- dlmodeler.fit.MLE(y, build.fun, c(0,0))

# generate forecasts for observations 81 to 100
f <- dlmodeler.forecast(y, fit$model, start=80, ahead=20)
plot(y[1,],type='l',xlim=c(60,100),ylim=c(10,30))
lines(f$index,f$yhat,col='dark blue')
lines(f$index,f$lower,col='light blue')
lines(f$index,f$upper,col='light blue')

# simulate forecasts post-ex.
f <- dlmodeler.forecast(y, fit$model, ahead=20, start=20, iters=40)
plot(y[1,],type='p')

# show the one step ahead forecasts
with(f[f$distance==1,], lines(index,yhat,col='dark blue'))
# show the 10 step ahead forecasts
with(f[f$distance==10,], lines(index,yhat,col='blue'))
# show the 20 step ahead forecasts
with(f[f$distance==20,], lines(index,yhat,col='light blue'))