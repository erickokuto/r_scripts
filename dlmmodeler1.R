## Not run:
require(dlmodeler); require(dlm)

# analysis from Durbin & Koopman book page 32

# load and show the data
y <- matrix(Nile,nrow=1)
plot(y[1,],type='l')

# y(t)   = a(t) + eta(t)
# a(t+1) = a(t) + eps(t)
mod <- dlmodeler.build.polynomial(0,sigmaH=NA,sigmaQ=NA,name='p32')

# fit the model by maximum likelihood estimation
fit <- dlmodeler.fit(y, mod, method="MLE")

# compare the fitted parameters with those reported by the authors
fit$par[2]        # psi = -2.33
fit$model$Ht[1,1] # H   = 15099
fit$model$Qt[1,1] # Q   = 1469.1

# compute the filtered and smoothed values
f <- dlmodeler.filter(y, fit$mod, smooth=TRUE)

# f.ce represents the filtered one steap ahead observation
# prediction expectations E[y(t) | y(1), y(2), ..., y(t-1)]
f.ce <- dlmodeler.extract(f, fit$model,
                          type="observation", value="mean")

# s.ce represents the smoothed observation expectations
# E[y(t) | y(1), y(2), ..., y(n)]
s.ce <- dlmodeler.extract(f$smooth, fit$model,
                          type="observation", value="mean")

# plot the components
plot(y[1,],type='l')
lines(f.ce$p32[1,],col='light blue',lty=2)
lines(s.ce$p32[1,],col='dark blue')
## End(Not run)