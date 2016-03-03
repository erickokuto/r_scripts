
require(geoR)
require(geoRglm)
require(INLA)
require(INLABMA)
require(R2WinBUGS)
require(rstan)


#vignette("R2WinBUGS")
#require(SsfPack)

############################
####     EXAMPLE 2      ####
############################
rm(list=ls())
#Simulating a seasonal dynamic model with harmonics
set.seed(123456)

W1 = 0.002
W2 = 0.001
V = 0.01
n = 110
a0 = 0.1
b0 = 0.1
w1  = rnorm(n,0,sqrt(W1))
w2  = rnorm(n,0,sqrt(W2))
v  = rnorm(n,0,sqrt(V))
y = a  = b = rep(0,n)
a[1] = cos(pi/6)*a0 + sin(pi/6)*b0 + w1[1]
b[1] = -sin(pi/6)*a0 + cos(pi/6)*b0 + w2[1]
y[1] = a[1] + v[1]
for (t in 2:n){
  a[t] = cos(pi/6)*a[t-1] + sin(pi/6)*b[t-1] + w1[t]
  b[t] = -sin(pi/6)*a[t-1] + cos(pi/6)*b[t-1] + w2[t]
  y[t] = a[t] + v[t]
}


# Plotting the seasonal DLM
# -------------------------
plot(y,type="l",xlab="time",ylab="")
lines(a,col=2)
lines(b,col=4)
legend("topleft",legend=c("y","a_t","b_t"), col=c("black", "red","blue"),lty=c(1,1,1),bty="n")
title(paste("V=",V," ; W1=",W1," ; W2=",W2,sep=""))

m = n-1
f=10        # forecast horizon
p=n-f
cosw = cos(pi/6)
sinw = sin(pi/6)

yp = c(y[1:p],rep(NA,f))

# building the augmented model
# ----------------------------
Y <- matrix(NA, n+2*m, 3)
Y[1:n,         1] <- yp             # actual observations (y)
Y[1:m + n,     2] <- 0              # faked observations associated to a_t (1st system equation)
Y[1:m + (n+m), 3] <- 0              # faked observations associated to b_t (2nd system equation)


# indices for the INLA library
# ----------------------------
i       = c(1:n, 2:n, rep(NA,m))                  # indices for a_t
j       = c(rep(NA,n), 2:n -1, 2:n -1)            # indices for a_{t-1}
we1     = c(rep(NA,n), rep(-cosw,m), rep(sinw,m)) # weights for a_{t-1}
l       = c(rep(NA,n+m-1), 1:n)                   # indices for b_t
we2     = c(rep(NA,n+m), rep(1,m))                # weights for b_t
o       = c(rep(NA,n), 2:n -1, 2:n -1)            # indices for b_{t-1}
we3     = c(rep(NA,n), rep(-sinw,m), rep(-cosw,m))# weights for b_{t-1}
q       = c(rep(NA,n), 2:n, rep(NA,m))            # indices for the perturbation terms w_{1,t}
s       = c(rep(NA,n+m), 2:n)                     # indices for the perturbation terms w_{2,t}

# formulating the model
# ---------------------
formula = Y ~ f(i, model="iid", initial=-10, fixed=T) +
  f(j, we1, copy="i") +
  f(l, we2, model="iid", initial=-10, fixed=T) +
  f(o, we3, copy="l") +
  f(q, model ="iid") + 
  f(s, model ="iid",param=c(1,0.001)) -1


# loading the INLA library
# ------------------------
require(INLA)


# call to fit the model
# ---------------------
r = inla(formula, data = data.frame(i,j,we1,l,we2,o,we3,q,s),
         family = rep("gaussian", 3),
         quantiles=c(0.025, 0.05, 0.5, 0.95, 0.975),
         control.family = list(list(), list(initial=10, fixed=T), list(initial=10, fixed=T)),
         control.predictor=list(compute=TRUE))


# elapsed time
r$cpu.used
summary(r)
r$summary.random
x = r$summary.random
r$summary.random[[1]][1:n,4:8]

## plotting the results
# ---------------------

par(mfrow=c(2,2), mar=c(3,3,2,1), mgp=c(2,1,0))
# graph for observations (y)
#par(mfrow=c(1,1), mar=c(5,6.5,2,1), mgp=c(4,1,0),cex.axis=2,cex.main=2,cex.lab=3)
rang <- range(r$summary.fitted.values[1:n, 3:7], y)
plot(1:p,r$summary.fitted.values[1:p,1], type="l", ylim=rang, col="blue", xlim=c(1,n),ylab=expression(y[t]),xlab="time")
lines(1:p,r$summary.fitted.values[1:p,3], col="blue", lty=3)
lines(1:p,r$summary.fitted.values[1:p,7], col="blue", lty=3)
lines(p:n,r$summary.fitted.values[p:n,1], col="red")
lines(p:n,r$summary.fitted.values[p:n,3], col="red", lty=3)
lines(p:n,r$summary.fitted.values[p:n,7], col="red", lty=3)
lines(y[1:n])
legend("topleft",legend=c(expression(paste("simulated",~ y[t])),"forecasted mean","90% CI"), col=c("black", "red","red"),lty=c(1,1,2),bty="n",cex=1.5)

# graph for states (a_t)
#par(mfrow=c(1,1), mar=c(5,6.5,2,1), mgp=c(4,1,0),cex.axis=2,cex.main=2,cex.lab=3)
rang <- range(r$summary.random[[1]][1:n,4:8], a[1:n])
plot(1:p,r$summary.random[[1]][1:p,2], type="l", ylim=rang, col="blue", xlim=c(1,n),ylab=expression(a[t]),xlab="time")
lines(1:p,r$summary.random[[1]][1:p,4], col="blue", lty=3)
lines(1:p,r$summary.random[[1]][1:p,8], col="blue", lty=3)
lines(p:n,r$summary.random[[1]][p:n,2], col="red")
lines(p:n,r$summary.random[[1]][p:n,4], col="red", lty=3)
lines(p:n,r$summary.random[[1]][p:n,8], col="red", lty=3)
lines(a[1:n])
legend("topleft",legend=c(expression(paste("simulated",~ a[t])),"forecasted mean","90% CI"), col=c("black", "red","red"),lty=c(1,1,2),bty="n",cex=1.5)

#graph for states (b_t)
#par(mfrow=c(1,1), mar=c(5,6.5,2,1), mgp=c(4,1,0),cex.axis=2,cex.main=2,cex.lab=3)
rang <- range(r$summary.random[[2]][1:m,c(4,8)], b[1:m])
plot(2:p,r$summary.random[[2]][2:p,2], type="l", ylim=rang, col="blue", xlim=c(1,n),ylab=expression(b[t]),xlab="time")
lines(2:p,r$summary.random[[2]][2:p,4], col="blue", lty=3)
lines(2:p,r$summary.random[[2]][2:p,8], col="blue", lty=3)
lines(p:n,r$summary.random[[2]][p:n,2], col="red")
lines(p:n,r$summary.random[[2]][p:n,4], col="red", lty=3)
lines(p:n,r$summary.random[[2]][p:n,8], col="red", lty=3)
lines(b[1:n])
legend("topleft",legend=c(expression(paste("simulated",~ b[t])),"forecasted mean","90% CI"), col=c("black", "red","red"),lty=c(1,1,2),bty="n",cex=1.5)


# hyperparameter estimation values
# ---------------------------------
r$summary.hyperpar


# marginal posterior densities for precision parameters
# -----------------------------------------------------
par(mfrow=c(2,2))
#par(mfrow=c(1,1), mar=c(5,6.5,2,1), mgp=c(4,1,0),cex.axis=2,cex.main=2,cex.lab=3)
plot(r$marginals.hyperpar[[1]], type="l", xlab="1/V", main="precision for y", ylab="density",xlim=c(0,300),lwd=2)  # precision of V
abline(v=1/V,col=2, lwd=2)
#par(mfrow=c(1,1), mar=c(5,6.5,2,1), mgp=c(4,1,0),cex.axis=2,cex.main=2,cex.lab=3)
plot(r$marginals.hyperpar[[2]], type="l", xlab=expression(1/W[1]), main="precision for a_t", ylab="density",xlim=c(0,3000),lwd=2)  # precision of W1
abline(v=1/W1,col=2, lwd=2)
#par(mfrow=c(1,1), mar=c(5,6.5,2,1), mgp=c(4,1,0),cex.axis=2,cex.main=2,cex.lab=3)
plot(r$marginals.hyperpar[[3]], type="l", xlab=expression(1/W[2]), main="precision for b_t", ylab="density",xlim=c(0,10000),lwd=2)  # precision of W2
abline(v=1/W2,col=2, lwd=2)

