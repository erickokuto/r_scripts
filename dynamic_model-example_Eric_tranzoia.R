
rm(list=ls())
require(INLA)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  TRANZOIA COUNTY
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
tranzoiadata=read.csv("F:/CLIM_DATA/tranzoia_county/combine.tranz.csv")
dim(tranzoiadata)
names(tranzoiadata)
tranzoiadata2=tranzoiadata[, 1:14]
names(tranzoiadata2)
dim(tranzoiadata2)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   defining the spatial locations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
n <- 150; set.seed(1); coo <- as.matrix(tranzoiadata2[1:150, 1:2])

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#We sample from a random field on a set of location:
#We use the above function to draw k (number of time points)
#samples from the random field
#####################################################
rrf <- function(n, coords, kappa, variance, nu=1) {
  m <- as.matrix(dist(coords))
  m <- exp((1-nu)*log(2) + nu*log(kappa*m)-lgamma(nu))*besselK(m*kappa, nu)
  diag(m) <- 1
  chol(variance*m)%*%matrix(rnorm(nrow(coords)*n), ncol=n)
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#we make it temporally correlated considering the time
#autoregression: 
#where the 1-rho[2]^2 term is in accord to the parametrization 
#of the AR(1) model in INLA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
kappa <- c(10, 12); tauE <- c(1, 3/2)
k <- ncol(tranzoiadata2[,-c(1:2)]); rho <- c(0.9, 0.8)
E1 <- rrf(k, coo, kappa[1], 1/tauE[1]) * (1-rho[1]^2)
E2 <- rrf(k, coo, kappa[2], 1/tauE[2]) * (1-rho[2]^2)

for (j in 2:k) {
  E1[, j] <- E1[,j-1]*rho[1] + E1[,j]
  E2[, j] <- E2[,j-1]*rho[2] + E2[,j]
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Creating the covariates
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# #cyclic dynamic model with harmonics
# yp=c(0.2243,0.2727,0.342133333,0.339433333,0.4415,0.414766667,
#      0.373166667,0.359566667,0.316733333,0.3049,0.2611,0.2415)
# n=12
# t=1:n
# w=pi/6
# amplitude = cos(w*t)
# phase = sin(w*t)
# y=yp
# 
# df=data.frame(y,amplitude,phase)
# formula=y~amplitude+phase
# r = inla(formula, data = df,
#          family = "gaussian",
#          quantiles=c(0.025, 0.05, 0.5, 0.95, 0.975),
#          control.predictor=list(compute=TRUE))
# summary(r)
# 
# # Plotting the seasonal DLM
# par(mfrow=c(1,1))
# rang <- range(r$summary.fitted.values[1:n, 3:7], y)
# plot(1:n,r$summary.fitted.values[1:n,1], type="l", ylim=rang, col="blue", xlim=c(1,n),ylab=expression(y[t]),xlab="time")
# lines(1:n,r$summary.fitted.values[1:n,3], col="blue", lty=3)
# lines(1:n,r$summary.fitted.values[1:n,7], col="blue", lty=3)
# lines(y[1:n], col="red")
# legend("bottom",legend=c("forecasted mean","90% CI","obs y"), col=c("blue","blue","red"),lty=c(1,2,1),bty="n",cex=1.5)
# 
# 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
set.seed(1)
w=(pi/6)*runif(n*k)
#w=(pi/6)
time.mat=matrix(rep(1:12, each=n), nrow=n)
theta_t.mat=round(time.mat*w, 4)
cos.amplitude <- round(matrix(cos(theta_t.mat),  nrow=n), 4) ### covariate1
sine.amplitude <- round(matrix(sin(theta_t.mat), nrow=n), 4) ### covariate2
dim(cos.amplitude)
dim(sine.amplitude)

tranzoiadata3=round(as.matrix(tranzoiadata2[, -(1:2)]), 4)
colnames(tranzoiadata3) <- NULL 
dim(tranzoiadata3)
dim(cos.amplitude)

plot(tranzoiadata3[10, ], type="l")
plot(cos.amplitude[19, ], col="blue", type="l")
plot(sine.amplitude[1, ], type="l", col="green")
plot(cos.amplitude[, 1], col="blue", type="l")
plot(sine.amplitude[, 2], type="l", col="green")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#To get the response, we do simulations for the covariate,
#compute the mean and add an error term  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

muE <- c(-5, 1); taue <- 5

y <- as.vector(tranzoiadata3)
length(y)
plot(y , type="l")

# length(y <- (muE[1] + E1) + (muE[2]+E2)*xx + ### dynamic regression part
#          rnorm(n*k, 0, sqrt(1/taue))) ### error in the observation
plot(y[,2], type="l")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Fitting the model:  y = Aξ0 + x(Aξ1) + e 
#To work out the second term in the right side of the last 
#equation we just need to supply the covariate values as the
#weights argument in the inla.spde.make.A() function
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mesh <- inla.mesh.2d(coo, max.edge=0.15, offset=0.05, cutoff=0.05)
(spde <- inla.spde2.matern(mesh))$n.spde

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# projector matrices and the indexes set as follows
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
A1 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=as.vector(cos.amplitude))
A2 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=as.vector(sine.amplitude))

iE1 <- inla.spde.make.index('E1', spde$n.spde, n.group=k)
iE2 <- inla.spde.make.index('E2', spde$n.spde, n.group=k)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# The data stack is as follows
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.y <- inla.stack(data=list(y=y), tag='y',
                    A=list(A1, A2, 1),
                    effects=list(iE1, iE2,
                    data.frame(cos.amplitude=as.vector(cos.amplitude), 
                    sine.amplitude=as.vector(sine.amplitude), mu1=1)))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   The formula take these things into account
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
form <- y ~ 0 + cos.amplitude + sine.amplitude + ### to fit mu_x
  f(E1, model=spde, group=E1.group, control.group=list(model='ar1')) +
  f(E2, model=spde, group=E2.group, control.group=list(model='ar1'))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#we define starting values for the hyperparameters in the
#internal scale considering the values used to simute the data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(theta.ini <- c(log(taue),
                -log(4*pi*tauE[1]*kappa[1]^2)/2, log(kappa[1]),
                qlogis(rho[1]),
                -log(4*pi*tauE[2]*kappa[2]^2)/2, log(kappa[2]),
                qlogis(rho[2])))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   Fitting the model considering the initial values defined above
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
res <- inla(form, family='gaussian', data=inla.stack.data(stk.y),
            control.predictor=list(A=inla.stack.A(stk.y)),
            control.mode=list(theta=theta.ini, restart=TRUE),
            verbose=FALSE)
summary(res)

names(res)
res$nhyper
res$cpu.used
res$logfile
res$names.fixed

round(cbind(res$summary.fix), 4)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   The results for the random fields
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rf1 <- inla.spde2.result(res, 'E1', spde)
rf2 <- inla.spde2.result(res, 'E2', spde)

head(rf1)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Posterior marginal distributions for the dynamic continuous
#spatial model hyperparameters
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
par(mfrow=c(3, 3), mar=c(2.5,2.5,0.3,0.3), mgp=c(1.5,0.5,0))
plot(res$marginals.hy[[1]], type='l', xlab=expression(tau[E]), ylab='Density')
abline(v=taue, col=gray(.4))
plot(res$marginals.hy[[4]], type='l', xlab=expression(rho[1]), ylab='Density')
abline(v=rho[1], col=gray(.4))
plot(res$marginals.hy[[7]], type='l', xlab=expression(rho[2]), ylab='Density')
abline(v=rho[2], col=gray(.4))
plot(rf1$marginals.variance[[1]], type='l', xlab=expression(sigma[1]^2), ylab='Density')
abline(v=1/tauE[1], col=gray(.4))
plot(rf1$marginals.kappa[[1]], type='l', xlab=expression(kappa[1]), ylab='Density')
abline(v=kappa[1], col=gray(.4))
plot(rf1$marginals.range[[1]], type='l', xlab='range 1', ylab='Density')
abline(v=sqrt(8*1)/kappa[1], col=gray(.4))
plot(rf2$marginals.variance[[1]], type='l', xlab=expression(sigma[2]^2), ylab='Density')
abline(v=1/tauE[2], col=gray(.4))
plot(rf2$marginals.kappa[[1]], type='l', xlab=expression(kappa[2]), ylab='Density')
abline(v=kappa[2], col=gray(.4))
plot(rf2$marginals.range[[1]], type='l', xlab='range 2', ylab='Density')
abline(v=sqrt(8*1)/kappa[2], col=gray(.4))

