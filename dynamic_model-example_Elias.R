
require(INLA)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   defining the spatial locations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
n <- 100; set.seed(1); coo <- matrix(runif(2*n), n)

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
k <- 10; rho <- c(0.9, 0.8)
E1 <- rrf(k, coo, kappa[1], 1/tauE[1]) * (1-rho[1]^2)
E2 <- rrf(k, coo, kappa[2], 1/tauE[2]) * (1-rho[2]^2)
for (j in 2:k) {
  E1[, j] <- E1[,j-1]*rho[1] + E1[,j]
  E2[, j] <- E2[,j-1]*rho[2] + E2[,j]
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#To get the response, we do simulations for the covariate,
#compute the mean and add an error term  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
xx <- matrix(runif(n*k), n) ### covariate
plot(xx[,1], type="l")

muE <- c(-5, 1); taue <- 5
length(y <- (muE[1] + E1) + (muE[2]+E2)*xx + ### dynamic regression part
         rnorm(n*k, 0, sqrt(1/taue))) ### error in the observation
plot(y[,1], type="l")
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
                       group=rep(1:k, each=n))
A2 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=as.vector(xx))

iE1 <- inla.spde.make.index('E1', spde$n.spde, n.group=k)
iE2 <- inla.spde.make.index('E2', spde$n.spde, n.group=k)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# The data stack is as follows
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.y <- inla.stack(data=list(y=as.vector(y)), tag='y',
                    A=list(A1, A2, 1),
                    effects=list(iE1, iE2,
                    data.frame(mu1=1, x=as.vector(xx))))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   The formula take these things into account
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
form <- y ~ 0 + mu1 + x + ### to fit mu_x
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

round(cbind(true=muE, res$summary.fix), 4)

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