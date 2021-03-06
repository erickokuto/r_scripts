
rm(list=ls())
require(INLA)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Test record from kenya county in Kenya
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
kenyadata=read.csv("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Kenya_evi2_Records/kenya.monthly.20KMunsmoothedEVI2.csv")
dim(kenyadata)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   defining the spatial locations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
n <- nrow(kenyadata[, -c(1:2)])
k <- ncol(kenyadata[,-c(1:2)])

set.seed(1)

coo <- as.matrix(kenyadata[1:n, 1:2])

require(reshape)          # matrix to vector columnwise
y<- melt(cbind(kenyadata[, -(1:2)]), na.rm=FALSE)$value 
length(y)

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
#we make it temporally correlated considering an autoregression of order 1: 
#where the 1-rho[2]^2 term is in accord to the parametrization 
#of the AR(1) model in INLA
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
kappa <- c(10, 8, 8); sigma2 <- c(1/2, 1/3, 1/3)
rho <- c(0.9, 0.8, 0.8)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#We use the function to draw k (number of time points) 
#samples from the random field
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
beta0 <- rrf(k, coo, kappa[1], sigma2[1]) * (1-rho[1]^2)
beta1 <- rrf(k, coo, kappa[2], sigma2[2]) * (1-rho[2]^2)
beta2 <- rrf(k, coo, kappa[3], sigma2[3]) * (1-rho[3]^2)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#We make the samples drawn  temporally correlated
#considering AR(1) process
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
for (j in 2:k) {
  beta0[, j] <- beta0[,j-1]*rho[1] + beta0[,j]
  beta1[, j] <- beta1[,j-1]*rho[2] + beta1[,j]
  beta2[, j] <- beta2[,j-1]*rho[3] + beta2[,j]
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#creating the regression covariates cos and sine amplitude terms
#The two are necessary to compute annual phase and growing season length
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
hh1 <- as.vector(rep(sin(30 * (2*pi) * (1:k/k)), each=n)) ### just 30 cycles
hh2 <- as.vector(rep(cos(30 * (2*pi) * (1:k/k)), each=n)) ### just 30 cycles

length(hh1); length(hh2)
par(mfrow=c(2,1))
plot(hh1, type="l")
plot(hh2, type="l")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#To get the response, we do simulations for the covariate,
#compute the mean and add an error term  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

taue <- 5; mu.beta <- c(0.2, 1, 1)

#>>>>>>>>>>>>>>>>>>>>>>>>>>
#Simulated/Fake data
#>>>>>>>>>>>>>>>>>>>>>>>>>>
length(yy <- (mu.beta[1] + beta0) + (mu.beta[2]+beta1)*hh1 + (mu.beta[3]+beta2)*hh2 + ### dynamic regression part
         rnorm(n*k, 0, sqrt(1/taue))) ### error in the observation

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Fitting the model as sugested in the SPDE tutorial :  
#y = (Aξ0) + x1(Aξ1) + x2(Aξ2) + e 
#To work out the second  and third term in the right side of the last 
#equation we just need to supply the covariate values as the
#weights argument in the inla.spde.make.A() function
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#We might want to use a mesh which is based on a non-convex hull to avoid
#adding many small triangles outside the domain of interest
#(more triangles = larger computation times)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# require(rgeos)
# require(maptools)
# require(spdep)
# kenya <- readShapePoly("F:/CLIM_DATA/tranzoia_county/trans.shp")
# KE.border <- unionSpatialPolygons(kenya, rep(1, nrow(kenya)))
# KE.bdry <- inla.sp2segment(KE.border)
# 
# (mesh <- inla.mesh.2d(boundary=KE.bdry, cutoff=0.1,
#                          max.edge=c(0.1, 0.1)))$n
# 
# par(mfrow=c(1,1))
# plot(mesh)

(mesh <- inla.mesh.2d(coo, max.edge=1.5, offset=1.5, cutoff=2))$n
(spde <- inla.spde2.matern(mesh))$n.spde

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# projector matrices and the indexes set as follows
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
A0 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=1)
A1 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=hh1)
A2 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=hh2)

i0 <- inla.spde.make.index('i0', spde$n.spde, n.group=k)
i1 <- inla.spde.make.index('i1', spde$n.spde, n.group=k)
i2 <- inla.spde.make.index('i2', spde$n.spde, n.group=k)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# The data stack is as follows
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.y <- inla.stack(data=list(y=y), tag='y',
                    A=list(A0, A1, A2, 1),
                    effects=list(i0, i1, i2,
                    data.frame(mu1=1, hh1=hh1, 
                    hh2=hh2)))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#A non-random (for computational reasons) intercept (mu1) 
#is added 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
formula <- y ~ 0 + mu1 + hh1 + hh2 + 
  f(i0, model=spde, group=i0.group, control.group=list(model='ar1')) +
  f(i1, model=spde, group=i1.group, control.group=list(model='ar1')) +
  f(i2, model=spde, group=i2.group, control.group=list(model='ar1'))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#we define starting values for the hyperparameters in the
#internal scale considering the values used to simute the data
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(theta.ini <- c(log(taue),
                -log(4*pi*sigma2[1]*kappa[1]^2)/2, log(kappa[1]),
                log((1+rho[1])/(1-rho[1])),
                -log(4*pi*sigma2[2]*kappa[2]^2)/2, log(kappa[2]),
                log((1+rho[2])/(1-rho[2])),
                -log(4*pi*sigma2[3]*kappa[3]^2)/2, log(kappa[3]),
                log((1+rho[3])/(1-rho[3]))))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   Fitting the model considering the initial values defined above
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
res <- inla(formula, family='gaussian', data=inla.stack.data(stk.y),
            control.predictor=list(A=inla.stack.A(stk.y), compute=TRUE), 
            control.mode=list(theta=theta.ini, restart=TRUE),
            control.compute = list(config=FALSE, dic=TRUE, waic=TRUE),
            verbose=TRUE)

summary(res)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#BySampling from the model:
#The first n elements of the latent field are the linear 
#predictor for the observed data and the next m elements are for 
#the location at the mesh nodes
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

index=inla.stack.index(stk.y, 'y')$data

require(reshape)          
df <- melt(cbind(kenyadata[, -(1:2)]), na.rm=FALSE)
pred <- round(res$summary.fitted.values[index, "mean"], 4)
df <- cbind(df, pred)
evi2pred <- cbind(coo, df)
write.csv(evi2pred, "F:/CLIM_DATA/kenya_county/Evi2predTranzoia_SPDE.csv")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Fixed effects (annual mean, cos and sine amplitude) estimates 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
round(cbind(res$summary.fix), 4)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Summary of the posterior marginal density for the AR1 process
#for each of the random fields
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
round(cbind(res$summary.hy[c(4, 7, 10), ]), 4)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   The results for the two random fields
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rf0 <- inla.spde2.result(res, 'i0', spde, do.transf=TRUE)
rf1 <- inla.spde2.result(res, 'i1', spde, do.transf=TRUE)
rf2 <- inla.spde2.result(res, 'i2', spde, do.transf=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#The practical range of the spatio-temporal process
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
round(cbind(prior=sqrt(8)/kappa, t(sapply(list(rf0, rf1, rf2),
             function(rrf) unlist(inla.zmarginal(
             rrf$marginals.range.nom[[1]], silent=TRUE))))), 3)

#Variance for each spatio-temporal process
round(cbind(prior=sigma2, t(sapply(list(rf0, rf1, rf2), function(rrf)
  unlist(inla.zmarginal(rrf$marginals.variance.nominal[[1]],
                        silent=TRUE))))), 3)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   Scaling/smoothing parameter estimate
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
round(cbind(prior=kappa, t(sapply(list(rf0, rf1, rf2), function(rrf)
  unlist(inla.zmarginal(rrf$marginals.kappa[[1]], silent=TRUE))))), 3)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Using cos and sine amplitude estimates to estimate annual phase 
# and growing season length
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
par(mfrow=c(5, 2), mar=c(2.5,2.5,0.3,0.3), mgp=c(1.5,0.5,0))
plot(res$marginals.hy[[1]], type='l', xlab=expression(tau[E]), ylab='Density')
abline(v=taue, col=gray(.4))
plot(res$marginals.hy[[4]], type='l', xlab=expression(rho[0]), ylab='Density')
abline(v=rho[1], col=gray(.4))
plot(res$marginals.hy[[7]], type='l', xlab=expression(rho[1]), ylab='Density')
abline(v=rho[2], col=gray(.4))
plot(res$marginals.hy[[10]], type='l', xlab=expression(rho[2]), ylab='Density')
abline(v=rho[3], col=gray(.4))
plot(rf0$marginals.variance[[1]], type='l', xlab=expression(sigma[0]^2), ylab='Density')
abline(v=1/taue[1], col=gray(.4))
plot(rf0$marginals.kappa[[1]], type='l', xlab=expression(kappa[0]), ylab='Density')
abline(v=kappa[1], col=gray(.4))
plot(rf0$marginals.range[[1]], type='l', xlab='range 0', ylab='Density')
abline(v=sqrt(8*1)/kappa[1], col=gray(.4))
plot(rf1$marginals.kappa[[1]], type='l', xlab=expression(kappa[1]), ylab='Density')
abline(v=kappa[2], col=gray(.4))
plot(rf1$marginals.range[[1]], type='l', xlab='range 1', ylab='Density')
abline(v=sqrt(8*1)/kappa[2], col=gray(.4))
plot(rf1$marginals.variance[[1]], type='l', xlab=expression(sigma[1]^2), ylab='Density')
abline(v=1/taue[1], col=gray(.4))
plot(rf2$marginals.kappa[[1]], type='l', xlab=expression(kappa[2]), ylab='Density')
abline(v=kappa[3], col=gray(.4))
plot(rf2$marginals.range[[1]], type='l', xlab='range 2', ylab='Density')
abline(v=sqrt(8*1)/kappa[3], col=gray(.4))
plot(rf2$marginals.variance[[1]], type='l', xlab=expression(sigma[2]^2), ylab='Density')
abline(v=1/taue[1], col=gray(.4))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
starting.value = inla(formula,
                      data=c(str.data$data, list(spde=spde_m)),
                      family="gev",
                      control.inla = list(diagonal = 100, strategy = "gaussian", int.strategy = "eb"),
                      control.predictor=list(A=str.data$A),
                      verbose=TRUE)

#The starting value can then be used by calling the control.mode option. 
#This tells INLA where to start looking for the mode and restart=TRUE tells it to keep improving the approximation.

result = inla(formula,
              data=c(str.data$data, list(spde=spde_m)),	
              family="gev",
              control.mode = list(result = starting.value, restart = TRUE),
              control.predictor=list(A=str.data$A),
              verbose=TRUE)

