
rm(list=ls())
require(INLA)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Test record from Tranzoia county in Kenya
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
tranzoiadata=read.csv("F:/CLIM_DATA/tranzoia_county/combine.tranz.csv")
tranzoiadata2=tranzoiadata[, 1:14]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   defining the spatial locations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
n <- nrow(tranzoiadata2[, -c(1:2)])
k <- ncol(tranzoiadata2[,-c(1:2)])
set.seed(1)
coo <- as.matrix(tranzoiadata2[1:n, 1:2])

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
kappa <- c(8, 8); sigma2 <- c(1/3, 1/3)
rho <- c(0.8, 0.8)
beta1 <- rrf(k, coo, kappa[1], sigma2[1]) * (1-rho[1]^2)
beta2 <- rrf(k, coo, kappa[2], sigma2[2]) * (1-rho[2]^2)

for (j in 2:k) {
  beta1[, j] <- beta1[,j-1]*rho[1] + beta1[,j]
  beta2[, j] <- beta2[,j-1]*rho[2] + beta2[,j]
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#creating the regression covariates cos and sine amplitude terms
#The two are necessary to compute annual phase and growing season length
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
w=2*pi/k
time.mat=round(matrix(rep(w*(1:k/k), each=n), nrow=n), 4)
cos.amplitude <- round(matrix(cos(time.mat),  nrow=n), 4) ### covariate1
sine.amplitude <- round(matrix(sin(time.mat), nrow=n), 4) ### covariate2

tranzoiadata3=round(as.matrix(tranzoiadata2[, -(1:2)]), 4) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#To get the response, we do simulations for the covariate,
#compute the mean and add an error term  
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

taue <- 5

y <- as.vector(tranzoiadata3)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Fitting the model as sugested in the SPDE tutorial :  
#y = B0 + x1(Aξ1) + x2(Aξ2) + e 
#To work out the second  and third term in the right side of the last 
#equation we just need to supply the covariate values as the
#weights argument in the inla.spde.make.A() function
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
(mesh <- inla.mesh.2d(coo, max.edge=0.25, offset=0.05, cutoff=0.05))$n
(spde <- inla.spde2.matern(mesh))$n.spde

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# projector matrices and the indexes set as follows
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
A1 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=as.vector(cos.amplitude))
A2 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=as.vector(sine.amplitude))

i1 <- inla.spde.make.index('i1', spde$n.spde, n.group=k)
i2 <- inla.spde.make.index('i2', spde$n.spde, n.group=k)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# The data stack is as follows
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stk.y <- inla.stack(data=list(y=y), tag='y',
                    A=list(A1, A2, 1),
                    effects=list(i1, i2,
                    data.frame(mu1=1, cos.amplitude=as.vector(cos.amplitude), 
                    sine.amplitude=as.vector(sine.amplitude))))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#A non-random (for computational reasons) intercept (mu1) 
#is added 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
formula <- y ~ 0 + mu1 + cos.amplitude + sine.amplitude + 
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
                log((1+rho[2])/(1-rho[2]))))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   Fitting the model considering the initial values defined above
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
res <- inla(formula, family='gaussian', data=inla.stack.data(stk.y),
            control.predictor=list(A=inla.stack.A(stk.y)), 
            control.mode=list(theta=theta.ini, restart=TRUE),
            verbose=FALSE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Extracting the spataily dependent regression parameters
#for each pixel/coordinate
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Fixed effects (annual mean, cos and sine amplitude) estimates 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
round(cbind(res$summary.fix), 4)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Summary of the posterior marginal density for the AR1 process
#for each of the random fields
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
round(cbind(res$summary.hy[c(4, 7), ]), 4)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   The results for the two random fields
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rf1 <- inla.spde2.result(res, 'i1', spde, do.transf=TRUE)
rf2 <- inla.spde2.result(res, 'i2', spde, do.transf=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#The practical range of the spatio-temporal process
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
round(cbind(prior=sqrt(8)/kappa, t(sapply(list(rf1, rf2),
             function(rrf) unlist(inla.zmarginal(
             rrf$marginals.range.nom[[1]], silent=TRUE))))), 3)

#Variance for each spatio-temporal process
round(cbind(prior=sigma2, t(sapply(list(rf1, rf2), function(rrf)
  unlist(inla.zmarginal(rrf$marginals.variance.nominal[[1]],
                        silent=TRUE))))), 3)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   Scaling/smoothing parameter estimate
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
round(cbind(prior=kappa, t(sapply(list(rf1, rf2), function(rrf)
  unlist(inla.zmarginal(rrf$marginals.kappa[[1]], silent=TRUE))))), 3)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Using cos and sine amplitude estimates to estimate annual phase 
# and growing season length
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

