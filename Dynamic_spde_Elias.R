
require(INLA)
n <- 100; set.seed(1); coo <- matrix(runif(2*n), n)

rrf <- function(n, coords, kappa, variance, nu=1) {
  m <- as.matrix(dist(coords))
  m <- exp((1-nu)*log(2) + nu*log(kappa*m)-lgamma(nu))*besselK(m*kappa, nu)
  diag(m) <- 1
  chol(variance*m)%*%matrix(rnorm(nrow(coords)*n), ncol=n)
}

kappa <- c(10, 8); sigma2 <- c(1/2, 1/3)
k <- 20; rho <- c(0.9, 0.7)
beta0 <- rrf(k, coo, kappa[1], sigma2[1]) * (1-rho[1]^2)
beta1 <- rrf(k, coo, kappa[2], sigma2[2]) * (1-rho[2]^2)
for (j in 2:k) {
  beta0[, j] <- beta0[,j-1]*rho[1] + beta0[,j]
  beta1[, j] <- beta1[,j-1]*rho[2] + beta1[,j]
}

hh <- rep(sin(2 * (2*pi) * (1:k/k)), each=n) ### just two cycles
mu.beta <- c(-5, 1); taue <- 5
length(y <- (mu.beta[1] + beta0) + (mu.beta[2]+beta1)*hh + ### dynamic regression part
         rnorm(n*k, 0, sqrt(1/taue))) ### error in the observation

(mesh <- inla.mesh.2d(coo, max.edge=0.25, offset=0.05, cutoff=0.05))$n
spde <- inla.spde2.matern(mesh)

i0 <- inla.spde.make.index('i0', spde$n.spde, n.group=k)
i1 <- inla.spde.make.index('i1', spde$n.spde, n.group=k)

A0 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n))
A1 <- inla.spde.make.A(mesh, cbind(rep(coo[,1], k), rep(coo[,2], k)),
                       group=rep(1:k, each=n), weights=hh)

stk.y <- inla.stack(data=list(y=as.vector(y)), tag='y',
                    A=list(A0, A1, 1),
                    effects=list(i0, i1,
                                 data.frame(mu1=1, h=hh)))

form <- y ~ 0 + mu1 + h + ### to fit mu_beta
  f(i0, model=spde, group=i0.group, control.group=list(model='ar1')) +
  f(i1, model=spde, group=i1.group, control.group=list(model='ar1'))

(theta.ini <- c(log(taue),
                -log(4*pi*sigma2[1]*kappa[1]^2)/2, log(kappa[1]),
                log((1+rho[1])/(1-rho[1])),
                -log(4*pi*sigma2[2]*kappa[2]^2)/2, log(kappa[2]),
                log((1+rho[2])/(1-rho[2]))))

(res <- inla(form, family='gaussian', data=inla.stack.data(stk.y),
             control.predictor=list(A=inla.stack.A(stk.y)),
             control.mode=list(theta=theta.ini, restart=TRUE)))$cpu

round(cbind(true=mu.beta, res$summary.fix), 4)

rf12 <- list(rf1=inla.spde2.result(res, 'i0', spde),
             rf2=inla.spde2.result(res, 'i1', spde))

par(mfrow=c(3, 3), mar=c(2.5,2.5,0.3,0.3), mgp=c(1.5,0.5,0))
for (i in 1:2) for (j in 15:17) {
  plot(rf12[[i]][[j]][[1]], type='l', ylab='Density',
       xlab=paste('rf', i, '.', substring(names(rf12[[i]])[j],11), sep=''))
  abline(v=c(kappa[i], sigma2[i], sqrt(8*1)/kappa[i])[j-14], col=4)
}
for (i in 1:2) {
  plot(res$marginals.hy[[c(3,6)[i]]], type='l',
       xlab=bquote(rho[.(i)]), ylab='Density')
  abline(v=rho[i], col=4)
}

plot(res$marginals.hy[[1]], type='l', xlab=expression(tau[E]), ylab='Density')
abline(v=taue, col=4)