
require("INLA")
kenyadata=read.csv("F:/CLIM_DATA/tranzoia_county/combine.tranz.10KMunsmoothed.csv")


n <- nrow(kenyadata[, -c(1:2)]); set.seed(1); coo <- cbind(kenyadata[1:n, 1:2])

rrf <- function(n, coords, kappa, variance, nu=1) {
  m <- as.matrix(dist(coords))
  m <- exp((1-nu)*log(2) + nu*log(kappa*m)-lgamma(nu))*besselK(m*kappa, nu)
  diag(m) <- 1
  chol(variance*m)%*%matrix(rnorm(nrow(coords)*n), ncol=n)
}

kappa <- c(10, 8); sigma2 <- c(1/2, 1/3)
k <- ncol(kenyadata[,-c(1:2)]); rho <- c(0.9, 0.7)
beta0 <- rrf(k, coo, kappa[1], sigma2[1]) * (1-rho[1]^2)
beta1 <- rrf(k, coo, kappa[2], sigma2[2]) * (1-rho[2]^2)
for (j in 2:k) {
  beta0[, j] <- beta0[,j-1]*rho[1] + beta0[,j]
  beta1[, j] <- beta1[,j-1]*rho[2] + beta1[,j]
}

hh <- rep(sin(30 * (2*pi) * (1:k/k)), each=n) ### just 30 cycles
mu.beta <- c(0.3, -0.04); taue <- 2

require(reshape)          # matrix to vector columnwise
length(y<- melt(cbind(kenyadata[, -(1:2)]), na.rm=FALSE)$value) 

yreal=y
length(ysim <- (mu.beta[1] + beta0) + (mu.beta[2]+beta1)*hh + ### dynamic regression part
         rnorm(n*k, 0, sqrt(1/taue))) ### error in the observation

ysim2 <- as.vector(ysim)
simdf <- data.frame(yreal, ysim2)
simdf$ysimNA<- ifelse(is.na(simdf[, "yreal"]), NA, simdf[, "ysim2"]) 
simdf <- simdf[, -2]
head(simdf, n=1000)

(mesh <- inla.mesh.2d(coo, max.edge=0.5, offset=0.5, cutoff=0.5))$n
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
             control.predictor=list(A=inla.stack.A(stk.y), compute=TRUE),
             control.compute = list(config=FALSE, dic=TRUE, waic=TRUE),
             control.mode=list(theta=theta.ini, restart=TRUE)))$cpu

round(cbind(true=mu.beta, res$summary.fix), 4)

gofstatistic = cbind(dic=res$dic$dic, wiac=res$waic$waic)
write.csv(gofstatistic, "F:/CLIM_DATA/tranzoia_county/other Results/gofstatistic_realEvi2Record.csv")

rf12 <- list(rf1=inla.spde2.result(res, 'i0', spde),
             rf2=inla.spde2.result(res, 'i1', spde))

par(mfrow=c(3, 3), mar=c(2.5,2.5,0.3,0.3), mgp=c(1.5,0.5,0))
for (i in 1:2) for (j in 15:17) {
  plot(rf12[[i]][[j]][[1]], type='l', ylab='Density',
       xlab=paste('rf', i, '.', substring(names(rf12[[i]])[j],11), sep=''))
  #abline(v=c(kappa[i], sigma2[i], sqrt(8*1)/kappa[i])[j-14], col=4)
}
for (i in 1:2) {
  plot(res$marginals.hy[[c(3,6)[i]]], type='l',
       xlab=bquote(rho[.(i)]), ylab='Density')
}

plot(res$marginals.hy[[1]], type='l', xlab=expression(tau[E]), ylab='Density')

fixed_effects = round(cbind(res$summary.fix), 4)
write.csv(fixed_effects, "F:/CLIM_DATA/tranzoia_county/other Results/fixed_effects_evi2RealData.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Summary of the posterior marginal density for the AR1 process
#for each of the random fields
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
hyperPar_effects_est = round(cbind(res$summary.hy[, ]), 4)
write.csv(hyperPar_effects_est, "F:/CLIM_DATA/tranzoia_county/other Results/hyperPar_effects_est.csv")


require(reshape) 
index=inla.stack.index(stk.y, 'y')$data
df <- melt(cbind(kenyadata[, -(1:2)]), na.rm=FALSE)
df$pred.evi2 <- round(res$summary.fitted.values[index, "mean"], 4)
evi2preddf <- cbind(coo, df)
write.csv(evi2preddf, "F:/CLIM_DATA/kenya_county/Evi2predTranzoia_SPDE.csv")
