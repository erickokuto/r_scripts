
library(INLA)
set.seed(123)

n = 100
nz = 5
nb = 4
tmp = matrix(rnorm(nz^2), nz, nz)
Qz = tmp %*% t(tmp)
Z = matrix(runif(n*nz), n, nz)
b = inla.qsample(1, Q=Qz)
beta = 10 + rnorm(nb)
X = matrix(runif(n*nb), n, nb)

eta = X %*% beta + Z %*% b
y = eta + rnorm(n, sd=0.1)
plot(y,type="l", xlab="time", ylab="")

idx.beta = c(1:nb, rep(NA, nz))
idx.b = c(rep(NA, nb), 1:nz)
hyper.fixed = list(prec = list(initial = log(0.001), fixed=TRUE))
formula = y ~ -1 + f(idx.beta,  model="iid", hyper = hyper.fixed, constr = TRUE) +
                   f(idx.b, model="generic", Cmatrix=Qz)
result = inla(formula, data = list(y=y, idx.beta=idx.beta, idx.b=idx.b),
        control.predictor = list(A=cBind(X, Z)))   # cBind is sparse matrix version of cbind
formula = y ~ -1 + f(idx.beta,  model="iid", hyper = hyper.fixed, constr = FALSE) +
  f(idx.b, model="generic", Cmatrix=Qz)
result1 = inla(formula, data = list(y=y, idx.beta=idx.beta, idx.b=idx.b),
              control.predictor = list(A=cBind(X, Z)))   # cBind is sparse matrix version of cbind

beta
result$summary.random$idx.beta
result1$summary.random$idx.beta
b
result$summary.random$idx.b
result1$summary.random$idx.b


par(mfrow=c(1, 2))


plot(result$summary.random$idx.beta$mean , beta, xlim= c(-2,12),  ylim= c(-2,12))
points(result1$summary.random$idx.beta$mean , beta, pch =  6)
abline(0, 1)

plot(result$summary.random$idx.b$mean , b, xlim= c(-20,12),  ylim= c(-20,12))
points(result1$summary.random$idx.b$mean , b, pch =  6)

abline(0, 1)
