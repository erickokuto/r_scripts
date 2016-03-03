


rm(list=ls())
require(spTDyn, warn.conflicts = FALSE)
require(spTimer)
require(INLA)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Test record from kenya county in Kenya
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
kenyadata=read.csv("F:/CLIM_DATA/tranzoia_county/combine.tranz.10KMunsmoothed.csv")
dim(kenyadata)


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
head(simdf, n=100)

coords <- as.matrix(kenyadata[1:n, 1:2])
xx=rep(coords[,1], times=k)
yy=rep(coords[,2], times=k)

formula <- ysimNA ~ mu1 + tp(mu1) + hh + tp(hh) -1
dataFit=data.frame(xx, yy, mu1, hh, ysimNA=simdf$ysimNA )
head(dataFit)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Code for analysing phenology metrics using EVI2 records
## Model: Spatially varying coefficient process models ##
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# MCMC via Gibbs using default choices

nItr<-1000
nBurn<-100

post.sp <- GibbsDyn(formula, data=dataFit, nItr=nItr, nBurn=nBurn, coords=coords,
                    cov.fnc="matern", 
                    initials=NULL,
                    spatial.decay=decay(distribution=Gamm(2,1), tuning=0.06))

plot(post.sp)

fit.val <- fitted.values(post.sp)
head(fit.val)
dim(fit.val)


sumFitReal <- summary(post.sp)[1]
write.csv(sumFitReal, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/fixed_effects_real.csv")

require(reshape) 
df <- melt(cbind(kenyadata[, -(1:2)]), na.rm=FALSE)
head(df)
df$pred.evi2.mean <- fitted.values(post.sp)[,1]
df$pred.evi2.sd <- fitted.values(post.sp)[,2]
df$x=rep(coords[,1], times=360)
df$y=rep(coords[,2], times=360)
evi2preddf <- df
head(evi2preddf)
write.csv(evi2preddf, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/RealEvi2predTranzoia_spDTyn_10km.csv")

df11<-read.csv("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/SimEvi2predTranzoia_spDTyn_10km.csv")
kenyadata=read.csv("F:/CLIM_DATA/tranzoia_county/combine.tranz.10KMunsmoothed.csv")
n <- nrow(kenyadata[, -c(1:2)])
coo <- cbind(kenyadata[1:n, 1:2])

names(df11)
#reshaping to wide format
df11$id <- rep(1:40, times=360)
names(df11)
df22=df11[, c(1, 2, 7)]
WideSimData11 <- reshape(df22, timevar = "variable",idvar = "id", direction = "wide")
WideSimData11 <- cbind(coo, WideSimData11[, -1])
write.csv(WideSimData11, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/WideSimMeans10KM.csv")
names(WideSimData11)

df33=df11[, c(1, 3, 7)]
WideSimData22 <- reshape(df33, timevar = "variable", idvar = "id", direction = "wide")
WideSimData22 <- cbind(coo, WideSimData22[, -1])
write.csv(WideSimData22, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/WideSimFittedMeans10KM.csv")
names(WideSimData22)
names(df11)

df44=df11[, c(1, 4, 7)]
WideSimData33 <- reshape(df44, timevar = "variable",idvar = "id", direction = "wide")
WideSimData33 <- cbind(coo, WideSimData33[, -1])
write.csv(WideSimData33, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/WideSimFittedSD10KM.csv")
names(WideSimData33)



