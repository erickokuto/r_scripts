

rm(list=ls())
require(spTDyn, warn.conflicts = FALSE)
require(spTimer)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Test record from kenya county in Kenya
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
kenyadata=read.csv("F:/CLIM_DATA/EVI2_Smoothing_Datasets/Kenya_evi2_Records/kenya.monthly.20KMunsmoothedEVI2.csv")
dim(kenyadata)
kenyadata2<-kenyadata[, 1:14]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   defining the spatial locations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
n <- nrow(kenyadata2[, -c(1:2)])
k <- ncol(kenyadata2[,-c(1:2)])

set.seed(1)

coords <- as.matrix(kenyadata[1:n, 1:2])
xx=rep(coo[,1], times=k)
yy=rep(coo[,2], times=k)

require(reshape)          # matrix to vector columnwise
evi2<- melt(cbind(kenyadata2[, -(1:2)]), na.rm=FALSE)$value 
length(evi2)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#creating the regression covariates cos and sine amplitude terms
#The two are necessary to compute annual phase and growing season length
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mu1 <- rep(1, times = length(evi2))
hh1 <- as.vector(rep(sin(1 * (2*pi) * (1:k/k)), each=n)) ### just 30 cycles
hh2 <- as.vector(rep(cos(1 * (2*pi) * (1:k/k)), each=n)) ### just 30 cycles

length(hh1); length(hh2)
par(mfrow=c(2,1))
plot(hh1, type="l")
plot(hh2, type="l")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#A non-random (for computational reasons) intercept (mu1) 
#is added 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
formula <- evi2 ~ mu1 + sp(mu1) + hh1 + sp(hh1) + hh2 + sp(hh2)-1
dataFit=data.frame(xx, yy, mu1, hh1, hh2, evi2 )


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Code for analysing phenology metrics using EVI2 records
## Model: Spatially varying coefficient process models ##
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# MCMC via Gibbs using default choices

nItr<-3000
nBurn<-1000

post.sp <- GibbsDyn(formula, data=dataFit, nItr=nItr, nBurn=nBurn, coords=coo,
                    cov.fnc="matern", initials=NULL,
                    spatial.decay=decay(distribution=Gamm(2,1), tuning=0.06))
print(post.sp)

# parameter summary
summary(post.sp)                  # without spatially varying coefficients
summary(post.sp, coefficient="spatial")

plot(post.sp, density=FALSE) # without spatially varying coefficients
plot(post.sp, coefficient="spatial", density=FALSE)











