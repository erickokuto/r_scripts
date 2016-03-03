


rm(list=ls())
require(spTDyn, warn.conflicts = FALSE)
require(spTimer)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#  Test record from kenya county in Kenya
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
kenyadata=read.csv("F:/CLIM_DATA/tranzoia_county/combine.tranz.10KMunsmoothed.csv")
dim(kenyadata)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#   defining the spatial locations
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
n <- nrow(kenyadata[, -c(1:2)])
k <- ncol(kenyadata[,-c(1:2)])

set.seed(1)

coords <- as.matrix(kenyadata[1:n, 1:2])
xx=rep(coords[,1], times=k)
yy=rep(coords[,2], times=k)

require(reshape)          # matrix to vector columnwise
evi2<- melt(cbind(kenyadata[, -(1:2)]), na.rm=FALSE)$value 
length(evi2)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#creating the regression covariates cos and sine amplitude terms
#The two are necessary to compute annual phase and growing season length
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
mu1 <- rep(1, times = length(evi2))
hh <- rep(sin(30 * (2*pi) * (1:k/k)), each=n)

length(hh1); length(hh2)
par(mfrow=c(1,1))
plot(hh, type="l")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#A non-random (for computational reasons) intercept (mu1) 
#is added 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
formula <- evi2 ~ mu1 + tp(mu1) + hh + tp(hh) -1
dataFit=data.frame(xx, yy, mu1, hh,evi2 )


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
write.csv(sumFitReal, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/fixed_effects_real.csv")

require(reshape) 
df <- melt(cbind(kenyadata[, -(1:2)]), na.rm=FALSE)
head(df)
df$pred.evi2.mean <- fitted.values(post.sp)[,1]
df$pred.evi2.sd <- fitted.values(post.sp)[,2]
df$x=rep(coords[,1], times=360)
df$y=rep(coords[,2], times=360)
evi2preddf <- df
write.csv(evi2preddf, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/RealEvi2predTranzoia_SPDE_10km.csv")

df11<-read.csv("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/RealEvi2predTranzoia_SPDE_10km.csv")
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
write.csv(WideSimData11, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/WideObsMeans10KM.csv")
names(WideSimData11)

df33=df11[, c(1, 3, 7)]
WideSimData22 <- reshape(df33, timevar = "variable", idvar = "id", direction = "wide")
WideSimData22 <- cbind(coo, WideSimData22[, -1])
write.csv(WideSimData22, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/WideObsFittedMeans10KM.csv")
names(WideSimData22)

df44=df11[, c(1, 4, 7)]
WideSimData33 <- reshape(df44, timevar = "variable",idvar = "id", direction = "wide")
WideSimData33 <- cbind(coo, WideSimData33[, -1])
write.csv(WideSimData33, "F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/WideObsFittedSD10KM.csv")




