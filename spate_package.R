


require(spate)
########################################################
#      Innovation spectrum Q and Mat´ern spectrum
#######################################################
n <- 100
set.seed(1)
## Simulate Matern field
matern.spec <- matern.spec(wave=spate.init(n=n,T=1)[["wave"]],
                               n=n,rho0=0.05,sigma2=1,norm=TRUE)
matern.sim <- real.fft(sqrt(matern.spec)*rnorm(n*n),n=n,inv=FALSE)
## Simulate stochstic innovation field epsilon
innov.spec <- innov.spec(wave=spate.init(n=n,T=1)[["wave"]],
                               n=n, rho0=0.05, sigma2=1, zeta=0.5,
                               rho1=0.05, alpha=pi/4, gamma=2,norm=TRUE)
innov.sim <- real.fft(sqrt(innov.spec)*rnorm(n*n),n=n,inv=FALSE)
plot(innov.sim)

############################################
#    Propagator matrix G
#############################################
n <- 4
wave <- wave.numbers(n)
G <- get.propagator(wave=wave[["wave"]], indCos=wave[["indCos"]], zeta=0.5,
                      rho1=0.1,gamma=2, alpha=pi/4, muX=0.2, muY=-0.15)

n <- 50
wave <- wave.numbers(n)
spec <- matern.spec(wave=wave[["wave"]],n=n,
                      rho0=0.05,sigma2=1,norm=TRUE)
## Initial state
alphat <- sqrt(spec)*rnorm(n*n)
## Propagate state
G <- get.propagator(wave=wave[["wave"]],indCos=wave[["indCos"]],zeta=0.1,
                        rho1=0.02, gamma=2,alpha=pi/4,muX=0.2,muY=0.2,dt=1,ns=4)
alphat1a <- as.vector(G%*%alphat)
Gvec <- get.propagator.vec(wave=wave[["wave"]],indCos=wave[["indCos"]],zeta=0.1,
                             rho1=0.02, gamma=2,alpha=pi/4,muX=0.2,muY=0.2,dt=1,ns=4)
alphat1b <- propagate.spectral(alphat,n=n,Gvec=Gvec)
## Both methods do the same thing:
sum(abs(alphat1a-alphat1b))

###################################################################
#       Two-dimensional real Fourier transform
#################################################################
n <- 20
wave <- wave.numbers(n=n)
Phi <- get.real.dft.mat(wave=wave[["wave"]],indCos=wave[["indCos"]],n=n)

## Example: reduced dimensional image reconstruction
n <- 50
## Define image
image <- rep(0,n*n)
for(i in 1:n){
  for(j in 1:n){
    image[(i-1)*n+j] <- cos(5*(i-n/2)/n*pi)*sin(5*(j)/n*pi)*
      (1-abs(i/n-1/2)-abs(j/n-1/2))
    }
  }
## Low-dimensional: only 45 (of potentially 2500) Fourier functions
spateObj <- spate.init(n=n,T=17,NF=45)
Phi.LD <- get.real.dft.mat(wave=spateObj$wave, indCos=spateObj$indCos,
                             ns=spateObj$ns, n=n)
## Mid-dimensional: 545 (of potentially 2500) Fourier functions
spateObj <- spate.init(n=n,T=17,NF=101)
Phi.MD <- get.real.dft.mat(wave=spateObj$wave, indCos=spateObj$indCos,
                             ns=spateObj$ns, n=n)
## High-dimensional: all 2500 Fourier functions
spateObj <- spate.init(n=n,T=17,NF=2500)
Phi.HD <- get.real.dft.mat(wave=spateObj$wave, indCos=spateObj$indCos,
                             ns=spateObj$ns, n=n)
## Aply inverse Fourier transform, dimension reduction,
## and then Fourier transform
image.LD <- Phi.LD %*% (t(Phi.LD) %*% image)
image.MD <- Phi.MD %*% (t(Phi.MD) %*% image)
image.HD <- Phi.HD %*% (t(Phi.HD) %*% image)

#################################################################
###  Simulation and plotting
###############################################################
StartVal <- rep(0,100^2)
StartVal[75*100+75] <- 1000
par <- c(rho0=0.05,sigma2=0.7^2,zeta=-log(0.99),rho1=0.06,
           gamma=3,alpha=pi/4,muX=-0.1,muY=-0.1,tau2=0.00001)
spateSim <- spate.sim(par=par,n=100,T=5,StartVal=StartVal,seed=1)
plot(spateSim,mfrow=c(1,5),mar=c(2,2,2,2),indScale=TRUE,
       cex.axis=1.5,cex.main=2)

####################################################################
#  Inference: log-likelihood evaluation and sampling
#             from the full conditional
###################################################################
## Example of use of ✬sample.four.coef✬
## Simulate data
##################################################################
n <- 50
T <- 4
par <- c(rho0=0.1,sigma2=0.2,zeta=0.5,rho1=0.1,
           gamma=2,alpha=pi/4,muX=0.2,muY=-0.2,tau2=0.01)
spateSim <- spate.sim(par=par,n=n,T=T,seed=4)
w <- spateSim$w
## Sample from full conditional
Nmc <- 50
alphaS <- array(0,c(T,n*n,Nmc))
wFT <- real.fft.TS(w,n=n,T=T)
for(i in 1:Nmc){
  alphaS[,,i] <- sample.four.coef(wFT=wFT,par=par,n=n,T=T,NF=n*n)
  }
## Mean from full conditional
alphaMean <- apply(alphaS,c(1,2),mean)
xiMean <- real.fft.TS(alphaMean,n=n,T=T,inv=FALSE)
#################################################################
##     Example of use of loglike
###################################################################
## Evaluation of log-likelihood
loglike(par=par,w=w,n=n,T=T)

## Equivalently, one can use the Fourier transformed data ✬wFT✬
loglike(par=par,wFT=wFT,n=n,T=T)

####################################################################
#      Maximum likelihood estimation
#####################################################################
## Simulate data
n <- 20
T <- 20
par <- c(rho0=0.1,sigma2=0.2,zeta=0.5,rho1=0.1,
          gamma=2,alpha=pi/4,muX=0.2,muY=-0.2,tau2=0.01)
spateSim <- spate.sim(par=par,n=n,T=T,seed=4)
w <- spateSim$w
## Initial values for optim
parI <- c(rho0=0.2,sigma2=0.1,zeta=0.25,rho1=0.01,gamma=1,
              alpha=0.3,muX=0,muY=0,tau2=0.005)
## Transform to log-scale
logInd=c(1,2,3,4,5,9)
parI[logInd] <- log(parI[logInd])

## Maximum likelihood estimation using optim
wFT <- real.fft.TS(w,n=n,T=T)
spateMLE <- optim(par=parI,loglike,control=list(trace=TRUE,maxit=1000),
                    wFT=wFT,method="L-BFGS-B",
                    lower=c(-10,-10,-10,-10,-10,0,-0.5,-0.5,-10),
                    upper=c(10,10,10,10,10,pi/2,0.5,0.5,10),
                    negative=TRUE,logScale=TRUE,
                    logInd=c(1,2,3,4,5,9),hessian=TRUE,n=n,T=T)
mle <- spateMLE$par
mle[logInd] <- exp(mle[logInd])
sd=sqrt(diag(solve(spateMLE$hessian)))
## Calculate confidence intervals
MleConfInt <- data.frame(array(0,c(4,9)))
colnames(MleConfInt) <- names(par)
rownames(MleConfInt) <- c("True","Estimate","Lower","Upper")
MleConfInt[1,] <- par
MleConfInt[2,] <- mle
MleConfInt[3,] <- spateMLE$par-2*sd
MleConfInt[4,] <- spateMLE$par+2*sd
MleConfInt[c(3,4),logInd] <- exp(MleConfInt[c(3,4),logInd])
## Results: estimates and confidence intervals
round(MleConfInt,digits=3)


##############################################################
#      Bayesian inference using MCMC
################################################################
## Simulate data
par <- c(rho0=0.1,sigma2=0.2,zeta=0.5,rho1=0.1,
             gamma=2,alpha=pi/4,muX=0.2,muY=-0.2,tau2=0.01)
spateSim <- spate.sim(par=par,n=20,T=20,seed=4)
w <- spateSim$w

## This is an example to illustrate the use of the MCMC algorithm.
## In practice, more samples (Nmc) are needed for a sufficiently
## large effective sample size.
spateMCMC <- spate.mcmc(y=w,x=NULL,SV=c(rho0=0.2,sigma2=0.1,
                                           zeta=0.25,rho1=0.2,gamma=1,
                                           alpha=0.3,muX=0,muY=0,tau2=0.005),
                           RWCov=diag(c(0.005,0.005,0.05,0.005,
                                          0.005,0.001,0.0002,0.0002,0.0002)),
                           Nmc=10000,BurnIn=2000,seed=4,NCovEst=500,
                           BurnInCovEst=500,trace=FALSE,Padding=FALSE)
spateMCMC

spateMCMC <- spate.mcmc(y=y,x=covTS,DataModel="SkewTobit",Sind=Sind,
                          n=100,DimRed=TRUE,NFour=29,
                          IncidenceMat=TRUE,FixEffMetrop=TRUE,Nmc=105000,
                          BurnIn=5000,Padding=TRUE,
                          NCovEst=500,BurnInCovEst=1000)
plot(spateMCMC,true=par,hist=FALSE,ask=FALSE)

####################################################################
#           Making predictions with spate.predict
####################################################################
## Make predictions
predict <- spate.predict(y=w, tPred=(21:23),
                             spateMCMC=spateMCMC, Nsim = 100,
                             BurnIn = 10, DataModel = "Normal",seed=4)
Pmean <- apply(predict,c(1,2),mean)
Psd <- apply(predict,c(1,2),sd)

?spate.mcmc
