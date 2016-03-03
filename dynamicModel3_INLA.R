
require(INLA)
inla.upgrade(testing=TRUE)
## Example for implementing the standard mixed model
##    y = X%*%beta + Z%*%b+e
## with b ~N(0,tau * Q) With Q a precision matrix
df<-read.csv("C:/Users/eokuto/Desktop/Projects/Project2/testhormonic/testhormonicreg.csv")

n = dim(df)[1]
nz = 2
nb = 1
b = inla.qsample(1, Q=Qz)
tmp = matrix(rnorm(nz^2), nz, nz)
Qz = tmp %*% t(tmp)
Z = matrix(runif(n*nz), n, nz)
X = matrix(runif(n*nb), n, nb)
#X[,1] = 1  
y = df$x

par(mfrow=c(1, 1))
plot(y,type="l", xlab="time", ylab="")

formula = y ~ -1 + X +  f(id.z, model="z", Cmatrix=Qz,  Z=Z,
                          constr=FALSE)

result = inla(formula, 
              data = list(y=y, id.z = 1:n, X=X),
              control.compute=list(cpo=FALSE, config=FALSE),
              control.predictor=list(compute=TRUE))

summary(result)

result$summary.random$id.z
result$summary.fixed[,"mean"]    # estimate for beta
result$summary.linear.pred[,"mean"]
##################################################################
#to express the uncertainty in the structured ("random")
#effects of your INLA model 
#in terms of the standard deviation
#########################################################
s <- inla.contrib.sd(result)
ss=s[1]
ss[1:10,]

#The resulting object s contains two elements:
s$hyper
#####################################################
#with the summary on the standard deviation scale.
#The other element s$samples contains the simulated
#values from the posterior, which can be used for 
#example to draw a histogram of the distribution
#which in this case produces the following graph
######################################################
hist(s$samples,xlab="standard deviation for z",main="")

#####################################################
# summary of posterior hyperparameters
# -------------------------------------
#result$summary.hyperpar

# # graph for observations (y)
# rang <- range(result$summary.random$id.z[1:n, 4:6], y)
# plot(result$summary.random$id.z[1:n,2], type="l", 
#      ylim=rang, col="red", xlim=c(1,n),ylab=expression(y[t]),xlab="time",lwd=1)
# lines(result$summary.random$id.z[1:n,4], col="blue", lty=3,lwd=1)
# lines(result$summary.random$id.z[1:n,6], col="blue", lty=3,lwd=1)
# lines(y[1:n],lwd=1)
# legend(0,0.7, legend=c("observed","post mean","95% CI"), 
#        col=c("black", "red","blue"),lty=c(1,1,2),lwd=c(1,1,1),
#        bty="n",cex=1)
# title("NDVI timeseries")


