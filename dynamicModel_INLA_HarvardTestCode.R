
require(INLA)
df<-read.csv("C:/Users/eokuto/Desktop/Projects/Project2/testhormonic/testhormonicreg.csv")

n = dim(df)[1]
nk = 180
nb = 1
X = matrix(runif(n*nb), n, nb) 
y = df$x


harmonics = matrix(0, n, nk)
for(k in 1:nk) {
  harmonics[, k] = sin(pi/n * k * 1:n)
}

# for(k in 1:nk) {
#   harmonics[, k] = sin(pi/n * k * 1:n)+cos(pi/n * k * 1:n)
# }

idx = 1:n
formula = (y ~ -1 + X + f(idx, model="z",  Z=harmonics,
                      hyper = list(prec = list(initial = -4,
                                               fixed=TRUE))))
result = inla(formula,
         data = list(y=y, idx=idx, 
                     harmonics = harmonics, X=X),
         family = "gaussian",
         control.predictor = list(compute=TRUE))
summary(result)

plot(result)

result$summary.random$idx
result$summary.fixed[,"mean"]    # estimate for beta



result$summary.linear.pred[,"mean"]

# par(mfrow=c(2, 1))
# plot(eta, result$summary.linear.predictor$mean,
#      xlab = "true",  ylab="estimated")
# abline(a=0, b=1)
# title("linear predictors")
# plot(c(beta), result$summary.random$idx$mean[(n+1):(n+nk)],
#      xlab = "true",  ylab="estimated")
# abline(a=0, b=1)
# title("betas")


result$summary.random$idx[1:2,]
par(mfrow=c(1, 1))
# # graph for observations (y)
rang <- range(result$summary.random$id.z[1:n, 4:6], y)
plot(result$summary.random$idx[1:n,2], type="l", 
     ylim=rang, col="red", xlim=c(1,n),ylab=expression(y[t]),xlab="time",lwd=1)
lines(result$summary.random$idx[1:n,4], col="blue", lty=3,lwd=1)
lines(result$summary.random$idx[1:n,6], col="blue", lty=3,lwd=1)
lines(y[1:n],lwd=1)
legend(0,0.65, legend=c("observed","post mean","95% CI"), 
       col=c("black", "red","blue"),lty=c(1,1,2),lwd=c(1,1,1),
       bty="n",cex=1)
title("EVI2")
