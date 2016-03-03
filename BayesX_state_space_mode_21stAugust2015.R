
#############################################################################
#      BayesX with MCMC
##############################################################################
require(BayesX)
require(R2BayesX)
#hyper-parameters a and b for the inverse gamma prior
hyp.prior = c(1, 0.005)

## random walks
bayesx.construct(sx(x1, bs = "rw1"))
bayesx.construct(sx(x1, bs = "rw2"))

## seasonal
bayesx.construct(sx(x, bs = "season"))

formula2 = test_obs ~ sx(trend.effect, bs = "rw1", center = FALSE) +
  sx(cyclical.effect, bs = "rw1") +
  sx(seasonal.effect, bs="season")-1

## seasonal
bayesx.construct(sx(x, bs = "season"))

## estimate models with
## bayesx MCMC and REML
result1 <- bayesx(formula = formula2, method = "MCMC", 
                  family = "gaussian", data = df,
                  bayesx.control(predict = TRUE),
                  verbose=TRUE)

result2 <- bayesx(formula = formula2, method = "REML", 
                  family = "gaussian", data = df,
                  verbose=TRUE)

## prediction model from refitting with weights

Pred<-subset(df, is.na(df$test_obs))

obs.pred <- predict(result2, newdata = Pred[, -1], type = "response")


#result2$effects$sx("seasonal.effect")
fitted<-result2[5]
resid<-result2[6]
respons<-result2[7]
random_effects_estimates<-result2[8]

plot(seasonal)
trend<-result2$effects$`sx(trend.effect)`[, c(2, 5)]
cyclic<-result2$effects$`sx(cyclical.effect)`[, c(2, 5)]
season<-result2$effects$`sx(seasonal.effect)`[, c(2, 5)]


plot(trend[,1], lty=1)
plot(cyclic[,1], lty=1)
plot(season[,1], lty=1)

##compare reported output
summary(result1)
summary(result2)

## extract fitted values
fit <- fitted(result1)
fitted(result1, term = "sx(trend.effect)")$Mean
fitted(result1, term = "sx(cyclical.effect)")$Mean
fitted(result1, term = "sx(seasonal.effect)")$Mean
fitted(result1, term = "sx(trend.effect)")$Mean

## now extract 1st model term
## and plot it
fx <- fitted(b1, term = "sx(x)")


## Not run:
## generate some data
set.seed(121)
n <- 500

## regressors
dat <- data.frame(x = runif(n, -3, 3), z = runif(n, 0, 1),
                  w = runif(n, 0, 3))
dfpred<-dat[,-(4)]

## generate response 
dat$y <- with(dat, 1.5 + sin(x) + z -3 * w + rnorm(n, sd = 0.6))

## estimate model
b <- bayesx(y ~ sx(x) + z + w, data = dat)

## create some data for which predictions are required
nd <- data.frame(x = seq(2, 5, length = 100), z = 1, w = 0)

## prediction model from refitting with weights
dfpred$fit <- predict(b, newdata = dfpred)
plot(fit ~ x, type = "l", data = nd)
## End(Not run)

install.packages("rjags", dependencies=TRUE)

require(rjags)
jags.model(file, data=sys.frame(sys.parent()), inits,
           n.chains = 1, n.adapt=1000, quiet=FALSE)