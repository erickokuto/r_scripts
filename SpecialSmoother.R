smoother_inla<- function(stk, n=24) {
  univariate.smoother<- function(v) {
    x <- v[1:size]
    y=as.vector(x)
    N=length(y)
    nyears=N/n
    trend.effect <- seq(1:N)
    seasonal.effect<-rep(rep(1:n, each=2), times=nyears)
    if (all(is.na(y))) {
      return(rep(NA, times = size))
    } else {
      formula.univariate = y ~ f(trend.effect, model = "rw2",
                                 diagonal=1e-8, scale.model=FALSE, constr=TRUE) +
        f(seasonal.effect, model="seasonal", 
          season.length=n, constr=TRUE, 
          diagonal=1e-8) -1
      result.univariate = try(inla(formula.univariate,
                                   family="gaussian",
                                   data = data.frame(y,seasonal.effect,cyclical.effect,trend.effect),
                                   quantiles=NULL,
                                   control.compute=list(cpo=TRUE, dic = TRUE, waic = TRUE),
                                   control.predictor=list(compute=TRUE), verbose=TRUE), silent=TRUE)
      post.means=if(class(result.univariate)=="try-error") NA 
      else unclass(round(result.univariate$summary.fitted.values[1:N, "mean"], 4))
      
      
#OUTPUTS
      results.vector=post.means
      results.matrix=cbind(results.vector)
    } 
  }
  s <- stack(stk)
  size <- nlayers(s)
  result<-calc(s, fun=univariate.smoother)
  return(result)
} 
