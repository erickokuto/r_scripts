
#provides tools for plotting model fits along with confidence intervals of predicted 
#mean values (confidence bands)
install.packages("visreg", dependencies=TRUE)
require(visreg)
z <- lm(y ~ x, data = mydata)                       # regression model
visreg(z)                                           # basic plot
visreg(z, points.par = list(cex = 1.2, col = "red")) # with points options