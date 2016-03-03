
require(stats)
harmonic.test <- read.csv("G:/Rdirectory/Phenology_metrics/hormonic2.csv")
attach(harmonic.test)
#STL: stands for “Seasonal Decomposition of Time Series by LOESS”.
#put the data into a time series
brz.veg.index2=brz.veg.index[1:720]
brz.veg.ts = ts(brz.veg.index, frequency=24, start=c(1982,1), end=c(2011,24))
plot(brz.veg.ts)

#subset the time series from 2008 forward using window commd
#house2 = window(house.ts, start=c(2008,1), end=c(2013, 6))
#plot(house2) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>
# from quick R
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# save a numeric vector containing 48 monthly observations
# from Jan 2009 to Dec 2014 as a time series object
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12)

# subset the time series (June 2014 to December 2014)
#myts2 <- window(myts, start=c(2014, 6), end=c(2014, 12))

# plot series
#plot(myts)

brz.veg.index2=brz.veg.index[1:720]
brz.veg.ts = ts(brz.veg.index, frequency=24, start=c(1982,1), end=c(2011,23))
plot(brz.veg.ts)

# Seasonal decomposition
fit <- stl(brz.veg.ts, s.window="period")
plot(fit)
fit2 <- as.data.frame(fit)

#additional plots
monthplot(brz.veg.ts)
library(forecast)
seasonplot(brz.veg.ts) 














require(graphics)

plot(stl(nottem, "per"))
plot(stl(nottem, s.window = 7, t.window = 50, t.jump = 1))

plot(stllc <- stl(log(co2), s.window = 21))
summary(stllc)
## linear trend, strict period.
plot(stl(log(co2), s.window = "per", t.window = 1000))

## Two STL plotted side by side :
stmd <- stl(mdeaths, s.window = "per") # non-robust
summary(stmR <- stl(mdeaths, s.window = "per", robust = TRUE))
op <- par(mar = c(0, 4, 0, 3), oma = c(5, 0, 4, 0), mfcol = c(4, 2))
plot(stmd, set.pars = NULL, labels  =  NULL,
     main = "stl(mdeaths, s.w = \"per\",  robust = FALSE / TRUE )")
plot(stmR, set.pars = NULL)
# mark the 'outliers' :
(iO <- which(stmR $ weights  < 1e-8)) # 10 were considered outliers
sts <- stmR$time.series
points(time(sts)[iO], 0.8* sts[,"remainder"][iO], pch = 4, col = "red")
par(op)   # reset

