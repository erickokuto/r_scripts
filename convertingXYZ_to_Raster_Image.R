

require(raster)
install.packages("arulesViz")
library(arulesViz)
#Real dataset
xyz1 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/WideObsMeans10KM.csv")
rstack.real.obs <- rasterFromXYZ(xyz1)
writeRaster(rstack.real.obs, 'F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataObs/RealObsEviTranz.tif', bylayer=TRUE)

xyz2 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/WideObsFittedMeans10KM.csv")
names(xyz2)
rstack.real.Fitted <- rasterFromXYZ(xyz2)
writeRaster(rstack.real.Fitted, 'F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/RealDataFitted/FittedObsEviTranz.tif', bylayer=TRUE)
plot(rstack.real.Fitted[[1]])

# Simulated data
xyz3 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/INLA/SimData/WideSimData10KM.csv")
names(xyz3)
rstack.real.obs3 <- rasterFromXYZ(xyz3)
writeRaster(rstack.real.obs3, 'F:/CLIM_DATA/tranzoia_county/other Results/INLA/SimData/SimDataObs/SimEviTranz.tif', bylayer=TRUE)
plot(rstack.real.obs3[[1]])


xyz4 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/INLA/SimData/WideSimFittedmeans10KM.csv")
names(xyz4)
rstack.real.Fitted4 <- rasterFromXYZ(xyz4)
writeRaster(rstack.real.Fitted4, 'F:/CLIM_DATA/tranzoia_county/other Results/INLA/SimData/SimDataFitted/FittedSimTranz.tif', bylayer=TRUE)

# spDTyn
xyz11 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/INLA/realData/WideObsMeans10KM.csv")
names(xyz11)
rstack.real.obs11 <- rasterFromXYZ(xyz11)
writeRaster(rstack.real.obs11, 'F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/RealDataObs/RealSimEviTranz.tif', bylayer=TRUE)

xyz22 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/WideObsFittedMeans10KM.csv")
names(xyz22)
rstack.real.Fitted22 <- rasterFromXYZ(xyz22)
writeRaster(rstack.real.Fitted22, 'F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/RealData/RealDataFitted/FittedSimEviTranz.tif', bylayer=TRUE)


# Simulated data
xyz33 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/WideSimData10KM.csv")
names(xyz33)
rstack.real.obs33 <- rasterFromXYZ(xyz33)
writeRaster(rstack.real.obs33, 'F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/SimDataObs/SimEviTranz.tif', bylayer=TRUE)

plot(rstack.real.obs33[[3]])

xyz44 <- read.csv("F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/WideSimFittedmeans10KM.csv")
names(xyz44)
rstack.real.Fitted44 <- rasterFromXYZ(xyz44)
writeRaster(rstack.real.Fitted44, 'F:/CLIM_DATA/tranzoia_county/other Results/spDTyn/SimData/SimDataFitted/FittedSimTranz.tif', bylayer=TRUE)


