
##################################################################################################
#
# script with R  code to implement all the examples at the paper entitled
# "Direct Fitting of dynamic models using integrated nested Laplace approximations - INLA"
# 
# We highly recommend to read the paper before running the examples.
#
# This is not intended to be an INLA tutorial. For that purpose, readers should first access the 
# introductory material at <http://www.r-inla.org/>
#
# Authors: Ramiro Ruiz C?rdenas <ramiro@est.ufmg.br> ; Elias Teixeira Krainski <eliaskr.ufpr.br>
#
# last update: 2011-10-23
##################################################################################################
# an alternative way to fit the same model using and RW1 process
# --------------------------------------------------------------


require(raster)
require(foreach)
require(INLA)


DRCevi2_lists <- list.files(file.path("F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/EVI2_Records"),
                            pattern="*.tif", full.names=TRUE)
DRCevi2_rasters<-foreach(files=1:length(DRCevi2_lists), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(DRCevi2_lists[[files]])
DRC_stk.Data.evi2<-stack(DRCevi2_rasters, bands=NULL, native=FALSE, RAT=TRUE)
nlayers(DRC_stk.Data.evi2)


#############################################################################
#   Application of joint and univariate models
##########################################################################

source("BayesianSmoother.R")

evi2.filtered <- smoother_inla(stk=DRC_stk.Data.evi2, n=24)

#univariate model
evi2.filtered.univariate <- univariate.model(stk=DRC_stk.Data.evi2)
writeRaster(evi2.filtered.univariate, 
            file="F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/Results/Univariate_model/result_stk_univariate_model/evi2.filtered.univariate.tif") 

#joint model
evi2.filtered.joint <- joint.model(stk=DRC_stk.Data.evi2)
writeRaster(evi2.filtered.joint, 
            file="F:/CLIM_DATA/DRC_EVI2_Unsmoothed_Records/Results/Univariate_model/result_stk_univariate_model/evi2.filtered.joint.tif") 


