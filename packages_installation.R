
install.packages(c("tidyr"), dependencies=TRUE)
install.packages(c("BayesX"), dependencies=TRUE)
install.packages(c("gam","mgcv"), dependencies=TRUE)
install.packages(c("hydroGOF","hydromad"), dependencies=TRUE)
require(spTDyn)
require(INLA)
require(spate)
require(INLA)
require(raster)
require(rasterVis)
require(spam)
require(colorspace)
require(gam)
require(mgcv)
install.packages(c("spatstat","gridExtra","deldir", 
                   "RandomFields","geoR","splancs"), dependencies=TRUE)


install.packages(c("numDeriv", "Rgraphviz", "graph", "fields",
                   "rgl", "mvtnorm", "parallel", "pixmap", "splancs", 
                   "orthopolynom", "compiler"), 
                 dependencies=TRUE)

install.packages(c("raster", "rasterVis", "ggplot2", "spate"), 
                 dependencies=TRUE)

install.packages(c("spam"), 
                 dependencies=TRUE)

install.packages("INLA", 
                 repos="http://www.math.ntnu.no/inla/R/testing")
update.packages("INLA")