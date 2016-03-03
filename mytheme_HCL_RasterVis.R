
library(raster)
library(rasterVis)

##Solar irradiation data from CMSAF 
setwd(tempdir())
download.file('https://raw.github.com/oscarperpinan/spacetime-vis/master/data/SISmm2008_CMSAF.zip',
              'SISmm2008_CMSAF.zip', method='wget')
unzip('SISmm2008_CMSAF.zip')

listFich <- dir(pattern='\.nc')
stackSIS <- stack(listFich)
stackSIS <- stackSIS*24 ##from irradiance (W/m2) to irradiation Wh/m2
idx <- seq(as.Date('2008-01-15'), as.Date('2008-12-15'), 'month')
SISmm <- setZ(stackSIS, idx)
names(SISmm) <- month.abb

levelplot(SISmm)
levelplot(SISmm, layers=1, margin = list(FUN = 'median'), contour=TRUE)

#Including shapefile
library(maptools)
proj <- CRS('+proj=longlat +ellps=WGS84')
##Change to your folder
mapaSHP <- readShapeLines('~/Datos/ESP_adm/ESP_adm2.shp', proj4string=proj)

p <- levelplot(SISmm, layers=1, margin = list(FUN = median))
p + layer(sp.lines(mapaSHP, lwd=0.8, col='darkgray'))

f <- system.file("external/test.grd", package="raster")
r <- raster(f)
levelplot(r^2, zscaleLog=TRUE, contour=TRUE)

#selecting layer 8
Aug <- raster(SISmm, 8)
meanAug <- cellStats(Aug, mean)

#using  diverging palette is specially well suited to this data: 
levelplot(Aug-meanAug, par.settings=RdBuTheme)


#Besides, it is easy to define a new theme with a different palette. 
#For example, using a sequential palette from colorspace: 
library(colorspace)
myTheme <- rasterTheme(region=sequential_hcl(10, power=2.2))
levelplot(Aug, par.settings=myTheme, contour=TRUE)

#or with the colour-blindness corrections from the dichromat package: 
library(dichromat)
myTheme <- rasterTheme(region=dichromat(terrain.colors(15)))
levelplot(Aug, par.settings=myTheme)

#Categorical data
#A raster that contains categorical data can be defined with the ratify function
r <- raster(nrow=10, ncol=10)
r[] = 1
r[51:100] = 3
r[3:6, 1:5] = 5
r <- ratify(r)

#The levels are stored in the “Raster Attribute Table” (RAT) 
#that can be manipulated with the levels function
rat <- levels(r)[[1]]
rat$landcover <- c('Pine', 'Oak', 'Meadow')
rat$class <- c('A1', 'B2', 'C3')
levels(r) <- rat

#displaying with levelplot
levelplot(r, col.regions=c('palegreen', 'midnightblue', 'indianred1'))

#There is the att argument to choose the variable (column) from the RAT: 
levelplot(r, att='class', col.regions=c('palegreen', 'midnightblue', 'indianred1'))

#Scatterplots and histograms
bwplot(SISmm)

histogram(SISmm, FUN=as.yearqtr)

#Adding title to legend
library(lattice)
library(grid)
x = 1:10
y = rep(x,rep(10,10))
x = rep(x,rep(10))
z = x+y  
levelplot(z~x*y, colorkey=list(labels=list(cex=1,font=2,col="brown"),height=1,width=1.4),main=list('b',side=1,line=0.5))
trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
grid.text(expression(m^3/m^3), 0.2, 0, hjust=0.5, vjust=1)
trellis.unfocus()

library(maps)
map()
