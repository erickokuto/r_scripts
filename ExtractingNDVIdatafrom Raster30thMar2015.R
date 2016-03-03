##########################################################################
#  Test timeseries: point in Brazil
######################################################################
require(plyr);require(raster);require(foreach)
size<-720   #nlayers(stk.Jan_VIP)
days.15<-rep(c("15a", "15b"), times=size/2)
months<-rep(rep(1:12, each=2), times=30)
years<-rep(1982:2011, each=24)
# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# years.gimms<-c(rep(2000:2011, each=24), rep(1982:1999, each=24))
# months.gimms<-rep(rep(c("apr", "aug", "dec", "feb", "jan", "jul", "jun",
#                         "mar", "may", "nov", "oct", "sep"), each=2), times=30)
# days.15.gimms<-rep(c("15a", "15b"), times=size/2)
# #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
df<-data.frame(days.15,months,years)

evi2.sgolay <- list.files(file.path("J:/ProcessedData/DataSGOLAYSmoothed"),pattern="*.tif", full.names=TRUE)
evi2.whit <- list.files(file.path("J:/ProcessedData/DataWhittakerSmoothed"),pattern="*.tif", full.names=TRUE)
evi2.spde <- list.files(file.path("J:/ProcessedData/DataSPDESmoothed"),pattern="*.tif", full.names=TRUE)
evi2.raw.filtered <- list.files(file.path("J:/ProcessedData/DataFilteredEvi2"),pattern="*.tif", full.names=TRUE)


# based on variable values
LAI=read.csv("J:/ProcessedData/LAI-EVI2/LAI_data_clean_1394samples2.csv")
# names(mydata)
# names(mydata)
# newdata <- mydata[ which(mydata$crop_type=='maize'
#                          | mydata$crop_type=='wheat' 
#                           | mydata$crop_type=='soybean'), ]
# dim(newdata)
# write.csv(newdata, "J:/ProcessedData/LAI-EVI2/LAI_data_clean_688samples.csv",overwrite=TRUE)


raster.evi2.sgolay<-foreach(VIP=1:length(evi2.sgolay), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.sgolay[[VIP]])
raster.evi2.whit<-foreach(VIP=1:length(evi2.whit), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.whit[[VIP]])
raster.evi2.spde<-foreach(VIP=1:length(evi2.spde), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.spde[[VIP]])
raster.evi2.raw.filtered<-foreach(VIP=1:length(evi2.raw.filtered), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(evi2.raw.filtered[[VIP]])



# VIP.raster.NDVI<-llply(VIP.list.NDVI, .fun = raster, .progress = "none", .inform = FALSE,
#                        .print = FALSE, .parallel = FALSE, .paropts = NULL)
# VIP.raster.EVI2<-llply(VIP.list.EVI2, .fun = raster, .progress = "none", .inform = FALSE,
#                        .print = FALSE, .parallel = FALSE, .paropts = NULL)
# GIMMS.raster<-llply(VIP.list.GIMMS, .fun = raster, .progress = "none", .inform = FALSE,
#                     .print = FALSE, .parallel = FALSE, .paropts = NULL)

stack.sgolay<- stack(raster.evi2.sgolay)
stack.whit<- stack(raster.evi2.whit)
stack.spde<- stack(raster.evi2.spde)
stack.raw.filtered<- stack(raster.evi2.raw.filtered)

names(LAI)
coord.df<-cbind(LAI[, c("lon","lat")])
names(coord.df)

#Latlong<-yFromRow(stk.1.mean_VIP, row=1:nrow(stk.1.mean_VIP))
points2<-as.matrix(coord.df)

cell.nos<-cellFromXY(stack.sgolay[[1]], points2)

#xy from a raster:
xyfromraster<-xyFromCell(stack.sgolay[[1]], cell.nos)

#xyfromraster2=as.data.frame(xyfromraster)
#write.csv(xyfromraster2, "J:/ProcessedData/LAI-EVI2/longlatpixel.csv")

#combine.coord<-cbind(points2,xyfromraster)
#head(combine.coord)
#colnames(combine.coord)<-c("long_fromLAI", "lat_fromLAI","long_fromraster","latfromraster")
#write.csv(combine.coord,"/vm/eokuto/final_VIP_GIMMS_5km_15days/combine.coord.for.LAI.csv")


#LAI=read.csv("J:/ProcessedData/LAI-EVI2/LAIsubdata24thMar2015.csv")
#points2=cbind(LAI[,1:2])
evi2.sgolay.extracted<-extract(stack.sgolay, xyfromraster)
evi2.whit.extracted<-extract(stack.whit, xyfromraster)
evi2.spde.extracted<-extract(stack.spde, xyfromraster)
evi2.raw.extracted<-extract(stack.raw.filtered, xyfromraster)

# VIP.NDVI<-extract(VIP.stack.NDVI, xyfromraster)
# VIP.EVI2<-extract(VIP.stack.EVI2, xyfromraster)
# GIMMS.NDVI<-extract(GIMMS.stack.NDVI, xyfromraster)

# dim(VIP.NDVI)[1];dim(VIP.NDVI)[2]

evi2.sgolay.extracted2<-cbind(xyfromraster, evi2.sgolay.extracted)
evi2.whit.extracted2<-cbind(xyfromraster, evi2.whit.extracted)
evi2.spde.extracted2<-cbind(xyfromraster, evi2.spde.extracted)
evi2.raw.extracted2<-cbind(xyfromraster, evi2.raw.extracted)


for ( i in 1:ncol(evi2.raw.extracted2)) { 
  colnames(evi2.raw.extracted2)<- c("lon","lat",  
                                       foreach(i=3:ncol(evi2.raw.extracted2)-2, .packages="raster", .combine=c, .verbose=FALSE) %do%
                                         paste("evi2","_",years[i],"_",months[i],"_", days.15[i],"_","raw", sep=''))
} 

write.csv(evi2.raw.extracted2, "J:/ProcessedData/LAI-EVI2/evi2.raw.extracted.csv")


for ( i in 1:ncol(evi2.sgolay.extracted2)) { 
  colnames(evi2.sgolay.extracted2)<- c("lon","lat",  
              foreach(i=3:ncol(evi2.sgolay.extracted2)-2, .packages="raster", .combine=c, .verbose=FALSE) %do%
              paste("evi2","_",years[i],"_",months[i],"_", days.15[i],"_","sgolay", sep=''))
} 

write.csv(evi2.sgolay.extracted2, "J:/ProcessedData/LAI-EVI2/evi2.sgolay.extracted.csv")


for ( i in 1:ncol(evi2.whit.extracted2)) { 
  colnames(evi2.whit.extracted2)<- c("lon","lat",
                                       foreach(i=3:ncol(evi2.whit.extracted2)-2, .packages="raster", .combine=c, .verbose=FALSE) %do%
                                         paste("evi2","_",years[i],"_",months[i],"_", days.15[i],"_","whit", sep=''))
} 

write.csv(evi2.whit.extracted2, "J:/ProcessedData/LAI-EVI2/evi2.whit.extracted.csv")

for ( i in 1:ncol(evi2.spde.extracted2)) { 
  colnames(evi2.spde.extracted2)<- c("lon","lat",
                                       foreach(i=3:ncol(evi2.spde.extracted2)-2, .packages="raster", .combine=c, .verbose=FALSE) %do%
                                         paste("evi2","_",years[i],"_",months[i],"_", days.15[i],"_","spde", sep=''))
} 

write.csv(evi2.spde.extracted2, "J:/ProcessedData/LAI-EVI2/evi2.spde.extracted.csv")
##############################################################################################################

# df.LAI=read.csv("J:/ProcessedData/LAI-EVI2/mainfileLAImerge.csv")
# names(df.LAI)
# fun=function(x){ifelse(x<=15, "15a", "15b")}
# 
# df.LAI$day2=apply(cbind(df.LAI$day), 2,fun)
# df.LAI$day3=paste(df.LAI$year,"_",df.LAI$month,"_", df.LAI$day2, sep='')
# ##################################################################################
#   LAI DATA EXTRACTION 
###################################################################################
LAI=read.csv("J:/ProcessedData/LAI-EVI2/LAI_data_clean_1394samples2.csv")
LAI=LAI[ , -which(names(LAI) %in% c("lon","lat"))]

sgolay=read.csv("J:/ProcessedData/LAI-EVI2/evi2.sgolay.extracted.csv")
whit=read.csv("J:/ProcessedData/LAI-EVI2/evi2.whit.extracted.csv")
spde=read.csv("J:/ProcessedData/LAI-EVI2/evi2.spde.extracted.csv")
raw=read.csv("J:/ProcessedData/LAI-EVI2/evi2.raw.extracted.csv")
LAI$lon=sgolay[,1]
LAI$lat=sgolay[,2]
#sgolay[1:2,]
#sgolay$id=seq(1:dim(sgolay)[1])

fun=function(x){ifelse(x<=15, "15a", "15b")}
LAI$day2=apply(cbind(LAI$day), 2,fun)
LAI$date=paste(LAI$year,"_",LAI$month,"_", LAI$day2, sep='')

LAI=LAI[, c("lon","lat","plant", "LAI","TOA_EVI2","RF_EVI2","date")]
head(LAI)
names(LAI)
head(sgolay)

require(reshape2)
names(sgolay)



meltsgolay <- melt(sgolay, id.vars = c("lon", "lat")) # [a]ir [q]uality [l]ong format
meltwhit<-melt(whit, id.vars = c("lon", "lat"))
meltspde<-melt(spde, id.vars = c("lon", "lat"))
meltraw<-melt(raw, id.vars = c("lon", "lat"))
dim(meltsgolay)


date0=read.csv("J:/ProcessedData/LAI-EVI2/Dates.csv")
date1=rep(date0$date, each=dim(meltsgolay)[1]/(dim(sgolay)[2]-2))
length(date1)

meltsgolay$date=date1
meltwhit$date=date1
meltspde$date=date1
meltraw$date=date1

names(meltraw)
#meltsgolay2=as.data.frame(meltsgolay)
meltsgolay2=meltsgolay[ ,-3]
meltwhit2=meltwhit[ ,-3]
meltspde2=meltspde[ ,-3]
meltraw2=meltraw[ ,-3]

meltsgolay2$id2=rep(1:1394, times=720)
meltwhit2$id2=rep(1:1394, times=720)
meltspde2$id2=rep(1:1394, times=720)
meltraw2$id2=rep(1:1394, times=720)

dim(meltsgolay2)
names(meltsgolay2)

write.csv(meltsgolay2, "J:/ProcessedData/LAI-EVI2/meltsgolay2.csv")
write.csv(meltwhit2, "J:/ProcessedData/LAI-EVI2/meltwhit2.csv")
write.csv(meltspde2, "J:/ProcessedData/LAI-EVI2/meltspde2.csv")
write.csv(meltraw2, "J:/ProcessedData/LAI-EVI2/meltraw2.csv")

write.csv(LAI, "J:/ProcessedData/LAI-EVI2/LAI.csv")
########################################################################################################
##     GCB PAPER: LAI EXTRACTION REVIEWED
##
##########################################################################################################

require(plyr);require(raster);require(foreach)
gimms<-read.csv("J:/LAI_EXT_VI/New_extracted_LAI/gimms.NDVI.updated.csv")
vip.ndvi<-read.csv("J:/LAI_EXT_VI/New_extracted_LAI/vip.NDVI.updated.csv")
vip.evi2<-read.csv("J:/LAI_EXT_VI/New_extracted_LAI/vip.EVI2.updated.csv")
coods<-read.csv("J:/LAI_EXT_VI/New_extracted_LAI/combine.coord.for.LAI.csv")
LAI<-read.csv("J:/LAI_EXT_VI/New_extracted_LAI/LAI_data_Dec2014.csv")
LAI$lon=coods[, "x_fromraster"]
LAI$lat=coods[, "y_fromraster"]

names(gimms)
names(vip.ndvi)
names(vip.evi2)
#r.8kmR<-raster("J:/LAI_EXT_VI/geo05apr15a_grefv3.tif")
View(vip.evi2)
View(gimms)
View(vip.ndvi)
View(coods)
View(LAI)

head(LAI)
size<-720   #nlayers(stk.Jan_VIP)
days.15<-rep(c("15a", "15b"), times=size/2)
months<-rep(rep(1:12, each=2), times=30)
years<-rep(1982:2011, each=24)

months.gimms<-rep(rep(c(4,8,12,2,1,7,6,3,5,11,10,9), each=2), times=30)
years.gimms<-rep(c(2000:2011, 1982:1999), each=24)

#coord.df<-cbind(gimms[, c("lon","lat")])
#names(coord.df)





for ( i in 1:ncol(gimms)) { 
  colnames(gimms)<- c("lon","lat",  
                                    foreach(i=3:ncol(gimms)-2, .packages="raster", .combine=c, .verbose=FALSE) %do%
                                      paste("gimms","_",years.gimms[i],"_",months.gimms[i],"_", days.15[i], sep=''))
} 

write.csv(gimms, "J:/LAI_EXT_VI/New_extracted_LAI/gimms-NDVI.csv")


for ( i in 1:ncol(vip.ndvi)) { 
  colnames(vip.ndvi)<- c("lon","lat",  
                                       foreach(i=3:ncol(vip.ndvi)-2, .packages="raster", .combine=c, .verbose=FALSE) %do%
                                         paste("ndvi","_",years[i],"_",months[i],"_", days.15[i], sep=''))
} 

write.csv(vip.ndvi, "J:/LAI_EXT_VI/New_extracted_LAI/vip-ndvi.csv")


for ( i in 1:ncol(vip.evi2)) { 
  colnames(vip.evi2)<- c("lon","lat",
                                     foreach(i=3:ncol(vip.evi2)-2, .packages="raster", .combine=c, .verbose=FALSE) %do%
                                       paste("evi2","_",years[i],"_",months[i],"_", days.15[i], sep=''))
} 

write.csv(vip.evi2, "J:/LAI_EXT_VI/New_extracted_LAI/vip-evi2.csv")
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
##  Preparing data for reshaping
##
#####>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fun=function(x){ifelse(x<=15, "15a", "15b")}
LAI$day2=apply(cbind(LAI$Day), 2,fun)
LAI$date=paste(LAI$Year,"_",LAI$Month,"_", LAI$day2, sep='')

LAI=LAI[, c("lon","lat","Crop", "Crop.Class" , "LAI", "LS.NDVI", "LS.EVI2","date")]
head(LAI)
names(LAI)
write.csv(LAI, "J:/LAI_EXT_VI/New_extracted_LAI/LAI_Updated_30thMar2015.csv")



require(reshape2)


meltgimms<- melt(gimms, id.vars = c("lon", "lat")) # [a]ir [q]uality [l]ong format
meltvip.ndvi<-melt(vip.ndvi, id.vars = c("lon", "lat"))
meltvip.evi2<-melt(vip.evi2, id.vars = c("lon", "lat"))
dim(meltgimms)
dim(gimms)[1]    
dim(vip.ndvi)[1]  
names(meltgimms)

date.gimms=data.frame(years.gimms,months.gimms,days.15)
date.vip=data.frame(years,months,days.15)
head(date.gimms)


date.gmmsFinal=rep(paste(date.gimms$years.gimms,"_",date.gimms$months.gimms,"_", date.gimms$days.15, sep=''), each=dim(gimms)[1] )
date.vipFinal=rep(paste(date.vip$years,"_",date.vip$months,"_", date.vip$days.15, sep=''), each=dim(vip.ndvi)[1] )

length(date.gmmsFinal)
length(date.vipFinal)

meltgimms$date=date.gmmsFinal
meltvip.ndvi$date=date.vipFinal
meltvip.evi2$date=date.vipFinal


names(meltvip.evi2)
#meltsgolay2=as.data.frame(meltsgolay)
meltgimms2=meltgimms[ ,-3]
meltvip.ndvi2=meltvip.ndvi[ ,-3]
meltvip.evi2=meltvip.evi2[ ,-3]

write.csv(meltgimms2, "J:/LAI_EXT_VI/New_extracted_LAI/meltgimms2.csv")
write.csv(meltvip.ndvi2, "J:/LAI_EXT_VI/New_extracted_LAI/meltvip.ndvi2.csv")
write.csv(meltvip.evi2, "J:/LAI_EXT_VI/New_extracted_LAI/meltvip.evi2.csv")
##########################








##############################################################################################################

# df.LA


#################################################################################################################################################################################3
#   15Day to Monthly data
##################################################################################################################################################################################
require(foreach); require(raster)

fun.flag<-function(x){
  y=ifelse(x > 1, 1,
           ifelse(x < -1, -1, x))
  y[is.na(y)] <- -999
  y[ is.nan(y) ] <- -999
  return(y)
}

##############################################################################
# January
##############################################################################
VIP.Jan <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Jan/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Jan <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Jan/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Jan_files_VIP<-foreach(Jan=1:length(VIP.Jan), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Jan[[Jan]])
Raster_Jan_files_GIMMS<-foreach(Jan=1:length(GIMMS.Jan), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Jan[[Jan]])


Raster_Jan_files_VIP <- stack(calc(stack(Raster_Jan_files_VIP[[1]], Raster_Jan_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Jan_files_VIP[[3]], Raster_Jan_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[5]], Raster_Jan_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[7]], Raster_Jan_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[9]], Raster_Jan_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[11]], Raster_Jan_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[13]], Raster_Jan_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[15]], Raster_Jan_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[17]], Raster_Jan_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[19]], Raster_Jan_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[21]], Raster_Jan_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[23]], Raster_Jan_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[25]], Raster_Jan_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[27]], Raster_Jan_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[29]], Raster_Jan_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[31]], Raster_Jan_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[33]], Raster_Jan_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[35]], Raster_Jan_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[37]], Raster_Jan_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[39]], Raster_Jan_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[41]], Raster_Jan_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[43]], Raster_Jan_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[45]], Raster_Jan_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[47]], Raster_Jan_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[49]], Raster_Jan_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[51]], Raster_Jan_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[53]], Raster_Jan_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[55]], Raster_Jan_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[57]], Raster_Jan_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Jan_files_VIP[[59]], Raster_Jan_files_VIP[[60]]), fun=max))

Raster_Jan_files_GIMMS <- stack(calc(stack(Raster_Jan_files_GIMMS[[25]], Raster_Jan_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[27]], Raster_Jan_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[29]], Raster_Jan_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[31]], Raster_Jan_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[33]], Raster_Jan_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[35]], Raster_Jan_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[37]], Raster_Jan_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[39]], Raster_Jan_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[41]], Raster_Jan_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[43]], Raster_Jan_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[45]], Raster_Jan_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[47]], Raster_Jan_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[49]], Raster_Jan_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[51]], Raster_Jan_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[53]], Raster_Jan_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[55]], Raster_Jan_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[57]], Raster_Jan_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[59]], Raster_Jan_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[1]], Raster_Jan_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[3]], Raster_Jan_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[5]], Raster_Jan_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[7]], Raster_Jan_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[9]], Raster_Jan_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[11]], Raster_Jan_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[13]], Raster_Jan_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[15]], Raster_Jan_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[17]], Raster_Jan_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[19]], Raster_Jan_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[21]], Raster_Jan_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Jan_files_GIMMS[[23]], Raster_Jan_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Jan_files_VIP <- foreach(Jan=1:nlayers(Raster_Jan_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Jan_files_VIP[[Jan]], fun=fun.flag)
Raster_Jan_files_GIMMS<-foreach(Jan=1:nlayers(Raster_Jan_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Jan_files_GIMMS[[Jan]], fun=fun.flag)

for (i in 1:length(Raster_Jan_files_VIP)) {
  writeRaster(Raster_Jan_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i],01,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Jan_files_GIMMS)) {
  writeRaster(Raster_Jan_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 01,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# February
##############################################################################
VIP.Feb <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Feb/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Feb <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Feb/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Feb_files_VIP<-foreach(Feb=1:length(VIP.Feb), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Feb[[Feb]])
Raster_Feb_files_GIMMS<-foreach(Feb=1:length(GIMMS.Feb), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Feb[[Feb]])

Raster_Feb_files_VIP <- stack(calc(stack(Raster_Feb_files_VIP[[1]], Raster_Feb_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Feb_files_VIP[[3]], Raster_Feb_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[5]], Raster_Feb_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[7]], Raster_Feb_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[9]], Raster_Feb_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[11]], Raster_Feb_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[13]], Raster_Feb_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[15]], Raster_Feb_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[17]], Raster_Feb_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[19]], Raster_Feb_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[21]], Raster_Feb_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[23]], Raster_Feb_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[25]], Raster_Feb_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[27]], Raster_Feb_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[29]], Raster_Feb_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[31]], Raster_Feb_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[33]], Raster_Feb_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[35]], Raster_Feb_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[37]], Raster_Feb_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[39]], Raster_Feb_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[41]], Raster_Feb_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[43]], Raster_Feb_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[45]], Raster_Feb_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[47]], Raster_Feb_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[49]], Raster_Feb_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[51]], Raster_Feb_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[53]], Raster_Feb_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[55]], Raster_Feb_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[57]], Raster_Feb_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Feb_files_VIP[[59]], Raster_Feb_files_VIP[[60]]), fun=max))

Raster_Feb_files_GIMMS <- stack(calc(stack(Raster_Feb_files_GIMMS[[25]], Raster_Feb_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[27]], Raster_Feb_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[29]], Raster_Feb_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[31]], Raster_Feb_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[33]], Raster_Feb_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[35]], Raster_Feb_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[37]], Raster_Feb_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[39]], Raster_Feb_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[41]], Raster_Feb_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[43]], Raster_Feb_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[45]], Raster_Feb_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[47]], Raster_Feb_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[49]], Raster_Feb_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[51]], Raster_Feb_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[53]], Raster_Feb_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[55]], Raster_Feb_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[57]], Raster_Feb_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[59]], Raster_Feb_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[1]], Raster_Feb_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[3]], Raster_Feb_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[5]], Raster_Feb_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[7]], Raster_Feb_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[9]], Raster_Feb_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[11]], Raster_Feb_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[13]], Raster_Feb_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[15]], Raster_Feb_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[17]], Raster_Feb_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[19]], Raster_Feb_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[21]], Raster_Feb_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Feb_files_GIMMS[[23]], Raster_Feb_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Feb_files_VIP <- foreach(Feb=1:nlayers(Raster_Feb_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Feb_files_VIP[[Feb]], fun=fun.flag)
Raster_Feb_files_GIMMS<-foreach(Feb=1:nlayers(Raster_Feb_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Feb_files_GIMMS[[Feb]], fun=fun.flag)

for (i in 1:length(Raster_Feb_files_VIP)) {
  writeRaster(Raster_Feb_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 02,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Feb_files_GIMMS)) {
  writeRaster(Raster_Feb_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 02,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# March
##############################################################################
VIP.Mar <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Mar/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Mar <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Mar/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Mar_files_VIP<-foreach(Mar=1:length(VIP.Mar), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Mar[[Mar]])
Raster_Mar_files_GIMMS<-foreach(Mar=1:length(GIMMS.Mar), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Mar[[Mar]])

Raster_Mar_files_VIP <- stack(calc(stack(Raster_Mar_files_VIP[[1]], Raster_Mar_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Mar_files_VIP[[3]], Raster_Mar_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[5]], Raster_Mar_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[7]], Raster_Mar_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[9]], Raster_Mar_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[11]], Raster_Mar_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[13]], Raster_Mar_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[15]], Raster_Mar_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[17]], Raster_Mar_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[19]], Raster_Mar_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[21]], Raster_Mar_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[23]], Raster_Mar_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[25]], Raster_Mar_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[27]], Raster_Mar_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[29]], Raster_Mar_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[31]], Raster_Mar_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[33]], Raster_Mar_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[35]], Raster_Mar_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[37]], Raster_Mar_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[39]], Raster_Mar_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[41]], Raster_Mar_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[43]], Raster_Mar_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[45]], Raster_Mar_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[47]], Raster_Mar_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[49]], Raster_Mar_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[51]], Raster_Mar_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[53]], Raster_Mar_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[55]], Raster_Mar_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[57]], Raster_Mar_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Mar_files_VIP[[59]], Raster_Mar_files_VIP[[60]]), fun=max))

Raster_Mar_files_GIMMS <- stack(calc(stack(Raster_Mar_files_GIMMS[[25]], Raster_Mar_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[27]], Raster_Mar_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[29]], Raster_Mar_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[31]], Raster_Mar_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[33]], Raster_Mar_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[35]], Raster_Mar_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[37]], Raster_Mar_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[39]], Raster_Mar_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[41]], Raster_Mar_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[43]], Raster_Mar_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[45]], Raster_Mar_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[47]], Raster_Mar_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[49]], Raster_Mar_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[51]], Raster_Mar_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[53]], Raster_Mar_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[55]], Raster_Mar_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[57]], Raster_Mar_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[59]], Raster_Mar_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[1]], Raster_Mar_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[3]], Raster_Mar_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[5]], Raster_Mar_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[7]], Raster_Mar_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[9]], Raster_Mar_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[11]], Raster_Mar_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[13]], Raster_Mar_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[15]], Raster_Mar_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[17]], Raster_Mar_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[19]], Raster_Mar_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[21]], Raster_Mar_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Mar_files_GIMMS[[23]], Raster_Mar_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Mar_files_VIP <- foreach(Mar=1:nlayers(Raster_Mar_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Mar_files_VIP[[Mar]], fun=fun.flag)
Raster_Mar_files_GIMMS<-foreach(Mar=1:nlayers(Raster_Mar_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Mar_files_GIMMS[[Mar]], fun=fun.flag)

for (i in 1:length(Raster_Mar_files_VIP)) {
  writeRaster(Raster_Mar_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 03,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Mar_files_GIMMS)) {
  writeRaster(Raster_Mar_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 03,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# April
##############################################################################
VIP.Apr <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Apr/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Apr <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Apr/GIMMS"),
                        pattern="*.tif", full.names=TRUE)
Raster_Apr_files_VIP<-foreach(Apr=1:length(VIP.Apr), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Apr[[Apr]])
Raster_Apr_files_GIMMS<-foreach(Apr=1:length(GIMMS.Apr), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Apr[[Apr]])


Raster_Apr_files_VIP <- stack(calc(stack(Raster_Apr_files_VIP[[1]], Raster_Apr_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Apr_files_VIP[[3]], Raster_Apr_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[5]], Raster_Apr_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[7]], Raster_Apr_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[9]], Raster_Apr_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[11]], Raster_Apr_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[13]], Raster_Apr_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[15]], Raster_Apr_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[17]], Raster_Apr_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[19]], Raster_Apr_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[21]], Raster_Apr_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[23]], Raster_Apr_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[25]], Raster_Apr_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[27]], Raster_Apr_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[29]], Raster_Apr_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[31]], Raster_Apr_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[33]], Raster_Apr_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[35]], Raster_Apr_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[37]], Raster_Apr_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[39]], Raster_Apr_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[41]], Raster_Apr_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[43]], Raster_Apr_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[45]], Raster_Apr_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[47]], Raster_Apr_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[49]], Raster_Apr_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[51]], Raster_Apr_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[53]], Raster_Apr_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[55]], Raster_Apr_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[57]], Raster_Apr_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Apr_files_VIP[[59]], Raster_Apr_files_VIP[[60]]), fun=max))

Raster_Apr_files_GIMMS <- stack(calc(stack(Raster_Apr_files_GIMMS[[25]], Raster_Apr_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[27]], Raster_Apr_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[29]], Raster_Apr_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[31]], Raster_Apr_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[33]], Raster_Apr_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[35]], Raster_Apr_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[37]], Raster_Apr_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[39]], Raster_Apr_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[41]], Raster_Apr_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[43]], Raster_Apr_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[45]], Raster_Apr_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[47]], Raster_Apr_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[49]], Raster_Apr_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[51]], Raster_Apr_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[53]], Raster_Apr_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[55]], Raster_Apr_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[57]], Raster_Apr_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[59]], Raster_Apr_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[1]], Raster_Apr_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[3]], Raster_Apr_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[5]], Raster_Apr_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[7]], Raster_Apr_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[9]], Raster_Apr_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[11]], Raster_Apr_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[13]], Raster_Apr_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[15]], Raster_Apr_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[17]], Raster_Apr_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[19]], Raster_Apr_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[21]], Raster_Apr_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Apr_files_GIMMS[[23]], Raster_Apr_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Apr_files_VIP <- foreach(Apr=1:nlayers(Raster_Apr_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Apr_files_VIP[[Apr]], fun=fun.flag)
Raster_Apr_files_GIMMS<-foreach(Apr=1:nlayers(Raster_Apr_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Apr_files_GIMMS[[Apr]], fun=fun.flag)

for (i in 1:length(Raster_Apr_files_VIP)) {
  writeRaster(Raster_Apr_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 04,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Apr_files_GIMMS)) {
  writeRaster(Raster_Apr_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 04,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# May
##############################################################################
VIP.May <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/May/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.May <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/May/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_May_files_VIP<-foreach(May=1:length(VIP.May), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.May[[May]])
Raster_May_files_GIMMS<-foreach(May=1:length(GIMMS.May), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.May[[May]])

Raster_May_files_VIP <- stack(calc(stack(Raster_May_files_VIP[[1]], Raster_May_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_May_files_VIP[[3]], Raster_May_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[5]], Raster_May_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[7]], Raster_May_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[9]], Raster_May_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[11]], Raster_May_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[13]], Raster_May_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[15]], Raster_May_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[17]], Raster_May_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[19]], Raster_May_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[21]], Raster_May_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[23]], Raster_May_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[25]], Raster_May_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[27]], Raster_May_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[29]], Raster_May_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[31]], Raster_May_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[33]], Raster_May_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[35]], Raster_May_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[37]], Raster_May_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[39]], Raster_May_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[41]], Raster_May_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[43]], Raster_May_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[45]], Raster_May_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[47]], Raster_May_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[49]], Raster_May_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[51]], Raster_May_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[53]], Raster_May_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[55]], Raster_May_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[57]], Raster_May_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_May_files_VIP[[59]], Raster_May_files_VIP[[60]]), fun=max))

Raster_May_files_GIMMS <- stack(calc(stack(Raster_May_files_GIMMS[[25]], Raster_May_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[27]], Raster_May_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[29]], Raster_May_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[31]], Raster_May_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[33]], Raster_May_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[35]], Raster_May_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[37]], Raster_May_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[39]], Raster_May_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[41]], Raster_May_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[43]], Raster_May_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[45]], Raster_May_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[47]], Raster_May_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[49]], Raster_May_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[51]], Raster_May_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[53]], Raster_May_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[55]], Raster_May_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[57]], Raster_May_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[59]], Raster_May_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[1]], Raster_May_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[3]], Raster_May_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[5]], Raster_May_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[7]], Raster_May_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[9]], Raster_May_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[11]], Raster_May_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[13]], Raster_May_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[15]], Raster_May_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[17]], Raster_May_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[19]], Raster_May_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[21]], Raster_May_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_May_files_GIMMS[[23]], Raster_May_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_May_files_VIP <- foreach(May=1:nlayers(Raster_May_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_May_files_VIP[[May]], fun=fun.flag)
Raster_May_files_GIMMS<-foreach(May=1:nlayers(Raster_May_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_May_files_GIMMS[[May]], fun=fun.flag)

for (i in 1:length(Raster_May_files_VIP)) {
  writeRaster(Raster_May_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 05,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_May_files_GIMMS)) {
  writeRaster(Raster_May_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 05,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# June
##############################################################################
VIP.Jun <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Jun/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Jun <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Jun/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Jun_files_VIP<-foreach(Jun=1:length(VIP.Jun), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Jun[[Jun]])
Raster_Jun_files_GIMMS<-foreach(Jun=1:length(GIMMS.Jun), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Jun[[Jun]])

Raster_Jun_files_VIP <- stack(calc(stack(Raster_Jun_files_VIP[[1]], Raster_Jun_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Jun_files_VIP[[3]], Raster_Jun_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[5]], Raster_Jun_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[7]], Raster_Jun_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[9]], Raster_Jun_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[11]], Raster_Jun_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[13]], Raster_Jun_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[15]], Raster_Jun_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[17]], Raster_Jun_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[19]], Raster_Jun_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[21]], Raster_Jun_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[23]], Raster_Jun_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[25]], Raster_Jun_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[27]], Raster_Jun_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[29]], Raster_Jun_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[31]], Raster_Jun_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[33]], Raster_Jun_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[35]], Raster_Jun_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[37]], Raster_Jun_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[39]], Raster_Jun_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[41]], Raster_Jun_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[43]], Raster_Jun_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[45]], Raster_Jun_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[47]], Raster_Jun_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[49]], Raster_Jun_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[51]], Raster_Jun_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[53]], Raster_Jun_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[55]], Raster_Jun_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[57]], Raster_Jun_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Jun_files_VIP[[59]], Raster_Jun_files_VIP[[60]]), fun=max))

Raster_Jun_files_GIMMS <- stack(calc(stack(Raster_Jun_files_GIMMS[[25]], Raster_Jun_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[27]], Raster_Jun_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[29]], Raster_Jun_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[31]], Raster_Jun_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[33]], Raster_Jun_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[35]], Raster_Jun_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[37]], Raster_Jun_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[39]], Raster_Jun_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[41]], Raster_Jun_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[43]], Raster_Jun_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[45]], Raster_Jun_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[47]], Raster_Jun_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[49]], Raster_Jun_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[51]], Raster_Jun_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[53]], Raster_Jun_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[55]], Raster_Jun_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[57]], Raster_Jun_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[59]], Raster_Jun_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[1]], Raster_Jun_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[3]], Raster_Jun_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[5]], Raster_Jun_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[7]], Raster_Jun_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[9]], Raster_Jun_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[11]], Raster_Jun_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[13]], Raster_Jun_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[15]], Raster_Jun_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[17]], Raster_Jun_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[19]], Raster_Jun_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[21]], Raster_Jun_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Jun_files_GIMMS[[23]], Raster_Jun_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Jun_files_VIP <- foreach(Jun=1:nlayers(Raster_Jun_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Jun_files_VIP[[Jun]], fun=fun.flag)
Raster_Jun_files_GIMMS<-foreach(Jun=1:nlayers(Raster_Jun_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Jun_files_GIMMS[[Jun]], fun=fun.flag)

for (i in 1:length(Raster_Jun_files_VIP)) {
  writeRaster(Raster_Jun_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 06,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Jun_files_GIMMS)) {
  writeRaster(Raster_Jun_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 06,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# July
##############################################################################
VIP.Jul <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Jul/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Jul <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Jul/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Jul_files_VIP<-foreach(Jul=1:length(VIP.Jul), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Jul[[Jul]])
Raster_Jul_files_GIMMS<-foreach(Jul=1:length(GIMMS.Jul), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Jul[[Jul]])

Raster_Jul_files_VIP <- stack(calc(stack(Raster_Jul_files_VIP[[1]], Raster_Jul_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Jul_files_VIP[[3]], Raster_Jul_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[5]], Raster_Jul_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[7]], Raster_Jul_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[9]], Raster_Jul_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[11]], Raster_Jul_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[13]], Raster_Jul_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[15]], Raster_Jul_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[17]], Raster_Jul_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[19]], Raster_Jul_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[21]], Raster_Jul_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[23]], Raster_Jul_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[25]], Raster_Jul_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[27]], Raster_Jul_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[29]], Raster_Jul_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[31]], Raster_Jul_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[33]], Raster_Jul_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[35]], Raster_Jul_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[37]], Raster_Jul_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[39]], Raster_Jul_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[41]], Raster_Jul_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[43]], Raster_Jul_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[45]], Raster_Jul_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[47]], Raster_Jul_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[49]], Raster_Jul_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[51]], Raster_Jul_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[53]], Raster_Jul_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[55]], Raster_Jul_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[57]], Raster_Jul_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Jul_files_VIP[[59]], Raster_Jul_files_VIP[[60]]), fun=max))

Raster_Jul_files_GIMMS <- stack(calc(stack(Raster_Jul_files_GIMMS[[25]], Raster_Jul_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[27]], Raster_Jul_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[29]], Raster_Jul_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[31]], Raster_Jul_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[33]], Raster_Jul_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[35]], Raster_Jul_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[37]], Raster_Jul_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[39]], Raster_Jul_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[41]], Raster_Jul_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[43]], Raster_Jul_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[45]], Raster_Jul_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[47]], Raster_Jul_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[49]], Raster_Jul_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[51]], Raster_Jul_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[53]], Raster_Jul_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[55]], Raster_Jul_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[57]], Raster_Jul_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[59]], Raster_Jul_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[1]], Raster_Jul_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[3]], Raster_Jul_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[5]], Raster_Jul_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[7]], Raster_Jul_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[9]], Raster_Jul_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[11]], Raster_Jul_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[13]], Raster_Jul_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[15]], Raster_Jul_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[17]], Raster_Jul_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[19]], Raster_Jul_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[21]], Raster_Jul_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Jul_files_GIMMS[[23]], Raster_Jul_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Jul_files_VIP <- foreach(Jul=1:nlayers(Raster_Jul_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Jul_files_VIP[[Jul]], fun=fun.flag)
Raster_Jul_files_GIMMS<-foreach(Jul=1:nlayers(Raster_Jul_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Jul_files_GIMMS[[Jul]], fun=fun.flag)

for (i in 1:length(Raster_Jul_files_VIP)) {
  writeRaster(Raster_Jul_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 07,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Jul_files_GIMMS)) {
  writeRaster(Raster_Jul_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 07,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# August
##############################################################################
VIP.Aug <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Aug/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Aug <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Aug/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Aug_files_VIP<-foreach(Aug=1:length(VIP.Aug), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Aug[[Aug]])
Raster_Aug_files_GIMMS<-foreach(Aug=1:length(GIMMS.Aug), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Aug[[Aug]])

Raster_Aug_files_VIP <- stack(calc(stack(Raster_Aug_files_VIP[[1]], Raster_Aug_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Aug_files_VIP[[3]], Raster_Aug_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[5]], Raster_Aug_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[7]], Raster_Aug_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[9]], Raster_Aug_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[11]], Raster_Aug_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[13]], Raster_Aug_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[15]], Raster_Aug_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[17]], Raster_Aug_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[19]], Raster_Aug_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[21]], Raster_Aug_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[23]], Raster_Aug_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[25]], Raster_Aug_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[27]], Raster_Aug_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[29]], Raster_Aug_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[31]], Raster_Aug_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[33]], Raster_Aug_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[35]], Raster_Aug_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[37]], Raster_Aug_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[39]], Raster_Aug_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[41]], Raster_Aug_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[43]], Raster_Aug_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[45]], Raster_Aug_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[47]], Raster_Aug_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[49]], Raster_Aug_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[51]], Raster_Aug_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[53]], Raster_Aug_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[55]], Raster_Aug_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[57]], Raster_Aug_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Aug_files_VIP[[59]], Raster_Aug_files_VIP[[60]]), fun=max))

Raster_Aug_files_GIMMS <- stack(calc(stack(Raster_Aug_files_GIMMS[[25]], Raster_Aug_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[27]], Raster_Aug_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[29]], Raster_Aug_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[31]], Raster_Aug_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[33]], Raster_Aug_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[35]], Raster_Aug_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[37]], Raster_Aug_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[39]], Raster_Aug_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[41]], Raster_Aug_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[43]], Raster_Aug_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[45]], Raster_Aug_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[47]], Raster_Aug_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[49]], Raster_Aug_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[51]], Raster_Aug_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[53]], Raster_Aug_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[55]], Raster_Aug_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[57]], Raster_Aug_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[59]], Raster_Aug_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[1]], Raster_Aug_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[3]], Raster_Aug_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[5]], Raster_Aug_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[7]], Raster_Aug_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[9]], Raster_Aug_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[11]], Raster_Aug_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[13]], Raster_Aug_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[15]], Raster_Aug_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[17]], Raster_Aug_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[19]], Raster_Aug_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[21]], Raster_Aug_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Aug_files_GIMMS[[23]], Raster_Aug_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Aug_files_VIP <- foreach(Aug=1:nlayers(Raster_Aug_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Aug_files_VIP[[Aug]], fun=fun.flag)
Raster_Aug_files_GIMMS<-foreach(Aug=1:nlayers(Raster_Aug_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Aug_files_GIMMS[[Aug]], fun=fun.flag)

for (i in 1:length(Raster_Aug_files_VIP)) {
  writeRaster(Raster_Aug_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 08,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Aug_files_GIMMS)) {
  writeRaster(Raster_Aug_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 08,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# September
##############################################################################
VIP.Sep <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Sep/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Sep <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Sep/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Sep_files_VIP<-foreach(Sep=1:length(VIP.Sep), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Sep[[Sep]])
Raster_Sep_files_GIMMS<-foreach(Sep=1:length(GIMMS.Sep), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Sep[[Sep]])


Raster_Sep_files_VIP <- stack(calc(stack(Raster_Sep_files_VIP[[1]], Raster_Sep_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Sep_files_VIP[[3]], Raster_Sep_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[5]], Raster_Sep_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[7]], Raster_Sep_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[9]], Raster_Sep_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[11]], Raster_Sep_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[13]], Raster_Sep_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[15]], Raster_Sep_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[17]], Raster_Sep_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[19]], Raster_Sep_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[21]], Raster_Sep_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[23]], Raster_Sep_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[25]], Raster_Sep_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[27]], Raster_Sep_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[29]], Raster_Sep_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[31]], Raster_Sep_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[33]], Raster_Sep_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[35]], Raster_Sep_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[37]], Raster_Sep_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[39]], Raster_Sep_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[41]], Raster_Sep_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[43]], Raster_Sep_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[45]], Raster_Sep_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[47]], Raster_Sep_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[49]], Raster_Sep_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[51]], Raster_Sep_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[53]], Raster_Sep_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[55]], Raster_Sep_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[57]], Raster_Sep_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Sep_files_VIP[[59]], Raster_Sep_files_VIP[[60]]), fun=max))

Raster_Sep_files_GIMMS <- stack(calc(stack(Raster_Sep_files_GIMMS[[25]], Raster_Sep_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[27]], Raster_Sep_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[29]], Raster_Sep_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[31]], Raster_Sep_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[33]], Raster_Sep_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[35]], Raster_Sep_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[37]], Raster_Sep_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[39]], Raster_Sep_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[41]], Raster_Sep_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[43]], Raster_Sep_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[45]], Raster_Sep_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[47]], Raster_Sep_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[49]], Raster_Sep_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[51]], Raster_Sep_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[53]], Raster_Sep_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[55]], Raster_Sep_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[57]], Raster_Sep_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[59]], Raster_Sep_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[1]], Raster_Sep_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[3]], Raster_Sep_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[5]], Raster_Sep_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[7]], Raster_Sep_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[9]], Raster_Sep_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[11]], Raster_Sep_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[13]], Raster_Sep_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[15]], Raster_Sep_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[17]], Raster_Sep_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[19]], Raster_Sep_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[21]], Raster_Sep_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Sep_files_GIMMS[[23]], Raster_Sep_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Sep_files_VIP <- foreach(Sep=1:nlayers(Raster_Sep_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Sep_files_VIP[[Sep]], fun=fun.flag)
Raster_Sep_files_GIMMS<-foreach(Sep=1:nlayers(Raster_Sep_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Sep_files_GIMMS[[Sep]], fun=fun.flag)

for (i in 1:length(Raster_Sep_files_VIP)) {
  writeRaster(Raster_Sep_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 09,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Sep_files_GIMMS)) {
  writeRaster(Raster_Sep_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 09,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# October
##############################################################################
VIP.Oct <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Oct/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Oct <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Oct/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Oct_files_VIP<-foreach(Oct=1:length(VIP.Oct), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Oct[[Oct]])
Raster_Oct_files_GIMMS<-foreach(Oct=1:length(GIMMS.Oct), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Oct[[Oct]])

Raster_Oct_files_VIP <- stack(calc(stack(Raster_Oct_files_VIP[[1]], Raster_Oct_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Oct_files_VIP[[3]], Raster_Oct_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[5]], Raster_Oct_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[7]], Raster_Oct_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[9]], Raster_Oct_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[11]], Raster_Oct_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[13]], Raster_Oct_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[15]], Raster_Oct_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[17]], Raster_Oct_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[19]], Raster_Oct_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[21]], Raster_Oct_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[23]], Raster_Oct_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[25]], Raster_Oct_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[27]], Raster_Oct_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[29]], Raster_Oct_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[31]], Raster_Oct_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[33]], Raster_Oct_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[35]], Raster_Oct_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[37]], Raster_Oct_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[39]], Raster_Oct_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[41]], Raster_Oct_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[43]], Raster_Oct_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[45]], Raster_Oct_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[47]], Raster_Oct_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[49]], Raster_Oct_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[51]], Raster_Oct_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[53]], Raster_Oct_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[55]], Raster_Oct_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[57]], Raster_Oct_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Oct_files_VIP[[59]], Raster_Oct_files_VIP[[60]]), fun=max))

Raster_Oct_files_GIMMS <- stack(calc(stack(Raster_Oct_files_GIMMS[[25]], Raster_Oct_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[27]], Raster_Oct_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[29]], Raster_Oct_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[31]], Raster_Oct_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[33]], Raster_Oct_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[35]], Raster_Oct_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[37]], Raster_Oct_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[39]], Raster_Oct_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[41]], Raster_Oct_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[43]], Raster_Oct_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[45]], Raster_Oct_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[47]], Raster_Oct_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[49]], Raster_Oct_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[51]], Raster_Oct_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[53]], Raster_Oct_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[55]], Raster_Oct_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[57]], Raster_Oct_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[59]], Raster_Oct_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[1]], Raster_Oct_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[3]], Raster_Oct_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[5]], Raster_Oct_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[7]], Raster_Oct_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[9]], Raster_Oct_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[11]], Raster_Oct_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[13]], Raster_Oct_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[15]], Raster_Oct_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[17]], Raster_Oct_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[19]], Raster_Oct_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[21]], Raster_Oct_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Oct_files_GIMMS[[23]], Raster_Oct_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Oct_files_VIP <- foreach(Oct=1:nlayers(Raster_Oct_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Oct_files_VIP[[Oct]], fun=fun.flag)
Raster_Oct_files_GIMMS<-foreach(Oct=1:nlayers(Raster_Oct_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Oct_files_GIMMS[[Oct]], fun=fun.flag)

for (i in 1:length(Raster_Oct_files_VIP)) {
  writeRaster(Raster_Oct_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 10,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Oct_files_GIMMS)) {
  writeRaster(Raster_Oct_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 10,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}

##############################################################################
# November
##############################################################################
VIP.Nov <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Nov/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Nov <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Nov/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Nov_files_VIP<-foreach(Nov=1:length(VIP.Nov), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Nov[[Nov]])
Raster_Nov_files_GIMMS<-foreach(Nov=1:length(GIMMS.Nov), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Nov[[Nov]])

Raster_Nov_files_VIP <- stack(calc(stack(Raster_Nov_files_VIP[[1]], Raster_Nov_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Nov_files_VIP[[3]], Raster_Nov_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[5]], Raster_Nov_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[7]], Raster_Nov_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[9]], Raster_Nov_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[11]], Raster_Nov_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[13]], Raster_Nov_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[15]], Raster_Nov_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[17]], Raster_Nov_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[19]], Raster_Nov_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[21]], Raster_Nov_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[23]], Raster_Nov_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[25]], Raster_Nov_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[27]], Raster_Nov_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[29]], Raster_Nov_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[31]], Raster_Nov_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[33]], Raster_Nov_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[35]], Raster_Nov_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[37]], Raster_Nov_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[39]], Raster_Nov_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[41]], Raster_Nov_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[43]], Raster_Nov_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[45]], Raster_Nov_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[47]], Raster_Nov_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[49]], Raster_Nov_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[51]], Raster_Nov_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[53]], Raster_Nov_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[55]], Raster_Nov_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[57]], Raster_Nov_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Nov_files_VIP[[59]], Raster_Nov_files_VIP[[60]]), fun=max))

Raster_Nov_files_GIMMS <- stack(calc(stack(Raster_Nov_files_GIMMS[[25]], Raster_Nov_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[27]], Raster_Nov_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[29]], Raster_Nov_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[31]], Raster_Nov_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[33]], Raster_Nov_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[35]], Raster_Nov_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[37]], Raster_Nov_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[39]], Raster_Nov_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[41]], Raster_Nov_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[43]], Raster_Nov_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[45]], Raster_Nov_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[47]], Raster_Nov_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[49]], Raster_Nov_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[51]], Raster_Nov_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[53]], Raster_Nov_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[55]], Raster_Nov_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[57]], Raster_Nov_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[59]], Raster_Nov_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[1]], Raster_Nov_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[3]], Raster_Nov_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[5]], Raster_Nov_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[7]], Raster_Nov_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[9]], Raster_Nov_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[11]], Raster_Nov_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[13]], Raster_Nov_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[15]], Raster_Nov_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[17]], Raster_Nov_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[19]], Raster_Nov_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[21]], Raster_Nov_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Nov_files_GIMMS[[23]], Raster_Nov_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Nov_files_VIP <- foreach(Nov=1:nlayers(Raster_Nov_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Nov_files_VIP[[Nov]], fun=fun.flag)
Raster_Nov_files_GIMMS<-foreach(Nov=1:nlayers(Raster_Nov_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Nov_files_GIMMS[[Nov]], fun=fun.flag)

for (i in 1:length(Raster_Nov_files_VIP)) {
  writeRaster(Raster_Nov_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 11,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Nov_files_GIMMS)) {
  writeRaster(Raster_Nov_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 11,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}
##############################################################################
# DECEMBER
##############################################################################
VIP.Dec <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Dec/VIP"),
                      pattern="*.tif", full.names=TRUE)
GIMMS.Dec <- list.files(file.path("/vm/eokuto/final_VIP_GIMMS_8km_15days/Dec/GIMMS"),
                        pattern="*.tif", full.names=TRUE)

Raster_Dec_files_VIP<-foreach(Dec=1:length(VIP.Dec), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(VIP.Dec[[Dec]])
Raster_Dec_files_GIMMS<-foreach(Dec=1:length(GIMMS.Dec), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(GIMMS.Dec[[Dec]])

Raster_Dec_files_VIP <- stack(calc(stack(Raster_Dec_files_VIP[[1]], Raster_Dec_files_VIP[[2]]),    fun=max),
                              calc(stack(Raster_Dec_files_VIP[[3]], Raster_Dec_files_VIP[[4]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[5]], Raster_Dec_files_VIP[[6]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[7]], Raster_Dec_files_VIP[[8]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[9]], Raster_Dec_files_VIP[[10]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[11]], Raster_Dec_files_VIP[[12]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[13]], Raster_Dec_files_VIP[[14]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[15]], Raster_Dec_files_VIP[[16]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[17]], Raster_Dec_files_VIP[[18]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[19]], Raster_Dec_files_VIP[[20]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[21]], Raster_Dec_files_VIP[[22]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[23]], Raster_Dec_files_VIP[[24]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[25]], Raster_Dec_files_VIP[[26]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[27]], Raster_Dec_files_VIP[[28]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[29]], Raster_Dec_files_VIP[[30]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[31]], Raster_Dec_files_VIP[[32]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[33]], Raster_Dec_files_VIP[[34]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[35]], Raster_Dec_files_VIP[[36]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[37]], Raster_Dec_files_VIP[[38]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[39]], Raster_Dec_files_VIP[[40]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[41]], Raster_Dec_files_VIP[[42]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[43]], Raster_Dec_files_VIP[[44]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[45]], Raster_Dec_files_VIP[[46]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[47]], Raster_Dec_files_VIP[[48]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[49]], Raster_Dec_files_VIP[[50]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[51]], Raster_Dec_files_VIP[[52]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[53]], Raster_Dec_files_VIP[[54]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[55]], Raster_Dec_files_VIP[[56]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[57]], Raster_Dec_files_VIP[[58]]), fun=max),
                              calc(stack(Raster_Dec_files_VIP[[59]], Raster_Dec_files_VIP[[60]]), fun=max))

Raster_Dec_files_GIMMS <- stack(calc(stack(Raster_Dec_files_GIMMS[[25]], Raster_Dec_files_GIMMS[[26]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[27]], Raster_Dec_files_GIMMS[[28]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[29]], Raster_Dec_files_GIMMS[[30]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[31]], Raster_Dec_files_GIMMS[[32]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[33]], Raster_Dec_files_GIMMS[[34]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[35]], Raster_Dec_files_GIMMS[[36]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[37]], Raster_Dec_files_GIMMS[[38]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[39]], Raster_Dec_files_GIMMS[[40]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[41]], Raster_Dec_files_GIMMS[[42]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[43]], Raster_Dec_files_GIMMS[[44]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[45]], Raster_Dec_files_GIMMS[[46]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[47]], Raster_Dec_files_GIMMS[[48]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[49]], Raster_Dec_files_GIMMS[[50]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[51]], Raster_Dec_files_GIMMS[[52]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[53]], Raster_Dec_files_GIMMS[[54]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[55]], Raster_Dec_files_GIMMS[[56]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[57]], Raster_Dec_files_GIMMS[[58]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[59]], Raster_Dec_files_GIMMS[[60]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[1]], Raster_Dec_files_GIMMS[[2]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[3]], Raster_Dec_files_GIMMS[[4]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[5]], Raster_Dec_files_GIMMS[[6]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[7]], Raster_Dec_files_GIMMS[[8]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[9]], Raster_Dec_files_GIMMS[[10]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[11]], Raster_Dec_files_GIMMS[[12]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[13]], Raster_Dec_files_GIMMS[[14]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[15]], Raster_Dec_files_GIMMS[[16]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[17]], Raster_Dec_files_GIMMS[[18]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[19]], Raster_Dec_files_GIMMS[[20]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[21]], Raster_Dec_files_GIMMS[[22]]), fun=max),
                                calc(stack(Raster_Dec_files_GIMMS[[23]], Raster_Dec_files_GIMMS[[24]]), fun=max))

years<-1982:2011

Raster_Dec_files_VIP <- foreach(Dec=1:nlayers(Raster_Dec_files_VIP), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Dec_files_VIP[[Dec]], fun=fun.flag)
Raster_Dec_files_GIMMS<-foreach(Dec=1:nlayers(Raster_Dec_files_GIMMS), .packages="raster", .combine=c, .verbose=FALSE) %do%
  calc(Raster_Dec_files_GIMMS[[Dec]], fun=fun.flag)

for (i in 1:length(Raster_Dec_files_VIP)) {
  writeRaster(Raster_Dec_files_VIP[[i]],filename=paste('/vm/eokuto/MonthyVegData/VIP/', 
                                                       'vip', years[i], 12,'ndvi_v3', pattern=".tif",
                                                       sep=''), overwrite=TRUE)
}

for (i in 1:length(Raster_Dec_files_GIMMS)) {
  writeRaster(Raster_Dec_files_GIMMS[[i]],filename=paste('/vm/eokuto/MonthyVegData/GIMMS/', 
                                                         'gimms', years[i], 12,'ndvi_v3', pattern=".tif",
                                                         sep=''), overwrite=TRUE)
}
