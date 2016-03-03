

require(foreach); require(raster)

fun.flag<-function(x){
  y=ifelse(x > 1, 1,
           ifelse(x < -1, -1, x))
  y[is.na(y)] <- NA
  y[is.nan(y) ] <- NA
  return(y)
}

# based on variable values
newdata <- mydata[ which(mydata$gender=='F'
                         & mydata$age > 65), ]


##############################################################################
# January
##############################################################################
sgolayData4 <- list.files(file.path("J:/MonthlyEvi2Data/sgolay"),
                      pattern="*.tif", full.names=TRUE)
Raster_sgolayData4<-foreach(i=1:length(sgolayData4), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(sgolayData4[[i]])

years4<-rep(1982:2011,each=12)
size4<-length(Raster_sgolayData4)
months4<-rep(c(016,289,320,350,047,075,106,136,167,197,228,259), times=size4/12)

# for (i in 1:length(Raster_sgolayData4)) {
#   writeRaster(Raster_sgolayData4[[i]],filename=paste('J:/MonthlyEvi2Data/CorrectedMonthlySGolay/', 
#                                                      'VIP15P4','-','A',years4[i],months4[i], '-',003, '-', 'evi2-sgolay', pattern=".tif",
#                                                      sep=''Raster_spdeData))
# }

foreach(i=1:length(Raster_sgolayData4), .combine=c, .verbose=FALSE) %do%
  writeRaster(Raster_sgolayData4[[i]],filename=paste('J:/MonthlyEvi2Data/CorrectedMonthlySGolay/', 
                                                     'VIP15P4','-','A',years4[i],months4[i], '-',003, '-', 'evi2-sgolay', pattern=".tif",
                                                     sep=''))

###############################################################################################################
whitData4 <- list.files(file.path("J:/MonthlyEvi2Data/whittaker"),
                          pattern="*.tif", full.names=TRUE)
Raster_whitData4<-foreach(i=1:length(whitData4), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(whitData4[[i]])

years4<-rep(1982:2011,each=12)
size4<-length(Raster_whitData4)
months4<-rep(c(016,289,320,350,047,075,106,136,167,197,228,259), times=size4/12)

# for (i in 1:length(Raster_whitData4)) {
#   writeRaster(Raster_whitData4[[i]],filename=paste('J:/MonthlyEvi2Data/CorrectedMonthlyWhit/', 
#                                                      'VIP15P4','-','A',years4[i],months4[i], '-',003, '-', 'evi2-whit', pattern=".tif",
#                                                      sep=''))
# }

foreach(i=1:length(Raster_whitData4), .combine=c, .verbose=FALSE) %do%
  writeRaster(Raster_whitData4[[i]],filename=paste('J:/MonthlyEvi2Data/CorrectedMonthlyWhit/', 
                                                   'VIP15P4','-','A',years4[i],months4[i], '-',003, '-', 'evi2-whit', pattern=".tif",
                                                   sep=''))

# rasterTmpFile(prefix='raster_tmp_')
# showTmpFiles()
# removeTmpFiles(h=24)
#################################################################################################
#######################################################################################
#SPDE
#####################################################################################
require(raster);require(foreach)
sgolayData <- list.files(file.path("J:/Smoothed_using_sgolay"),
                         pattern="*.tif", full.names=TRUE)
Raster_sgolayData<-foreach(i=1:length(sgolayData), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(sgolayData[[i]])

years2<-rep(1982:2011,each=24)
size2<-length(Raster_sgolayData)
months2<-rep(c(001,016,274,289,305,320,335,350,032,047,060,075,091,106,121,136,152,167,182,197,213,228,244,259), times=size2/24)

# for (i in 1:length(Raster_sgolayData)) {
#   writeRaster(Raster_sgolayData[[i]],filename=paste('J:/SGOLAY_CorrectedData/', 
#                                                   'VIP15P4','-','A',years2[i],months2[i], '-',003, '-', 'evi2-sgolay', pattern=".tif",
#                                                   sep=''))
# }

foreach(i=1:length(Raster_sgolayData), .combine=c, .verbose=FALSE) %do%
  writeRaster(Raster_sgolayData[[i]],filename=paste('J:/SGOLAY_CorrectedData/', 
                                                    'VIP15P4','-','A',years2[i],months2[i], '-',003, '-', 'evi2-sgolay', pattern=".tif",
                                                    sep=''))

#####################################################################################################################
whitData <- list.files(file.path("J:/smoothed_using_whittaker"),
                       pattern="*.tif", full.names=TRUE)
Raster_whitData<-foreach(i=1:length(whitData), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(whitData[[i]])

years1<-rep(1982:2011,each=24)
size1<-length(Raster_whitData)
months1<-rep(c(001,016,274,289,305,320,335,350,032,047,060,075,091,106,121,136,152,167,182,197,213,228,244,259), times=size1/24)

# for (i in 1:length(Raster_whitData)) {
#   writeRaster(Raster_whitData[[i]],filename=paste('J:/WHITTAKER_CorrectedData/', 
#                                                     'VIP15P4','-','A',years1[i],months1[i], '-',003, '-', 'evi2-whit', pattern=".tif",
#                                                     sep=''))
# }

foreach(i=1:length(Raster_whitData), .combine=c, .verbose=FALSE) %do%
  writeRaster(Raster_whitData[[i]],filename=paste('J:/WHITTAKER_CorrectedData/', 
                                                  'VIP15P4','-','A',years1[i],months1[i], '-',003, '-', 'evi2-whit', pattern=".tif",
                                                  sep=''))

#####################################################################################################################
rasterOptions()$tmpdir
spdeData <- list.files(file.path("J:/Smoothed_using_spde"),
                         pattern="*.tif", full.names=TRUE)
Raster_spdeData<-foreach(i=1:length(spdeData), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(spdeData[[i]])

years<-rep(1982:2011,each=24)
size<-length(Raster_spdeData)
months<-rep(c(001,016,274,289,305,320,335,350,032,047,060,075,091,106,121,136,152,167,182,197,213,228,244,259), times=size/24)

for (i in 1:length(Raster_spdeData)) {
  writeRaster(Raster_spdeData[[i]],filename=paste('J:/SPDE_CorrectedData/', 
                                                  'VIP15P4','-','A',years[i],months[i], '-',003, '-', 'evi2-spde', pattern=".tif",
                                                  sep=''))
}

#####################################################################################################
spdeData <- list.files(file.path("J:/SPDE_CorrectedData"),
                       pattern="*.tif", full.names=TRUE)
Raster_spdeData<-foreach(i=1:length(spdeData), .packages="raster", .combine=c, .verbose=FALSE) %do%
  raster(spdeData[[i]])

stkRaster_spde2 <- stack(max(Raster_spdeData[[1]], Raster_spdeData[[2]]),
                         max(Raster_spdeData[[3]], Raster_spdeData[[4]]),
                         max(Raster_spdeData[[5]], Raster_spdeData[[6]]),
                         max(Raster_spdeData[[7]], Raster_spdeData[[8]]),
                         max(Raster_spdeData[[9]], Raster_spdeData[[10]]),
                         max(Raster_spdeData[[11]], Raster_spdeData[[12]]),
                         max(Raster_spdeData[[13]], Raster_spdeData[[14]]),
                         max(Raster_spdeData[[15]], Raster_spdeData[[16]]),
                         max(Raster_spdeData[[17]], Raster_spdeData[[18]]),
                         max(Raster_spdeData[[19]], Raster_spdeData[[20]]),
                         max(Raster_spdeData[[21]], Raster_spdeData[[22]]),
                         max(Raster_spdeData[[23]], Raster_spdeData[[24]]),
                         max(Raster_spdeData[[25]], Raster_spdeData[[26]]),
                         max(Raster_spdeData[[27]], Raster_spdeData[[28]]),
                         max(Raster_spdeData[[29]], Raster_spdeData[[30]]),
                         max(Raster_spdeData[[31]], Raster_spdeData[[32]]),
                         max(Raster_spdeData[[33]], Raster_spdeData[[34]]),
                         max(Raster_spdeData[[35]], Raster_spdeData[[36]]),
                         max(Raster_spdeData[[37]], Raster_spdeData[[38]]),
                         max(Raster_spdeData[[39]], Raster_spdeData[[40]]),
                         max(Raster_spdeData[[41]], Raster_spdeData[[42]]),
                         max(Raster_spdeData[[43]], Raster_spdeData[[44]]),
                         max(Raster_spdeData[[45]], Raster_spdeData[[46]]),
                         max(Raster_spdeData[[47]], Raster_spdeData[[48]]),
                         max(Raster_spdeData[[49]], Raster_spdeData[[50]]),
                         max(Raster_spdeData[[51]], Raster_spdeData[[52]]),
                         max(Raster_spdeData[[53]], Raster_spdeData[[54]]),
                         max(Raster_spdeData[[55]], Raster_spdeData[[56]]),
                         max(Raster_spdeData[[57]], Raster_spdeData[[58]]),
                         max(Raster_spdeData[[59]], Raster_spdeData[[60]]),
                         max(Raster_spdeData[[61]], Raster_spdeData[[62]]),
                         max(Raster_spdeData[[63]], Raster_spdeData[[64]]),
                         max(Raster_spdeData[[65]], Raster_spdeData[[66]]),
                         max(Raster_spdeData[[67]], Raster_spdeData[[68]]),
                         max(Raster_spdeData[[69]], Raster_spdeData[[70]]),
                         max(Raster_spdeData[[71]], Raster_spdeData[[72]]),
                         max(Raster_spdeData[[73]], Raster_spdeData[[74]]),
                         max(Raster_spdeData[[75]], Raster_spdeData[[76]]),
                         max(Raster_spdeData[[77]], Raster_spdeData[[78]]),
                         max(Raster_spdeData[[79]], Raster_spdeData[[80]]),
                         max(Raster_spdeData[[81]], Raster_spdeData[[82]]),
                         max(Raster_spdeData[[83]], Raster_spdeData[[84]]),
                         max(Raster_spdeData[[85]], Raster_spdeData[[86]]),
                         max(Raster_spdeData[[87]], Raster_spdeData[[88]]),
                         max(Raster_spdeData[[89]], Raster_spdeData[[90]]),
                         max(Raster_spdeData[[91]], Raster_spdeData[[92]]),
                         max(Raster_spdeData[[93]], Raster_spdeData[[94]]),
                         max(Raster_spdeData[[95]], Raster_spdeData[[96]]),
                         max(Raster_spdeData[[97]], Raster_spdeData[[98]]),
                         max(Raster_spdeData[[99]], Raster_spdeData[[100]]),
                         max(Raster_spdeData[[101]], Raster_spdeData[[102]]),
                         max(Raster_spdeData[[103]], Raster_spdeData[[104]]),
                         max(Raster_spdeData[[105]], Raster_spdeData[[106]]),
                         max(Raster_spdeData[[107]], Raster_spdeData[[108]]),
                         max(Raster_spdeData[[109]], Raster_spdeData[[110]]),
                         max(Raster_spdeData[[111]], Raster_spdeData[[112]]),
                         max(Raster_spdeData[[113]], Raster_spdeData[[114]]),
                         max(Raster_spdeData[[115]], Raster_spdeData[[116]]),
                         max(Raster_spdeData[[117]], Raster_spdeData[[118]]),
                         max(Raster_spdeData[[119]], Raster_spdeData[[120]]),
                         max(Raster_spdeData[[121]], Raster_spdeData[[122]]),
                         max(Raster_spdeData[[123]], Raster_spdeData[[124]]),
                         max(Raster_spdeData[[125]], Raster_spdeData[[126]]),
                         max(Raster_spdeData[[127]], Raster_spdeData[[128]]),
                         max(Raster_spdeData[[129]], Raster_spdeData[[130]]),
                         max(Raster_spdeData[[131]], Raster_spdeData[[132]]),
                         max(Raster_spdeData[[133]], Raster_spdeData[[134]]),
                         max(Raster_spdeData[[135]], Raster_spdeData[[136]]),
                         max(Raster_spdeData[[137]], Raster_spdeData[[138]]),
                         max(Raster_spdeData[[139]], Raster_spdeData[[140]]),
                         max(Raster_spdeData[[141]], Raster_spdeData[[142]]),
                         max(Raster_spdeData[[143]], Raster_spdeData[[144]]),
                         max(Raster_spdeData[[145]], Raster_spdeData[[146]]),
                         max(Raster_spdeData[[147]], Raster_spdeData[[148]]),
                         max(Raster_spdeData[[149]], Raster_spdeData[[150]]),
                         max(Raster_spdeData[[151]], Raster_spdeData[[152]]),
                         max(Raster_spdeData[[153]], Raster_spdeData[[154]]),
                         max(Raster_spdeData[[155]], Raster_spdeData[[156]]),
                         max(Raster_spdeData[[157]], Raster_spdeData[[158]]),
                         max(Raster_spdeData[[159]], Raster_spdeData[[160]]),
                         max(Raster_spdeData[[161]], Raster_spdeData[[162]]),
                         max(Raster_spdeData[[163]], Raster_spdeData[[164]]),
                         max(Raster_spdeData[[165]], Raster_spdeData[[166]]),
                         max(Raster_spdeData[[167]], Raster_spdeData[[168]]),
                         max(Raster_spdeData[[169]], Raster_spdeData[[170]]),
                         max(Raster_spdeData[[171]], Raster_spdeData[[172]]),
                         max(Raster_spdeData[[173]], Raster_spdeData[[174]]),
                         max(Raster_spdeData[[175]], Raster_spdeData[[176]]),
                         max(Raster_spdeData[[177]], Raster_spdeData[[178]]),
                         max(Raster_spdeData[[179]], Raster_spdeData[[180]]),
                         max(Raster_spdeData[[181]], Raster_spdeData[[182]]),
                         max(Raster_spdeData[[183]], Raster_spdeData[[184]]),
                         max(Raster_spdeData[[185]], Raster_spdeData[[186]]),
                         max(Raster_spdeData[[187]], Raster_spdeData[[188]]),
                         max(Raster_spdeData[[189]], Raster_spdeData[[190]]),
                         max(Raster_spdeData[[191]], Raster_spdeData[[192]]),
                         max(Raster_spdeData[[193]], Raster_spdeData[[194]]),
                         max(Raster_spdeData[[195]], Raster_spdeData[[196]]),
                         max(Raster_spdeData[[197]], Raster_spdeData[[198]]),
                         max(Raster_spdeData[[199]], Raster_spdeData[[200]]),
                         max(Raster_spdeData[[201]], Raster_spdeData[[202]]),
                         max(Raster_spdeData[[203]], Raster_spdeData[[204]]),
                         max(Raster_spdeData[[205]], Raster_spdeData[[206]]),
                         max(Raster_spdeData[[207]], Raster_spdeData[[208]]),
                         max(Raster_spdeData[[209]], Raster_spdeData[[210]]),
                         max(Raster_spdeData[[211]], Raster_spdeData[[212]]),
                         max(Raster_spdeData[[213]], Raster_spdeData[[214]]),
                         max(Raster_spdeData[[215]], Raster_spdeData[[216]]),
                         max(Raster_spdeData[[217]], Raster_spdeData[[218]]),
                         max(Raster_spdeData[[219]], Raster_spdeData[[220]]),
                         max(Raster_spdeData[[221]], Raster_spdeData[[222]]),
                         max(Raster_spdeData[[223]], Raster_spdeData[[224]]),
                         max(Raster_spdeData[[225]], Raster_spdeData[[226]]),
                         max(Raster_spdeData[[227]], Raster_spdeData[[228]]),
                         max(Raster_spdeData[[229]], Raster_spdeData[[230]]),
                         max(Raster_spdeData[[231]], Raster_spdeData[[232]]),
                         max(Raster_spdeData[[233]], Raster_spdeData[[234]]),
                         max(Raster_spdeData[[235]], Raster_spdeData[[236]]),
                         max(Raster_spdeData[[237]], Raster_spdeData[[238]]),
                         max(Raster_spdeData[[239]], Raster_spdeData[[240]]),
                         max(Raster_spdeData[[241]], Raster_spdeData[[242]]),
                         max(Raster_spdeData[[243]], Raster_spdeData[[244]]),
                         max(Raster_spdeData[[245]], Raster_spdeData[[246]]),
                         max(Raster_spdeData[[247]], Raster_spdeData[[248]]),
                         max(Raster_spdeData[[249]], Raster_spdeData[[250]]),
                         max(Raster_spdeData[[251]], Raster_spdeData[[252]]),
                         max(Raster_spdeData[[253]], Raster_spdeData[[254]]),
                         max(Raster_spdeData[[255]], Raster_spdeData[[256]]),
                         max(Raster_spdeData[[257]], Raster_spdeData[[258]]),
                         max(Raster_spdeData[[259]], Raster_spdeData[[260]]),
                         max(Raster_spdeData[[261]], Raster_spdeData[[262]]),
                         max(Raster_spdeData[[263]], Raster_spdeData[[264]]),
                         max(Raster_spdeData[[265]], Raster_spdeData[[266]]),
                         max(Raster_spdeData[[267]], Raster_spdeData[[268]]),
                         max(Raster_spdeData[[269]], Raster_spdeData[[270]]),
                         max(Raster_spdeData[[271]], Raster_spdeData[[272]]),
                         max(Raster_spdeData[[273]], Raster_spdeData[[274]]),
                         max(Raster_spdeData[[275]], Raster_spdeData[[276]]),
                         max(Raster_spdeData[[277]], Raster_spdeData[[278]]),
                         max(Raster_spdeData[[279]], Raster_spdeData[[280]]),
                         max(Raster_spdeData[[281]], Raster_spdeData[[282]]),
                         max(Raster_spdeData[[283]], Raster_spdeData[[284]]),
                         max(Raster_spdeData[[285]], Raster_spdeData[[286]]),
                         max(Raster_spdeData[[287]], Raster_spdeData[[288]]),
                         max(Raster_spdeData[[289]], Raster_spdeData[[290]]),
                         max(Raster_spdeData[[291]], Raster_spdeData[[292]]),
                         max(Raster_spdeData[[293]], Raster_spdeData[[294]]),
                         max(Raster_spdeData[[295]], Raster_spdeData[[296]]),
                         max(Raster_spdeData[[297]], Raster_spdeData[[298]]),
                         max(Raster_spdeData[[299]], Raster_spdeData[[300]]),
                         max(Raster_spdeData[[301]], Raster_spdeData[[302]]),
                         max(Raster_spdeData[[303]], Raster_spdeData[[304]]),
                         max(Raster_spdeData[[305]], Raster_spdeData[[306]]),
                         max(Raster_spdeData[[307]], Raster_spdeData[[308]]),
                         max(Raster_spdeData[[309]], Raster_spdeData[[310]]),
                         max(Raster_spdeData[[311]], Raster_spdeData[[312]]),
                         max(Raster_spdeData[[313]], Raster_spdeData[[314]]),
                         max(Raster_spdeData[[315]], Raster_spdeData[[316]]),
                         max(Raster_spdeData[[317]], Raster_spdeData[[318]]),
                         max(Raster_spdeData[[319]], Raster_spdeData[[320]]),
                         max(Raster_spdeData[[321]], Raster_spdeData[[322]]),
                         max(Raster_spdeData[[323]], Raster_spdeData[[324]]),
                         max(Raster_spdeData[[325]], Raster_spdeData[[326]]),
                         max(Raster_spdeData[[327]], Raster_spdeData[[328]]),
                         max(Raster_spdeData[[329]], Raster_spdeData[[330]]),
                         max(Raster_spdeData[[331]], Raster_spdeData[[332]]),
                         max(Raster_spdeData[[333]], Raster_spdeData[[334]]),
                         max(Raster_spdeData[[335]], Raster_spdeData[[336]]),
                         max(Raster_spdeData[[337]], Raster_spdeData[[338]]),
                         max(Raster_spdeData[[339]], Raster_spdeData[[340]]),
                         max(Raster_spdeData[[341]], Raster_spdeData[[342]]),
                         max(Raster_spdeData[[343]], Raster_spdeData[[344]]),
                         max(Raster_spdeData[[345]], Raster_spdeData[[346]]),
                         max(Raster_spdeData[[347]], Raster_spdeData[[348]]),
                         max(Raster_spdeData[[349]], Raster_spdeData[[350]]),
                         max(Raster_spdeData[[351]], Raster_spdeData[[352]]),
                         max(Raster_spdeData[[353]], Raster_spdeData[[354]]),
                         max(Raster_spdeData[[355]], Raster_spdeData[[356]]),
                         max(Raster_spdeData[[357]], Raster_spdeData[[358]]),
                         max(Raster_spdeData[[359]], Raster_spdeData[[360]]),
                         max(Raster_spdeData[[361]], Raster_spdeData[[362]]),
                         max(Raster_spdeData[[363]], Raster_spdeData[[364]]),
                         max(Raster_spdeData[[365]], Raster_spdeData[[366]]),
                         max(Raster_spdeData[[367]], Raster_spdeData[[368]]),
                         max(Raster_spdeData[[369]], Raster_spdeData[[370]]),
                         max(Raster_spdeData[[371]], Raster_spdeData[[372]]),
                         max(Raster_spdeData[[373]], Raster_spdeData[[374]]),
                         max(Raster_spdeData[[375]], Raster_spdeData[[376]]),
                         max(Raster_spdeData[[377]], Raster_spdeData[[378]]),
                         max(Raster_spdeData[[379]], Raster_spdeData[[380]]),
                         max(Raster_spdeData[[381]], Raster_spdeData[[382]]),
                         max(Raster_spdeData[[383]], Raster_spdeData[[384]]),
                         max(Raster_spdeData[[385]], Raster_spdeData[[386]]),
                         max(Raster_spdeData[[387]], Raster_spdeData[[388]]),
                         max(Raster_spdeData[[389]], Raster_spdeData[[390]]),
                         max(Raster_spdeData[[391]], Raster_spdeData[[392]]),
                         max(Raster_spdeData[[393]], Raster_spdeData[[394]]),
                         max(Raster_spdeData[[395]], Raster_spdeData[[396]]),
                         max(Raster_spdeData[[397]], Raster_spdeData[[398]]),
                         max(Raster_spdeData[[399]], Raster_spdeData[[400]]),
                         max(Raster_spdeData[[401]], Raster_spdeData[[402]]),
                         max(Raster_spdeData[[403]], Raster_spdeData[[404]]),
                         max(Raster_spdeData[[405]], Raster_spdeData[[406]]),
                         max(Raster_spdeData[[407]], Raster_spdeData[[408]]),
                         max(Raster_spdeData[[409]], Raster_spdeData[[410]]),
                         max(Raster_spdeData[[411]], Raster_spdeData[[412]]),
                         max(Raster_spdeData[[413]], Raster_spdeData[[414]]),
                         max(Raster_spdeData[[415]], Raster_spdeData[[416]]),
                         max(Raster_spdeData[[417]], Raster_spdeData[[418]]),
                         max(Raster_spdeData[[419]], Raster_spdeData[[420]]),
                         max(Raster_spdeData[[421]], Raster_spdeData[[422]]),
                         max(Raster_spdeData[[423]], Raster_spdeData[[424]]),
                         max(Raster_spdeData[[425]], Raster_spdeData[[426]]),
                         max(Raster_spdeData[[427]], Raster_spdeData[[428]]),
                         max(Raster_spdeData[[429]], Raster_spdeData[[430]]),
                         max(Raster_spdeData[[431]], Raster_spdeData[[432]]),
                         max(Raster_spdeData[[433]], Raster_spdeData[[434]]),
                         max(Raster_spdeData[[435]], Raster_spdeData[[436]]),
                         max(Raster_spdeData[[437]], Raster_spdeData[[438]]),
                         max(Raster_spdeData[[439]], Raster_spdeData[[440]]),
                         max(Raster_spdeData[[441]], Raster_spdeData[[442]]),
                         max(Raster_spdeData[[443]], Raster_spdeData[[444]]),
                         max(Raster_spdeData[[445]], Raster_spdeData[[446]]),
                         max(Raster_spdeData[[447]], Raster_spdeData[[448]]),
                         max(Raster_spdeData[[449]], Raster_spdeData[[450]]),
                         max(Raster_spdeData[[451]], Raster_spdeData[[452]]),
                         max(Raster_spdeData[[453]], Raster_spdeData[[454]]),
                         max(Raster_spdeData[[455]], Raster_spdeData[[456]]),
                         max(Raster_spdeData[[457]], Raster_spdeData[[458]]),
                         max(Raster_spdeData[[459]], Raster_spdeData[[460]]),
                         max(Raster_spdeData[[461]], Raster_spdeData[[462]]),
                         max(Raster_spdeData[[463]], Raster_spdeData[[464]]),
                         max(Raster_spdeData[[465]], Raster_spdeData[[466]]),
                         max(Raster_spdeData[[467]], Raster_spdeData[[468]]),
                         max(Raster_spdeData[[469]], Raster_spdeData[[470]]),
                         max(Raster_spdeData[[471]], Raster_spdeData[[472]]),
                         max(Raster_spdeData[[473]], Raster_spdeData[[474]]),
                         max(Raster_spdeData[[475]], Raster_spdeData[[476]]),
                         max(Raster_spdeData[[477]], Raster_spdeData[[478]]),
                         max(Raster_spdeData[[479]], Raster_spdeData[[480]]),
                         max(Raster_spdeData[[481]], Raster_spdeData[[482]]),
                         max(Raster_spdeData[[483]], Raster_spdeData[[484]]),
                         max(Raster_spdeData[[485]], Raster_spdeData[[486]]),
                         max(Raster_spdeData[[487]], Raster_spdeData[[488]]),
                         max(Raster_spdeData[[489]], Raster_spdeData[[490]]),
                         max(Raster_spdeData[[491]], Raster_spdeData[[492]]),
                         max(Raster_spdeData[[493]], Raster_spdeData[[494]]),
                         max(Raster_spdeData[[495]], Raster_spdeData[[496]]),
                         max(Raster_spdeData[[497]], Raster_spdeData[[498]]),
                         max(Raster_spdeData[[499]], Raster_spdeData[[500]]),
                         max(Raster_spdeData[[501]], Raster_spdeData[[502]]),
                         max(Raster_spdeData[[503]], Raster_spdeData[[504]]),
                         max(Raster_spdeData[[505]], Raster_spdeData[[506]]),
                         max(Raster_spdeData[[507]], Raster_spdeData[[508]]),
                         max(Raster_spdeData[[509]], Raster_spdeData[[510]]),
                         max(Raster_spdeData[[511]], Raster_spdeData[[512]]),
                         max(Raster_spdeData[[513]], Raster_spdeData[[514]]),
                         max(Raster_spdeData[[515]], Raster_spdeData[[516]]),
                         max(Raster_spdeData[[517]], Raster_spdeData[[518]]),
                         max(Raster_spdeData[[519]], Raster_spdeData[[520]]),
                         max(Raster_spdeData[[521]], Raster_spdeData[[522]]),
                         max(Raster_spdeData[[523]], Raster_spdeData[[524]]),
                         max(Raster_spdeData[[525]], Raster_spdeData[[526]]),
                         max(Raster_spdeData[[527]], Raster_spdeData[[528]]),
                         max(Raster_spdeData[[529]], Raster_spdeData[[530]]),
                         max(Raster_spdeData[[531]], Raster_spdeData[[532]]),
                         max(Raster_spdeData[[533]], Raster_spdeData[[534]]),
                         max(Raster_spdeData[[535]], Raster_spdeData[[536]]),
                         max(Raster_spdeData[[537]], Raster_spdeData[[538]]),
                         max(Raster_spdeData[[539]], Raster_spdeData[[540]]),
                         max(Raster_spdeData[[541]], Raster_spdeData[[542]]),
                         max(Raster_spdeData[[543]], Raster_spdeData[[544]]),
                         max(Raster_spdeData[[545]], Raster_spdeData[[546]]),
                         max(Raster_spdeData[[547]], Raster_spdeData[[548]]),
                         max(Raster_spdeData[[549]], Raster_spdeData[[550]]),
                         max(Raster_spdeData[[551]], Raster_spdeData[[552]]),
                         max(Raster_spdeData[[553]], Raster_spdeData[[554]]),
                         max(Raster_spdeData[[555]], Raster_spdeData[[556]]),
                         max(Raster_spdeData[[557]], Raster_spdeData[[558]]),
                         max(Raster_spdeData[[559]], Raster_spdeData[[560]]),
                         max(Raster_spdeData[[561]], Raster_spdeData[[562]]),
                         max(Raster_spdeData[[563]], Raster_spdeData[[564]]),
                         max(Raster_spdeData[[565]], Raster_spdeData[[566]]),
                         max(Raster_spdeData[[567]], Raster_spdeData[[568]]),
                         max(Raster_spdeData[[569]], Raster_spdeData[[570]]),
                         max(Raster_spdeData[[571]], Raster_spdeData[[572]]),
                         max(Raster_spdeData[[573]], Raster_spdeData[[574]]),
                         max(Raster_spdeData[[575]], Raster_spdeData[[576]]),
                         max(Raster_spdeData[[577]], Raster_spdeData[[578]]),
                         max(Raster_spdeData[[579]], Raster_spdeData[[580]]),
                         max(Raster_spdeData[[581]], Raster_spdeData[[582]]),
                         max(Raster_spdeData[[583]], Raster_spdeData[[584]]),
                         max(Raster_spdeData[[585]], Raster_spdeData[[586]]),
                         max(Raster_spdeData[[587]], Raster_spdeData[[588]]),
                         max(Raster_spdeData[[589]], Raster_spdeData[[590]]),
                         max(Raster_spdeData[[591]], Raster_spdeData[[592]]),
                         max(Raster_spdeData[[593]], Raster_spdeData[[594]]),
                         max(Raster_spdeData[[595]], Raster_spdeData[[596]]),
                         max(Raster_spdeData[[597]], Raster_spdeData[[598]]),
                         max(Raster_spdeData[[599]], Raster_spdeData[[600]]),
                         max(Raster_spdeData[[601]], Raster_spdeData[[602]]),
                         max(Raster_spdeData[[603]], Raster_spdeData[[604]]),
                         max(Raster_spdeData[[605]], Raster_spdeData[[606]]),
                         max(Raster_spdeData[[607]], Raster_spdeData[[608]]),
                         max(Raster_spdeData[[609]], Raster_spdeData[[610]]),
                         max(Raster_spdeData[[611]], Raster_spdeData[[612]]),
                         max(Raster_spdeData[[613]], Raster_spdeData[[614]]),
                         max(Raster_spdeData[[615]], Raster_spdeData[[616]]),
                         max(Raster_spdeData[[617]], Raster_spdeData[[618]]),
                         max(Raster_spdeData[[619]], Raster_spdeData[[620]]),
                         max(Raster_spdeData[[621]], Raster_spdeData[[622]]),
                         max(Raster_spdeData[[623]], Raster_spdeData[[624]]),
                         max(Raster_spdeData[[625]], Raster_spdeData[[626]]),
                         max(Raster_spdeData[[627]], Raster_spdeData[[628]]),
                         max(Raster_spdeData[[629]], Raster_spdeData[[630]]),
                         max(Raster_spdeData[[631]], Raster_spdeData[[632]]),
                         max(Raster_spdeData[[633]], Raster_spdeData[[634]]),
                         max(Raster_spdeData[[635]], Raster_spdeData[[636]]),
                         max(Raster_spdeData[[637]], Raster_spdeData[[638]]),
                         max(Raster_spdeData[[639]], Raster_spdeData[[640]]),
                         max(Raster_spdeData[[641]], Raster_spdeData[[642]]),
                         max(Raster_spdeData[[643]], Raster_spdeData[[644]]),
                         max(Raster_spdeData[[645]], Raster_spdeData[[646]]),
                         max(Raster_spdeData[[647]], Raster_spdeData[[648]]),
                         max(Raster_spdeData[[649]], Raster_spdeData[[650]]),
                         max(Raster_spdeData[[651]], Raster_spdeData[[652]]),
                         max(Raster_spdeData[[653]], Raster_spdeData[[654]]),
                         max(Raster_spdeData[[655]], Raster_spdeData[[656]]),
                         max(Raster_spdeData[[657]], Raster_spdeData[[658]]),
                         max(Raster_spdeData[[659]], Raster_spdeData[[660]]),
                         max(Raster_spdeData[[661]], Raster_spdeData[[662]]),
                         max(Raster_spdeData[[663]], Raster_spdeData[[664]]),
                         max(Raster_spdeData[[665]], Raster_spdeData[[666]]),
                         max(Raster_spdeData[[667]], Raster_spdeData[[668]]),
                         max(Raster_spdeData[[669]], Raster_spdeData[[670]]),
                         max(Raster_spdeData[[671]], Raster_spdeData[[672]]),
                         max(Raster_spdeData[[673]], Raster_spdeData[[674]]),
                         max(Raster_spdeData[[675]], Raster_spdeData[[676]]),
                         max(Raster_spdeData[[677]], Raster_spdeData[[678]]),
                         max(Raster_spdeData[[679]], Raster_spdeData[[680]]),
                         max(Raster_spdeData[[681]], Raster_spdeData[[682]]),
                         max(Raster_spdeData[[683]], Raster_spdeData[[684]]),
                         max(Raster_spdeData[[685]], Raster_spdeData[[686]]),
                         max(Raster_spdeData[[687]], Raster_spdeData[[688]]),
                         max(Raster_spdeData[[689]], Raster_spdeData[[690]]),
                         max(Raster_spdeData[[691]], Raster_spdeData[[692]]),
                         max(Raster_spdeData[[693]], Raster_spdeData[[694]]),
                         max(Raster_spdeData[[695]], Raster_spdeData[[696]]),
                         max(Raster_spdeData[[697]], Raster_spdeData[[698]]),
                         max(Raster_spdeData[[699]], Raster_spdeData[[700]]),
                         max(Raster_spdeData[[701]], Raster_spdeData[[702]]),
                         max(Raster_spdeData[[703]], Raster_spdeData[[704]]),
                         max(Raster_spdeData[[705]], Raster_spdeData[[706]]),
                         max(Raster_spdeData[[707]], Raster_spdeData[[708]]),
                         max(Raster_spdeData[[709]], Raster_spdeData[[710]]),
                         max(Raster_spdeData[[711]], Raster_spdeData[[712]]),
                         max(Raster_spdeData[[713]], Raster_spdeData[[714]]),
                         max(Raster_spdeData[[715]], Raster_spdeData[[716]]),
                         max(Raster_spdeData[[717]], Raster_spdeData[[718]]),
                         max(Raster_spdeData[[719]], Raster_spdeData[[720]]))

stkRaster_spde3<-unstack(stkRaster_spde2)
years<-rep(1982:2011,each=12)
size<-length(stkRaster_spde3)
months<-rep(c(016,047,075,106,136,167,197,228,259,289,320,350), times=size/12)

for (i in 1:length(stkRaster_spde3)) {
  writeRaster(stkRaster_spde3[[i]],filename=paste('J:/MonthlyEvi2Data/spde/', 
                                                    'VIP15P4','-','A',years[i],months[i], '-',003, '-', 'evi2-spde', pattern=".tif",
                                                    sep=''))
}

