
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
#########################################################
#cyclic model with harmonics
###########################################################
tranzoiadata=read.csv("F:/CLIM_DATA/tranzoia_county/combine.tranz.csv")
dim(tranzoiadata)
names(tranzoiadata)

#######################
#some data management
########################
require(reshape2)

w=pi/6
nloc=150    #number of pixels
nyears=31   #number of years
nmonths=372 #number of months from 1982:2012 (12*nyears)

loc=as.matrix(melt(tranzoiadata, id.vars=c("x", "y"))[1:150,c("x","y")]) #pixel center coordinates
yp=melt(tranzoiadata, id.vars=c("x", "y"))[ , "value"] # reshaped evi values
month=rep(rep(1:12, each=150), times=31)
year=rep(1982:2012, each=nloc*12) 

amplitude.cos <- cos(w*month)
amplitude.sine <- sin(w*month)
weight <- w*month

plot(amplitude.cos, type="l")
plot(amplitude.sine, type="l")
plot(yp, type="l")

###########################################
# SPDE model functionality using INLA
###########################################
require(INLA)
kappa <- 5

mesh = inla.mesh.2d(loc=loc, max.edge=c(0.5, 1)/kappa, cutoff=0.1/kappa)
plot(mesh)

spde = inla.spde2.matern(mesh, alpha = 2)
index = inla.spde.make.index("space", n.spde = spde$n.spde, group=year, n.group = nyears)

A = inla.spde.make.A(mesh, loc = loc,
                        index = seq_len(nrow(loc)),
                        weights=weight,
                        group = year,
                        n.group= nyears)

intercept=rep(1, times=nloc*nmonths)
stack = inla.stack(data = list(y = yp),
                   A = list(A, 1, 1, 1), 
                   effects = list(index, amplitude.cos, 
                                  amplitude.sine, 
                                  intercept),
                   tag='est')

data.stk = inla.stack.data(stack) 
ind = inla.stack.index(stack, tag='est')$data

formula = y ~ 0 + intercept + 
  f(space, weight=amplitude.cos, model=spde, group = space.group,
    control.group = list(model = "ar1")) + 
  f(space, weight=amplitude.sine, model=spde, group = space.group,
    control.group = list(model = "ar1")) 

result = inla(formula,
                    family="gaussian", 
                    data=data.stk,
                    quantiles=NULL, 
                    control.compute=list(cpo=FALSE, config=FALSE),
                    control.predictor=list(A=inla.stack.A(stk),
                                           compute=TRUE),
                    control.inla = list(strategy = "gaussian"),
                    verbose=FALSE)

############################################
#extracting space varying coefficients
############################################
#cos.amplitude=result$summary.random$<something>$mean 
#sine.amplitude=result$summary.random$<something>$mean
