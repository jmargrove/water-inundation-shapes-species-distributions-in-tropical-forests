# Adult distribution analysis for 16 species
# This analysis is carried out on a multi core computer (16 cors) and still takes +24hours 
# Clear the work space 
rm(list = ls())
# Packages required 
library(INLA)
library(arm)
library(ggplot2)
library(doSNOW)

# load the dataframe the dataframe dataframe called data 
source('./src/data/inla-spde-species-distributions/plot-occurance.dataframe.R')
str(data) # the dataframe 
# coords and the mesh 
coords <- data[,c('longitude', 'latitude')]

m1 <- inla.mesh.2d(coords, max.edge = c(200, 500), 
                   cutoff =100, 
                   offset=c(200,400))

spde <- inla.spde2.matern(m1, alpha=2) # spde
#Making the data stack, only for modeling the spatial effect
A <- inla.spde.make.A(m1, loc=as.matrix(coords))

# stack dataframe 
stk<- inla.stack(tag = "stk", 
                 data=list(occ=sdata$occurance), 
                 A=list(A, 1), 
                 effects=list(list(i=1:spde$n.spde),
                              data.frame(int=1,
                                         elev=data$elevation,
                                         FocSp=data$focal_species)))

# Setting up the cores
# getDoParWorkers()# 16 cores 
# nclust <- makeCluster(16, type="SOCK")
# registerDoSNOW(nclust)
# clusterExport(nclust, c("stk","spde","inla","inla.stack.data","inla.stack.A"))

#Running the INLA model
spc1<- inla(occ ~ 0 + int + elev + I(elev^2) +
                f(i, model=spde),
            family="binomial",  data=inla.stack.data(stk), control.predictor=list(A=inla.stack.A(stk)),
            control.fixed = list(expand.factor.strategy="inla"), 
            num.threads = 16, control.compute=list(cpo=TRUE))
#
spc2<- inla(occ ~ 0 + int + elev + I(elev^2) + FocSp +
                f(i, model=spde),
            family="binomial",  data=inla.stack.data(stk), control.predictor=list(A=inla.stack.A(stk)),
            control.fixed = list(expand.factor.strategy="inla"), 
            num.threads = 16, control.compute=list(cpo=TRUE))
#
spc3<- inla(occ ~ 0 + int + elev + I(elev^2) + FocSp + FocSp:elev +
                f(i, model=spde),
            family="binomial",  data=inla.stack.data(stk), control.predictor=list(A=inla.stack.A(stk)),
            control.fixed = list(expand.factor.strategy="inla"), 
            num.threads = 16, control.compute=list(cpo=TRUE))
#
spc4<- inla(occ ~ 0 + int + elev + I(elev^2) + FocSp + FocSp:elev + FocSp:I(elev^2) +
                f(i, model=spde),
            family="binomial",  data=inla.stack.data(stk), control.predictor=list(A=inla.stack.A(stk)),
            control.fixed = list(expand.factor.strategy="inla"), 
            num.threads = 16, control.compute=list(cpo=TRUE))
#
spc5<- inla(occ ~ 0 + int + elev + I(elev^2) + FocSp + FocSp:elev + FocSp:I(elev^2) + FocSp:elev:I(elev^2) + 
                f(i, model=spde),
            family="binomial",  data=inla.stack.data(stk), control.predictor=list(A=inla.stack.A(stk)),
            control.fixed = list(expand.factor.strategy="inla"), 
            num.threads = 16, control.compute=list(cpo=TRUE))

# Check the cpo values 
cpo1 <- sum(log(spc1$cpo$cpo))*-2
cpo2 <- sum(log(spc2$cpo$cpo))*-2
cpo3 <- sum(log(spc3$cpo$cpo))*-2
cpo4 <- sum(log(spc4$cpo$cpo))*-2
cpo5 <- sum(log(spc5$cpo$cpo))*-2
stopCluster(nclust)

# extract the fixed effects from the model spc3 - the best model for the data 
cf1 <- spc3$summary.fixed[,1] # parameters 

# consrtruct a dataframe of coef values for each species 
coefVals <- data.frame(int=c(cf1[1]+cf1[4:19]), 
                       x1=c(cf1[2]+cf1[20:35]), 
                       x2=rep(cf1[3], 16),
                       sp=sp)

dtf <- sapply(1:16, function(x){
  cf1 <- as.numeric(coefVals[x,])
  spcf1 <- function(x) cf1[1] + cf1[2]*x + cf1[3]*x^2
  invlogit(spcf1(0:130))})

dtf <- as.vector(dtf) # convert matrix to vector 
preds <- data.frame(sp=rep(sp, each=131), p=dtf, elev=rep(0:130, times=16))

# calculate the most likely occurance (differentiate to find the peek occurance)
pelev <- (-(coefVals[,2])/((coefVals[,3])*2))
pelev_data <- data.frame(sp = levels(data$focal_sp), pelev = pelev)
# save the predictions 
write.csv(preds, "preds5.txt", row.names = F)
write.csv(pelev_data, file = 'occurance.analysis.txt', row.names = F)

# `mean sample`
spc3$summary.fixed$mean

# Now to calculate the 95% confidence intervals. Inla model needs to be run with the control.compute=list(config=TRUE)) 
#' to calcualte the posteriour 
# one sample
require(INLA)
require(foreach)
spc3<- inla(occ ~ 0 + int + elev + I(elev^2) + FocSp + FocSp:elev +
                f(spatial.feild, model=spde, replicate=spatial.feild.repl),
            family="binomial",  data=inla.stack.data(stk), control.predictor=list(A=inla.stack.A(stk)),
            control.fixed = list(expand.factor.strategy="inla"), 
            num.threads = 16, control.compute=list(config=TRUE))
stopCluster(nclust)

FunBoots <- function(x){
  s = inla.posterior.sample(1, result=spc3)
  s = s[[1]]$latent
  cf1 <- tail(s, 35)
  cf1
  coef=rownames(cf1)
  cf1[20:35] <- cf1[20:35][order(coef[20:35])]
  cf1[4:19] <- cf1[4:19][order(coef[4:19])]
  coefVals <- data.frame(int=c(cf1[1]+cf1[4:19]), 
                         x1=c(cf1[2]+cf1[20:35]), 
                         x2=rep(cf1[3], each=16),
                         sp=sp)

#calculate the actual curves
  dtf <- sapply(1:16, function(x){
    cf1 <- as.numeric(coefVals[x,])
    spcf1 <- function(x) cf1[1] + cf1[2]*x + cf1[3]*x^2
    invlogit(spcf1(0:130))})

  dtf <- as.vector(dtf)
  pele <- (-(coefVals[,2])/((coefVals[,3])*2))
  pele[pele<0] <- 0 
  dtf <- c(dtf,pele)
  return(dtf)
}

# posterior sampling 
post.samp2 <- foreach(i = 1:5000, .combine="cbind") %do% FunBoots()
write.table(post.samp2, "post.samp5000.txt")
