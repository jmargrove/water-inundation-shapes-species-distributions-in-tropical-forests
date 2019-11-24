#!/usr/bin/env Rscript --vanilla
##' @description Testing the inla-spde models for focalSpecies distributions.  

main <- function(){
  wd <- "/Users/jamesmargrove/Documents/analysis"
  pjt <- "/water-inundation-shapes-species-distributions-in-tropical-forests"
  data <- read.csv(paste(wd, pjt, '/src/data/sepilok-160ha-plot.csv', sep = ""))
  
  # Take one species 
  print("Constructing the dataframe 🚀")
  speciesWithMinNumber <- which(tapply(data$species, data$species, length) > 30)
  df <- data.frame(data[0,], focalSpecies = data[0, 1])
  
  for(species in names(speciesWithMinNumber)){
    restrictedDf <- data[, c("sp", "species", "longitude", "latitude", "elevation")]
    restrictedDf$occurance <- rep(0, length(restrictedDf$sp))
    restrictedDf$occurance[which(restrictedDf$species == species)] <- 1
    restrictedDf$focalSpecies <- rep(species, length(restrictedDf$species))
    df <- rbind(df, restrictedDf)
  }
  
  print("Datafrom construction completed")
  str(df)
  
  # Construct an inla 
  print("Getting the inla lib")
  library(INLA)
  print("inla lib handled")
  
  print("Constructing a mesh")
  coords <- with(df, cbind(longitude, latitude))
  mesh <- inla.mesh.2d(coords, max.edge = c(300, 500), 
                       cutoff = 20, 
                       offset=c(200,400))
  
  # priors 
  rho0 <- 20
  sig0 <- 0.5
  print(paste("Priors:"))
  print(paste("    rho0:", rho0))
  print(paste("    sig0:", sig0))
  
  
  # spde
  spde <- inla.spde2.pcmatern(mesh, 
                              alpha = 2,
                              prior.range = c(rho0, 0.01), 
                              prior.sigma = c(sig0, 0.01))
  print("spde complete")
  
  # A
  A <- inla.spde.make.A(mesh, loc=as.matrix(coords))
  print("A complete")
  
  # stk 
  stk <- inla.stack(tag = "stk", 
                    data = list(occurance = df$occurance), 
                    A = list(A, 1), 
                    effects = list(list(i = 1:spde$n.spde),
                                   data.frame(int = 1,
                                              elevation = df$elevation,
                                              focalSpecies = df$focalSpecies)))
  
  print("Stack complete")
  # formula 
  formula1 <- occurance ~ 0 + int + focalSpecies * elevation + I(elevation^2) + f(i, model = spde)
  print("Fromula complete")
  
  model1 <- inla(formula1, 
                 data = inla.stack.data(stk),
                 control.predictor = list(A = inla.stack.A(stk)),
                 control.fixed = list(expand.factor.strategy = "inla"),
                 family = "binomial")
  
  
  print("Model complete")
  summary(model1)
  print(summary(model1))
  file <- paste(wd, pjt, '/src/analysis/inla-spde-species-distributions/models/model1.R', sep = '')
  save(model1, file = file)
  print("model saved")
}


# Call main to start analysis function 
main()

