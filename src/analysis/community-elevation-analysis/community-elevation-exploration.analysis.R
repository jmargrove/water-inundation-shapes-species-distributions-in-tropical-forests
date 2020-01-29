#" Community elevation analysis Fig1d
(function(){
  # loading the required packages
  library(vegan);library(ggplot2);library(raster);library(Hmisc)  
  
  # load and process the data 
  
  plot_data <- read.csv("./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv")
  plot_data <- subset(plot_data, sp != "Unkn" & DBH > 50) # get all individuals above 50
  plot_data <- droplevels(plot_data) # dropping levels 
  
  # Creating the factor elevation,
  expected_n_bands <- 12
  plot_data$felv25 <- cut2(plot_data$elev, g = expected_n_bands, m = 100)
  bands <- cut2(plot_data$elev, g = expected_n_bands, m = 100, onlycuts = TRUE)
  minElevation <- round(min(plot_data$elevation), 1)
  maxElevation <- round(max(plot_data$elevation), 1)
  elevationRange <- c(minElevation, maxElevation)
  bands[c(1, length(bands))] <- elevationRange
  # Number of bands used
  length(bands) - 1
  sd(with(plot_data, tapply(sp, felv25, length)))
  
  # Creating the matrix for the analysis 
  matSp <- with(plot_data, as.matrix(tapply(sp, list(felv25, sp), length)))
  
  matSp[is.na(matSp)] <- 0
  ### How many of the species occur at each of the top brackets 
  sp_data <- read.csv("./src/data/sepilok-field-experiment/field-experiment-species-list.table.csv") # load species list
  species_short <- as.character(sp_data$sp)
  #sp_data$sp # get species shorthand name 
  length(which(matSp[12, species_short] != 0)) # at high elevations
  length(which(matSp[1, species_short] != 0)) # at the lowest elevation 
  
  # Run the NMDS from the vegan package
  model <- metaMDS(matSp)
  md1 <- m1$points[, 1] # we want the sites 
  analysis <- list(
    model = model, 
    data = plot_data
  )
  
  return(analysis)
})()