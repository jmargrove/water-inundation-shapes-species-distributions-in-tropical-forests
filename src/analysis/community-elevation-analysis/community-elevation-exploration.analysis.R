#" Community elevation analysis Fig1d
(function(){
  # loading the required packages
  library(vegan);library(ggplot2);library(raster);library(Hmisc)  
  
  # Load and process the data 
  plot_data <- read.csv("./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv")
  plot_data <- subset(plot_data, sp != "Unkn" & DBH > 50) # get all individuals above 50
  plot_data <- droplevels(plot_data) # Dropping levels 
  
  # Creating the factor elevation,
  expected_n_bands <- 12
  plot_data$felv25 <- cut2(plot_data$elev, g = expected_n_bands, m = 100)

  bands <- cut2(x = plot_data$elev, 
                g = expected_n_bands, 
                m = 100, 
                onlycuts = TRUE)
  
  # What is the elevation range of the plot?
  elevationRange <- round(range(plot_data$elevation), 1)
  bands[c(1, length(bands))] <- elevationRange

  # Creating the matrix for the analysis 
  matSp <- with(plot_data, as.matrix(tapply(sp, list(felv25, sp), length)))
  
  matSp[is.na(matSp)] <- 0
  ### How many of the species occur at each of the top brackets 
  sp_data <- source('./src/summary-tables/field-experiment-species-list.table.R')$value
  species_short <- as.character(sp_data$sp)
  #sp_data$sp # get species shorthand name 
  length(which(matSp[12, species_short] != 0)) # at high elevations
  length(which(matSp[1, species_short] != 0)) # at the lowest elevation 
  
  # Run the NMDS from the vegan package
  model <- metaMDS(matSp)

  analysis <- list(
    model = model, 
    bands = bands,
    data = plot_data
  )
  
  return(analysis)
})()