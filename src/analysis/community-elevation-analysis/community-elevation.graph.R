# Graphic of the ndms analysis 
(function(){
  # Import the analysis file
  analysis <- source('./src/analysis/community-elevation-analysis/community-elevation-exploration.analysis.R')$value
  
  # Import the map
  r <- raster("./src/data/community-elevation-analysis/sepilok-160-dem.tiff") # This is the Sepilok, map
  rd <- raster::aggregate(r, fact = 8) # descale the raster, resolution is too high
  
  e <- extent(603945.651, 
              605788.486, 
              647133.509, 
              648054.815) # extent of the 160ha forest plot
  
  plot160c <- crop(x = rd, e)
  plot160c <- plot160c - min(plot160c@data@values)
  plot160c <- (plot160c / max(plot160c@data@values)) * (137.9 - 55.9) + 55.9 # calibrate to digital data used in for species.
  rVals <- plot160c@data@values # the values of the raster 
  comPlot <- plot160c # new file for the community plot
  plot(comPlot)
  
  # Adding in the new values per band
  for (i in 1:length(analysis$bands) - 1) {
    cVals <- which(rVals >= analysis$bands[i] & rVals <= analysis$bands[i + 1])
    comPlot@data@values[cVals] <- analysis$model$points[, 1][i]
  }
  
  # Using ggplot raster to map the image 
  # Convert to gg plot format x, y, z 
  ggCom <- as.data.frame(comPlot, xy = TRUE)
  colnames(ggCom) <- c("longitude", "latitude", "z")

  breaks <- round(seq(min(comPlot@data@values), 0.97, length = 5), 2)
  
  # Check the graphic before saving 
  gg_theme <- source('./src/utils/gg-theme.graph.R')$value
  themed <- source('./src/utils/theme.R')$value
  
  # Create the raster 
  RasterComponent <- geom_raster(data = ggCom, aes(x = longitude, y = latitude, fill = z))
  
  # Scales 
  ScaleComponent <- scale_fill_gradient(low = themed$selectGreenLow(), 
                           high = themed$selectGreenHigh(), 
                           name = "", 
                           breaks = breaks) 
  
  p1 <- ggplot() + 
    RasterComponent +
    ScaleComponent +
    theme_void() +
    theme(legend.text = element_text(family = gg_theme$font_family))
  
  p1
  
  result <- list(
    plot = p1, 
    analysis = analysis
  )
  
  return(result)
})()