# Graphic of the ndms analysis 
(function(){
  # Import the analysis file
  analysis <- source('./src/analysis/community-elevation-analysis/community-elevation-exploration.analysis.R')$value
  
  # Import the map
  map_raw <- raster("./src/data/sepilok-160-dem.raw.tiff") # This is the Sepilok, map
  map <- raster::aggregate(map_raw, fact = 8) # descale the raster, resolution is too high
  
  e_crop <- extent(603945.651, 
              605788.486, 
              647133.509, 
              648054.815) # extent of the 160ha forest plot

  plot <- crop(x = map, e_crop)
  plot <- plot - min(plot@data@values)
  plot <- (plot / max(plot@data@values)) * (137.9 - 55.9) + 55.9 # calibrate to digital data used in for species.
  rVals <- plot@data@values # the values of the raster 
  community_plot <- plot # new file for the community plot
  plot(community_plot)
  
  # Adding in the new values per band
  for (i in 1:length(analysis$bands) - 1) {
    cVals <- which(rVals >= analysis$bands[i] & rVals <= analysis$bands[i + 1])
    community_plot@data@values[cVals] <- analysis$model$points[, 1][i]
  }
  
  # Using ggplot raster to map the image 
  # Convert to gg plot format x, y, z 
  community_plot_df <- as.data.frame(community_plot, xy = TRUE)
  colnames(community_plot_df) <- c("longitude", "latitude", "z")

  breaks <- round(seq(min(community_plot@data@values), 0.97, length = 5), 2)
  
  # Check the graphic before saving 
  gg_theme <- source('./src/utils/gg-theme.graph.R')$value
  themed <- source('./src/utils/theme.R')$value
  
  # Create the raster 
  RasterComponent <- geom_raster(data = community_plot_df, aes(x = longitude, y = latitude, fill = z))
  
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