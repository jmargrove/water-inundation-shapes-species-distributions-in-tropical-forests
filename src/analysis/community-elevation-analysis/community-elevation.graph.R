# graphic of the ndms analysis 
(function(){
  
  analysis <- source('./src/analysis/community-elevation-analysis/community-elevation-exploration.analysis.R')$value
  
  # Import the map
  r <- raster("./src/data/community-elevation-analysis/sepilok-160-dem.tiff") # This is the Sepilok, map
  rd <- disaggregate(r, fact = 40) # descale the raster 
  
  e <- extent(603945.651, 
              605788.486, 
              647133.509, 
              648054.815) # extent of the 160ha forest plot
  plot160c <- crop(x = rd, e)
  plot160c <- plot160c - min(plot160c@data@values)
  plot160c <- (plot160c / max(plot160c@data@values)) * (137.9 - 55.9) + 55.9 # calibrate to digital data used in for species.
  rVals <- plot160c@data@values # the values of the raster 
  comPlot <- plot160c # new file for the community plot
  
  # Adding in the new values per band
  for (i in 1:length(bands) - 1) {
    cVals <- which(rVals >= bands[i] & rVals <= bands[i + 1])
    comPlot@data@values[cVals] <- md1[i]
  }
  
  # Using ggplot raster to map the image 
  # Convert to gg plot format x, y, z 
  ggCom <- as.data.frame(comPlot, xy = TRUE)
  colnames(ggCom) <- c("x", "y", "z")
  rast
  breaks <- round(seq(min(comPlot@data@values), 0.97, length = 5), 2)
  
  # Check the graphic before saving 
  p1 <- ggplot(ggCom) + 
    geom_raster(aes(x, y, fill = z)) +
    scale_fill_gradient(low = "#232d29", high = "#6A9113", name = "", breaks = breaks) +
    theme_void()
  p1
  
})()