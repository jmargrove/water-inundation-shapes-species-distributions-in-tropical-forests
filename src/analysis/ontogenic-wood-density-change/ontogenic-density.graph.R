# graphing
(function(){
  # load the analysis of the data 
  analysis <- source("./src/analysis/ontogenic-wood-density-change/ontogenic-density.analysis.R")$value
  
  # load ggplot 
  library(draw)
  
  # Set drawing settings

  
  # Create a new drawing page
  drawPlot <- function(){
    width <-  120
    height <- width / 3 * 2
    drawSettings(pageWidth = width, 
                 pageHeight = height, 
                 units = "px")
    drawPage() 
    ?drawSettings
    maxx <- width - 0.5
    minx <- 0.5
    midx <- width / 2
    
    maxy <- height - 0.5
    miny <- 0.5
    midy <-  height / 2
    drawLine(x = c(minx, maxx, midx, minx),
             y = c(maxy, maxy, miny, maxy))
    
    # r.squared
    drawText(x = midx, y = midy, text = "r2", size = 24)
    
    # draw on corner labels 
    drawText(x = midx, y = miny, text = "Sapling", size = 24)
    drawText(x = minx, y = maxy, text = "Seedling", size = 24)
    drawText(x = maxx, y = maxy, text = "Adult", size = 24)
    
    # Draw r squared values 
    # seedling_sap 
    drawText(x = (midx - minx) / 2 + minx, y = (maxy - miny) / 2 + miny, 
             text = round(analysis$r2[2, 2], 2), 
             size = 24)
    
    # adult_sap
    drawText(x = 3.5, y = 2.75, 
             text = round(analysis$r2[3, 2], 2), 
             size = 24)
    drawText(x = 2.5, y = 3.7, text = round(analysis$r2[1, 2], 2), size = 24)
  }
  
  drawPlot()  
  
})()
