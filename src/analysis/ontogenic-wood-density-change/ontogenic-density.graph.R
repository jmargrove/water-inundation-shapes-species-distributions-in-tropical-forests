# graphing
(function(){
  # load the analysis of the data 
  analysis <- source('./src/analysis/ontogenic-wood-density-change/ontogenic-density.analysis.R')$value
  
  # load ggplot 
  library(draw)
  
  # Set drawing settings

  
  # Create a new drawing page
  drawPlot <- function(){
    drawSettings(pageWidth = 7, pageHeight = 5, units = "inches")
    drawPage() 
    maxx <- 5
    minx <- 0.5
    midx <- (maxx - minx) / 2 + minx
    
    maxy <- 3.5 
    miny <- 0.5
    midy <-  (maxy - miny) / 2 + miny
    drawLine(x = c(minx, maxx, midx, minx),
             y = c(maxy, maxy, miny, maxy))
    
    # r.squared
    drawText(x = midx, y = midy, text = "r2", size = 24)
    
    # draw on corner labels 
    drawText(x = 2.5, y = 1.9, text = "Sapling")
    drawText(x = 1, y = 3.7, text = "Seedling")
    drawText(x = 4, y = 3.7, text = "Adult")
    
    # Draw r squared values 
    drawText(x = 1.5, y = 2.75, text = round(analysis$r2[2, 2], 2))
    drawText(x = 3.5, y = 2.75, text = round(analysis$r2[3, 2], 2))
    drawText(x = 2.5, y = 3.7, text = round(analysis$r2[1, 2], 2))
  }
  
  drawPlot()  
  
})()
