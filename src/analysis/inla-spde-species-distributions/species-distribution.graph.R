# Graphing the species probability distribution (16)
(function(){
  # Import packages 
  library(ggplot2);library(gtable);library(grid);
  
  # Import plot data and the species lists 
  plot_data <- read.csv("./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv")
  spnames <- read.csv("./src/data/sepilok-field-experiment/field-experiment-species-list.table.csv")
  
  # Import predictions from the models 
  prediction_wd <- "./src/analysis/inla-spde-species-distributions/predictions/"
  data <- read.csv(paste(prediction_wd, "prediction-curves.dataframe.csv", sep = "")) # predictions 
  load(paste(prediction_wd, "prediction-curves-confidence-intervals.R", sep = "")) # confidence intervals 
  
  # Adding confidence intervals to the data frame 
  data$CI025 <- CI[1, 1:(131 * 16)] # Lower 
  data$CI975 <- CI[2, 1:(131 * 16)] # Upper 
  
  # Plot elevation ranges  
  min_elevation <- min(plot_data$elevation) # min plot elevation (with a tree)
  max_elevation <- max(plot_data$elevation) # max plot elevation (with a tree)
  
  # Chop the prediction data to within the range of the elevation. 
  data_ranged <- data[data$elev > min_elevation & data$elev < max_elevation,]
  
  # Import themes
  gg_theme <- source('./src/utils/gg-theme.graph.R')$value
  themed <- source('./src/utils/theme.R')$value
  
  # Graph 
  p1 <- ggplot(data_ranged, aes(x = elev, y = p)) +
    geom_ribbon(aes(ymin = CI025, ymax = CI975, linetype = NA), alpha = gg_theme$ribbon_alpha, fill = gg_theme$ribbon_color) +
    geom_line() +
    facet_wrap(~sp, scale = "free_y", nrow = 4) +
    theme_classic() +
    theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
    xlab("Elevation (m) asl") + ylab("p(occurance)") +
    theme(legend.position = "none") +
    theme_classic() +
    gg_theme$t2 +
    theme(strip.text = element_text(face = "italic", 
                                    family = gg_theme$font_family, 
                                    size = 8, 
                                    margin = margin(t = 0.25, r = 0, b = 0.25, l = 0, unit = "mm"))) + 
    theme(axis.text.x = element_text(size = 6)) +
    theme(axis.text.y = element_text(size = 6)) +
    theme(axis.line = element_line(size = 0.2, color = themed$selectBlack())) +
    theme(axis.ticks = element_line(size = 0.15)) +
    theme(strip.background = element_rect(colour="black", size=0.25))
  
  
  result <- list(
    plot = p1)
  
  return(result)

})()