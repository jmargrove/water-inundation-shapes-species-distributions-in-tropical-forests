# Graphing the species probability distribution 
rm(list = ls())
# Import packages 
library(ggplot2)
library(gtable)
library(grid)

# Import plot data and the species lists 
plot_data <- read.csv("./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv")
spnames <- read.csv("./src/data/sepilok-field-experiment/field-experiment-species-list.table.csv")

prediction_wd <- "./src/analysis/inla-spde-species-distributions/predictions/"
# Import predictions from the models 
data <- read.csv(paste(prediction_wd, "prediction-curves.dataframe.csv", sep = "")) # predictions 
load(paste(prediction_wd, "prediction-curves-confidence-intervals.R", sep = "")) # confidence intervals 

# Adding confidence intervals to the data frame 
data$CI025 <- CI[1, 1:(131 * 16)] # lower 
data$CI975 <- CI[2, 1:(131 * 16)] # upper 

# plot elevation ranges  
min_elevation <- min(plot_data$elevation) # min plot elevation (with a tree)
max_elevation <- max(plot_data$elevation) # max plot elevation (with a tree)

# chop the prediction data to within the range of the elevation. 
data_ranged <- data[data$elev > min_elevation & data$elev < max_elevation,]

# Graph 
p1 <- ggplot(data_ranged, aes(x = elev, y = p)) +
  geom_ribbon(aes(ymin = CI025, ymax = CI975, linetype = NA), alpha = 0.22) +
  geom_line() +
  facet_wrap(~sp, scale = "free_y", nrow = 4) +
  theme_classic() +
  theme(axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black")) +
  xlab("Elevation (m)") + ylab("p(occurance)") +
  theme(legend.position = "none") +
  theme(strip.text = element_text(face = "italic")) +
  theme(text = element_text(size = 20))

p1
