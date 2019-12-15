#' Graphing the difference between the intial and control wood density in the nusery experiment. 
rm(list = ls())

# load 
load('./src/analysis/nursery-experiment/compare-initial-to-control.model.R') # the model 
data <- read.csv('./src/data/nursery-experiment/initial-control-wood-density.raw.csv') # data 
source('./src/utils/units.R') # units 
source('./src/utils/theme.R') # theme 
source('./src/utils/labels.R') # theme 
library(ggplot2) # load pakage ggplot2 

# view data 
str(data)

# prediction dataframe 
prediction_dataframe <- expand.grid(sp = levels(data$sp), time = levels(data$time)) # generate dataframe 
prediction_dataframe$wood_density <- predict(model_max, prediction_dataframe) # predict
se <- predict(model_max, prediction_dataframe, se.fit = TRUE)$se.fit # get standard error 
prediction_dataframe$CI025 <- prediction_dataframe$wood_density + se * 1.96 # calculate the lower confidence interval 
prediction_dataframe$CI975 <- prediction_dataframe$wood_density - se * 1.96 # calculate the upper confidence interval 

# plot the results 
p = ggplot(prediction_dataframe, aes(x = sp, y = wood_density, color = time)) + 
  geom_point() + 
  gErrorBars(prediction_dataframe$CI025, prediction_dataframe$CI975) +
  scale_color_manual(values = c(t$selectBlack(), t$selectRed())) +
  labels$xLabSpecies() +
  labels$yLabWoodDensity() +
  plotTheme() 

ggsave("./src/graphs/nursery_exp_initial_vs_control.plot.png", p, width = 8, height = 4)
