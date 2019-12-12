#' Compare intial wood density to the control analysis

#' clear workspace 
rm(list = ls())

#' load packages 
library(ggplot2)

#' load inital trait data
trait_data <- read.csv("./src/data/nursery-experiment/wood-density-base.table.csv")
#' load experiment data 
exp_data <- read.csv("./src/data/nursery-experiment/wood-density-final.raw.csv")

#' Prepairing the date from the control of the experiment (time = final)
control_data <- exp_data[ which(exp_data$treatment == 0),] # control 
control_data <- control_data[, c("species", "sp", "final_wood_density")] # select specific cols 
names(control_data) <- c("species", "sp", "wood_density") # rename columns to fit the trait data
control_data$time <- rep("final", length(control_data$species)) # time as a factor 

#' Prepairing the trait data (time = initial)
trait_data <- trait_data[, c("species", "sp", "wood_density")]
trait_data$time <- rep("initial", length(trait_data$species))

#' combine the data frames 
data <- rbind(trait_data, control_data)
data$time <- as.factor(data$time )
# linear model testing for an interaction between species and time 
head(data)
model_max <- lm(wood_density ~ time * sp, data)
model_additive <- update(model_max, . ~ . - sp:time)

# compare models by AIC
AIC(model_additive) # -316.7375
AIC(model_max) # -303.7545
AIC(model_max) - AIC(model_additive) 
# 12.98293

# plot the residuals 
plot(model_max, which = 1) # good
plot(model_max, which = 2) # good 
plot(model_max, which = 3) # good 
plot(model_max, which = 4) # good 
# ok not exactly the best but good enought 

# prediction dataframe 
prediction_dataframe <- expand.grid(sp = levels(data$sp), time = levels(data$time))
prediction_dataframe$wood_density <- predict(model_max, prediction_dataframe)

se <- predict(model_max, prediction_dataframe, se.fit = TRUE)$se.fit # get standard error 

prediction_dataframe$CI025 <- prediction_dataframe$wood_density + se * 1.96 # calculate the lower confidence interval 
prediction_dataframe$CI975 <- prediction_dataframe$wood_density - se * 1.96 # calculate the upper confidence interval 

# plot the results 
ggplot(prediction_dataframe, aes(x = sp, y = wood_density, color = time)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975))

# save the model 
save(model_max, file = "./src/analysis/nursery-experiment/compare-initial-to-control.model.R")

