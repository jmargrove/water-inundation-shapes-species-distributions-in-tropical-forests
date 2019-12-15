#' Compare intial wood density to the control analysis

#' clear workspace 
rm(list = ls())

#' load packages 
library(ggplot2)
#' Load helper functions
source('./src/utils/dAIC.R')

#' load inital trait data
trait_data <- read.csv("./src/data/nursery-experiment/wood-density-nursery-base.raw.csv")
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
data$time <- as.factor(data$time)

# write this raw data to file for later use
write.csv(data, './src/data/nursery-experiment/initial-control-wood-density.raw.csv', row.names = FALSE)

# linear model testing for an interaction between species and time 
head(data)
model_max <- lm(wood_density ~ time * sp, data)
model_additive <- update(model_max, . ~ . - sp:time)

# compare models by AIC
dAIC(model_max, model_additive)
# 3.43927

# plot the residuals 
plot(model_max, which = 1) # good
plot(model_max, which = 2) # good 
plot(model_max, which = 3) # good 
# one slight outlier yet within possible range of wood density values.

# save the model 
save(model_max, file = "./src/analysis/nursery-experiment/compare-initial-to-control.model.R")

