# Load packages 
library(ggplot2)
library(lme4)
# Load the experiment data
exp_data <- read.csv("./src/data/nursery-experiment/wood-density-final.raw.csv")
# Graph the raw data of the change in wood density

# Clear workspace 
rm(list = ls())

# Exploration of the data 
str(exp_data)
# Plot final wood density predicted by the treatment
ggplot(exp_data, aes(x = treatment, y = final_wood_density)) +
  geom_point() +
  stat_smooth(method = lm)

# Plot final wood density predicted by log(treatment + 1)
ggplot(exp_data, aes(x = log(treatment + 1), y = final_wood_density)) +
  geom_point() +
  stat_smooth(method = lm)

# Plot final wood density predicted by the initial diameter 
ggplot(exp_data, aes(x = initial_diameter, y = final_wood_density)) +
  geom_point() +
  stat_smooth(method = lm)

# Plot the wood density vs species 
ggplot(exp_data, aes(x = sp, y = final_wood_density)) +
  geom_point() +
  geom_boxplot(alpha = 0.5)

# Plot the wood density vs mother
ggplot(exp_data, aes(x = mother, y = final_wood_density)) +
  geom_point() +
  geom_boxplot(alpha = 0.5)

# Plot the wood density vs block 
ggplot(exp_data, aes(x = block, y = final_wood_density)) +
  geom_point() +
  geom_boxplot(alpha = 0.5)