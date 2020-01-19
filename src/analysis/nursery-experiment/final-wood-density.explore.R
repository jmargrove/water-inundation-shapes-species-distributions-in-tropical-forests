#" load packages 
library(ggplot2)
library(lme4)
# load the experiment data
exp_data <- read.csv("./src/data/nursery-experiment/wood-density-final.raw.csv")
#" Graph the raw data of the change in wood density

# clear workspace 
rm(list = ls())

# exploration of the data 
str(exp_data)
#" plot final wood density predicted by the treatment
ggplot(exp_data, aes(x = treatment, y = final_wood_density)) +
  geom_point() +
  stat_smooth(method = lm)

#" plot final wood density predicted by log(treatment + 1)
ggplot(exp_data, aes(x = log(treatment + 1), y = final_wood_density)) +
  geom_point() +
  stat_smooth(method = lm)

#" plot final wood density prediced by the initial diameter 
ggplot(exp_data, aes(x = initial_diameter, y = final_wood_density)) +
  geom_point() +
  stat_smooth(method = lm)

#" plot the wood density vs species 
ggplot(exp_data, aes(x = sp, y = final_wood_density)) +
  geom_point() +
  geom_boxplot(alpha = 0.5)

#" plot the wood density vs mother
ggplot(exp_data, aes(x = mother, y = final_wood_density)) +
  geom_point() +
  geom_boxplot(alpha = 0.5)

#" plot the wood density vs block 
ggplot(exp_data, aes(x = block, y = final_wood_density)) +
  geom_point() +
  geom_boxplot(alpha = 0.5)