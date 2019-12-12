#' Analysis of the change in wood density

# clear workspace 
rm(list = ls())

#' load packages 
library(ggplot2)
library(lme4)

# load the experiment data
exp_data <- read.csv("./src/data/nursery-experiment/wood-density-final.raw.csv")

# exploration of the data 
str(exp_data)
#' plot final wood density predicted by the treatment
ggplot(exp_data, aes(x = treatment, y = final_wood_density)) + 
  geom_point() + 
  stat_smooth(method = lm)

#' plot final wood density predicted by log(treatment + 1)
ggplot(exp_data, aes(x = log(treatment + 1), y = final_wood_density)) + 
  geom_point() + 
  stat_smooth(method = lm)

#' plot final wood density prediced by the initial diameter 
ggplot(exp_data, aes(x = initial_diameter, y = final_wood_density)) + 
  geom_point() + 
  stat_smooth(method = lm)

#' plot the wood density vs species 
ggplot(exp_data, aes(x = sp, y = final_wood_density)) + 
  geom_point() +
  geom_boxplot(alpha = 0.5)

#' plot the wood density vs mother
ggplot(exp_data, aes(x = mother, y = final_wood_density)) + 
  geom_point() +
  geom_boxplot(alpha = 0.5)

#' plot the wood density vs block 
ggplot(exp_data, aes(x = block, y = final_wood_density)) + 
  geom_point() +
  geom_boxplot(alpha = 0.5)

#' Run the model testing the hypothesis that the final wood density
#' is prediced by the treatment and initial diameter + random effects 
#' of mother and block REML false for model comparison 

model <-lmer(final_wood_density ~ treatment + 
               initial_diameter + 
               (1 | mother) + 
               (1 | block), 
             data = exp_data, 
             REML = FALSE)
#' Remove the treatment and replace with log(treatment + 1)
model2 <- update(model, . ~ . - treatment + log(treatment + 1))

#' compare the models 
AIC(model) # aic model 
AIC(model2) # aic log treatment 
# model2 is 51.57 points lower 

#' Update the final model to REML
final_model <- update(model2, . ~ ., REML = TRUE)

#' model validation of model2 
pearson_resid <- resid(final_model, type = "pearson")
fitted_values <- fitted(final_model)
plot(fitted_values, pearson_resid) # one outlier, i think the anlaysis is ok 




#' perdiction data frame 
prediction_dataframe = expand.grid(
  treatment = seq(0, 21, length = 100), 
  initial_diameter = mean(exp_data$initial_diameter)
  )

# final wood density prediction. 
prediction_dataframe$final_wood_density <- predict(
  final_model, 
  prediction_dataframe, re.form = ~0
  )

#' overall the model looks good and the quick plot looks ok too. publication plotting 
#' in delta-wood-density.graph.R 
save(final_model, file = './src/analysis/nursery-experiment/delta-wood-density.model.R')