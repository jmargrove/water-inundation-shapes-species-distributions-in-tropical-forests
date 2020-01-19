#adult distribution generalize linear model
source("./src/data/inla-spde-species-distributions/plot-occurance.dataframe.R")

# Modeling occurrence data as a binomial generalized linear model  
model1 <- glm(occurance ~ focal_sp * elevation + I(elevation ^ 2), family = "binomial", data = data)
summary(model1)

# Creating prediction data frame 
preds <- expand.grid(focal_sp = data_species$sp,
                     elevation = with(data, seq(min(elevation), max(elevation), length = 100)))

preds$pElevation <- predict(model1, preds, type = "response")

# Predicting values 
require(ggplot2)

# Graph of the predicted values...
ggplot(preds, aes(x = elevation, y = pElevation, color = focal_sp)) +
  facet_wrap(~focal_sp, scales = "free_y") +
  geom_line() +
  theme_bw()

