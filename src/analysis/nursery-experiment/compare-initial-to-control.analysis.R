rm(list = ls())


trait_data <- read.csv("./src/data/nursery-experiment/wood-density-nursery-base.raw.csv")

exp_data <- read.csv("./src/data/nursery-experiment/wood-density-final.raw.csv")

head(exp_data)

# Prepairing the date from the control of the experiment (time = final)
control_data <- exp_data[ which(exp_data$treatment == 0),]
control_data <- control_data[, c("species", "sp", "final_wood_density")]
names(control_data) <- c("species", "sp", "wood_density")
control_data$time <- rep("final", length(control_data$species))

# Prepairing the trait data (time = initial)
trait_data <- trait_data[, c("species", "sp", "wood_density")]
trait_data$time <- rep("initial", length(trait_data$species))

# combine the data frames 
data <- rbind(trait_data, control_data)
data$time <- as.factor(data$time )
# linear model testing for an interaction between species and time 
head(data)
model_max <- lm(wood_density ~ time * species, data)
summary(model_max)

plot(model_max, which = 1)
plot(model_max, which = 2)
plot(model_max, which = 3)
plot(model_max, which = 4)

# ok not exactly the best but good enought 
preds <- expand.grid(species = levels(data$species), time = levels(data$time))
preds$wood_density <- predict(model_max, preds)

se <- predict(model_max, preds, se.fit = TRUE)$se.fit

preds$CI025 <- preds$wood_density + se * 1.96 
preds$CI975 <- preds$wood_density - se * 1.96 


library(ggplot2)
ggplot(preds, aes(x = species, y = wood_density, color = time)) + geom_point() + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975))






