# Adult distribution generalize linear model
(function(){
  # Import data
  data <- source("./src/data/inla-spde-species-distributions/plot-occurance.dataframe.R")$value
  
  # Modeling occurrence data as a binomial generalized linear model  
  model <- glm(occurance ~ focal_sp * elevation + I(elevation ^ 2), family = "binomial", data = data)

  # Creating prediction data frame 
  preds <- expand.grid(focal_sp = unique(data$focal_sp),
                       elevation = with(data, seq(min(elevation), max(elevation), length = 100)))
  
  preds$pElevation <- predict(model1, preds, type = "response")
  
  # Model line 
  ModelComponent <- geom_line(data = preds, aes(x = elevation, y = pElevation, color = focal_sp))
  
  # Faceting 
  FacetComponent <- facet_wrap(~focal_sp, scales = "free_y")
  
  # Graph of the predicted values...
  p1 <- ggplot2::ggplot() +
    ModelComponent + 
    FacetComponent 
  
  # Object with the model, plot and result.
  result <- list(
    plot = p1, 
    model = model, 
    data = data)
  
  # return the values 
  return(result)

})()

