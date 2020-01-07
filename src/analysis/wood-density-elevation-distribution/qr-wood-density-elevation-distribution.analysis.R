# quantile regression of wood density elevation analysis 
# module provides the predictions
(function(){
  data <- source("./src/data/wood-density-elevation-distribution/convert-density-points-matrix.dataframe.R")$value
  
  # model the quantile regression 
  library(quantreg)
  taus <- c(0.025, 0.1, 0.5, 0.9, 0.975)
  quant_model <- rq(elevation_mean ~ density_mean, data = data, tau = taus)
  qr_pred <- data.frame(density_mean = seq(min(data$density_mean), max(data$density_mean), length = 100))
  qr_preds <- expand.grid(density_mean = qr_pred$density_mean, Quantile = as.factor(taus))
  str(qr_preds)
  qr_preds$elevation_mean <- as.vector(predict(quant_model, qr_pred, type = "response"))
  qr_preds$Quantile <- as.factor(qr_preds$Quantile)
  return(list(preds = qr_preds, 
              model = quant_model
              )
         )
})()
