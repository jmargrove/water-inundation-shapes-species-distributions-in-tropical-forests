#' Analysis of the change in wood density

# clear workspace 
rm(list = ls())
(function(){
  #' load packages 
  library(lme4)
  # load the experiment data
  exp_data <- read.csv("./src/data/nursery-experiment/wood-density-final.raw.csv")
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
  
  summary(final_model)
  # prediction dataframe 
  prediction_dataframe = expand.grid(
    treatment = seq(0, 21, length = 100),
    initial_diameter = mean(exp_data$initial_diameter)
    )

  # final wood density prediction.
  prediction_dataframe$final_wood_density <- predict(
    final_model,
    prediction_dataframe, re.form = ~0
    )
  
  
  boots_dir <- './src/analysis/nursery-experiment/predictions/' # location of predictions 
  boots_file <- paste(boots_dir, 'ci-treatment-delta-density.bootstapped.R', sep = '') # bootstrap file name
  
  if(file.exists(boots_file)){
    CI <- read.csv(boots_file)
    prediction_dataframe$CI025 <- unlist(CI[1, ])
    prediction_dataframe$CI975 <- unlist(CI[2, ])
  } else {
    source("./src/utils/booter.R")
    CI <- booter(model = final_model, data = exp_data, preds = prediction_dataframe, n = 5000,  MEM = TRUE)
    prediction_dataframe$CI025 <- CI[1, ]
    prediction_dataframe$CI975 <- CI[2, ]
    write.csv(CI, file = boots_file, row.names = FALSE)
  }
  
  coef_boots_file <- paste(boots_dir, 'coef-ci-delta-density.bootstrapped.R', sep = '')
  coefs <- as.data.frame(summary(final_model)$coef)
  if(file.exists(coef_boots_file)){
    CI_coefs <- read.csv(coef_boots_file) # load the CIs for the coefs
    coefs$CI025 <- unlist(CI_coefs[1, ]) # asign to preds 
    coefs$CI975 <- unlist(CI_coefs[2, ])
    
  } else {
    source("./src/utils/booter.R")
    # calculate the CIs for the coefs if there is no file present
    CI_coefs <- booter(model = final_model, data = exp_data, preds = prediction_dataframe, n = 5000,  MEM = TRUE, coef = TRUE)
    coefs$CI025 <- unlist(CI_coefs[1, ])
    coefs$CI975 <- unlist(CI_coefs[2, ])
    write.csv(CI_coefs, file = coef_boots_file, row.names = FALSE)
  }
  
  # return the model, preds, with confidence intervals and coefs all boostrapped
  return(list(mode = final_model, preds_treat = prediction_dataframe, coef = coef))
})()
