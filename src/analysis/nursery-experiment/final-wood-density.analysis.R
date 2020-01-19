# Analysis of the change in wood density
(function() {
    # load packages 
    library(lme4)
    # load the experiment data
    exp_data <- read.csv("./src/data/nursery-experiment/wood-density-final.raw.csv")
    # Run the model testing the hypothesis that the final wood density
    # is predicted by the treatment and initial diameter + random effects 
    # of mother and block REML false for model comparison 

    model <- lmer(final_wood_density ~ treatment +
                 initial_diameter +
                 (1 | mother) +
                 (1 | block),
               data = exp_data,
               REML = FALSE)
    # Remove the treatment and replace with log(treatment + 1)
    model2 <- update(model, . ~ . - treatment + log(treatment + 1))

    # Compare the models 
    AIC(model) # AIC model 
    AIC(model2) # AIC log treatment 
    # Model2 is 51.57 points lower 

    # Update the final model to REML
    final_model <- update(model2, . ~ ., REML = TRUE)

    # Model validation of model2 
    pearson_resid <- resid(final_model, type = "pearson")
    fitted_values <- fitted(final_model)
    plot(fitted_values, pearson_resid) # one outlier, i think the analysis is ok 

    summary(final_model)
    # Prediction data frame.
    prediction_dataframe = expand.grid(
    treatment = seq(0, 21, length = 100),
    initial_diameter = mean(exp_data$initial_diameter)
    )

    # Final wood density prediction.
    prediction_dataframe$final_wood_density <- predict(
        final_model,
        prediction_dataframe, re.form = ~0
    )


    boots_dir <- "./src/analysis/nursery-experiment/predictions/" # Location of predictions 
    boots_file <- paste(boots_dir, "ci-treatment-delta-density.bootstapped.R", sep = "") # Bootstrap file name

    if (file.exists(boots_file)) {
        CI <- read.csv(boots_file)
        prediction_dataframe$CI025 <- unlist(CI[1,])
        prediction_dataframe$CI975 <- unlist(CI[2,])
    } else {
        source("./src/utils/booter.R")
        CI <- booter(model = final_model, data = exp_data, preds = prediction_dataframe, n = 5000, MEM = TRUE)
        prediction_dataframe$CI025 <- CI[1,]
        prediction_dataframe$CI975 <- CI[2,]
        write.csv(CI, file = boots_file, row.names = FALSE)
    }

    coef_boots_file <- paste(boots_dir, "coef-ci-final-density.bootstrapped.R", sep = "")
    coefs <- as.data.frame(summary(final_model)$coef)
    if (file.exists(coef_boots_file)) {
        CI_coefs <- read.csv(coef_boots_file) # load the CIs for the coefficients
        coefs$CI025 <- unlist(CI_coefs[1,]) # assign to predictions
        coefs$CI975 <- unlist(CI_coefs[2,])

    } else {
        source("./src/utils/booter.R")
        # Calculate the CIs for the coefficients if there is no file present
        CI_coefs <- booter(model = final_model, data = exp_data, preds = prediction_dataframe, n = 5000, MEM = TRUE, coef = TRUE)
        coefs$CI025 <- unlist(CI_coefs[1,])
        coefs$CI975 <- unlist(CI_coefs[2,])
        write.csv(CI_coefs, file = coef_boots_file, row.names = FALSE)
    }

    # Return the model, predictions, with confidence intervals and coefficients, with boot
    # strapped data.
    return(list(mode = final_model, preds_treat = prediction_dataframe, coef = coef))
})()
