
(function() {
    rm(list = ls())
    #" load packages 
    library(lme4)
    # load the experiment data
    exp_data <- read.csv("./src/data/nursery-experiment/wood-density-final.raw.csv")
    str(exp_data)
    base_data <- read.csv("./src/data/nursery-experiment/wood-density-nursery-base.raw.csv")
    str(base_data)

    # calculate the mean base wood density per species 
    mean_base_density_data <- with(base_data, aggregate(wood_density, list(sp = sp), mean))
    names(mean_base_density_data)[2] <- "mean_density"
    base_density <- c()
    for (i in 1:length(mean_base_density_data$sp)) {
        base_density[which(exp_data$sp == mean_base_density_data$sp[i])] <- mean_base_density_data$mean_density[i]
    }
    # assign the base density to the experiments dataframe
    exp_data$base_density <- base_density
    # calculate the delta density 
    exp_data$delta_density <- exp_data$final_wood_density - exp_data$base_density
    # great what does it look like...
    str(exp_data)

    # inital model 
    model <- lmer(delta_density ~ log(treatment + 1) *
                 initial_diameter +
                 (1 | sp:mother) +
                 (1 | block),
               data = exp_data,
               control = lmerControl(optimizer = "bobyqa"),
               REML = FALSE)
    summary(model)
    model2 <- update(model, . ~ . - treatment + log(treatment + 1))
    summary(model2)
    AIC(model)
    AIC(model2) # model 2 is lower just like the final density analysis 

    #" model validation of model2 
    pearson_resid <- resid(model2, type = "pearson")
    fitted_values <- fitted(model2)
    plot(fitted_values, pearson_resid) # one outlier, i think the anlaysis is ok 

    summary(model2)
    # prediction dataframe 
    prediction_dataframe = expand.grid(
    treatment = seq(0, 21, length = 100),
    initial_diameter = mean(exp_data$initial_diameter)
  )

    # final wood density prediction.
    prediction_dataframe$delta_density <- predict(
    model2,
    prediction_dataframe, re.form = ~0
  )


    boots_dir <- "./src/analysis/nursery-experiment/predictions/" # location of predictions 
    boots_file <- paste(boots_dir, "ci-treatment-delta-density.bootstapped.R", sep = "") # bootstrap file name

    if (file.exists(boots_file)) {
        CI <- read.csv(boots_file)
        prediction_dataframe$CI025 <- unlist(CI[1,])
        prediction_dataframe$CI975 <- unlist(CI[2,])
    } else {
        source("./src/utils/booter.R")
        CI <- booter(model = model2, data = exp_data, preds = prediction_dataframe, n = 5000, MEM = TRUE)
        prediction_dataframe$CI025 <- CI[1,]
        prediction_dataframe$CI975 <- CI[2,]
        write.csv(CI, file = boots_file, row.names = FALSE)
    }

    coef_boots_file <- paste(boots_dir, "coef-ci-delta-density.bootstrapped.R", sep = "")
    coefs <- as.data.frame(summary(model2)$coef)
    if (file.exists(coef_boots_file)) {
        CI_coefs <- read.csv(coef_boots_file) # load the CIs for the coefs
        coefs$CI025 <- unlist(CI_coefs[1,]) # asign to preds 
        coefs$CI975 <- unlist(CI_coefs[2,])

    } else {
        source("./src/utils/booter.R")
        # calculate the CIs for the coefs if there is no file present
        CI_coefs <- booter(model = model2, data = exp_data, preds = prediction_dataframe, n = 5000, MEM = TRUE, coef = TRUE)
        coefs$CI025 <- unlist(CI_coefs[1,])
        coefs$CI975 <- unlist(CI_coefs[2,])
        write.csv(CI_coefs, file = coef_boots_file, row.names = FALSE)
    }

    # return the model, preds, with confidence intervals and coefs all boostrapped
    return(list(mode = model2, preds_treat = prediction_dataframe, coef = coef))


})()