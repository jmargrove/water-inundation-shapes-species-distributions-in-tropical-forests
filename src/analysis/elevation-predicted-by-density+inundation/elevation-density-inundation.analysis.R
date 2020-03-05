# Does wood density and inundation predict elevation 
(function() {
    makeDataframe <- function() {
        # Import model & predictions 
        mort_analysis <- source("./src/analysis/sepilok-field-experiment/field-exp-mortality.analysis.R")$value
        # Import the elevation data for ordering 
        pelev_data <- read.csv("./src/analysis/inla-spde-species-distributions/predictions/occurance-probabilities.dataframe.csv")
        # import the wood density data for partitioning
        plot_density_data <- read.csv("./src/data/sepilok-plot/sepilok-adult-wood-density.table.csv", header = TRUE)
        # import species data 
        species_data <- source('./src/summary-tables/field-experiment-species-list.table.R')$value
        exp_density_data <- plot_density_data[plot_density_data$sp %in% as.character(species_data$sp),]

        data <- data.frame(
          species = exp_density_data$species,
          sp = exp_density_data$sp,
          riskratio = mort_analysis$preds$mortality[17:32] - mort_analysis$preds$mortality[1:16],
          elevation = pelev_data$occurance_probability,
          density = exp_density_data$density
    )

        return(data)
    }

    # init the data frame 
    data <- makeDataframe()
    # quick plot 
    library(ggplot2)
    ggplot(data, aes(y = elevation, x = riskratio)) +
    geom_point()

    ggplot(data, aes(y = elevation, x = density)) +
    geom_point()

    # model the data 
    model <- glm(elevation ~ density * riskratio, data, family = "gaussian")
    model2 <- update(model, . ~ . - density:riskratio)
    model3 <- update(model2, . ~ . - density)
    model4 <- update(model2, . ~ . - riskratio)
    AIC(model, model2, model3, model4)

    # Model 2 has fewer parameters with the lowest AIC 
    par(mfrow = c(2, 2))
    # plot(model2) # checking the residuals 
    par(mfrow = c(1, 1)) # reset the plots 
    # residuals are fine, now too make the predictions 

    range_riskratio <- range(data$riskratio)
    preds_riskratio <- expand.grid(riskratio = seq(range_riskratio[1], range_riskratio[2], length = 100),
                                 density = mean(data$density))

    tvalue <- qt(0.975, df = model2$df.residual)
    preds_riskratio$elevation <- predict(model2, preds_riskratio, type = "response")
    preds_riskratio$CI025 <- preds_riskratio$elev + predict(model2,
                                                          newdata = preds_riskratio,
                                                          type = "response",
                                                          se.fit = TRUE)$se.fit * -tvalue

    preds_riskratio$CI975 <- preds_riskratio$elev + predict(model2,
                                                          newdata = preds_riskratio,
                                                          type = "response",
                                                          se.fit = TRUE)$se.fit * tvalue

    # predictions for the density 
    range_density <- range(data$density)
    preds_density <- expand.grid(density = seq(range_density[1], range_density[2], length = 100),
                              riskratio = mean(data$riskratio))

    tvalue <- qt(0.975, df = model2$df.residual)
    preds_density$elevation <- predict(model2, preds_density, type = "response")
    preds_density$CI025 <- preds_density$elev + predict(model2,
                                                          newdata = preds_density,
                                                          type = "response",
                                                          se.fit = TRUE)$se.fit * -tvalue

    preds_density$CI975 <- preds_density$elev + predict(model2,
                                                          newdata = preds_density,
                                                          type = "response",
                                                          se.fit = TRUE)$se.fit * tvalue

    names <- c("density", "riskratio")
    coefs <- coef(model2)[names]
    density_CI <- summary(model2)$coef["density", "Std. Error"] * c(-tvalue, tvalue) + coefs["density"]
    riskratio_CI <- summary(model2)$coef["riskratio", "Std. Error"] * c(-tvalue, tvalue) + coefs["riskratio"]
    CI025 <- c(density_CI[1], riskratio_CI[1])
    CI975 <- c(density_CI[2], riskratio_CI[2])
    model_coefs <- data.frame(names, coefs, CI025, CI975)

    return(
    # return the predictions, the model and the coefficients
      list(
        model = model2,
        data = data,
        preds_density = preds_density,
        preds_riskratio = preds_riskratio,
        coefs = model_coefs
      )
   )
})()



