# quantile regression of wood density elevation analysis 
# module provides the predictions
(function() {
    data <- source("./src/data/wood-density-elevation-distribution/convert-density-points-matrix.dataframe.R")$value

    # model the quantile regression 
    library(quantreg)
    taus <- c(0.025, 0.1, 0.5, 0.9, 0.975)
    quant_model <- rq(elevation_mean ~ density_mean, data = data, tau = taus)
    qr_pred <- data.frame(density_mean = seq(min(data$density_mean), max(data$density_mean), length = 100))
    qr_preds <- expand.grid(density_mean = qr_pred$density_mean, Quantile = as.factor(taus))
    qr_preds$elevation_mean <- as.vector(predict(quant_model, qr_pred, type = "response"))
    qr_preds$Quantile <- as.factor(qr_preds$Quantile)

    # Bootstrapping the predicitons for confidence intervals around taus
    bootstrap_file = "./src/analysis/wood-density-elevation-distribution/bootstrapped/"
    bootstrap_file_lines <- paste(bootstrap_file, "qr-ci-wood-density-elevation-distribution.csv", sep = "")
    if (file.exists(bootstrap_file_lines)) {
        CI <- read.csv(bootstrap_file_lines)
        qr_preds$CI025 <- unlist(CI[1,], use.names = FALSE)
        qr_preds$CI975 <- unlist(CI[2,], use.names = FALSE)
    } else {
        source("./src/utils/booter.R")
        CI <- booter(quant_model, data = data, preds = qr_pred, quantreg = TRUE, n = 5000)
        write.csv(CI, file = bootstrap_file_lines, row.names = F)
        qr_preds$CI025 <- CI[1,] # assign the cis 
        qr_preds$CI975 <- CI[2,]
    }

    # Bootstrapping the coefs for confidence intervals 
    pred_coef <- data.frame(coef = rownames(coef(quant_model)),
                          tau = rep(colnames(coef(quant_model)), each = 2),
                          value = as.vector(coef(quant_model)))

    bootstrap_file_coefs <- paste(bootstrap_file, "qr-coef-wood-density-elevation-distribution.csv", sep = "")
    if (file.exists(bootstrap_file_coefs)) {
        CI_coefs <- read.csv(bootstrap_file_coefs)
        pred_coef$CI025 <- unlist(CI_coefs[1,], use.names = FALSE)
        pred_coef$CI975 <- unlist(CI_coefs[2,], use.names = FALSE)
    } else {
        source("./src/utils/booter.R")
        CI_coef <- booter(quant_model, data = data, preds = qr_pred, quantreg = TRUE, n = 5000, coef = TRUE)
        pred_coef$CI025 <- CI_coef[1,]
        pred_coef$CI975 <- CI_coef[2,]
        write.csv(CI_coef, file = bootstrap_file_coefs, row.names = F)
    }

    model_res <- list(preds = qr_preds,
                    coefs = pred_coef,
                   model = quant_model)
    return(model_res)
})()
