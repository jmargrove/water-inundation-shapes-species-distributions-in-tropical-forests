# lm / generalize least squares analysis of the wood density elevation data 
# module that does the generalize least squares analysis
(function() {
    data <- source("./src/data/wood-density-elevation-distribution/convert-density-points-matrix.dataframe.R")$value
    # step one. Simple linear model 
    model1 <- lm(elevation_mean ~ density_mean, data = data)
    # plot(model1, which= 1) # check residuals - easy to see that variance is not equal 

    # model2 modeling the unequal residuals.
    library(nlme)
    # create a variable for the groupings of elevation patches 
    desired_number_observations <- round(dim(data)[1] / 10, 0) # partition observations for varIdent
    library(Hmisc) # cutting
    data$fd <- cut2(data$density_mean, m = desired_number_observations) # cut

    # Model the residuals 
    model2 <- gls(elevation_mean ~ density_mean, data = data,
                weights = varIdent(form = ~1 | fd))

    summary(model2) # summary 
    d_seq <- with(data, seq(min(density_mean), max(density_mean), length = 100))
    gls_preds <- data.frame(density_mean = d_seq)
    gls_preds$elevation_mean <- predict(model2, newdata = gls_preds, type = "response")

    # bootstrap the model
    # n = 5000  #number of bootstrapping samples, if n is not defined 10 rounds will be done. See "ternary" below
    # Bootstrapping the predictions for confidence intervals around taus
    bootstrap_file_gls = "./src/analysis/wood-density-elevation-distribution/bootstrapped/"
    bootstrap_file_gls_lines <- paste(bootstrap_file_gls, "gls-ci-wood-density-elevation-distribution.csv", sep = "")

    if (file.exists(bootstrap_file_gls_lines)) {

        CI <- read.csv(bootstrap_file_gls_lines)
        gls_preds$CI025 <- unlist(CI[1,], use.names = FALSE)
        gls_preds$CI975 <- unlist(CI[2,], use.names = FALSE)

    } else {

        source("./src/utils/booter.R")
        CI <- booter(model2,
                 data = data,
                 preds = gls_preds,
                 n = 5000)
        write.csv(CI, file = bootstrap_file_gls_lines, row.names = F)
        gls_preds$CI025 <- CI[1,] # assign the cis 
        gls_preds$CI975 <- CI[2,]

    }


    # calculating the gls coef cis with bootstrap
    # Bootstrapping the coefs for confidence intervals 
    pred_coef <- data.frame(coef = names(coef(model2)),
                          value = as.vector(coef(model2)))

    bootstrap_file_coefs <- paste(bootstrap_file_gls, "gls-coef-wood-density-elevation-distribution.csv", sep = "")
    if (file.exists(bootstrap_file_coefs)) {
        CI_coefs <- read.csv(bootstrap_file_coefs)
        pred_coef$CI025 <- unlist(CI_coefs[1,], use.names = FALSE)
        pred_coef$CI975 <- unlist(CI_coefs[2,], use.names = FALSE)
    } else {
        source("./src/utils/booter.R")
        CI_coef <- booter(model2, data = data, preds = qr_pred, quantreg = TRUE, n = 5000, coef = TRUE)
        pred_coef$CI025 <- CI_coef[1,]
        pred_coef$CI975 <- CI_coef[2,]
        write.csv(CI_coef, file = bootstrap_file_coefs, row.names = F)
    }


    return(
        list(preds = gls_preds,
              model = model2
            )
        )
})()
