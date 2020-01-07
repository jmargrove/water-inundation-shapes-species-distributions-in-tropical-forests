# lm / gls analysis of the wood density elevation data 
# module that does the gls analysis
(function(){
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
  # n = 5000  #number of bootstrapping samples, if n is not defined 10 rounds will be done. See 'ternary' below
  n = F
  source("./src/utils/booter.R")
  CI <- booter(model2,
               data = data,
               preds = gls_preds,
               n = if(n) n else 10)
  
  gls_preds$CI025 <- CI[1,]
  gls_preds$CI975 <- CI[2,]
  
  return(list(preds = gls_preds, 
              model = model2)
         )
})()