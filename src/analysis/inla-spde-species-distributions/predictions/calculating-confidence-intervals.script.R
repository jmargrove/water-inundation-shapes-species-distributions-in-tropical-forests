# Calculating the confidence intervals from the posterior sampling in INLA SPDE
rm(list = ls()) # clear the work space 
prediction_wd <- "./src/analysis/inla-spde-species-distributions/predictions/"
post_sample <- read.table(paste(prediction_wd,"post.samp5000.txt", sep = ""), header = TRUE)
# Calculate the Confidence intervals
CI <- apply(as.matrix(post_sample), 1, quantile, c(0.025, 0.975))
save(CI, file = paste(prediction_wd, "prediction-curves-confidence-intervals.R", sep = ""))

