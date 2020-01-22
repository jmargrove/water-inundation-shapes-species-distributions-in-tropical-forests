

plot <- source("./src/analysis/wood-density-elevation-distribution/wood-density-elevation-prediction.graph.R")$value

# printing the coeficients 
plot$qr_analysis$coefs[c(2, 10), ]
 
# the plot 
