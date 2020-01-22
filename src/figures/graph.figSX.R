

plot <- source("./src/analysis/wood-density-elevation-distribution/wood-density-elevation-prediction.graph.R")$value

# printing the coeficients 
plot$qr_analysis$coefs[c(2, 10), ]
 
# the plot 
# save the panel plot for fig 3
ggsave(plot$plot, file ='./src/figures/graph.figS2.png', width = dims, height = dims / 2, units = "mm")

