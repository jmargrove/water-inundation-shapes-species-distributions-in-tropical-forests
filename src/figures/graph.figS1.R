
# file to construct the final graph a
plot <- source('./src/analysis/sepilok-field-experiment/delta-mortality-exp.graph.R')$value 

# dimentions of a4 
dims <- (209.9 - (25.4 * 2)) # full width of A4 * 2
# save the panel plot for fig 3
ggsave(plot$plot, file ='./src/figures/graph.figS1.png', width = dims, height = dims / 2, units = "mm")
