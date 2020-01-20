(function(){
  library(ggplot2)
  
  themed <- source('./src/utils/theme.R')$value
  
  fig3_theme <- list(t = theme(text = element_text(size = 12)) + 
    theme_bw() +
    theme(plot.margin = unit(c(2, 2, 0, 0), units = "mm"), 
          axis.text.x = element_text(vjust = 0.5)), # superscript causing paine
    
    # additional params 
    anova_text = 3, 
    species_names_size = 2.75, 
    raw_data_points_size = 1, 
    partial_points_size = 2,
    ribbon_alpha = 0.4, 
    ribbon_color = themed$selectMedGrey()
    ) # spaces out the plots 
  
  
  
  return(fig3_theme)
})()

