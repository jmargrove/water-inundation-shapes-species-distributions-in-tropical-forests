rm(list = ls())
(function(){
  # import the model, predictions and the data
  result <- source("./src/analysis/elevation-predicted-by-density+inundation/elevation-density-inundation.analysis.R")$value 
  # calculating the partial residuals 
  result$data$partials_density <- residuals(result$model, type = "partial")[, 1] + mean(result$data$elevation)
  # lines to join the partials 
  partial_lines_data <- data.frame(x = rep(result$data$density, 2), 
                                   y = c(result$data$elevation, result$data$partials_density))
  
  # ANOVA type II to compare the variance 
  var_res_dd <- car::Anova(lm(elevation ~ density + riskratio, result$data))[,1]
  density_explaied_variation <- paste('ANOVA:', round(var_res_dd / sum(var_res_dd) * 100, 1)[1], "%")
  
  # jittering the species names 
  vj <- rep(2.5, 16)
  vj[which(result$data$sp == 'Spar')] <- -5
  vj[which(result$data$sp == 'Smec')] <- -7
  vj[which(result$data$sp == 'Sfal')] <- 5
  vj[which(result$data$sp == 'Sxan')] <- 22
  vj[which(result$data$sp == 'Swal')] <- -5
  
  #horazontal 
  hj <- rep(0.01, 16)
  hj[which(result$data$sp == 'Spar')] <- -0.025
  hj[which(result$data$sp == 'Smec')] <- -0.02
  hj[which(result$data$sp == 'Slep')] <- 0.02
  hj[which(result$data$sp == 'Pmal')] <- -0.02
  hj[which(result$data$sp == 'Sacu')] <- -0.02
  hj[which(result$data$sp == 'Sgib')] <- -0.02
  hj[which(result$data$sp == 'Sbec')] <- 0.02
  hj[which(result$data$sp == 'Spau')] <- 0.02
  hj[which(result$data$sp == 'Sfal')] <- -0.02
  hj[which(result$data$sp == 'Dry')] <- 0.02
  hj[which(result$data$sp == 'Ssem')] <- -0.025
  hj[which(result$data$sp == 'Ptom')] <- -0.025
  
  # plotting the data 
  themed <- source('./src/utils/theme.R')$value
  p1_wooddensity <- ggplot(result$preds_density, aes(x = density, y = elevation)) + 
    geom_line(data = partial_lines_data, aes(x = x, y = y, group = factor(x)), alpha = 0.3) + 
    geom_line() + 
    geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.2) + 
    geom_point(data = result$data, aes(y = partials_density, x = density),
               color = themed$selectBlack(), 
               pch = 21, 
               fill = themed$selectLightGrey(), size = 3)  +
    theme_bw() +
    geom_point(data = result$data, aes(y = elevation, x = density), color = themed$selectRed()) + 
    geom_text(aes(x = 0.4, y = 120, label = density_explaied_variation), size = 5) + 
    ylab('E(elevation) m asl') + 
    xlab(expression("Adult wood density"~g~cm^-3)) + 
    stat_smooth(data = result$data, aes(x = density, y = elevation), 
                se = F, method = 'lm', color = themed$selectRed(), 
                linetype = 2, size = 0.5) + 
    theme(text = element_text(size = 20)) + 
    geom_text(data = result$data, aes(label = sp), 
              size = 5,
              nudge_y = vj, 
              nudge_x = hj, 
              fontface = 'italic')
  
  # returning the wood density graph
  return(p1_wooddensity)
})()