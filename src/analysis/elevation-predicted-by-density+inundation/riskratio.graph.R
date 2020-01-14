(function(){
  # import the the model, data, and preds module
  analysis <- source("./src/analysis/elevation-predicted-by-density+inundation/elevation-density-inundation.analysis.R")$value 
  # import the theme
  themed <- source('./src/utils/theme.R')$value 
  
  # calculating the partial residuals 
  analysis$data$partials_rr <- residuals(analysis$model, type = "partial")[, 2] + mean(analysis$data$elevation)
  # lines to join the partials 
  partial_lines_data <- data.frame(x = rep(analysis$data$riskratio, 2), 
                                   y = c(analysis$data$elevation, analysis$data$partials_rr))
  # ANOVA type II to compare the variance 
  var_res_rr <- car::Anova(lm(elevation ~ density + riskratio, analysis$data))[,1]
  rr_explaied_variation <- paste('ANOVA:', round(var_res_rr / sum(var_res_rr) * 100, 1)[2], "%")
  
  # positioning needs to be better
  # jittering the species names 
  vj_rr <- rep(2.5, 16)
  vj_rr[which(analysis$data$sp == 'Spar')] <- -2.5
  vj_rr[which(analysis$data$sp == 'Smac')] <- 12
  vj_rr[which(analysis$data$sp == 'Slep')] <- -4
  vj_rr[which(analysis$data$sp == 'Smec')] <- -3
  vj_rr[which(analysis$data$sp == 'Pmal')] <- -4
  vj_rr[which(analysis$data$sp == 'Sgib')] <- 8
  vj_rr[which(analysis$data$sp == 'Ssmi')] <- -4
  vj_rr[which(analysis$data$sp == 'Sacu')] <- -4
  
  #horazontal 
  hj_rr <- rep(0.01, 16)
  hj_rr[which(analysis$data$sp == 'Ssmi')] <- -0.02
  hj_rr[which(analysis$data$sp == 'Sbec')] <- -0.02
  hj_rr[which(analysis$data$sp == 'Sxan')] <- 0.01
  
  
  # plotting the data 
  p1 <- ggplot(analysis$preds_riskratio, aes(x = riskratio, y = elevation)) + 
    geom_line(data = partial_lines_data, aes(x = x, y = y, group = factor(x)), alpha = 0.5) + 
    geom_line() + 
    geom_point(data = analysis$data, aes(y = elevation, x = riskratio), color = themed$selectRed()) + 
    geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.2) + 
    theme_bw() + 
    geom_point(data = analysis$data, aes(y = partials_rr, x = riskratio), 
               color = themed$selectBlack(), 
               pch = 21, 
               fill = themed$selectLightGrey(), size = 3) + 
    geom_text(aes(x = 0.12, y = 120, label = rr_explaied_variation), size = 5) + 
    ylab('E(elevation) m asl') + 
    xlab('Inundation sensitivity') + 
    stat_smooth(data = analysis$data, aes(x = riskratio, y = elevation), 
                se = F, method = 'lm', color = themed$selectRed(), 
                linetype = 2, size = 0.5) + 
    theme(text = element_text(size = 20)) + 
    geom_text(data = analysis$data, aes(label = sp), 
              size = 5,
              nudge_y = vj_rr, 
              nudge_x = hj_rr, 
              fontface = 'italic')
  
  # return the graph
  return(list(plot = p1, analysis = analysis))
})()