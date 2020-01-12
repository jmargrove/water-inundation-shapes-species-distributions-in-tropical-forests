
rm(list = ls())
theme <- source('./src/utils/theme.R')$value
result <- source("./src/analysis/elevation-predicted-by-density+inundation/elevation-density-inundation.analysis.R")$value 

# calculating the partial residuals 
result$data$partials_rr <- residuals(result$model, type = "partial")[, 2] + mean(result$data$elevation)
# lines to join the partials 
partial_lines_data <- data.frame(x = rep(result$data$riskratio, 2), 
                                 y = c(result$data$elevation, result$data$partials_rr))
# ANOVA type II to compare the variance 
var_res_rr <- car::Anova(result$model)[,1]
rr_explaied_variation <- paste('ANOVA:', round(var_res_rr / sum(var_res_rr) * 100, 1)[1], "%")

# jittering the species names 
vj_rr <- rep(2.5, 16)
vj_rr[which(result$data$sp == 'Spar')] <- -2.5
vj_rr[which(result$data$sp == 'Smac')] <- 12
vj_rr[which(result$data$sp == 'Slep')] <- -4
vj_rr[which(result$data$sp == 'Smec')] <- -3
vj_rr[which(result$data$sp == 'Pmal')] <- -4
vj_rr[which(result$data$sp == 'Sgib')] <- 8
vj_rr[which(result$data$sp == 'Ssmi')] <- -4
vj_rr[which(result$data$sp == 'Sacu')] <- -4

#horazontal 
hj_rr <- rep(0.01, 16)
hj_rr[which(result$data$sp == 'Ssmi')] <- -0.02
hj_rr[which(result$data$sp == 'Sbec')] <- -0.02
hj_rr[which(result$data$sp == 'Sxan')] <- 0.01


# plotting the data 
p1 <- ggplot(result$preds_riskratio, aes(x = riskratio, y = elevation)) + 
  geom_line(data = partial_lines_data, aes(x = x, y = y, group = factor(x)), alpha = 0.3) + 
  geom_line() + 
  geom_point(data = result$data, aes(y = elevation, x = riskratio), alpha = 0.5, color = theme$selectRed()) + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.2) + 
  theme_bw() + 
  geom_point(data = result$data, aes(y = partials_rr, x = riskratio), 
             color = theme$selectBlack(), 
             pch = 21, 
             fill = 'grey', size = 3) + 
  geom_text(aes(x = 0.12, y = 120, label = rr_explaied_variation), size = 5) + 
  ylab('E(elevation) m asl') + 
  xlab('Inundation sensitivity') + 
  # ylim(40, 125) + 
  stat_smooth(data = result$data, aes(x = riskratio, y = elevation), 
              se = F, method = 'lm', color = theme$selectRed(), 
              linetype = 2, size = 0.5) + 
  theme(text = element_text(size = 20)) + 
  geom_text(data = result$data, aes(label = sp), 
            size = 5,
            nudge_y = vj_rr, 
            nudge_x = hj_rr, 
            fontface = 'italic')
p1
