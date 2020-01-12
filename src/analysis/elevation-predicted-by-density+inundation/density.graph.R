
rm(list = ls())
theme <- source('./src/utils/theme.R')$value
result <- source("./src/analysis/elevation-predicted-by-density+inundation/elevation-density-inundation.analysis.R")$value 


# calculating the partial residuals 
result$data$partials_density <- residuals(result$model, type = "partial")[, 1] + mean(result$data$elevation)
# lines to join the partials 
partial_lines_data <- data.frame(x = rep(result$data$density, 2), 
                                 y = c(result$data$elevation, result$data$partials_density))
# ANOVA type II to compare the variance 
var_res_rr <- car::Anova(result$model)[,1]
rr_explaied_variation <- paste('ANOVA:', round(var_res_rr / sum(var_res_rr) * 100, 1)[2], "%")



# jittering the species names 
vj <- rep(2.5, 16)
vj[which(riskratio$sp == 'Spar')] <- -5
vj[which(riskratio$sp == 'Smec')] <- -7
vj[which(riskratio$sp == 'Sfal')] <- 5
vj[which(riskratio$sp == 'Sxan')] <- 22
vj[which(riskratio$sp == 'Swal')] <- -5

#horazontal 
hj <- rep(0.01, 16)
hj[which(riskratio$sp == 'Spar')] <- -0.025
hj[which(riskratio$sp == 'Smec')] <- -0.02
hj[which(riskratio$sp == 'Slep')] <- 0.02
hj[which(riskratio$sp == 'Pmal')] <- -0.02
hj[which(riskratio$sp == 'Sacu')] <- -0.02
hj[which(riskratio$sp == 'Sgib')] <- -0.02
hj[which(riskratio$sp == 'Sbec')] <- 0.02
hj[which(riskratio$sp == 'Spau')] <- 0.02
hj[which(riskratio$sp == 'Sfal')] <- -0.02
hj[which(riskratio$sp == 'Dry')] <- 0.02
hj[which(riskratio$sp == 'Ssem')] <- -0.025
hj[which(riskratio$sp == 'Ptom')] <- -0.025



# plotting the data 
p1_wooddensity <- ggplot(result$preds_density, aes(x = dden, y = elev)) + 
  geom_line(data = partial_lines_data, aes(x = x, y = y, group = factor(x)), alpha = 0.3) + 
  geom_line() + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.2) + 
  geom_point(data = result$data, aes(y = partials_dd, x = dden),
             color = 'black', 
             pch = 21, 
             fill = 'grey', size = 3)  +
  theme_bw() +
  geom_point(data = riskratio, aes(y = elev, x = dden), color = 'red', alpha = 0.5) + 
  geom_text(aes(x = 0.4, y = 120, label = dd_explaied_variation), size = 5) + 
  ylab('E(elevation) m asl') + 
  xlab(expression("Adult wood density"~g~cm^-3)) + 
  ylim(40, 125) + 
  stat_smooth(data = riskratio, aes(x = dden, y = elev), 
              se = F, method = 'lm', color = 'red', 
              linetype = 2, size = 0.5) + 
  theme(text = element_text(size = 20)) + 
  geom_text(data = riskratio, aes(label = sp), 
            size = 5,
            nudge_y = vj, 
            nudge_x = hj, 
            fontface = 'italic')

