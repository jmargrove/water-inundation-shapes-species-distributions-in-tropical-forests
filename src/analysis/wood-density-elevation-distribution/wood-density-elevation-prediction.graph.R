# Combining gls result and quantile regression into one figure 
# Clear working space 
rm(list = ls())
source("./src/utils/import.R")

dir <- './src/analysis/wood-density-elevation-distribution/'

data <- import("./src/data/wood-density-elevation-distribution/convert-density-points-matrix.dataframe.R")
# model the quantile regression, with predictions
qr_analysis <- import(paste(dir, 'qr-wood-density-elevation-distribution.analysis.R', sep = ''))
# gls analysis with predictions 
gls_analysis <- import(paste(dir, 'gls-wood-density-elevation-distribution.analysis.R', sep = ''))

# colors 
theme <- import('./src/utils/theme.R')

# ggplot of the results 
p1 <- ggplot(gls_analysis$preds, aes(x = density_mean, y = elevation_mean)) + 
  geom_point(data = data, aes(x = density_mean, y = elevation_mean), alpha = 0.5) + 
  geom_line() + 
  theme_bw() +
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.5) + 
  ylab("Elevation (m asl)") + 
  xlab(bquote("Adult wood density g" ~cm^-3 )) +
  theme(legend.position = "top") + 
  scale_fill_manual(values = theme$selectColorPalette()) + 
  xlim(NA, 0.62) +
  ylim(NA, 140) +
  theme(text = element_text(size=20)) + 
  geom_line(data = qr_analysis$preds[qr_analysis$preds$Quantile != '0.5',], # except the median 
            aes(x = density_mean, y = elevation_mean, color = Quantile), linetype = 1, size = 1.5) + 
  scale_color_manual(values = theme$selectColorPalette())  # color the lines

p1


