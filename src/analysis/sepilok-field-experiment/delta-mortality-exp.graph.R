# the differeances
#graphing the seedling mortality data 
rm(list = ls())
# import bootstrapped data 

booted_seedling_mortality <- read.table('./src/analysis/sepilok-field-experiment/predictions/delta-mortality.bootstrapped.R', header = TRUE)
# import model
mort_analysis <- source('./src/analysis/sepilok-field-experiment/field-exp-mortality.analysis.R')$value 

# colors 
source('./colors.R')
# prediction data.frame 
pelev_data <- read.table("./data/pelev_data.txt", header = TRUE)


mortality <- predict(mort_analysis$model, preds, type = "response", re.form = NA)

require(ggplot2)
pred_diff <- preds[1:16,]
pred_diff$diff_mort <- mortality[17:32] - mortality[1:16] 
pred_diff$CI025 <- as.numeric(booted_seedling_mortality[1, 33:48])
pred_diff$CI975 <- as.numeric(booted_seedling_mortality[2, 33:48])
pred_diff$pe <- pelev_data$pe
pred_diff$`Different from Zero` <- rep("diff", 16)
pred_diff$`Different from Zero`[which(pred_diff$CI025 < 0)] <- 'no diff'
pred_diff$Wooddensity <- read.table('./data/dden_adult.txt', header = TRUE)$dden_adult

write.table(pred_diff, file = './analysis/inundation_predicts_species_distributions/data/riskratio.txt')

p1 <- ggplot(pred_diff, aes(x = reorder(sp, pe), y = diff_mort, color = `Different from Zero`)) + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.5, alpha = 0.5, size = 1)  + 
  theme_bw() + 
  xlab("Species") + 
  ylab("Risk ratio (Mortality)") + 
  theme(axis.text.x = element_text(face = "italic", angle = 45, vjust = .7)) + 
  scale_color_manual(values = c("black", cols[4])) + 
  geom_point(size = 3, pch = 21, fill = 'white') + 
  geom_hline(aes(yintercept = 0), linetype = 2, col = cols[5]) + 
  theme(legend.position = c(0.15, 0.8)) + 
  theme(text = element_text(size=20))


p1



