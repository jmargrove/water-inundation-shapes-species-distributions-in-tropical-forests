# Combining gls result and quantile regression into one figure 
# Clear working space 
rm(list = ls())

data <- source("./src/data/wood-density-elevation-distribution/convert-density-points-matrix.dataframe.R")$value

# model the quantile regression 
library(quantreg)
taus <- c(0.025, 0.1, 0.5, 0.9, 0.975)
quantModelLog <- rq(elevation_mean ~ density_mean, data = data, tau = taus)
qr_pred <- data.frame(density_mean = seq(min(data$density_mean), max(data$density_mean), length = 100))
qr_preds <- expand.grid(density_mean = qr_pred$density_mean, Quantile = as.factor(taus))
str(qr_preds)
qr_preds$elevation_mean <- as.vector(predict(quantModelLog, qr_pred, type = "response"))

# gls analysis 
# step one. Simple linear model 
model1 <- lm(elevation_mean ~ density_mean, data = data)
# plot(model1, which= 1) # check residuals - easy to see that variance is not equal 

# model2 modeling the unequal residuals.
library(nlme)
# create a variable for the groupings of elevation patches 
desired_number_observations <- round(dim(data)[1] / 10, 0) # partition observations for varIdent
library(Hmisc) # cutting
data$fd <- cut2(data$density_mean, m = desired_number_observations) # cut

# Model the residuals 
model2 <- gls(elevation_mean ~ density_mean, data = data, 
              weights = varIdent(form = ~1 | fd))

summary(model2) # summary 
d_seq <- with(data, seq(min(density_mean), max(density_mean), length = 100))
gls_preds <- data.frame(density_mean = d_seq)
gls_preds$elevation_mean <- predict(model2, newdata = gls_preds, type = "response")

# bootstrap the model
# n = 5000  #number of bootstrapping samples, if n is not defined 10 rounds will be done. See 'ternary' below
n = F
source("./src/utils/booter.R")
CI <- booter(model2,
                data = data,
                preds = gls_preds,
                n = if(n) n else 10)

gls_preds$CI025 <- CI[1,]
gls_preds$CI975 <- CI[2,]

qr_preds$Quantile <- as.factor(qr_preds$Quantile)
str(qr_preds)

# colors 
cols <- c("#8CB369", "#F4E285", "#4C8577","#F4A259", "#BC4B51")

# ggplot of the results 
p1 <- ggplot(gls_preds, aes(x = density_mean, y = elevation_mean)) + 
  geom_point(data = data, aes(x = density_mean, y = elevation_mean), alpha = 0.5) + 
  geom_line() + 
  theme_bw() +
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 0.5) + 
  ylab("Elevation (m asl)") + 
  xlab(bquote("Adult wood density g" ~cm^-3 )) +
  theme(legend.position = "top") + 
  scale_fill_manual(values = cols) + 
  xlim(NA, 0.62) +
  ylim(NA, 140) +
  theme(text = element_text(size=20)) + 
  geom_line(data = qr_preds[qr_preds$Quantile != '0.5',], # except the median 
            aes(x = density_mean, y = elevation_mean, color = Quantile), linetype = 2, size = 1.5) + 
  scale_color_manual(values = cols)  # color the lines


p1
