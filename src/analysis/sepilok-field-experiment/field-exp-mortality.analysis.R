#" Seedling Mortality Model Selection and Evaluation module 
# Clear workspace 
(function() {
    data <- read.csv("./src/data/sepilok-field-experiment/seedling-survival-field-experiment.raw.csv")
    # file location 
    model_file <- "./src/analysis/sepilok-field-experiment/best-model.R"
    if (file.exists(model_file)) {
        load(model_file)
        preds <- expand.grid(dia = mean(data$diameter, na.rm = T),
                           ztopo = 0,
                           f.time = "3",
                           sp = levels(data$sp),
                           flood = levels(data$flood))

        preds$mortality <- predict(r3, preds, type = "response", re.form = NA)

        # boot strapped data 
        boots <- read.csv("./src/analysis/sepilok-field-experiment/predictions/mortality-interaction.bootstrapped.csv")
        CI <- apply(boots, 1, quantile, c(0.025, 0.975))
        preds$CI025 <- CI[1,]
        preds$CI975 <- CI[2,]
        return(list(model = r3, preds = preds))
    } else {
        # function to standardize variables 
        stand <- function(data, var) {
            (data[, var] - mean(data[, var], na.rm = T)) / sd(data[, var], na.rm = T)
        }

        # import packages 
        library(lme4)
        source(system.file("utils", "allFit.R", package = "lme4"))

        #Modeling
        #Standardize all the variables for analysis: micro_topography, light, sand, 
        data$Smicro_topography <- stand(data = data, var = "micro_topography")
        data$Slight <- stand(data = data, var = "light")
        data$Ssand <- stand(data = data, var = "sand")
        data$f.time <- factor(data$time)
        data$mortality <- (data$survival - 1) * -1 # create the mortality variable from survival data

        # Random effects
        #r1
        #Random effects are mother nested within sp, and plot (wl) nested within block. 
        #The fixed effects specified are those that we a) hypothesized to have an effect, 
        #and from data exploration seed to have an effect.
        library("optimx")
        r1 <- glmer(mortality ~ log(diameter) + Smicro_topography + f.time + flood + sp + sp:flood +
                (1 | mother) + (f.time | block:plot),
              family = binomial, data = data,
              control = glmerControl(optimizer = "nlminbw"))

        r2
        #r2
        #remove mother as a random effect. 
        r2 <- update(r1, . ~ . - (1 | mother))
        dAIC(r1, r2)
        AIC(r1, r2)

        #Using mother as a added effect does not help. 
        #r3
        #remove the nexted effect of blockL/wl and replace with the plot effect 
        r3 <- glmer(mortality ~ log(diameter) + Smicro_topography + f.time + flood + sp + sp:flood + (f.time | blockL:wl),
              family = binomial, data = data, control = glmerControl(optimizer = "nlminbw"))
        dAIC(r2, r3)
        AIC(r2, r3)

        #Block is not needed. R3 is the best 
        #Fixed Effects Structure
        #delta AIC must be more than 4AIC points for us to consider the more complex model better. 

        m1 <- update(r3, . ~ . - log(diameter) + diameter)
        dAIC(r3, m1)
        AIC(r3, m1)
        #NOTE:r3 is the best model still 

        #Does the Are there any extra interactions with Diameter 
        m2 <- update(r3, . ~ . + log(diameter):f.time)
        m3 <- update(r3, . ~ . + log(diameter):flood)
        m4 <- update(r3, . ~ . + log(diameter):Smicro_topography)
        m5 <- update(r3, . ~ . + log(diameter):sp)
        dAIC(m2, r3)
        AIC(r3, m2, m3, m4, m5)

        #NOTE:Reduces the model only by 2.07, hence stick with the original 
        m6 <- update(m1, . ~ . + diameter:f.time)
        m7 <- update(m1, . ~ . + diameter:flood)
        m8 <- update(m1, . ~ . + diameter:Smicro_topography)
        m9 <- update(m1, . ~ . + diameter:sp)
        AIC(r3, m1, m6, m7, m8, m9)

        summary(m3)
        dAIC(m3, r3)
        AIC(m3, r3)

        # AIC
        summary(m3)
        dAIC(m3, r3)
        AIC(m3, r3)

        #Light
        m4 <- update(r3, . ~ . + Slight)
        m5 <- update(r3, . ~ . + Slight + Slight:Smicro_topography)
        m6 <- update(r3, . ~ . + Slight + Slight:flood)
        AIC(r3, m3, m4, m5)

        #Sand
        m7 <- update(r3, . ~ . + Ssand)
        m8 <- update(r3, . ~ . + Ssand:Smicro_topography)
        m9 <- update(r3, . ~ . + Ssand:flood)
        AIC(r3, m7, m8, m9)

        #Site
        m10 <- update(r3, . ~ . + site)
        m11 <- update(r3, . ~ . + site:Smicro_topography)
        m12 <- update(r3, . ~ . + site:flood)
        AIC(r3, m10, m11, m12)

        # Other remaining model mixtures  
        m13 <- update(r3, . ~ . - flood:sp) # flooding should be kept in the model. 
        AIC(m13, r3)
        dAIC(m13, r3) # rounded it is dAIC=5, keep this in 

        require(MuMIn)
        NARM_data <- data[!is.na(data$diameter),] # removes rows with NAs in diameter 
        NARM_data <- NARM_data[!is.na(NARM_data$mortality),] # removes rows with NAs in diameter 
        r4 <- update(r3, . ~ ., data = NARM_data, na.action = "na.fail")
        dr4 <- dredge(r4, rank = "AIC", trace = TRUE) # dredge the rest to see what happens
        dr4

        r3 <- glmer(mortality ~ log(diameter) + micro_topography + f.time + flood + sp + sp:flood + (f.time | blockL:wl),
              family = binomial,
              data = data)

        summary(r3)
        save(r3, file = "./src/analysis/sepilok-field-experiment/best-model.R")
        return(list(model = r3))
    }
})()
