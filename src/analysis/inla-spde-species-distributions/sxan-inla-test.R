# Testing the inla-spde models for focalSpecies distributions. 
library(future)

data <- read.csv('./src/data/sepilok-160ha-plot.csv')

# Take one species 
speciesWithMinNumber <- which(tapply(data$species, data$species, length) > 30)
df <- data.frame(data[0,], focalSpecies = data[0, 1])

names(speciesWithMinNumber)

for (species in names(speciesWithMinNumber)) {
    restrictedDf <- data[, c("sp", "species", "longitude", "latitude", "elevation")]
    restrictedDf$occurance <- rep(0, length(restrictedDf$sp))
    restrictedDf$occurance[which(restrictedDf$species == species)] <- 1
    restrictedDf$focalSpecies <- rep(species, length(restrictedDf$species))
    df <- rbind(df, restrictedDf)
}

str(df)
head(df)
# Construct an inla 
library(INLA)

coords <- with(df, cbind(longitude, latitude))
mesh <- inla.mesh.2d(coords, max.edge = c(300, 500),
                     cutoff = 20,
                     offset = c(200, 400))
# priors 
rho0 <- 20
sig0 <- 0.5


# spde
spde <- inla.spde2.pcmatern(mesh,
                            alpha = 2,
                            prior.range = c(rho0, 0.01),
                            prior.sigma = c(sig0, 0.01))

# A
A <- inla.spde.make.A(mesh, loc = as.matrix(coords))

# stk 
stk <- inla.stack(tag = "stk",
                  data = list(occurance = df$occurance),
                  A = list(A, 1),
                  effects = list(list(i = 1:spde$n.spde),
                                 data.frame(int = 1,
                                            elevation = df$elevation,
                                            focalSpecies = df$focalSpecies)))

# formula 
formula1 <- occurance ~ 0 + int + focalSpecies * elevation + I(elevation ^ 2) + f(i, model = spde)

model1 <- inla(formula1,
               data = inla.stack.data(stk),
               control.predictor = list(A = inla.stack.A(stk)),
               control.fixed = list(expand.factor.strategy = "inla"),
               family = "binomial")

summary(model1)

