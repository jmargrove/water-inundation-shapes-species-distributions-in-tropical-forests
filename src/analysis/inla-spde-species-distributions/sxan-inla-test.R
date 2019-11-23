# Testing the inla-spde models for species distributions. 

data <- read.csv('./src/data/sepilok-160ha-plot.csv')
# Take one species 

df <- data.frame(data[0,])
for(species in levels(data$species)){
  restrictedDf <- data[, c("sp", "species", "longitude", "latitude", "elevation")]
  restrictedDf$occurance <- rep(0, length(restrictedDf$sp))
  restrictedDf$occurance[which(restrictedDf$species == species)] <- 1
  rbind(df, restrictedDf)
}

str(sxan)
# Construct an inla 
library(INLA)

coords <- with(sxan, cbind(longitude, latitude))
mesh <- inla.mesh.2d(coords, max.edge = c(300, 500), 
                     cutoff = 100, 
                     offset=c(200,400))
# priors 
rho0 <- 294.69
sig0 <- 1.27


# spde
spde <- inla.spde2.pcmatern(mesh, 
                            alpha = 2,
                            prior.range = c(rho0, 0.01), 
                            prior.sigma = c(sig0, 0.01))

# A
A <- inla.spde.make.A(mesh, loc=as.matrix(coords))

# stk 
stk <- inla.stack(tag = "stk", 
                  data = list(occurance = sxan$occurance), 
                  A = list(A, 1), 
                  effects = list(list(i = 1:spde$n.spde),
                                 data.frame(int = 1,
                                            elevation = sxan$elevation,
                                            species = sxan$species)))

# formula 
formula1 <- occurance ~ 0 + int + elevation + I(elevation^2) + f(i, model = spde)

# model 1 with the linear effect of elevation 
model1 <- inla(formula1, 
               data = inla.stack.data(stk),
               control.predictor = list(A = inla.stack.A(stk)),
               control.fixed = list(expand.factor.strategy = "inla"),
               family = "binomial")

summary(model1)


