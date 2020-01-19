#" Community elevation analysis Fig1d
rm(list = ls())
#" Loading the required packages 
library(vegan)
library(ggplot2)
library(raster)
library(Hmisc)

plot_data <- read.csv("./src/data/sepilok-plot/sepilok-160ha-plot.raw.csv")
plot_data <- subset(plot_data, sp != "Unkn" & DBH > 50) # get all individuals above 50
plot_data <- droplevels(plot_data) # dropping levels 

#Creating the factor elevation,
expected_n_bands <- 12
plot_data$felv25 <- cut2(plot_data$elev, g = expected_n_bands, m = 100)
bands <- cut2(plot_data$elev, g = expected_n_bands, m = 100, onlycuts = TRUE)
minElevation <- round(min(plot_data$elevation), 1)
maxElevation <- round(max(plot_data$elevation), 1)
elevationRange <- c(minElevation, maxElevation)
bands[c(1, length(bands))] <- elevationRange
# number of bands used
length(bands) - 1
sd(with(plot_data, tapply(sp, felv25, length)))

#Creating the matrix for the analysis 
matSp <- as.matrix(
  with(plot_data, tapply(sp, list(felv25, sp), length))
  )

matSp[is.na(matSp)] <- 0
### How many of the speceis occur at each of the top brackets 
sp_data <- read.csv("./src/data/sepilok-field-experiment/field-experiment-species-list.table.csv") # load species list
species_short <- as.character(sp_data$sp)
#sp_data$sp # get species shorthand name 
length(which(matSp[12, species_short] != 0)) # at high elevations
length(which(matSp[1, species_short] != 0)) # at the lowest elevation 

#Runing NMDS from the vegan package
m1 <- metaMDS(matSp)
md1 <- m1$points[, 1] # we want the sites 

#Import the map
r <- raster("./src/data/community-elevation-analysis/sepilok-160-dem.tiff") #This is the sepilok, map
e <- extent(603945.651, 605788.486, 647133.509, 648054.815) # extent of the 160ha forest plot
plot160c <- crop(x = r, e)


plot160c <- plot160c - min(plot160c@data@values)
plot160c <- (plot160c / max(plot160c@data@values)) * (137.9 - 55.9) + 55.9 # calibrate to dtm data used in for species.
rVals <- plot160c@data@values # the values of the raster 
comPlot <- plot160c # new file for the community plot

#Adding in the new values per band
for (i in 1:length(bands) - 1) {
    cVals <- which(rVals >= bands[i] & rVals <= bands[i + 1])
    comPlot@data@values[cVals] <- md1[i]
}

# Using ggplot raster to map the image 
# convert to ggplot format x, y, z 
ggCom <- as.data.frame(comPlot, xy = TRUE)
colnames(ggCom) <- c("x", "y", "z")
brk <- round(seq(min(comPlot@data@values), 0.97, length = 5), 2)

# Check the graphic before saving 
ggplot(ggCom) + geom_raster(aes(x, y, fill = z)) +
  scale_fill_gradient(low = "#232d29", high = "#6A9113", name = "", breaks = brk)

#Export as a raster 
writeRaster(comPlot, "./src/graphs/community-elevation-analysis.raster.tiff", format = "GTiff")



