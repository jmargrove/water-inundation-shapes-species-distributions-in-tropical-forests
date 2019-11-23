ls(list = ls())
wd_database <- read.csv('./data/wood-density-database.csv')
table <-  read.csv('./summary-tables/species-list-plot.table.csv')

# Creating empty variables 
density <- c()
sd <- c()
n <- c()

# values from the wood density database passed here...
for(i in 1:length(table$species)){
  index <- which(wd_database$species == as.character(table$species)[i])
  density[i] <- round(mean(wd_database$wood_density[index]), 2)
  sd[i] <- round(sd(wd_database$wood_density[index]), 2)
  count <- length(wd_database$wood_density[index])
  if(count == 0){
  n[i] <- 
    n[i] <- NA
  } else {
    n[i] <- count
  } 
}

table$density <- density
table$sd <- sd
table$n <- n

write.csv(table, file = "./summary-tables/sepilok-wood-density.table.csv", row.names = FALSE)


