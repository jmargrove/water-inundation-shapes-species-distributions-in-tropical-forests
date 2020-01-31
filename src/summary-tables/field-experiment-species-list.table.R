# Module to return the species list for the field experiment 
(function(){
  # Import raw data
  data <- read.csv('./src/data/sepilok-field-experiment/seedling-survival-field-experiment.raw.csv')
  
  # get species and sp cols and make data frame unique
  species_list <- unique(data[c("species", "sp")])
  #  Return the df 
  return(species_list)
})()