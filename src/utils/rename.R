# function to rename colnames 
rename <- function(data, old, new){
  newData <- data
  index <- which(names(newData) == old)
  names(newData)[index] <- new
  return(newData)
}