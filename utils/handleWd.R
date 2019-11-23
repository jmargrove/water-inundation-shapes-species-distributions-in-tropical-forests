# A function to handle the setting of working dirs 
handleWd <- function(localDir = '') {
  wd <- getwd()
  thisDir = paste(wd, localDir, sep = "")
  setwd(thisDir)
}
