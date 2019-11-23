# A function to handle the setting of working dirs 
source('./utils/resetWd.R')
handleWd <- function(localDir = '') {
  resetWd()
  firstChar <- substr(localDir, 1, 1)
  if(firstChar != '/'){
    warning("There is no forward slash!")
    stop()
  }
  wd <- getwd()
  thisDir = paste(wd, localDir, sep = "")
  setwd(thisDir)
}
