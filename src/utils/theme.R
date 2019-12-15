#install.packages("extrafont");
library(extrafont)


Theme = R6Class('Theme',
  public = list(
    colors = list(
      RED = 'red', 
      BLACK = 'black', 
      WHITE = 'white'
    ), 
    selectRed = function(){
      return(self$colors$RED)
    }, 
    selectBlack = function(){
      return(self$colors$BLACK)
    },
    selectWhite = function(){
      return(self$colors$WHITE)
    }
  )
)


gErrorBars = function( ymin, ymax){
  return(
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2 ) 
  )
  
}

t = Theme$new()
t$selectBlack()


plotTheme <- function(){
  return(
    theme_bw() +
      theme(text=element_text(family="Times"))
  )
}

