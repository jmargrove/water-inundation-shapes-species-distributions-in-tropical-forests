import("extrafont")
import("R6")

Theme <- R6Class("Theme",
  public = list(
    colors = list(
      BLACK = "black",
      WHITE = "white",
      GREEN = "#8CB369",
      YELLOW = "#F4E285",
      BLUE = "#348AA7",
      ORANGE = "#F4A259",
      RED = "#BC4B51",
      MEDGREY = "#A9A9A9",
      LIGHTGREY = "#D3D3D3",
      DARKGREY = "#696969",
      ALL = c("#8CB369", "#F4E285", "#4C8577", "#F4A259", "#BC4B51"),
      GREEN_GRAD = c("#232d29", "#6A9113")
    ),
    selectRed = function() {
      return(self$colors$RED)
    },
    selectGreenLow = function() {
      return(self$colors$GREEN_GRAD[1])
    },
    selectGreenHigh = function() {
      return(self$colors$GREEN_GRAD[2])
    },
    selectGreen = function() {
      return(self$colors$GREEN)
    },
    selectYellow = function() {
      return(self$colors$YELLOW)
    },
    selectBlue = function() {
      return(self$colors$BLUE)
    },
    selectOrange = function() {
      return(self$colors$ORANGE)
    },
    selectColorPalette = function() {
      return(self$colors$ALL)
    },
    selectBlack = function() {
      return(self$colors$BLACK)
    },
    selectMedGrey = function() {
      return(self$colors$MEDGREY)
    },
    selectLightGrey = function() {
      return(self$colors$LIGHTGREY)
    },
    selectDarkGrey = function() {
      return(self$colors$DARKGREY)
    },
    selectWhite = function() {
      return(self$colors$WHITE)
    }
  )
)

export(Theme$new())
