# ============================================================================ #
# Script Name : theme_functions
# Purpose     : Common theme functions to create reports
# Args        :
# Date        : Mon Jan 24 13:48:11 2022
# Author      : Pedro Magalhães
# Email       : pedro.magalhaes@mosaic.pt
# ============================================================================ #


# libraries --------------------------------------------------------------

library(ggplot2)
library(extrafont)
library(showtext)

#font_import()
#loadfonts(device = "win", quiet = TRUE)

#font_add_google("Open Sans", "Roboto")


# Plot themes -------------------------------------------------------------
#' ggplot theme
#'
#' @return A theme to be used with ggplot for consistency
#' @export
#'
#' @examples
#'
theme_master <- function() {
  
  theme_bw() %+replace%
    theme(
      text =  element_text(size = 10),
      plot.title = element_text(
        size = 12,
        lineheight = 5,
        face = "bold",
        hjust = 0
      ),
      plot.subtitle = element_text(
        hjust = 0
      ) ,
      plot.caption = element_text(
        hjust = 0,
        face = "italic",
        vjust = 1
      ),
      plot.caption.position = "plot",
      panel.background = element_rect(fill = "white", colour = "grey50"),
    )
  
}




# Font color --------------------------------------------------------------
#' Set color text based on output
#' `r colorize("some words in red", "red")` inline input
#'
#' @param x
#' @param color
#'
#' @return NA
#' @export
#'
#' @examples
#'
colorize <- function(x, color) {
  
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
            x)
  } else
    x
  
}