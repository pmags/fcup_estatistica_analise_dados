
# ======================================================================================= #
# Script Name :                                                                                            
# Purpose     : Eda helper functions                                                                     
# Args        : 
# Date        : Wed Mar 09 01:03:43 2022   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ======================================================================================= #


# Libraries ---------------------------------------------------------------

library(gridExtra)
library(grid)
library(ggplot2)


# Main theme --------------------------------------------------------------


#' ggplot theme
#'
#' @return A theme to be used with ggplot for consistency
#' @export
#'
#' @examples
#' 
theme_masterDS <- function() {
  
  theme_bw() %+replace% 
    theme(
      text =  element_text( size = 8),
      plot.title = element_text(size = 10, lineheight = .9, face = "bold"),
      plot.caption = element_text(hjust = 0.5, face = "italic"),
      plot.caption.position = "plot",
      panel.background = element_rect(fill = "white", colour = "grey50")
    )
  
}



# Continuous variable eda -------------------------------------------------



#' Continuous eda function
#'
#' @param data a dataframe containing the features under study
#' @param x a character as the column name
#' @param t a character representing the target variable 
#'
#' @return a gridExtra object with exploration information relevant for each variable
#' @export 
#'
#' @examples
#' don't run
#' grid.draw(x) #to view return object
eda_continuous <- function(data, x, t = "bought") {
  
  layout <- rbind(
    c(1,2,3),
    c(4,4,4)
  )
  
  # density plot
  density <- ggplot(data, aes(x = .data[[x]])) +
    geom_density(color="darkblue") +
    theme_masterDS() +
    labs(
      x = "",
      y = "",
      title = "Density"
    )
  
  boxplot <- ggplot(data, aes(x = 1, y = .data[[x]])) + 
    geom_boxplot() + 
    theme_masterDS()+
    labs(
      x = "",
      y="",
      title = "Boxplot"
    ) 
  
  qqplot <- ggplot(data, aes(sample = .data[[x]])) +
    stat_qq() +
    stat_qq_line() +
    theme_masterDS() +
    labs(
      x = "",
      y = "",
      title = "qq plot"
    )
  
  summary <- data %>% select(all_of(x)) %>% summary()
  
  grid <- arrangeGrob(qqplot, boxplot,tableGrob(summary), density,
                      layout_matrix = layout)  
  
  return(grid)
  
}


# Discrete variable eda ---------------------------------------------------



#' Discrete eda function
#'
#' @param data a dataframe containing the features under study
#' @param x a character as the column name
#' @param t a character representing the target variable 
#'
#' @return a gridExtra object with exploration information relevant for each variable
#' @export 
#'
#' @examples
#' don't run
#' grid.draw(x) #to view return object
eda_discrete <- function(data, x, t = "bought") {
  
  layout <- rbind(
    c(1,2,3),
    c(4,4,4),
    c(5,5,5)
  )
  
  # density plot
  histogram <- ggplot(data, aes(x = .data[[x]])) +
    geom_histogram(fill="darkblue", color="black") +
    theme_masterDS() +
    labs(
      x = "",
      y = "",
      title = "Histogram"
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  boxplot <- ggplot(data, aes(x = 1, y = .data[[x]])) +
    geom_boxplot() +
    theme_masterDS()+
    labs(
      x = "",
      y="",
      title = "Boxplot"
    )
  
  qqplot <- ggplot(data, aes(sample = .data[[x]])) +
    stat_qq() +
    stat_qq_line() +
    theme_masterDS() +
    labs(
      x = "",
      y = "",
      title = "qq plot"
    )
  
  freq_plot <- ggplot( data, aes(x = .data[[x]], y = ..density..) ) +
    geom_freqpoly( aes(color = as.factor(.data[[t]])) )
  
  summary <- data %>% select(all_of(x)) %>% summary()
  
  grid <- arrangeGrob(qqplot, boxplot,tableGrob(summary), histogram, freq_plot,
                      layout_matrix = layout)
  
  return(grid)
}



# Categorical variable eda ------------------------------------------------



#' Categorical eda function
#'
#' @param data a dataframe containing the features under study
#' @param x a character as the column name
#' @param t a character representing the target variable 
#'
#' @return a gridExtra object with exploration information relevant for each variable
#' @export 
#'
#' @examples
#' don't run
#' grid.draw(x) #to view return object
eda_categorical <- function(data, x, t = "bought") {
  
  layout <- rbind(
    c(1,2),
    c(3,3)
  )
  
  freq_plot <- ggplot(data, aes(x = .data[[x]])) +
    geom_bar(fill="darkblue", color="black") +
    theme_masterDS() +
    labs(
      x = "",
      y = "",
      title = "Frequency"
    ) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
  
  freq_plot_compare <- ggplot(data, aes( x = .data[[x]], fill = .data[[t]] )) +
    geom_bar() +
    theme_masterDS() +
    labs(
      x = "",
      y = "",
      title = "Frequency bought vs no bought"
    )
  
  
  summary <- data %>% select(all_of(x)) %>% summary()
  
  grid <- arrangeGrob(freq_plot, tableGrob(summary), freq_plot_compare,
                      layout_matrix = layout)
  
  return( grid )
  
}
