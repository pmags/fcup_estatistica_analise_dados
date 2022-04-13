
# ======================================================================================= #
# Script Name : pca_analysis                                                                                           
# Purpose     : pca analysis script                                                                     
# Args        : 
# Date        : Wed Apr 13 12:00:43 2022   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ======================================================================================= #


# libraries ---------------------------------------------------------------

library(tidyverse)
library(FactoMineR)

## Import dataset

data <- 
  readr::read_delim("data/Complexo_Bushveld.csv", 
                    delim = ";", 
                    escape_double = FALSE, 
                    trim_ws = TRUE)

## Transforming MaxDepth in categorical
data$MaxDepth <- cut(data$MaxDepth, breaks = c(-Inf,200, 400,Inf), right=FALSE, labels=c("Low", "Middle", "Depth"))
#data$MaxDepth <- as.factor(data$MaxDepth)

# Removing date column and converting character to factors

data <- data %>% 
  dplyr::mutate(
    HoleType = as.factor(HoleType),
    MaxDepth = as.factor(MaxDepth),
    Motherhole = as.factor(Motherhole),
    Stratigraphy = as.factor(Stratigraphy)
  ) %>% 
  dplyr::select(-Date)


