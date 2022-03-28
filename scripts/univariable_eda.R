
# libraries ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(corrplot)

source("scripts/eda_helper.R", local = TRUE, encoding = "UTF-8")


# collect variables -------------------------------------------------------

## Import dataset

data <- 
  read_delim("data/Complexo_Bushveld.csv", 
             delim = ";", 
             escape_double = FALSE, 
             trim_ws = TRUE)

2## Get individual features names and type

features_character <- 
  data %>%
  dplyr::select(where(is.character), -Date, -ProjectCode, -BH_ID, -Motherhole) %>% 
  names() %>% 
  set_names()

features_continuous <- 
  data %>%
  dplyr::select(-where(is.character), -Filter) %>% 
  names() %>% 
  set_names()



# run eda scripts ---------------------------------------------------------

eda_categorical <- map(features_character, ~eda_categorical(data, .x))
eda_continuous <- map(features_continuous, ~eda_continuous(data, .x))

eda_map <- c(eda_categorical, eda_continuous)

rm(eda_categorical, eda_continuous)




# Correlation plot --------------------------------------------------------

# corr matrix

corr_matrix <- 
  cor(
    dplyr::select( data, - ProjectCode, - BH_ID, - Motherhole ,- Date, - Stratigraphy, - HoleType ),
    method = c("pearson"),
    use = "complete.obs"
    )

