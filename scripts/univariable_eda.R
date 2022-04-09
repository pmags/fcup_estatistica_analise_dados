
# libraries ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(corrplot)

source("./scripts/eda_helper.R", local = TRUE, encoding = "UTF-8")


# collect variables -------------------------------------------------------

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



## Get individual features names and type

features_character <- 
  data %>%
  dplyr::select(where(is.factor), -ProjectCode, -BH_ID, -Motherhole) %>% 
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



# Chi square independence test --------------------------------------------

#https://stackoverflow.com/questions/39592879/r-dpylr-select-if-with-multiple-conditions
#https://stackoverflow.com/questions/62639111/map-dplyr-function-to-each-combination-of-variable-pairs-in-an-r-dataframe



chisquare_function <- function(x) {
  data %>% 
    select_if(is.factor) %>% 
    transmute_at(setdiff(names(.), x), ~ chisq.test(!!sym(x), .x)) %>%
    rename_all(.funs = ~ paste0(x, "_vs_", .x))
}


chi_test_factors <- list()
names <- data %>% select_if(is.factor) %>% names(.)
comb2 <- t(combn(names,2))

for(i in 1:nrow(comb2)) {
  
  x1 <- data[[comb2[i,1]]]
  x2 <- data[[comb2[i,2]]]
  res <- chisq.test(x1, x2)
  chi_test_factors[[i]] <- res
  
}



