
# ============================================================================ #
# Script Name : data transform into daily calls                                                                                           
# Purpose     :                                                                      
# Date        : Sat Jan 29 17:50:05 2022   
# Author      : Pedro Magalhães                                                
# Email       : pedro.magalhaes@mosaic.pt                                           
# ============================================================================ #


# Libraries and env -------------------------------------------------------

library(dplyr)
library(xts)
library(fpp2)
library(astsa)
library(forecast)
library(cowplot)



# convert daily data ------------------------------------------------------

#' Available data has a 15 min interval which is to much for the project
#' In order to simplify what was converted into daily data
#' 
#' Output: xts file

data_raw <- read.csv("./data/ts_phone_calls.csv", header = TRUE)
data_raw <- data_raw %>% dplyr::select(interval, total_calls)
data <- as.xts(x = dplyr::select(data_raw, -interval), 
               order.by = strptime(
                 data_raw$interval,"%Y-%m-%d")
)
days <- endpoints(data, on = "days")
months <- endpoints(data, on = "month")

daily_calls <- period.apply(data, 
                            FUN = function(x) apply(x, 2, sum, na.rm = TRUE),
                            INDEX = days)
index(daily_calls) <- as.Date(index(daily_calls))

# Output
saveRDS(daily_calls, file = "./data/daily_calls.rds")