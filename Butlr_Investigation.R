
library(data.table)
library(tidyverse)
library(lubridate)

# setwd('/Users/zhaoyuan/Documents/SIESTA-Rehab/Butlr')
general <- "Z:/SIESTA/Data/Butlr/"

# 23_104 (2022-06-07~2022-06-14): No abnormal data 
# read all data
dt <- read_csv('five_sec_dt.csv')
dt <- fread('five_sec_dt.csv')
## Manipulation
#dt$date <- as.Date(dt$five_sec)
#dt$t <- format(as.POSIXct(
#  dt$five_sec),format = "%H:%M")
#dt <- mutate_if(dt, is.numeric, ~replace(., is.na(.), 0))

## Investigation 
dt_24_045 <- dt %>% filter(
  sub == 045 & floor == 24
)