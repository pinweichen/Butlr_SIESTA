
library(data.table)
library(tidyverse)
library(lubridate)

# setwd('/Users/zhaoyuan/Documents/SIESTA-Rehab/Butlr')
# 23_104 (2022-06-07~2022-06-14): No abnormal data 
dt <- read_csv('five_sec_dt.csv')

## Summary
min(dt$five_sec)
max(dt$five_sec)

## Manipulation
dt$date <- as.Date(dt$five_sec)
dt$t <- format(as.POSIXct(
  dt$five_sec),format = "%H:%M")
dt <- mutate_if(dt, is.numeric, ~replace(., is.na(.), 0))

## Investigation 
d1 <- dt %>% filter(
  accum >= 20
)