
library(data.table)
library(tidyverse)
library(lubridate)

setwd('/Users/zhaoyuan/Documents/SIESTA-Rehab/Butlr')
# 23_104 2022-06-07~2022-06-14
dt <- read_csv('23_104_preprocessed.csv')

# Summary
min(dt$time)
max(dt$time)

# Manipulation
dt$date <- as.Date(dt$time)
dt$t <- format(as.POSIXct(
  dt$time),format = "%H:%M")
dt <- mutate_if(dt, is.numeric, ~replace(., is.na(.), 0))

# Investigation 
d1 <- dt %>% filter(
  accum >= 20
)
## No abnormal data shown 