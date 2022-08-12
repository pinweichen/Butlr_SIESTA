
library(data.table)
library(tidyverse)
library(lubridate)

# setwd('/Users/zhaoyuan/Documents/SIESTA-Rehab/Butlr')
general <- "Z:/SIESTA/Data/Butlr/"
head_p <- paste0(general,"Head_count/")
occupancy_p <- paste0(general,"Occupancy/")
ls_head_p <- list.files(path = head_p)
ls_occ_p <- list.files(path=occupancy_p)

ls_dt_five_sec <- list()
ls_dt_five_min <- list()

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
## ====================
## process all the data 
## ====================
sub_num <- '23'
floor_num <- '100'
setwd(head_p)

dt_h <- fread(paste0(head_p,sub_num,"_", floor_num,"_head.csv"))
colnames(dt_h) <-  c("Time","din", "exit", "out", "trajectory", "device_id")
dt_h <- dt_h[,.(Time,din,out)]
dt_h$sub <- sub_num
dt_h$floor <- floor_num
dt_h$timestamp <- as.POSIXct(dt_h$Time)
dt_h$Time <- NULL

dt_o <- fread(paste0(occupancy_p,sub_num,"_",floor_num, "_occ.csv"))
dt_o <- dt_o[,.(Time, occupancy)]
dt_o$sub <- sub_num
dt_o$floor <- floor_num
dt_o$timestamp <- as.POSIXct(dt_o$Time)
dt_o$Time <- NULL
# ==========================
## Align the first timestamp between the occupancy and head count sensors.
## This step is to prepare the time to group all data into 1sec,5sec and 5min bins
## With aligned beginning, both occupancy and head count sensors will be in the same time frame for the bins
## Easier to merge the two dataframe with timestamps.
# ==========================
if (dt_h$timestamp[1] > dt_o$timestamp[1]) {
  dt_o <- dt_o[timestamp > dt_h[1,timestamp], ]
  new_o<- dt_h[1,.(occupancy = 0,sub = sub_num, floor = floor_num, timestamp = timestamp)]
  dt_o <- rbind(new_o,dt_o)
} else if (dt_o$timestamp[1] >= dt_h$timestamp[1]) {
  dt_h <- dt_h[timestamp >= dt_o[1,timestamp], ] 
  new_h<- dt_o[1,.(din = 0, out = 0, sub = sub_num, floor = floor_num, timestamp = timestamp)]
  dt_h <- rbind(new_h,dt_h)
} 
# ==========================
## Regroup data into 1 second 
# ==========================
dt_o$one_sec <- cut(dt_o$timestamp, breaks = "1 sec")
dt_o_one <- dt_o[,.(occupancy = sum(occupancy)),by = c("one_sec")]

dt_o_one$one_sec <- as.POSIXct(dt_o_one$one_sec)

dt_h$five_sec <- cut(dt_h$timestamp, breaks = "5 sec")
dt_o_one$five_sec <- cut(dt_o_one$one_sec, breaks = "5 sec")
dt_o_one$five_min <- cut(dt_o_one$one_sec, breaks = "5 min")

dt_h$five_min <- cut(dt_h$timestamp, breaks = "5 min")

# ==========================
## Regroup data into 5 second 
# ==========================
#browser()

dt_h_sum <- dt_h[,.(
  sub = sub,
  floor = floor,
  in_sum = sum(din),
  out_sum = sum(out),
  inout_sub =  sum(din) - sum(out)
), by = c("five_sec")]

## rewrite: 
dt_h_sum <- dt_h[,.(sub = sub, floor = floor)]
# same idea with above
dt_h_sum <- dt_h %>%
  group_by(five_sec) %>%
  summarise(in_sum = sum(din))

# To eliminate duplication:
dt_h_sum <- dt_h_sum[!duplicated(dt_h_sum), ]
dt_h_sum[,accum := cumsum(inout_sub)]
dt_h_sum[,accum = cumsum(inout_sub)]
# check:
min(dt_h_sum$accum)
max(dt_h_sum$accum)

## occupancy data:
dt_o_sum <- dt_o_one[,.(
  occupany_sum = max(occupancy)
), by = c("five_sec")]
# check: 
min(dt_o_sum$occupany_sum)
max(dt_o_sum$occupany_sum)

dt_all <- merge(dt_h_sum,dt_o_sum, by = "five_sec", all = T)

# ==========================
## Regroup data into 5 minutes 
# ==========================

dt_h_sum_fivemin <- dt_h[,.(
  sub = sub,
  floor = floor,
  in_sum = sum(din),
  out_sum = sum(out),
  inout_sub =  sum(din) - sum(out)
), by = c("five_min")]

# reduce duplication:
dt_h_sum_fivemin <- dt_h_sum_fivemin[!duplicated(dt_h_sum_fivemin), ]
dt_h_sum_fivemin[,accum := cumsum(inout_sub)]
# check:
min(dt_h_sum_fivemin$accum)
max(dt_h_sum_fivemin$accum)


dt_o_sum_fivemin <- dt_o_one[,.(
  occupany_sum = max(occupancy)
), by = c("five_min")]
# check: 
min(dt_o_sum_fivemin$occupany_sum)
max(dt_o_sum_fivemin$occupany_sum)

dt_all_five_min <- merge(dt_h_sum_fivemin,dt_o_sum_fivemin, by = "five_min", all = T)

dt_processed <- dt_all_five_min
dt_processed$time <- dt_processed$five_min
dt_processed$time <- as.character(dt_processed$time)
dt_processed$five_sec<-NULL
 
preprocessed_p <- paste0("Z:/SIESTA/Data/Preprocessed/Butlr/Level_1/23_fiv_min")
setwd(preprocessed_p)

fwrite(dt_processed, paste0(floor, "_",floor_num,"_preprocessed.csv"))

