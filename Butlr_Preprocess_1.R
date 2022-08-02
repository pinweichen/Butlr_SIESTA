library(data.table)
library(caret)
library(tidyverse)
library(signal)
library(lubridate)
# 24_039 
# 24_036
# 23_104

rm(list=ls())
# ==========================
## SET path 
# ==========================
#general <- "/Volumes/RTO/SIESTA/Data/Butlr"
general <- "Z:/SIESTA/Data/Butlr/"


  head_p <- paste0(general,"Head_count/")
  occupancy_p <- paste0(general,"Occupancy/")
  ls_head_p <- list.files(path = head_p)
  ls_occ_p <- list.files(path=occupancy_p)
  
  ls_dt_five_sec <- list()
  ls_dt_five_min <- list()
  # ==========================
  ## Read csv
  # ==========================
  i = 1
  for (sub_name in ls_head_p) {
    sub_num <- word(sub_name, start = 2, sep = "_")
    floor_num <-  word(sub_name, start = 1, sep = "_")
    dt_h <- fread(paste0(head_p,floor_num,"_",sub_num,"_head.csv"))
    colnames(dt_h) <-  c("Time","din", "exit", "out", "trajectory", "device_id")
    dt_h <- dt_h[,.(Time,din,out)]
    dt_h$sub <- sub_num
    dt_h$floor <- floor_num
    dt_h$timestamp <- as.POSIXct(dt_h$Time)
    dt_h$Time <- NULL
    
    dt_o <- fread(paste0(occupancy_p,floor_num,"_",sub_num, "_occ.csv"))
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
    
    dt_h$five_min <- cut(dt_h$timestamp, breaks = "5 min")
    dt_o_one$five_min <- cut(dt_o_one$one_sec, breaks = "5 min")
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
    
    dt_h_sum[,accum := cumsum(inout_sub)]
    min(dt_h_sum$accum)
    dt_o_sum <- dt_o_one[,.(
      occupany_sum = max(occupancy)
    ), by = c("five_sec")]
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
    
    dt_h_sum_fivemin[,accum := cumsum(inout_sub)]
    min(dt_h_sum$accum)
    
    dt_o_sum_fivemin <- dt_o_one[,.(
      occupany_sum = max(occupancy)
    ), by = c("five_min")]
    
    
    dt_all_five_min <- merge(dt_h_sum_fivemin,dt_o_sum_fivemin, by = "five_min", all = T)
    
    ls_dt_five_sec[[i]] <- dt_all
    ls_dt_five_min[[i]] <- dt_all_five_min
    #rm(dt_h, dt_o, dt_h_sum_fivemin, dt_h_sum, dt_o_sum_fivemin, dt_o_sum)
 
    i <- i + 1
  }
  

five_min_all <- do.call(rbind,ls_dt_five_min)
five_sec_all <- do.call(rbind,ls_dt_five_sec)

rm(ls_head,ls_occ)
setwd(general)
fwrite(five_min_all,"five_min_dt.csv")
fwrite(five_sec_all, "five_sec_dt.csv")
## ==========================
## Merge file with time axis
# ==========================







#floor = 23
#floor = 24


# # ==========================
## Old codes for testing 
# # ==========================
# 
# 
# # ==========================
# ## Read csv
# # ==========================
# floor = 23
# sub_num <-"104"
# #setwd(head_p)
# dt_h <- fread(paste0(head_p,floor,"_",sub_num,"_head.csv"))
# dt_o <- fread(paste0(occupancy_p,floor,"_",sub_num, "_occ.csv"))
# # Subset columns
# colnames(dt_h) <-  c("Time","din", "exit", "out", "trajectory", "device_id")
# dt_h <- dt_h[,.(Time,din,out)]
# 
# dt_o <- dt_o[,.(Time, occupancy)]


## ==========================
## Merge file with time axis
# ==========================

dt_h$timestamp <- as.POSIXct(dt_h$Time)
dt_o$timestamp <- as.POSIXct(dt_o$Time)

dt_h$Time <- NULL
dt_o$Time <- NULL


if (dt_h$timestamp[1] > dt_o$timestamp[1]) {
  dt_o <- dt_o[timestamp > dt_h[1,timestamp], ]
  new_o<- dt_h[1,.(occupancy = 0, timestamp = timestamp)]
  dt_o <- rbind(new_o,dt_o)
} else if (dt_o$timestamp[1] >= dt_h$timestamp[1]) {
  dt_h <- dt_h[timestamp >= dt_o[1,timestamp], ] 
  new_h<- dt_o[1,.(din = 0, out = 0, timestamp = timestamp)]
  dt_h <- rbind(new_h,dt_h)
} 


dt_o$one_sec <- cut(dt_o$timestamp, breaks = "1 sec")
dt_o_one <- dt_o[,.(occupancy = sum(occupancy)),by = c("one_sec")]

dt_o_one$one_sec <- as.POSIXct(dt_o_one$one_sec)

dt_h$five_sec <- cut(dt_h$timestamp, breaks = "5 sec")
dt_o_one$five_sec <- cut(dt_o_one$one_sec, breaks = "5 sec")

dt_h$five_min <- cut(dt_h$timestamp, breaks = "5 min")
dt_o_one$five_min <- cut(dt_o_one$one_sec, breaks = "5 min")

dt_h_sum <- dt_h[,.(
  in_sum = sum(din),
  out_sum = sum(out),
  inout_sub =  sum(din) - sum(out)
), by = c("five_sec")]

dt_h_sum[,accum := cumsum(inout_sub)]
min(dt_h_sum$accum)

dt_o_sum <- dt_o_one[,.(
  occupany_sum = max(occupancy)
), by = c("five_sec")]


dt_all <- merge(dt_h_sum,dt_o_sum, by = "five_sec", all = T)



dt_h_sum_fivemin <- dt_h[,.(
  in_sum = sum(din),
  out_sum = sum(out),
  inout_sub =  sum(din) - sum(out)
), by = c("five_min")]

dt_h_sum_fivemin[,accum := cumsum(inout_sub)]
min(dt_h_sum$accum)

dt_o_sum_fivemin <- dt_o_one[,.(
  occupany_sum = max(occupancy)
), by = c("five_min")]


dt_all_five_min <- merge(dt_h_sum_fivemin,dt_o_sum_fivemin, by = "five_min", all = T)




# dt_all <- dt_all[!(occupany_sum == 0 & in_sum %in% NA),]

# 
# if(dt_all[1,din] == 0 & dt_all[1,out] == 0){
#   dt_all[,occupancy_by_head_count := din - out]
#   acc_sum <- cumsum(dt_all$occupancy_by_head_count)
#   acc_sum_diff<-diff(acc_sum,lag=1,differences = 2)
#   ind <- match(0, acc_sum_diff)
#   dt_all<-dt_all[(ind +1):nrow(dt_all),]
#   dt_all[,accum_head_count := cumsum(dt_all$occupancy_by_head_count)]
# } else {
#   dt_all[,occupancy_by_head_count := din - out]
#   acc_sum <- cumsum(dt_all$occupancy_by_head_count)
#   dt_all[,accum_head_count := cumsum(dt_all$occupancy_by_head_count)]
# }
# 
# 
# dt_final <- dt_all[,.(timestamp = as.character(timestamp),
#                       enter_room = din,
#                       exit_room = out,
#                       head_count_accumulation = accum_head_count,
#                       occupancy = occupancy)]

dt_processed <- dt_all
dt_processed$time <- dt_processed$five_sec
dt_processed$time <- as.character(dt_processed$time)
dt_processed$five_sec<-NULL

preprocessed_p <- paste0(general,"Preprocessed/Butlr/Level_1/",floor)
setwd(preprocessed_p)

fwrite(dt_processed, paste0(floor,"_",sub_num,"_preprocessed.csv"))


## ==========================
## Detect error
# ==========================


dt_o[, occupancy_delayed := shift(occupancy)]
dt_o[, changed := occupancy_delayed- occupancy]
dt_o_changed<-dt_o[changed != 0, ]


dt_h[]