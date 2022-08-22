
library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
rm(list=ls())

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
# Regroup data into 5 minutes 
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



## ========== graph ============
#23_100: 3 abnormal observations, 1 obs. > 45
dt_23_100 <- fread(paste0("23_100_preprocessed.csv"))
dt_23_103 <- fread(paste0("23_103_preprocessed.csv"))

abn_23_100 <- dt_23_100[dt_23_100$in_sum >= 10]
abn_23_103 <- dt_23_103[dt_23_103$in_sum >= 10]
# data summary:
dt_23_100 <- dt_23_100 %>% drop_na(in_sum)
dt_23_103 <- dt_23_103 %>% drop_na(in_sum)

# split time:
dt_23_100$t <- format(as.POSIXct(dt_23_100$five_min),format = "%H:%M:%S")
dt_23_100$d <- date(dt_23_100$five_min)
dt_23_103$t <- format(as.POSIXct(dt_23_103$five_min),format = "%H:%M:%S")

# Graph: 
dt_23_100 %>%
  filter(five_min >= "2022-05-21 12:00:00" & five_min <= "2022-05-22 0:00:00") %>%
  ggplot() +
  geom_point(aes(five_min, in_sum)) + 
  geom_line(aes(five_min, in_sum)) +
  #geom_point(aes(five_min, out_sum), color = 'blue') + 
  theme(axis.text.x = element_text(angle = 90.)) + 
  #facet_grid(rows = 'd')

# ========== 23rd floor five_sec abn data investigation ===========
preprocessed <- "Z:/SIESTA/Data/Preprocessed/Butlr/Level_1/23_fiv_min"
setwd(preprocessed)
rm(list = ls())
dt <- fread(paste0("23_100_preprocessed.csv"))
dt <- dt %>% drop_na(in_sum)
abn_dt <- dt[dt$in_sum >= 10, ]

# 23_100: 
floor = 23
sub_num <-"100"
setwd(head_p)
dt_h <- fread(paste0(head_p,floor,"_",sub_num,"_head.csv"))
dt_o <- fread(paste0(occupancy_p,floor,"_",sub_num, "_occ.csv"))
# Subset columns
colnames(dt_h) <-  c("Time","din", "exit", "out", "trajectory", "device_id")
dt_h <- dt_h[,.(Time,din,out)]

dt_o <- dt_o[,.(Time, occupancy)]



# =============== Summary of abnormal data =====================
library(openxlsx)
library(dplyr)
Butlr_installed <- "Z:/SIESTA/Data Collection/Daily Worksheets"
setwd(Butlr_installed)
Butlr_installed <- read.xlsx("Butlr_install_log.xlsx")
str(Butlr_installed)
placement <- data.frame(Subject = Butlr_installed$Subject, 
                        Room = Butlr_installed$Room, 
                        Kit_n = Butlr_installed$Butlr)

preprocessed <- "Z:/SIESTA/Data/Preprocessed/Butlr/Level_1/24_fiv_min"
save_p <- "Z:/SIESTA/Data/Butlr/Butlr_SIESTA/Abnormal_data"

setwd(preprocessed)

ls_dt <- list.files(path = preprocessed)
ls_abn_dt_five_min <- list()

# incomeplete loop tying to gather all abnormal data: 
i = 1
for (sub in ls_dt) {
  sub <- word(sub, start = 1, end = 2, sep = "_") #Subject ID
  dt <- read_csv(paste0(sub,"_preprocessed.csv"))
  dt <- dt %>% drop_na(in_sum)
  
  abn_dt <- dt[dt$in_sum >= 10, ]
  abn_dt$Subject <- sub
  
  ls_abn_dt_five_min[[i]] <- abn_dt
  i = i + 1
}

#abn_five_min__23fl_all <- do.call(rbind,ls_abn_dt_five_min)
abn_five_min__24fl_all <- do.call(rbind,ls_abn_dt_five_min)
#abn_five_sec_all <- do.call(rbind,abn_five_sec_all)

#add room & kit numbers
#abn_five_min__23fl_all <- inner_join(placement, abn_five_min__23fl_all, by = "Subject")
abn_five_min__24fl_all <- inner_join(placement, abn_five_min__24fl_all, by = "Subject")

#split the time:
abn_five_min__23fl_all <- abn_five_min__23fl_all %>%
  mutate(date = date(abn_five_min__23fl_all$five_min), .before = five_min )%>%
  mutate(time = format(as.POSIXct(abn_five_min__23fl_all$five_min),
                       format = "%H:%M:%S"), .before = five_min ) %>% 
  select(., -c("five_min"))
abn_five_min__24fl_all <- abn_five_min__24fl_all %>%
  mutate(date = date(abn_five_min__24fl_all$five_min), .before = five_min )%>%
  mutate(time = format(as.POSIXct(abn_five_min__24fl_all$five_min),
                       format = "%H:%M:%S"), .before = five_min ) %>% 
  select(., -c("five_min"))


setwd(save_p)
fwrite(abn_five_min__23fl_all,"abn_five_min__23fl_all.csv")
fwrite(abn_five_min__24fl_all,"abn_five_min__24fl_all.csv")
#fwrite(abn_five_sec_all, "abn_five_sec_all.csv")

