# Butlr_SIESTA
Butlr sensors data analysis 

#### Butlr_Preprocess_1.R
This script contains three sections.
First section: load and organize data into a full dt file contains all subjects headcount and occupancy data. 
This script will summarize data into 5 minutes window or 5 seconds window. It will output a full raw data in different windows (five_sec_dt.csv, five_min_dt.csv).
Second section: Cut data into nighttime and daytime.Summarize it by each subject the amount of disruptions. (output: five_min_sub_level.csv, five_min_floor_avg.csv)
Third section: Summarize data by floor and plot a bar graph looking at the number of disruptions
