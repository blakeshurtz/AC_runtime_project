set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
weinact <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/weinact/weinact.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(weinact$datetime)))
min((mdy_hms(weinact$datetime)))
max((mdy_hms(weinact$datetime)))
round(max((mdy_hms(weinact$datetime))) - min((mdy_hms(weinact$datetime))))

#sampling frequency
mdy_hms(weinact$datetime)[2] - mdy_hms(weinact$datetime)[1]
sampling_rate = 3

#check hoboware file for staging

#system capacity
nominal_high_stage_capacity = 3
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
weinact$on <- if_else(weinact$amps < 2, 0, 1)
weinact$staging <- if_else(weinact$on == 1, 1, 0)
weinact$capacity <- if_else(weinact$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)
weinact$cycle_no <- with(rle(weinact$on), rep(seq_along(lengths), lengths)) 


summary <- weinact %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

quantile(summary$fccc, .9) #90% fccc

head(summary)

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/weinact/fccc.csv", row.names = FALSE)
