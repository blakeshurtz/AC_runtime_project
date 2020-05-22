set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
loza <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/loza/loza.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(loza$datetime)))
min((mdy_hms(loza$datetime)))
max((mdy_hms(loza$datetime)))
round(max((mdy_hms(loza$datetime))) - min((mdy_hms(loza$datetime))))

#sampling frequency
mdy_hms(loza$datetime)[2] - mdy_hms(loza$datetime)[1]
sampling_rate = 3

#check hoboware file for staging

#system capacity
nominal_high_stage_capacity = 3
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
loza$on <- if_else(loza$amps < 2, 0, 1)
loza$staging <- if_else(loza$on == 1, 1, 0)
loza$capacity <- if_else(loza$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)
loza$cycle_no <- with(rle(loza$on), rep(seq_along(lengths), lengths)) 

summary <- loza %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

quantile(summary$fccc, .9) #90% fccc

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/loza/fccc.csv", row.names = FALSE)
