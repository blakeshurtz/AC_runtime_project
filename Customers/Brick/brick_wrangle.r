set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
brick <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/brick/brick.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(brick$datetime)))
min((mdy_hms(brick$datetime)))
max((mdy_hms(brick$datetime)))
round(max((mdy_hms(brick$datetime))) - min((mdy_hms(brick$datetime))))

#sampling frequency
mdy_hms(brick$datetime)[2] - mdy_hms(brick$datetime)[1]
sampling_rate = 3

#system capacity
nominal_high_stage_capacity = 3
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
brick$on <- if_else(brick$amps < 2, 0, 1)
brick$staging <- if_else(brick$on == 1, 1, 0)
brick$capacity <- if_else(brick$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)
brick$cycle_no <- with(rle(brick$on), rep(seq_along(lengths), lengths)) 

summary <- brick %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

quantile(summary$fccc, .9) #90% fccc

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/brick/fccc.csv", row.names = FALSE)
