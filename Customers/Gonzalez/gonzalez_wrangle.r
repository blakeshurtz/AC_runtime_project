set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
gonzalez <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/gonzalez/gonzalez.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(gonzalez$datetime)))
min((mdy_hms(gonzalez$datetime)))
max((mdy_hms(gonzalez$datetime)))
round(max((mdy_hms(gonzalez$datetime))) - min((mdy_hms(gonzalez$datetime))))

#sampling frequency
mdy_hms(gonzalez$datetime)[2] - mdy_hms(gonzalez$datetime)[1]
sampling_rate = 3

#system capacity
nominal_high_stage_capacity = 2
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
gonzalez$on <- if_else(gonzalez$amps < 2, 0, 1)
gonzalez$staging <- if_else(gonzalez$on == 1, 1, 0)
gonzalez$capacity <- if_else(gonzalez$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)
gonzalez$cycle_no <- with(rle(gonzalez$on), rep(seq_along(lengths), lengths)) 

summary <- gonzalez %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

quantile(summary$fccc, .9) #90% fccc

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/gonzalez/fccc.csv", row.names = FALSE)
