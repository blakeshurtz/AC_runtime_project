set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
violette <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/violette/violette.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(violette$datetime)))
min((mdy_hms(violette$datetime)))
max((mdy_hms(violette$datetime)))
round(max((mdy_hms(violette$datetime))) - min((mdy_hms(violette$datetime))))

#sampling frequency
mdy_hms(violette$datetime)[4] - mdy_hms(violette$datetime)[3]
sampling_rate = 2.1666666

#check hoboware file for staging

#system capacity
nominal_high_stage_capacity = 2
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
violette$on <- if_else(violette$amps < 2, 0, 1)
violette$staging <- if_else(violette$on == 1, 1, 0)
violette$capacity <- if_else(violette$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)
violette$cycle_no <- with(rle(violette$on), rep(seq_along(lengths), lengths)) 


summary <- violette %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

quantile(summary$fccc, .9) #90% fccc

head(summary)

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/violette/fccc.csv", row.names = FALSE)
