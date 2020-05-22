set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
laxx <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/laxx/laxx.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(laxx$datetime)))
min((mdy_hms(laxx$datetime)))
max((mdy_hms(laxx$datetime)))
round(max((mdy_hms(laxx$datetime))) - min((mdy_hms(laxx$datetime))))

#sampling frequency
mdy_hms(laxx$datetime)[2] - mdy_hms(laxx$datetime)[1]
sampling_rate = 1

#system capacity
nominal_high_stage_capacity = 2.5
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
laxx$on <- if_else(laxx$amps < 2, 0, 1)
laxx$staging <- if_else(laxx$on == 1, 1, 0)
laxx$capacity <- if_else(laxx$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)

'(only run code segment if equipment is variable capacity)
#check hoboware file for staging 
nominal_low_stage_capacity = .7 * actual_high_stage_capacity #low stage capacity is 70% of high stage capacity
actual_low_stage_capacity = nominal_low_stage_capacity * .8

#4 points gives the best stage split
model <- kmeans(laxx$amps, 4, nstart = 50, iter.max = 10)
thresholds <- sort(model$centers)
thresholds

#multi-var staging
laxx$staging <- if_else(laxx$amps < 2, 0, 
                        if_else(between(laxx$amps, 1, model$centers[3]), 1,
                                if_else(laxx$amps > model$centers[3], 2, NULL)
                        ))

#capacity accounting for sampling frequency
laxx$capacity <- if_else(laxx$staging == 1, sampling_rate * actual_low_stage_capacity/60,
                         if_else(laxx$staging == 2, sampling_rate * actual_high_stage_capacity/60, 0))

'



laxx$cycle_no <- with(rle(laxx$on), rep(seq_along(lengths), lengths)) 

summary <- laxx %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

quantile(summary$fccc, .9) #90% fccc

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/laxx/fccc.csv", row.names = FALSE)
