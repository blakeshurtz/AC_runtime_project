set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
kitchen <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/kitchen/kitchen.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(kitchen$datetime)))
min((mdy_hms(kitchen$datetime)))
max((mdy_hms(kitchen$datetime)))
round(max((mdy_hms(kitchen$datetime))) - min((mdy_hms(kitchen$datetime))))

#sampling frequency
mdy_hms(kitchen$datetime)[2] - mdy_hms(kitchen$datetime)[1]
sampling_rate = 3

#system capacity
nominal_high_stage_capacity = 2
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
kitchen$on <- if_else(kitchen$amps < 2, 0, 1)
kitchen$staging <- if_else(kitchen$on == 1, 1, 0)
kitchen$capacity <- if_else(kitchen$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)

'#(only run code segment if equipment is variable capacity)
nominal_low_stage_capacity = .7 * actual_high_stage_capacity #low stage capacity is 70% of high stage capacity
actual_low_stage_capacity = nominal_low_stage_capacity * .8

#4 points gives the best stage split
model <- kmeans(kitchen$amps, 4, nstart = 50, iter.max = 10)
thresholds <- sort(model$centers)
thresholds

#multi-var staging
kitchen$staging <- if_else(kitchen$amps < 2, 0, 
                        if_else(between(kitchen$amps, 1, model$centers[3]), 1,
                                if_else(kitchen$amps > model$centers[3], 2, NULL)
                        ))

#capacity accounting for sampling frequency
kitchen$capacity <- if_else(kitchen$staging == 1, sampling_rate * actual_low_stage_capacity/60,
                         if_else(kitchen$staging == 2, sampling_rate * actual_high_stage_capacity/60, 0))

'



kitchen$cycle_no <- with(rle(kitchen$on), rep(seq_along(lengths), lengths)) 

summary <- kitchen %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

quantile(summary$fccc, .9) #90% fccc
head(summary)
write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/kitchen/fccc.csv", row.names = FALSE)
