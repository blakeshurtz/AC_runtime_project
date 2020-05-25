set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
torgerson <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/torgerson/torgerson.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(torgerson$datetime)))
min((mdy_hms(torgerson$datetime)))
max((mdy_hms(torgerson$datetime)))
round(max((mdy_hms(torgerson$datetime))) - min((mdy_hms(torgerson$datetime))))

#sampling frequency
mdy_hms(torgerson$datetime)[2] - mdy_hms(torgerson$datetime)[1]
sampling_rate = 3

#check hoboware file for staging

#system capacity
nominal_high_stage_capacity = 3
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#4 points gives the best stage split
model <- kmeans(torgerson$amps, 4, nstart = 50, iter.max = 10)
thresholds <- sort(model$centers)
thresholds

#add vars
torgerson$on <- if_else(torgerson$amps < 2, 0, 1)
torgerson$staging <- if_else(torgerson$amps < 2, 0, 
                         if_else(between(torgerson$amps, 1, model$centers[3]), 1,
                                 if_else(torgerson$amps > model$centers[3], 2, NULL)
                         ))


#determining capacity
nominal_low_stage_capacity = .7 * actual_high_stage_capacity #low stage capacity is 70% of high stage capacity
actual_low_stage_capacity = nominal_low_stage_capacity * .8

#capacity accounting for sampling frequency
torgerson$capacity <- if_else(torgerson$staging == 1, sampling_rate * actual_low_stage_capacity/60,
                          if_else(torgerson$staging == 2, sampling_rate * actual_high_stage_capacity/60, 0))

torgerson$cycle_no <- with(rle(torgerson$on), rep(seq_along(lengths), lengths)) 
summary <- torgerson %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

quantile(summary$fccc, .9) #90% fccc

head(summary)

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/torgerson/fccc.csv", row.names = FALSE)
