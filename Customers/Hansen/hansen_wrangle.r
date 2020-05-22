set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)

#import data
hansen <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/hansen/hansen.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#date range
max(year(mdy_hms(hansen$datetime)))
min((mdy_hms(hansen$datetime)))
max((mdy_hms(hansen$datetime)))
round(max((mdy_hms(hansen$datetime))) - min((mdy_hms(hansen$datetime))))

#sampling frequency
mdy_hms(hansen$datetime)[2] - mdy_hms(hansen$datetime)[1]
sampling_rate = 3

#system capacity
nominal_high_stage_capacity = 3
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
hansen$on <- if_else(hansen$amps < 2, 0, 1)
hansen$staging <- if_else(hansen$on == 1, 1, 0)
hansen$capacity <- if_else(hansen$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)
hansen$cycle_no <- with(rle(hansen$on), rep(seq_along(lengths), lengths)) 


###note, code below for two stage
#check hoboware file for staging 
nominal_low_stage_capacity = .7 * actual_high_stage_capacity #low stage capacity is 70% of high stage capacity
actual_low_stage_capacity = nominal_low_stage_capacity * .8

#4 points gives the best stage split
model <- kmeans(hansen$amps, 4, nstart = 50, iter.max = 10)
thresholds <- sort(model$centers)
thresholds

#multi-var staging
hansen$staging <- if_else(hansen$amps < 2, 0, 
                        if_else(between(hansen$amps, 1, model$centers[3]), 1,
                                if_else(hansen$amps > model$centers[3], 2, NULL)
                        ))

#capacity accounting for sampling frequency
hansen$capacity <- if_else(hansen$staging == 1, sampling_rate * actual_low_stage_capacity/60,
                         if_else(hansen$staging == 2, sampling_rate * actual_high_stage_capacity/60, 0))


summary <- hansen %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

head(summary)

quantile(summary$fccc, .9) #90% fccc

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/hansen/fccc.csv", row.names = FALSE)
