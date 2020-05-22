set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)

setwd("D:/Google Drive/R/Projects/AC_runtime_project/customers_anon/data")

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

tbl_with_sources <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.))


#date range
max(year(mdy_hms(cruz$datetime)))
min((mdy_hms(cruz$datetime)))
max((mdy_hms(cruz$datetime)))
round(max((mdy_hms(cruz$datetime))) - min((mdy_hms(cruz$datetime))))

#sampling frequency
mdy_hms(cruz$datetime)[2] - mdy_hms(cruz$datetime)[1]
sampling_rate = 1

#system capacity
nominal_high_stage_capacity = 3
actual_high_stage_capacity = nominal_high_stage_capacity * .8 #80% rule of thumb- check Manual S Excel file

#on/off
cruz$on <- if_else(cruz$amps < 2, 0, 1)
cruz$staging <- if_else(cruz$on == 1, 1, 0)
cruz$capacity <- if_else(cruz$on == 1, sampling_rate * actual_high_stage_capacity/60, 0)
cruz$cycle_no <- with(rle(cruz$on), rep(seq_along(lengths), lengths)) 


###note, code below for two stage
#check hoboware file for staging 
nominal_low_stage_capacity = .7 * actual_high_stage_capacity #low stage capacity is 70% of high stage capacity
actual_low_stage_capacity = nominal_low_stage_capacity * .8

#4 points gives the best stage split
model <- kmeans(cruz$amps, 4, nstart = 50, iter.max = 10)
thresholds <- sort(model$centers)
thresholds

#multi-var staging
cruz$staging <- if_else(cruz$amps < 2, 0, 
                        if_else(between(cruz$amps, 1, model$centers[3]), 1,
                                if_else(cruz$amps > model$centers[3], 2, NULL)
                        ))

#capacity accounting for sampling frequency
cruz$capacity <- if_else(cruz$staging == 1, sampling_rate * actual_low_stage_capacity/60,
                         if_else(cruz$staging == 2, sampling_rate * actual_high_stage_capacity/60, 0))


summary <- cruz %>% 
  group_by(cycle_no) %>% 
  summarise(fccc = sum(capacity)/actual_high_stage_capacity,
            runtime = sampling_rate * length(cycle_no),
            avg_load = if_else(runtime < 60, fccc, fccc / (runtime/60))) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))

head(summary)

quantile(summary$fccc, .9) #90% fccc

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/cruz/fccc.csv", row.names = FALSE)
