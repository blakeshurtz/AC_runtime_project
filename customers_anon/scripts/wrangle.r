set.seed(1234)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)
library(readxl)
library(stringr)
library(tidyr)
library(broom)

suppressWarnings(expr)

setwd("D:/Google Drive/R/Projects/AC_runtime_project/customers_anon/data")

customer_data <- read_excel("customer_data.xlsx", 
                            col_types = c("text", "date", "date", "numeric", "numeric", "numeric", "numeric"))

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(name = flnm)
}

d <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.)) 

d$name <- str_sub(d$name, start = 3, end = -5)

customer_data$year <- map(customer_data$start_date, year)
customer_data$days <- ymd(customer_data$end_date) - ymd(customer_data$start_date)

#sampling rate in original data
#system capacity
customer_data$low_stage_capacity = if_else(customer_data$staging != 2,
                                           NA_real_,
                                           customer_data$high_stage_capacity * .7)

#join 
d <- customer_data %>% 
  select(name, staging, cooling_load, high_stage_capacity, low_stage_capacity, sampling_rate) %>% 
  left_join(d, by = "name")

###on to the data
d$on <- if_else(d$amps < 2, 0, 1) #on/off

###kmeans model
d <- d %>% 
  group_by(name) %>% 
  do(model = kmeans(.['amps'], 4, nstart = 50, iter.max = 10)) %>% 
  ungroup() %>% group_by(name) %>% 
  do(map_df(.$model, broom::tidy)) %>% ungroup() %>% 
  select(name, x1) %>% 
  arrange(name, x1) %>%
  ungroup() %>% 
  mutate(cluster = rep(c("c1", "c2", "c3", "c4"), length(unique(name)))) %>% 
  spread(cluster, x1) %>% 
  left_join(d, by = "name")

#multi-variate staging
d$stage <- d %>% 
  mutate(stage = if_else(amps < 2, 0, 
                         if_else(amps > 2 & amps < c3 | staging == 1, 1, 
                                 if_else(amps > c3, 2, NA_real_)))) %>% 
  select(stage)
d$stage <- as.numeric(unlist(d$stage))
table(d$stage)

#capacity accounting for sampling frequency
d$capacity <- if_else(d$stage == 1, d$sampling_rate * d$low_stage_capacity/60,
                     if_else(d$stage == 2, d$sampling_rate * d$high_stage_capacity/60, 0))

#cycle_no
d$cycle_no <- with(rle(d$on), rep(seq_along(lengths), lengths))

table(d$cooling_load)

summary <- d %>% 
  group_by(name, cycle_no) %>% 
  summarise(fccc = sum(capacity)/mean(high_stage_capacity),
            runtime = mean(sampling_rate) * length(cycle_no)) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))
head(summary)

quantile(summary$fccc, .9) #90% fccc

write.csv(summary$fccc, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/d/fccc.csv", row.names = FALSE)
