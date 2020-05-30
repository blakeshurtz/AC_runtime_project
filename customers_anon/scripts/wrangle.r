set.seed(38462346) #as always, smushing all keys simultaneously
library(tidyverse)
library(lubridate)
library(readxl)
library(broom)

setwd("./customers_anon/data")

customer_data <- read_excel("customer_data.xlsx", 
                            col_types = c("text", "date", "date", "numeric", "numeric", "numeric"))

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(name = flnm)
}

setwd("./logger data")

d <-
  list.files(pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_plus(.)) 

d$name <- str_sub(d$name, start = 3, end = -5)

###customer info
customer_data$year <- map(customer_data$start_date, year)
customer_data$days <- ymd(customer_data$end_date) - ymd(customer_data$start_date)

#measure sampling_frequency
customer_data$sampling_frequency <- d %>% 
  group_by(name) %>% 
  slice(1:2) %>% 
  summarise(sr = (as.numeric(mdy_hms(datetime))[2] - as.numeric(mdy_hms(datetime))[1])/60) %>% 
  ungroup() %>% 
  select(-name) %>% 
  unlist() %>% 
  as.numeric()

#if system is 2 stage, calculate low stage capacity
customer_data$low_stage_capacity = if_else(customer_data$num_stages == 1,
                                           NA_real_,
                                           customer_data$high_stage_capacity * .7)

#join customer data with logger data
d <- customer_data %>% 
  select(name, num_stages, cooling_load, high_stage_capacity, low_stage_capacity, sampling_frequency) %>% 
  left_join(d, by = "name") %>% 
  select(-c(index, datetime))

###define lower bound for equipment as on
d$on <- if_else(d$amps < 2, 0, 1) #on/off

###kmeans model to determine system staging
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
  left_join(d, by = "name") %>% 
  select(-c(c1, c2, c4))

#define system staging (for multi-stage systems) according to kmeans model
d$stage <- d %>% 
  mutate(stage = if_else(amps < 2, 0, 
                         if_else(amps > 2 & amps < c3 | num_stages == 1, 1, 
                                 if_else(amps > c3, 2, NA_real_)))) %>% 
  select(stage) %>% 
  unlist() %>% as.numeric()

#capacity (based on sampling frequency) 
d$capacity <- if_else(d$stage == 1, d$sampling_frequency * d$low_stage_capacity/60, 0)
d$capacity <- if_else(d$stage == 2 & d$num_stages == 2, d$sampling_frequency * d$high_stage_capacity/60, d$capacity) 
d$capacity <- if_else(d$stage == 2 & d$num_stages == 60, d$sampling_frequency * d$high_stage_capacity/60, d$capacity) 
d$capacity <- if_else(d$num_stages == 1 & d$on == 1, d$sampling_frequency * d$high_stage_capacity/60, d$capacity)
d$capacity <- if_else(d$num_stages == 60 & d$on == 1, d$sampling_frequency * d$low_stage_capacity/60 * .5, d$capacity)

#cycle_no
d$cycle_no <- with(rle(d$on), rep(seq_along(lengths), lengths))

#aggregate cycles
summary <- d %>% 
  group_by(name, cycle_no) %>% 
  summarise(ccl = sum(capacity)/mean(cooling_load)) %>% 
  filter(ccl != 0) %>% 
  arrange(desc(ccl))

#export data
write.csv(summary, file = "../ccl.csv", row.names = FALSE)
