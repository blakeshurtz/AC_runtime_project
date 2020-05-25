'#multi-var staging
if_else(d$staging != 1,
        if_else(d$amps < 2, 0, 
                if_else(between(d$amps, 1, d$c3), 1,
                        if_else(d$amps > d$c3, 2, NULL)
                )),
        1)


if_else(d$amps < 2, 0, 
        if_else(between(d$amps, 1, d$c3), 1,
                if_else(d$amps > d$c3, 2, NULL)
        ))
'

'###kmeans model
customer_data <- d %>% 
  group_by(name) %>% 
  do(model = kmeans(.['amps'], 4, nstart = 50, iter.max = 10)) %>% 
  ungroup() %>% group_by(name) %>% 
  do(map_df(.$model, broom::tidy)) %>% ungroup() %>% 
  select(name, x1) %>% 
  arrange(name, x1) %>% 
  mutate(cluster = rep(c("c1", "c2", "c3", "c4"), length(unique(name)))) %>% 
  spread(cluster, x1) %>% 
  left_join(customer_data, by = "name")

d <- d %>% 
  group_by(name) %>% 
  do(model = kmeans(.['amps'], 4, nstart = 50, iter.max = 10)) %>% 
  ungroup() %>% group_by(name) %>% 
  do(map_df(.$model, broom::tidy)) %>% ungroup() %>% 
  select(name, x1) %>% 
  arrange(name, x1) %>% 
  mutate(cluster = rep(c("c1", "c2", "c3", "c4"), length(unique(name)))) %>% 
  spread(cluster, x1) %>% 
  left_join(d, by = "name")

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

customer_data$temp <- d %>% 
  group_by(name) %>% 
  slice(1:2) %>% 
  summarise(sr = (as.numeric(mdy_hms(datetime))[2] - as.numeric(mdy_hms(datetime))[1])/60) %>% 
  ungroup() %>% 
  select(-name)

colnames(customer_data)[9] <- "sampling_rate"

str(customer_data)

customer_data$sampling_rate <- as.numeric(customer_data$sampling_rate)
'