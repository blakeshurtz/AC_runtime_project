summary <- d %>% 
  group_by(name, cycle_no) %>% 
  summarise(fccc = sum(capacity)/mean(high_stage_capacity),
            fccc_cl = sum(capacity)/mean(cooling_load),
            runtime = mean(sampling_frequency) * length(cycle_no)) %>% 
  filter(fccc != 0) %>% 
  arrange(desc(fccc))
