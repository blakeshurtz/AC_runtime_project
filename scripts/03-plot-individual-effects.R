#plot points
d <- read_csv("data/ccl.csv")

thresholds <- read_csv("data/customer-thresholds.csv")

d <- d %>%
  left_join(thresholds, by = 'name') %>% 
  filter(ccl >= threshold) %>%  
  select(name, ccl) %>%  
  arrange(name, ccl) 

#create ccdf
d <- d %>% group_by(name) %>% mutate(n = n(),
                                         ccdf = seq(1, 0, length.out = n)) 

###simulated data
params <- read.csv("data/indv_params_means.csv")

simulate_ccl <- function(i){
  rgpd(1000, mu = params$threshold[i], xi = params$k[i], sigma = params$sigma[i])
  }

#generate probabilities
sim_ccl <- matrix(0, nrow = 1000, ncol = nrow(params))
colnames(sim_ccl) <- unique(params$name)

#apply to all systems
for (i in 1:nrow(params)){
  sim_ccl[,i] <- simulate_ccl(i)
}

#gather for tall dataframe
sim_ccl <- gather(as.data.frame(sim_ccl), dplyr::starts_with("customer"), key = "name", value = "ccl")

#run below twice, why?
sim_ccl <- sim_ccl %>% group_by(name) %>% mutate(n = n(),
                                         ccdf = seq(1,0, length.out = n)) %>% 
                                  arrange(ccl)

sim_ccl <- sim_ccl %>% group_by(name) %>% mutate(n = n(),
                                                 ccdf = seq(1,0, length.out = n)) %>% 
  arrange(ccl)

ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = d, size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = sim_ccl, size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  theme_bw()

ggsave(filename = "plots/indv_plot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,
       width = 430, height = 250, units = "mm")

rm(d); rm(params); rm(params_extra)
rm(sim_ccl); rm(thresholds); rm(i); rm(simulate_ccl)
