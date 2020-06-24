#it is NOT recommended that you run this script in isolation
#it is recommended that you open "AC_runtime_project" R project
#and run the "primary_script.r" for all analysis

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#read in dataset
d <- read_csv("data/ccl.csv")

#read in thresholds
thresholds <- read_csv("data/customer-thresholds.csv")
ymin = thresholds$threshold

d <- d %>%
  left_join(thresholds, by = 'name') %>% 
  filter(ccl >= threshold) %>%  
  select(name, ccl) %>%  
  arrange(name, ccl) 

#determine max per group
ymax <- d %>% 
  group_by(name) %>% 
  summarise(ymax = max(ccl)) 

#define parameters for model
ymax <- as.numeric(ymax$ymax)
y <- as.numeric(d$ccl)
N = nrow(d)
K = length(unique(d$name))
s <- as.numeric(table(d$name))
ds <- list(ymin = ymin,
           ymax = ymax,
           N = N, 
           y = y,
           K = K, 
           s = s)

#compile and run stan model
ret <- stanc(file = "scripts/stan-models/individual-effects.stan")
ret_sm <- stan_model(stanc_ret = ret)
m_ind <- sampling(ret_sm, warmup = 1000, iter = 6000, seed = 4134, data = ds, 
                cores = parallel::detectCores(), chains = 4,
                control = list(adapt_delta = 0.99))

#calculate means across all chains
means <- get_posterior_mean(m_ind)
means <- as.data.frame(means)
means <- cbind(variable = rownames(means), means)

k <- means %>% select(variable, "mean-all chains") %>% 
  filter(!str_detect(variable, "k_offset|sigma|lp")) %>% 
  select("mean-all chains")
k <- as.numeric(k$`mean-all chains`)

sigma <- means %>% select(variable, "mean-all chains") %>% 
  filter(str_detect(variable, "sigma")) %>% 
  select("mean-all chains")
sigma <- as.numeric(sigma$`mean-all chains`)

means <- data.frame(name = unique(d$name),
                    threshold = ymin,
                    sigma = sigma,
                    k = k)

#export data
write.csv(means, file = "data/indv_params_means.csv", row.names = FALSE)

#export the model
saveRDS(m_ind, file = "models/individual_effects_model.rds")

#clean it up
rm(thresholds)
rm(d); rm(ds); rm(ret); rm(ret_sm)
rm(K); rm(N); rm(s); rm(y); rm(ymax); rm(ymin)
rm(m_ind); rm(means)
rm(k); rm(sigma)
