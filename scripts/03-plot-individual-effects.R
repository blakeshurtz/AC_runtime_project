#read in dataset
d <- read_csv("data/ccl.csv")

#read in thresholds (note threshold determined using the POT package)
#see the script "threshold_estimation.R" in dir "scripts/old & misc/"
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
  pgpd(seq(from = 0, to = 10, by = 0.01), mu = params$threshold[i], xi = params$k[i], sigma = params$sigma[i], lower.tail = FALSE) 
}

#generate probabilities
sim_ccl <- matrix(0, nrow = 1001, ncol = nrow(params))
colnames(sim_ccl) <- unique(params$name)

#apply to all systems
for (i in 1:nrow(params)){
  sim_ccl[,i] <- simulate_ccl(i)
}

#gather for tall dataframe
sim_ccl <- gather(as.data.frame(sim_ccl), dplyr::starts_with("customer"), key = "name", value = "ccdf")

#add CCDF
sim_ccl <- sim_ccl %>% group_by(name) %>% mutate(n = n(),
                                                 ccl = seq(from = 0, to = 10, by = 0.01)) %>% 
  arrange(name, desc(ccdf))


g1 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_01", "customer_02")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_01", "customer_02")), size = 1, show.legend = FALSE) +
  theme_bw() +
  facet_wrap(vars(name)) +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load")

g2 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_03", "customer_04")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_03", "customer_04")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load")


g3 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_05", "customer_06")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_05", "customer_06")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load")

g4 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_07", "customer_08")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_07", "customer_08")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load")

g5 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_09", "customer_10")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_09", "customer_10")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load")

g6 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_11", "customer_12")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_11", "customer_12")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load") 

g7 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_13", "customer_14")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_13", "customer_14")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load") 

g8 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_15", "customer_16")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_15", "customer_16")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load")

g9 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_17", "customer_18")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_17", "customer_18")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load") 

g10 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_19", "customer_20")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_19", "customer_20")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load")

g11 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("customer_21", "customer_22")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(sim_ccl, name %in% c("customer_21", "customer_22")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load") 


g <- arrangeGrob(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, ncol = 1)

ggsave(filename = "plots/indv_plot.jpeg", plot = g, device = "jpeg", dpi = 300,
       width = 430, height = 1150, units = "mm")

rm(d); rm(params); 
rm(sim_ccl); rm(thresholds); rm(i); rm(simulate_ccl);
rm(g1); rm(g2); rm(g3); rm(g4); rm(g5); rm(g6); rm(g7); rm(g8); rm(g9); rm(g10); rm(g11); rm(g)
