#read in thresholds (note threshold determined using the POT package)
#see the script "threshold_estimation.R" in dir "scripts/old & misc/"

#raw data
d <- read_csv("data/ccl.csv")

thresholds <- read_csv("data/customer-thresholds.csv")

d <- d %>%
  left_join(thresholds, by = 'name') %>% 
  filter(ccl >= threshold) %>%  
  select(name, ccl) %>%  
  arrange(name, ccl) 

#create empirical ccdf
d <- d %>% group_by(name) %>% mutate(n = n(),
                                     ccdf = seq(1, 0, length.out = n)) 

#plot points
samples <- read_csv("data/multi-level-samples.csv")

#specifying quantiles of interest
mean <- data.frame(k_mu = mean(samples$k_mu),
                   sigma_mu = mean(samples$sigma_mu))

q_975 <- data.frame(k_mu = quantile(samples$k_mu, 0.975),
                    sigma_mu = quantile(samples$sigma_mu, 0.975))

q_025 <- data.frame(k_mu = quantile(samples$k_mu, 0.025),
                    sigma_mu = quantile(samples$sigma_mu, 0.025))

q_75 <- data.frame(k_mu = quantile(samples$k_mu, 0.75),
                   sigma_mu = quantile(samples$sigma_mu, 0.75))

q_25 <- data.frame(k_mu = quantile(samples$k_mu, 0.25),
                   sigma_mu = quantile(samples$sigma_mu, 0.25))

quantiles <- bind_rows(mean, q_975, q_025, q_75, q_25)

#generate probabilities for CCL along quantiles (function)
generate_probs <- function(i){
  pgpd(seq(from = 0, to = 20, by = 0.01), mu = mean(thresholds$threshold), xi = quantiles$k_mu[i], sigma = quantiles$sigma_mu[i], lower.tail = FALSE) 
}

#create NULL matrix
ccl_probs <- matrix(0, nrow = length(seq(from = 0, to = 20, by = 0.01)), ncol = nrow(quantiles))

#generate probs
for (i in 1:nrow(quantiles)){
  ccl_probs[,i] <- generate_probs(i)
}

ccl_probs <- as.data.frame(ccl_probs)
colnames(ccl_probs) <- c("mean", "q_975", "q_025", "q_75", "q_25")

#gather for tall dataframe
ccl_probs <- gather(as.data.frame(ccl_probs), key = "name", value = "ccdf")

#add CCDF
ccl_probs <- ccl_probs %>% group_by(name) %>% mutate(n = n(),
                                                     ccl = seq(from = 0, to = 20, by = 0.01)) %>% 
  arrange(name, desc(ccdf))

#subset data for plotting
mean <- ccl_probs %>% filter(name == "mean")
q_975 <- ccl_probs %>% filter(name == "q_975")
q_025 <- ccl_probs %>% filter(name == "q_025")
q_75 <- ccl_probs %>% filter(name == "q_75")
q_25 <- ccl_probs %>% filter(name == "q_25")

#plot it
ggplot() + 
  geom_point(aes(ccl, ccdf), alpha = .3, data = d, size = 1, pch = 4) +
  geom_line(aes(ccl, ccdf, col = "Mean"), data = mean, size = 1) +
  geom_line(aes(ccl, ccdf, col = "95% Interval"),  data = q_975, size = 1) +
  geom_line(aes(ccl, ccdf, col = "95% Interval"), data = q_025, size = 1) +
  geom_line(aes(ccl, ccdf, col = "50% Interval"),  data = q_75, size = 1) +
  geom_line(aes(ccl, ccdf, col = "50% Interval"), data = q_25, size = 1) +
  coord_trans(y="log10", limx=c(0,20), limy=c(1e-3,1)) +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 20, by = 1)), limits=c(0,20), expand = c(0,0)) +
  geom_segment(mapping = aes(x = 0, xend=1, y=1-pgpd(1, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)), 
                             yend = 1-pgpd(1, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu))), lty = 2, col = "black") +
  geom_segment(mapping = aes(x = 1, xend=1, y=1-pgpd(1, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)), yend = 0.001), lty = 2, col = "black")  +
  geom_segment(mapping = aes(x = 0, xend=2, y=1-pgpd(2, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)), 
                             yend = 1-pgpd(2, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu))), lty = 2, col = "black") +
  geom_segment(mapping = aes(x = 2, xend=2, y=1-pgpd(2, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)), yend = 0.001), lty = 2, col = "black")  +
  geom_segment(mapping = aes(x = 0, xend=3, y=1-pgpd(3, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)), 
                             yend = 1-pgpd(3, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu))), lty = 2, col = "black") +
  geom_segment(mapping = aes(x = 3, xend=3, y=1-pgpd(3, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)), yend = 0.001), lty = 2, col = "black")  +
  annotate(geom = "text", x = 1, y = 1-pgpd(1, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)),
           label = "P(CCL > 1) = 45%", 
           hjust = 0, vjust = 0, size = 6) +
  annotate(geom = "text", x = 2, y = 1-pgpd(2, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)),
           label = "P(CCL > 2) = 15%", 
           hjust = 0, vjust = 0, size = 6) +
  annotate(geom = "text", x = 3, y = 1-pgpd(3, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu)),
           label = "P(CCL > 3) = 6%", 
           hjust = 0, vjust = 0, size = 6) +
  annotate(geom = "text", x = mean$ccl[[which.min(abs(mean$ccdf - 0.01))]], y = .01,
           label = "P(CCL > 5.88) = 1%", 
           hjust = 0, vjust = 0, size = 6) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  geom_segment(mapping = aes(x = 0, xend=mean$ccl[[which.min(abs(mean$ccdf - 0.01))]], y=.01, yend = .01), lty = 2, col = "black") +
  geom_segment(mapping = aes(x = mean$ccl[[which.min(abs(mean$ccdf - 0.01))]], xend=mean$ccl[[which.min(abs(mean$ccdf - 0.01))]], y=0.01, yend = 0.001), lty = 2, col = "black")  +
  scale_color_manual(values = c("Mean" = "blue", 
                                "95% Interval" = "red",
                                "50% Interval" = "green")) +
  labs(y = "CCDF: 1 - P(X < x)", x = "Cyclical Cooling Load", color = "Quantile") + 
  theme(legend.position="bottom")

ggsave(filename = "plots/mlm_plot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,
       width = 330, height = 175, units = "mm")

#CCL's at a given probability (with intervals)
#median
mean$ccl[[which.min(abs(mean$ccdf - 0.5))]] 
q_25$ccl[[which.min(abs(q_25$ccdf - 0.5))]]
q_75$ccl[[which.min(abs(q_75$ccdf - 0.5))]] 
q_025$ccl[[which.min(abs(q_025$ccdf - 0.5))]]
q_975$ccl[[which.min(abs(q_975$ccdf - 0.5))]] 

#95%
mean$ccl[[which.min(abs(mean$ccdf - 0.05))]] 
q_25$ccl[[which.min(abs(q_25$ccdf - 0.05))]]
q_75$ccl[[which.min(abs(q_75$ccdf - 0.05))]] 
q_025$ccl[[which.min(abs(q_025$ccdf - 0.05))]]
q_975$ccl[[which.min(abs(q_975$ccdf - 0.05))]] 

#99%
mean$ccl[[which.min(abs(mean$ccdf - 0.01))]] 
q_25$ccl[[which.min(abs(q_25$ccdf - 0.01))]]
q_75$ccl[[which.min(abs(q_75$ccdf - 0.01))]] 
q_025$ccl[[which.min(abs(q_025$ccdf - 0.01))]]
q_975$ccl[[which.min(abs(q_975$ccdf - 0.01))]] 

#probabilities at a given CCL
#CCL = 1
1- pgpd(1, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu))
1- pgpd(1, mu = mean(thresholds$threshold), xi = quantile(samples$k_mu, 0.75), sigma = quantile(samples$sigma_mu, 0.75))
1- pgpd(1, mu = mean(thresholds$threshold), xi = quantile(samples$k_mu, 0.25), sigma = quantile(samples$sigma_mu, 0.25))

1- pgpd(2, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu))
1- pgpd(3, mu = mean(thresholds$threshold), xi = mean(samples$k_mu), sigma = mean(samples$sigma_mu))


rm(d); rm(mean); rm(q_025); rm(q_975); rm(q_25); rm(q_75); rm(quantiles)
rm(ccl_probs); rm(generate_probs); rm(i); rm(samples)
