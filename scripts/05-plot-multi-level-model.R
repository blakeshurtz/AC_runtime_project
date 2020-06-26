#plot points
d <- read_csv("data/multi-level-samples.csv")

mean <- data.frame(k_mu = mean(d$k_mu),
                   sigma_mu = mean(d$sigma_mu))
mean <- rgpd(100000, mu = 0, xi = mean$k_mu, sigma = mean$sigma_mu)

q_975 <- data.frame(k_mu = quantile(d$k_mu, 0.975),
                   sigma_mu = quantile(d$sigma_mu, 0.975))
q_975 <- rgpd(100000, mu = 0, xi = q_975$k_mu, sigma = q_975$sigma_mu)

q_025 <- data.frame(k_mu = quantile(d$k_mu, 0.025),
                   sigma_mu = quantile(d$sigma_mu, 0.025))
q_025 <- rgpd(100000, mu = 0, xi = q_025$k_mu, sigma = q_025$sigma_mu)

quantiles <- data.frame(mean = mean,
                        q_975 = q_975,
                        q_025 = q_025)

#gather for tall dataframe
quantiles <- gather(as.data.frame(quantiles), key = "name", value = "ccl")

#run below twice, why?
quantiles <- quantiles %>% group_by(name) %>% mutate(n = n(),
                                                     ccdf = seq(1,0, length.out = n)) %>% 
  arrange(ccl)

quantiles <- quantiles %>% group_by(name) %>% mutate(n = n(),
                                                     ccdf = seq(1,0, length.out = n)) %>% 
  arrange(ccl)


#select first 100 samples
d <- d[1:1000,]

simulate_ml <- function(i){
  rgpd(1000, mu = 0, xi = d$k_mu[i], sigma = d$sigma_mu[i])
}

#generate probabilities
sim_ccl <- matrix(0, nrow = 1000, ncol = nrow(d))

#apply to all systems
for (i in 1:nrow(d)){
  sim_ccl[,i] <- simulate_ml(i)
}

sim_ccl <- as.data.frame(sim_ccl)

#gather for tall dataframe
sim_ccl <- gather(as.data.frame(sim_ccl), key = "name", value = "ccl")

#run below twice, why?
sim_ccl <- sim_ccl %>% group_by(name) %>% mutate(n = n(),
                                                 ccdf = seq(1,0, length.out = n)) %>% 
  arrange(ccl)

sim_ccl <- sim_ccl %>% group_by(name) %>% mutate(n = n(),
                                                 ccdf = seq(1,0, length.out = n)) %>% 
  arrange(ccl)

mean <- quantiles %>% filter(name == "mean")
q_975 <- quantiles %>% filter(name == "q_975")
q_025 <- quantiles %>% filter(name == "q_025")

ggplot() + 
#  geom_line(aes(ccl, ccdf), alpha = .1, data = sim_ccl, size = 1) +
  geom_line(aes(ccl, ccdf, col = "Mean"), data = mean, size = 1) +
  geom_line(aes(ccl, ccdf, col = "95% Interval"),  data = q_025, size = 1) +
  geom_line(aes(ccl, ccdf, col = "95% Interval"), data = q_975, size = 1) +
  coord_trans(y="log10", limx=c(0,20), limy=c(1e-3,1)) +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 20, by = 1)), limits=c(0,20), expand = c(0,0)) +
  geom_segment(mapping = aes(x = 0, xend=1, y=1-pgpd(1, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)), 
               yend = 1-pgpd(1, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu))), lty = 2, col = "black") +
  geom_segment(mapping = aes(x = 1, xend=1, y=1-pgpd(1, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)), yend = 0.001), lty = 2, col = "black")  +
  geom_segment(mapping = aes(x = 0, xend=2, y=1-pgpd(2, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)), 
                             yend = 1-pgpd(2, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu))), lty = 2, col = "black") +
  geom_segment(mapping = aes(x = 2, xend=2, y=1-pgpd(2, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)), yend = 0.001), lty = 2, col = "black")  +
  geom_segment(mapping = aes(x = 0, xend=3, y=1-pgpd(3, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)), 
                             yend = 1-pgpd(3, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu))), lty = 2, col = "black") +
  geom_segment(mapping = aes(x = 3, xend=3, y=1-pgpd(3, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)), yend = 0.001), lty = 2, col = "black")  +
  annotate(geom = "text", x = 1, y = 1-pgpd(1, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)),
           label = "P(CCL>1) = 29%", 
           hjust = 0, vjust = 0, size = 8) +
  annotate(geom = "text", x = 2, y = 1-pgpd(2, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)),
           label = "P(CCL>2) = 9%", 
           hjust = 0, vjust = 0, size = 8) +
  annotate(geom = "text", x = 3, y = 1-pgpd(3, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu)),
           label = "P(CCL>3) = 6%", 
           hjust = 0, vjust = 0, size = 8) +
  annotate(geom = "text", x = quantile(mean$ccl, c(.99)), y = .01,
           label = "P(CCL>6.63) = 1%", 
           hjust = 0, vjust = 0, size = 8) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  geom_segment(mapping = aes(x = 0, xend=quantile(mean$ccl, c(.99)), y=.01, yend = .01), lty = 2, col = "black") +
  geom_segment(mapping = aes(x = quantile(mean$ccl, c(.99)), xend=quantile(mean$ccl, c(.99)), y=0.01, yend = 0.001), lty = 2, col = "black")  +
  scale_color_manual(values = c("Mean" = "blue", "95% Interval" = "red")) +
  labs(y = "CCDF: 1 - P(X<x)", x = "Cyclical Cooling Load", color = "Quantile") + 
  theme(legend.position="bottom")

ggsave(filename = "plots/mlm_plot.jpeg", plot = last_plot(), device = "jpeg", dpi = 300,
       width = 430, height = 250, units = "mm")

#parameter values
quantile(mean$ccl, c(.5))
quantile(q_975$ccl, c(.5))
quantile(q_025$ccl, c(.5))

pgpd(1, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu))
pgpd(1, mu = 0, xi = quantile(d$k_mu, 0.975), sigma = quantile(d$sigma_mu, 0.975))
pgpd(1, mu = 0, xi = quantile(d$k_mu, 0.025), sigma = quantile(d$sigma_mu, 0.025))

pgpd(2, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu))
pgpd(3, mu = 0, xi = mean(d$k_mu), sigma = mean(d$sigma_mu))

quantile(mean$ccl, c(.95))
quantile(q_975$ccl, c(.95))
quantile(q_025$ccl, c(.95))

rm(d); rm(mean); rm(q_025); rm(q_975); rm(quantiles)
rm(sim_ccl); rm(simulate_ml); rm(i)
