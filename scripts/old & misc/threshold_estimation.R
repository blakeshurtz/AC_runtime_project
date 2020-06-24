#the POT package is used for threshold estimation
library(POT)
#read in ccl
d <- read_csv("data/ccl.csv")

#read in thresholds
thresholds <- read_csv("data/customer-thresholds.csv")

#join data
d <- d %>%
  left_join(thresholds, by = 'name') %>% 
# filter(ccl >= threshold) %>%  
  select(name, ccl, threshold) %>%  
  arrange(name, ccl) 

#select customer
c6 <- d %>% filter(name == "customer_06")
x <- c6$ccl
threshold <- c6 %>% select(threshold) %>% summarise(threshold = unique(threshold))
threshold <- as.numeric(threshold)

#plain mle
mle <- fitgpd(x, thresh = threshold, est = "mle")

par(mfrow = c(2,2))
dens(mle, plot.kernel = FALSE, rug= TRUE,
     main = "Density Plot for CCL", xlab = "CCL")
abline(v = threshold, col = "blue", lty = 2)#solid line is empirical, dashed is GPD
mrlplot(x, col = c("green", "black", "green"), nt = 20) #mean residual life plot
abline(v = threshold, col = "blue", lty = 2) #placeholder to check threshold
tcplot(x, which = 1, lty = 2) #scale
abline(v = threshold, col = "blue", lty = 2) #placeholder to check threshold
tcplot(x, which = 2) #shape
abline(v = threshold, col = "blue", lty = 2) #placeholder to check threshold

