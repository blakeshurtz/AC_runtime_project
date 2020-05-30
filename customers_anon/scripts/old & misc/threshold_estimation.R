#estimate threshold from data
hist(x)
mean(x)
mrlplot(x, col = c("green", "black", "green"), nt = 20) #mean residual life plot
abline(v = quantile(summary$fccc, .9)) #placeholder to check threshold
tcplot(x, which = 1) #scale
abline(v = quantile(summary$fccc, .9)) #placeholder to check threshold
tcplot(x, which = 2) #shape
abline(v = quantile(summary$fccc, .9)) #placeholder to check threshold
