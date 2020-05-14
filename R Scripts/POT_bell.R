library(POT)

hist(d)

mrlplot(d) #c(0.15, 0.3)
tcplot(d)
mle <- fitgpd(d, thresh = .13, est = "mle") #I see an outlier
print(mle)
##Generic function for the univariate and bivariate cases:
plot(mle)
##Return level plots:
?retlev
retlev(mle, ylim=c(0, 1)) #data are hourly, basically...
##Probability-Probability and Q-Q plots:
pp(mle); 
qq(mle)
##Plot the density:
dens(mle)
##Profile Likelihood (quantiles):
confint(mle, prob = 0.8)
##Profile Likelihood (parameters):
confint(mle, "scale", 0.8); 
confint(mle, "shape")
###MLEs


print(mle)
##Evaluate density at x=3 and probability of non-exceedance:
?dgpd
dgpd(.13, loc= 0, scale = 0.03296, shape = 0.27009);
pgpd(.13, loc= 0, scale = 0.03296, shape = 0.27009)
##Compute the quantile with non-exceedance probability 0.95:
qgpd(.13, loc= 0, scale = 0.03296, shape = 0.27009)

