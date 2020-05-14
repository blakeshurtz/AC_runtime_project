library(POT)
?rgpd
##Simulate a sample from a GPD(0,1,0.2):
x <- rgpd(100, 0, 1, 0.2)
hist(x)
##Evaluate density at x=3 and probability of non-exceedance:
dgpd(3, 0, 1, 0.2);
pgpd(3, 0, 1, 0.2)
##Compute the quantile with non-exceedance probability 0.95:
qgpd(0.95, 0, 1, 0.2)



mrlplot(x) #c(0.15, 0.3)
tcplot(x)

##Maximum likelihood estimate (threshold = 0):
mle <- fitgpd(x, 0)
##Maximum Goodness-of-Fit estimators:
fitgpd(x, 0, "mgf", stat = "ADR")
##Specifying a known parameter:
fitgpd(x, 0, "mple", shape = 0.2)
##Specifying starting values for numerical optimizations:
fitgpd(x, 0, "mdpd", start = list(scale = 1, shape = 0.2))

##Generic function for the univariate and bivariate cases:
plot(mle)
##Return level plots:
retlev(mle, npy = 2)
##Probability-Probability and Q-Q plots:
pp(mle); qq(mle)
##Plot the density:
dens(mle)
##Profile Likelihood (quantiles):
confint(mle, prob = 0.95)
gpd.fiscale(mle)

##Profile Likelihood (parameters):
confint(mle, "scale"); 
gpd.pfscale(mle, range = c(1, 2), conf = 0.9)

###profile shape
confint(mle, "shape", prob = 0.5)
gpd.pfshape(mle, range = c(1, 2), conf = 0.8)
