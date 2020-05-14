library(readr)
library(POT)
d <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/Brick/load.csv")
x <- d$x

#estimate threshold from data
hist(x)
mrlplot(x, col = c("green", "black", "green"), nt = 200) #mean residual life plot
tcplot(x, which = 1) #scale
tcplot(x, which = 2) #shape
threshold <- .12

#estimate parameters using various algorithms
mlep <- fitgpd(x, thresh = threshold, est = "mle")$param
mom <- fitgpd(x, thresh = threshold, "moments")$param
mle <- fitgpd(x, thresh = threshold, "mle")$param
pwmu <- fitgpd(x, thresh = threshold, "pwmu")$param
pwmb <- fitgpd(x, thresh = threshold, "pwmb")$param
pickands <- fitgpd(x, thresh = threshold, "pickands")$param
med <- fitgpd(x, thresh = threshold, "med", start = list(scale = .01, shape = 0.01))$param
mdpd <- fitgpd(x, thresh = threshold, "mdpd")$param
mple <- fitgpd(x, thresh = threshold, "mple")$param
ad2r <- fitgpd(x, thresh = threshold, "mgf", stat = "AD2R")$param
print(rbind(mom, mlep, pwmu, pwmb, pickands, med, mdpd, mple, ad2r))

#plain mle
mle <- fitgpd(x, thresh = threshold, est = "mle")
print(mle)

'
#markov chains for exceedances, alternative form of parameter estimation...
mc <- simmc(1000, alpha = 0.5, model = "log") #random values
mc <- qgpd(mc, loc= threshold, scale = mle$param[[1]], shape = mle$param[[2]]) #run values through likelihood
hist(mc)
hist(subset(x, x > threshold))
fitmcgpd(mc, threshold, "log")
mle <- fitgpd(mc, thresh = threshold, est = "mle")
'

#plots
#par(mfrow=c(2,2)); plot(mle)
'npy is mean number of events per year... based on observations instead'
retlev(mle, npy = 1, ylim = c(0, 1), xlab = "Return Period (Obserations)")
pp(mle) 
qq(mle)
dens(mle) #solid line is empirical, dashed is GPD


#inference for threshold
#confint(mle, prob = 0.95) #doesn't work as well
gpd.firl(mle, prob = 0.95) #for threshold...?
gpd.pfrl(mle, prob = 0.95, range = c(.23, .5), vert.lines = TRUE)

##inference for scale
gpd.fiscale(mle, conf = 0.95)
#gpd.pfscale(mle, conf = 0.8, range = c(.03, .10))
confint(mle, "scale", 0.8); #better graph than gpd.pfscale

#inference for shape
gpd.fishape(mle, conf = 0.95)
gpd.pfshape(mle, conf = 0.95, range = c(-0.5, .18))
confint(mle, "shape") #better graph than gpd.pfshape

##Evaluate density at cooling load and probability of non-exceedance:
cyclical_cooling_load = 23/26.4 #cl / capacity
dgpd(cyclical_cooling_load, loc= 0, scale = mle$param[[1]], shape = mle$param[[2]]);
pgpd(cyclical_cooling_load, loc= 0, scale = mle$param[[1]], shape = mle$param[[2]], lower.tail = FALSE)
##Compute the quantile with non-exceedance probability 0.95:
qgpd(cyclical_cooling_load, loc= 0, scale = mle$param[[1]], shape = mle$param[[2]], lower.tail = FALSE)

#prob2rp
prob <- pgpd(threshold, loc= 0, scale = mle$param[[1]], shape = mle$param[[2]])
prob2rp(prob, 1)
