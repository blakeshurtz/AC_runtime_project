library(readr)
library(POT)
d <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/silas/fccc.csv")
x <- d$x


#estimate threshold from data
hist(x)
mean(x)
mrlplot(x, col = c("green", "black", "green"), nt = 20) #mean residual life plot
abline(v = 1) #placeholder to check threshold
tcplot(x, which = 1) #scale
tcplot(x, which = 2) #shape
threshold <- 1

#estimate parameters using various algorithms
mlep <- fitgpd(x, thresh = threshold, est = "mle")$param
mom <- fitgpd(x, thresh = threshold, "moments")$param
mle <- fitgpd(x, thresh = threshold, "mle")$param
pwmu <- fitgpd(x, thresh = threshold, "pwmu")$param
pwmb <- fitgpd(x, thresh = threshold, "pwmb")$param
med <- fitgpd(x, thresh = threshold, "med")$param
mdpd <- fitgpd(x, thresh = threshold, "mdpd")$param
mple <- fitgpd(x, thresh = threshold, "mple")$param
ad2r <- fitgpd(x, thresh = threshold, "mgf", stat = "AD2R")$param
print(rbind(mom, mlep, pwmu, pwmb, med, mdpd, mple, ad2r))

#plain mle
mle <- fitgpd(x, thresh = threshold, est = "mle")
mle
mle$pat
mle$param

#plots
fccc_cl = 0.94 #insert from spreadsheet

#way to adjust pch?
retlev(mle, npy = 1, points = TRUE,
       col = "green", lty = 2, lwd = 2, ylab = "fccc", xlab = "# of Cycles")
abline(h = fccc_cl, col = "blue", lty = 2)
pp(mle) 
qq(mle)
dens(mle, plot.kernel = FALSE, rug= TRUE) #solid line is empirical, dashed is GPD

#inference for return level
confint(mle, prob = 0.95, range = c(5,20)) #doesn't work as well
gpd.firl(mle, prob = 0.95) 
gpd.pfrl(mle, prob = 0.95, vert.lines = TRUE) 

##inference for scale
gpd.fiscale(mle, conf = 0.95)
gpd.pfscale(mle, conf = 0.8, range = c(.03, 1.5))
confint(mle, "scale", 0.8); #better graph than gpd.pfscale

#inference for shape
gpd.fishape(mle, conf = 0.95)
gpd.pfshape(mle, conf = 0.95, range = c(-0.5, .18))
confint(mle, "shape") #better graph than gpd.pfshape

'
#markov chains for exceedances, alternative form of parameter estimation...
mc <- simmc(1000, alpha = 0.5, model = "log") #random values
mc <- qgpd(mc, loc = threshold, scale = mle$param[[1]], shape = mle$param[[2]]) #run values through likelihood
mcmc <- fitmcgpd(mc, threshold, "log")
'

1/.65

###probability of fccc > 1
pgpd(1, scale = mle$param[[1]], shape = mle$param[[2]], lower.tail = FALSE) #why 1?
##Evaluate probability of fccc_cl exceeding threshold. What is the target? 
pgpd(fccc_cl, scale = mle$param[[1]], shape = mle$param[[2]], lower.tail = FALSE)
##Compute the quantile with non-exceedance probability 0.95:
qgpd(.95, scale = mle$param[[1]], shape = mle$param[[2]], lower.tail = TRUE)

par(mfrow=c(2,2))
pp(mle) 
qq(mle)
retlev(mle, npy = 1, points = TRUE,
       col = "green", lty = 2, lwd = 2, ylab = "fccc", xlab = "# of Cycles")
confint(mle, prob = 0.95, main = "Return Level Likelihood Plot") #doesn't work as well

    