#plots
fccc_cl = 0.68 #insert from spreadsheet

#way to adjust pch?
retlev(mle, npy = 1, points = TRUE,
       col = "green", lty = 2, lwd = 2, ylab = "fccc", xlab = "# of Cycles")
abline(h = fccc_cl, col = "blue", lty = 2)
pp(mle) 
qq(mle)
dens(mle, plot.kernel = FALSE, rug= TRUE) #solid line is empirical, dashed is GPD

#inference for return level
confint(mle, prob = 0.95) #doesn't work as well
gpd.firl(mle, prob = 0.95) 
gpd.pfrl(mle, prob = 0.95, vert.lines = TRUE) 

##inference for scale
gpd.fiscale(mle, conf = 0.95)
gpd.pfscale(mle, conf = 0.8, range = c(.03, .10))
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

###probability of fccc > 1
pgpd(1, scale = mle$param[[1]], shape = mle$param[[2]], lower.tail = FALSE)
##Evaluate probability of fccc_cl exceeding threshold. What is the target? 
pgpd(fccc_cl, scale = mle$param[[1]], shape = mle$param[[2]], lower.tail = FALSE)
##Compute the quantile with non-exceedance probability 0.99:
qgpd(.99, scale = mle$param[[1]], shape = mle$param[[2]], lower.tail = TRUE)

