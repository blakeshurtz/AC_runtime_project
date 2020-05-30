#estimate parameters using various algorithms
mlep <- fitgpd(x, thresh = threshold, est = "mle")$param
mom <- fitgpd(x, thresh = threshold, "moments")$param
mle <- fitgpd(x, thresh = threshold, "mle")$param
pwmu <- fitgpd(x, thresh = threshold, "pwmu")$param
pwmb <- fitgpd(x, thresh = threshold, "pwmb")$param
med <- fitgpd(x, thresh = threshold, "med")$param
mple <- fitgpd(x, thresh = threshold, "mple")$param
ad2r <- fitgpd(x, thresh = threshold, "mgf", stat = "AD2R")$param
print(rbind(mom, mlep, pwmu, pwmb, med, mple, ad2r))
