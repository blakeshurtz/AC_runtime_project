###fccc
#function to fit MLE
customer_mle <- function(x, y){
  fccc <- d %>% filter(name == x) %>% select(fccc) %>% unlist()
  fitgpd(fccc, thresh = y, est = "mle")
}

#store percent above threshold (pat) and model parameters
fccc_parameters <- tibble(pat = rep(0, nrow(thresholds)), 
                          scale = rep(0, nrow(thresholds)),
                          shape = rep(0, nrow(thresholds)))

for (i in 1:nrow(thresholds)){
  fccc_parameters$pat[i] <- customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")), thresholds$threshold[i])$pat
  fccc_parameters$scale[i] <- customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")), thresholds$threshold[i])$param[[1]]
  fccc_parameters$shape[i] <- customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")), thresholds$threshold[i])$param[[2]]
}