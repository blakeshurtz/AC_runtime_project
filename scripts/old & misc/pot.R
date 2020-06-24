library(tidyverse)
library(lubridate)
library(readxl)
library(broom)
library(POT)

#import data
d <- read_csv("customers_anon/data/ccl.csv")
customer_data <- read_excel("customers_anon/data/customer_data.xlsx", 
                            col_types = c("text", "date", "date", "numeric", "numeric", "numeric"))

thresh = 0.3

#function to fit MLE
customer_mle <- function(x){
  ccl <- d %>% filter(name == x) %>% select(ccl) %>% unlist()
  fitgpd(ccl, thresh = thresh, est = "mple")
}

#store percent above threshold (pat) and model parameters
ccl <- tibble(name = rep(0, nrow(customer_data)),
              pat = rep(0, nrow(customer_data)), 
               scale = rep(0, nrow(customer_data)),
               shape = rep(0, nrow(customer_data)),
               scale_error = rep(0, nrow(customer_data)),
               shape_error = rep(0, nrow(customer_data)),
               max_d = rep(0, nrow(customer_data)),
               ret_lvl_lwr = rep(0, nrow(customer_data)),
               ret_lvl_est = rep(0, nrow(customer_data)),
               ret_lvl_upr = rep(0, nrow(customer_data)))

for (i in 1:nrow(customer_data)){
  ccl$name[i] <- customer_data$name[i]
  ccl$pat[i] <- customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")))$pat
  ccl$scale[i] <- customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")))$param[[1]]
  ccl$shape[i] <- customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")))$param[[2]]
  ccl$scale_error[i] <- customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")))$std.err[[1]]
  ccl$shape_error[i] <- customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")))$std.err[[2]]
  ccl$max_d[i] <- max(subset(d, name == paste0("customer_", str_pad(i, width=2, side="left", pad="0")), select = ccl))
  ccl$ret_lvl_lwr[i] <- gpd.firl(customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0"))), prob = 0.99)[[1]]
  ccl$ret_lvl_est[i] <- qgpd(.99, loc = 1,
                                    scale = customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")))$param[[1]],
                                   shape = customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0")))$param[[2]], 
                                   lower.tail = TRUE)
  ccl$ret_lvl_upr[i] <- gpd.firl(customer_mle(paste0("customer_", str_pad(i, width=2, side="left", pad="0"))), prob = 0.99)[[2]]
}

write.csv(ccl, file = "customers_anon/data/ccl_params.csv", row.names = FALSE)

pos_shape <- ccl %>% filter(shape > 0)

write.csv(pos_shape, file = "customers_anon/data/ccl_params_pos.csv", row.names = FALSE)
