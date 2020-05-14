set.seed(1234)
library(readr)
Bell <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/Bell/Bell.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))
model <- kmeans(Bell$amps, 3, nstart = 50, iter.max = 10)

thresholds <- model$centers[,]
thresholds <- sort(thresholds) 

library(dplyr)

Bell$on <- if_else(Bell$amps < 1, 0, 1)

Bell$staging <- if_else(Bell$amps < 1, 0, 
                if_else(between(Bell$amps, thresholds[2], thresholds[3]), 1,
                if_else(Bell$amps > thresholds[3], 2, 5)
                ))

Bell$capacity <- if_else(Bell$staging == 1, (2*.8)/60,
                         if_else(Bell$staging == 2, (3*.8)/60, 0))

Bell$cycle <- with(rle(Bell$on), rep(seq_along(lengths), lengths)) 


summary <- Bell %>% 
  group_by(cycle) %>% 
  summarise(load = sum(capacity)/2.4) %>% 
  filter(load != 0) %>% 
  arrange(desc(load))

#write.csv(summary$load, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/Bell/load.csv", row.names = FALSE)

d <- summary$load
