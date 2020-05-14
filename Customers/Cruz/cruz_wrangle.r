set.seed(1234)
library(readr)
Cruz <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/Cruz/Cruz.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))

#kmeans doesn't work as well
#model <- kmeans(Cruz$amps, 3, nstart = 50, iter.max = 10)
thresholds <- c(1, 5, 8)

#add vars
library(dplyr)
Cruz$on <- if_else(Cruz$amps < 1, 0, 1)
Cruz$staging <- if_else(Cruz$amps < 1, 0, 
                        if_else(between(Cruz$amps, thresholds[2], thresholds[3]), 1,
                                if_else(Cruz$amps > thresholds[3], 2, 5)
                        ))

#staging account for manual s
low_stage = (2 * .8)
high_stage = (3 * .8)

Cruz$capacity <- if_else(Cruz$staging == 1, low_stage/60,
                         if_else(Cruz$staging == 2, high_stage/60, 0))

Cruz$cycle <- with(rle(Cruz$on), rep(seq_along(lengths), lengths)) 

Cruz$lengths <- with(rle(Cruz$on), rep(seq_along(lengths), lengths))

summary <- Cruz %>% 
  group_by(cycle) %>% 
  summarise(load = sum(capacity)/high_stage,
            runtime = length(cycle)/60,
            n = ifelse(load > 1, load/runtime, load)) %>% 
  filter(load != 0) %>% 
  arrange(desc(load))

write.csv(summary$load, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/Cruz/load.csv", row.names = FALSE)
