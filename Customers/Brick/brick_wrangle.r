set.seed(1234)
library(readr)
Brick <- read_csv("D:/Google Drive/R/Projects/AC_runtime_project/Customers/Brick/Brick.csv", 
                 skip = 2,
                 col_names = c("index", "datetime", "amps"))


library(dplyr)

Brick$on <- if_else(Brick$amps < 1, 0, 1)

cap <- (3 * .8)

Brick$capacity <- if_else(Brick$on == 1, cap/60, 0)

Brick$cycle <- with(rle(Brick$on), rep(seq_along(lengths), lengths)) 


summary <- Brick %>% 
  group_by(cycle) %>% 
  summarise(load = sum(capacity)/cap) %>% 
  filter(load != 0) %>% 
  arrange(desc(load))

write.csv(summary$load, file = "D:/Google Drive/R/Projects/AC_runtime_project/Customers/Brick/load.csv", row.names = FALSE)
