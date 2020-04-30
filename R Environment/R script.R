x <- Weather2015$CDD
hist(x, breaks = 7, col = "blue", 
     main = "Distribution of Daily CDD", xlab = "Cooling Degree Days (CDD)", ylab = "Frequency",
      xlim = c(0,20), ylim = c(0,40))

y <- Weather2016$V2
hist(y, breaks = 7, col = "red", 
     main = "Distribution of Daily CDD", xlab = "Cooling Degree Days (CDD)", ylab = "Frequency",
     xlim = c(0,20), ylim = c(0,40))
