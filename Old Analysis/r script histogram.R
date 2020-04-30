library(ggplot2)
qplot(Bell.Runtimes$Runtimes,
  geom = "histogram",
  binwidth = 1, 
  main = "Histogram of Runtimes",
  xlab = "Minutes",
  colour = I("red"), 
  alpha=I(.2),
  fill = I("blue"),
  xlim = c(0, 30),
)

