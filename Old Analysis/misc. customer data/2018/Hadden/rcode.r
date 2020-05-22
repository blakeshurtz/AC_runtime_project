library(ggplot2)
library(plyr)

'cfm25'
cdat <- ddply(DuctLeakageCFM25, "inout", summarise, rating.mean=mean(CFM25))
cdat
ggplot(ANOVA_day_SAS_, aes(x=Longest_Run_Times, fill=Date)) +
  geom_histogram(binwidth= 1, alpha=.5, position="identity") +
  ggtitle("Duct Leakage Score- Test-in vs. Test-out, n=150")

