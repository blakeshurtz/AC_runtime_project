library(ggplot2)

#create initial sequence
x <- seq(0, 10, length.out=1000)

#generate probabilities for a given system with parameters scale and shape
generate_probs <- function(i){
  pgpd(x, loc = 1, scale = ccl$scale[[i]], shape = ccl$shape[[i]])
}

#generate probabilities
probabilities <- matrix(0, nrow = 1000, ncol = nrow(ccl))

#apply to all systems
for (i in 1:nrow(ccl)){
  probabilities[,i] <- generate_probs(i)
}

#gather for tall dataframe
probabilities <- gather(as.data.frame(probabilities), dplyr::starts_with("V"), key = "customer", value = "prob")

#create data frame
dat = data.frame(x = rep(seq(0, 10, length.out=1000), nrow(ccl)),
                 probs = probabilities$prob,
                 customer = probabilities$customer)

#plot average distribution
t <- pgpd(x, loc = 1, scale = mean(ccl$scale), shape = mean(ccl$shape))
dat2 <- data.frame(x = seq(0, 10, length.out=1000),
                   probs = t,
                   customer = rep("average", 1000))

#create plot
ggplot(dat, aes(x = x, y = probs, group = customer)) + 
  geom_line(alpha = .1) +
  geom_line(data = dat2, aes(x = x, y = probs)) +
  xlim(0,10)


