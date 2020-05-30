library(ggplot2)

#create initial sequence
x <- seq(0, 5, length.out=1000)

#generate probabilities for a given system with parameters scale and shape
generate_probs <- function(i){
  pgpd(x, scale = output$scale[[i]], shape = output$shape[[i]])
}

#generate probabilities
probabilities <- matrix(0, nrow = 1000, ncol = 23)

#apply to all systems
for (i in 1:nrow(output)){
  probabilities[,i] <- generate_probs(i)
}

#gather for tall dataframe
probabilities <- gather(as.data.frame(probabilities), V1, V2, V3, V4, V5, V6, V7, V8, V9, V10,
                        V11, V12, V13, V14, V15, V16, V17, V18, V19, V20,
                        V21, V22, V23, 
                        key = "customer", value = "prob")

#create data frame
dat = data.frame(x = rep(seq(0, 5, length.out=1000), nrow(output)),
                 probs = probabilities$prob,
                 customer = probabilities$customer)

#create plot
ggplot(dat, aes(x = x, y = probs, group = customer)) + 
  geom_line(aes(color = customer))


