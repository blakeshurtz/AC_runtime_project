#generate probabilities for a given system with parameters scale and shape
p_above_fclc <- function(i){
  pgpd(2, scale = fclc$scale[[i]], shape = fclc$shape[[i]], lower.tail = FALSE)
}

#generate probabilities
dp <- matrix(0, nrow = nrow(customer_data), ncol = 1)

#apply to all systems
for (i in 1:nrow(customer_data)){
  dp[i,] <- p_above_fclc(i)
}


#generate probabilities for a given system with parameters scale and shape
q_above_95 <- function(i){
  qgpd(.9, scale = fclc$scale[[i]], shape = fclc$shape[[i]], lower.tail = TRUE)
}

#generate probabilities
dq <- matrix(0, nrow = nrow(customer_data), ncol = 1)

#apply to all systems
for (i in 1:nrow(customer_data)){
  dq[i,] <- q_above_95(i)
}

#for average
pgpd(1, scale = mean(fclc$scale), shape = mean(fclc$shape), lower.tail = FALSE)
qgpd(.9, scale = mean(fclc$scale), shape = mean(fclc$shape), lower.tail = TRUE)
