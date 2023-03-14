library(tidyverse)


p <- c(0.75,0.70,0.5)


matrixp <- matrix(rep(1,6),nrow = 3)

out <- data.frame()

eps <- 0.05

for(i in 1:10000){
  randnum <- runif(1)
  
  if(randnum < eps){
    action <- sample(c(1,2,3),1)
    
    outcome <- rbinom(1,1,p[action])
    
    if(outcome == 1){
      matrixp[action,1] <- matrixp[action,1] +1
    } else {
      matrixp[action,2] <- matrixp[action,2] + 1
    }
  } else {
    
    samples <- apply(matrixp, 1, function(x) rbeta(1, x[1], x[2]))
    
    action <- which.max(samples)
    
    outcome <- rbinom(1,1,p[action])
    
    if(outcome == 1){
      matrixp[action,1] <- matrixp[action,1] +1
    } else {
      matrixp[action,2] <- matrixp[action,2] + 1
    }
    
    
  }
  
  out <- rbind(out, data.frame(iter = i, action = as.character(action), outcome = outcome))
  
}

out



out1 <- out %>% group_by(action) %>% mutate(rec = 1, count = cumsum(rec)) %>%
  ungroup() %>%
  mutate(movavg = cumsum(outcome)/iter, selavg = count/iter)

ggplot(out1[1:250,], aes(x = iter, y = count,color = action,group = action)) +
  geom_line()

ggplot(out1, aes(x = iter, y= movavg)) +
  geom_path()


ggplot(out1, aes(x = iter, y = selavg,color = action,group = action)) +
  geom_line()


sample(p,1)
rbeta(1,beta1[1],beta1[2])

sapply(matrixp, function)


params <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)

# Sample from the beta distribution for each row
samples <- apply(params, 1, function(x) rbeta(1, x[1], x[2]))

# Print the samples
samples
