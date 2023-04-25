install.packages("DirichletReg")
library(DirichletReg)
library(tidyverse)
# set the parameters of the Dirichlet distribution
alpha <- c(10,16,17,18,19,20,10)

# generate a random sample of size 1000 from the Dirichlet distribution
samples <- rdirichlet(n = 1000, alpha = alpha)

# print the first 5 samples
print(head(samples))


# get the index value of the maximum value in each sample
max_indices <- max.col(samples)

ggplot(data.frame(x = max_indices)) +
  geom_bar(aes(x=x))
# print the first 5 max indices
print(head(max_indices))
