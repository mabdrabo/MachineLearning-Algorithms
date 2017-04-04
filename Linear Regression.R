library(magrittr)
library(dplyr)
library(purrr)


my_linear_regression <- function(y, x) {
  stopifnot(length(y)==length(x))
  
  ## add 1s to simulate the intercept
  x <- cbind(1,x)
  theta <- c(0,0)
  m <- y %>% length()
  
  alpha <- 0.001
  iterations <- 20000
  ## update thetas using gradient descent
  map(seq_len(iterations), ~ {
    theta[1] <<- theta[1] - ((alpha / m) * (sum(((x %*% theta) - y) * x[,1])))
    theta[2] <<- theta[2] - ((alpha / m) * (sum(((x %*% theta) - y) * x[,2])))
  })
  cost <- (sum(((x %*% theta) - y)^2)) / (2*m)
  writeLines(paste('Final cost: ', cost %>% round(3)))
  writeLines(sprintf('Coefficients:\n(Intercept)\tx1\n%f\t%f', theta[1], theta[2]))
  theta
}

data(cars)
plot(cars)
abline(lm(dist~speed, data=cars), col='blue')
my_linear_regression(cars$dist, cars$speed) %>% abline(col='red')
