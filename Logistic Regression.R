setwd('~/Documents/Code Projects/MachineLearning Algorithms/')

dat <- read_csv('data_logistic_reg.csv')
names(dat) <- c('score1', 'score2', 'label')

plot(dat$score1, dat$score2, col=dat$label %>% as.factor())

my_sigmoid <- function(z) 1 / (1 + exp(-z))


my_logistic_regression <- function(y, x) {
  stopifnot(length(y)==nrow(x))
  
  m <- length(y)
  x <- cbind(1, x)
  theta <- c(0,0,0)
  
  my_logistic_cost <- function(theta) {
    g <- my_sigmoid(x %*% theta)
    (-1 / m) * sum((y * log(g)) + ((1 - y) * log(1 - g)))
  }

  theta <- optim(theta, my_logistic_cost)$par %>% round(4)
  writeLines(sprintf('Coefficients:\n(Intercept)\tx1\tx2\n%f\t%f\t%f', theta[1], theta[2], theta[3]))
}

dat %$% my_logistic_regression(label, cbind(score1, score2))

dat %$% glm(label ~ score1 + score2, family=binomial)
