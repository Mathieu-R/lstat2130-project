log.prior <- function(theta) {
  theta0 <- theta[1]
  theta1 <- theta[2]
  theta2 <- theta[3]
  
  prior.theta0 <- dnorm(theta0, mean = 0, sd = 10, log = TRUE)
  prior.theta1 <- dnorm(theta1, mean = 0, sd = 10, log = TRUE)
  prior.theta2 <- dnorm(theta2, mean = 0, sd = 10, log = TRUE)
  
  return(prior.theta0 + prior.theta1 + prior.theta2)
}

log.posterior <- function(theta, t, experiment) {
  log.likelihood(theta, t, experiment) + log.prior(theta)
} 