log.prior <- function(theta) {
  theta0 <- theta[1]
  theta1 <- theta[2]
  theta2 <- theta[3]
  
  prior.theta0 <- dunif(theta0, -100, 100, log = TRUE)
  prior.theta1 <- dunif(theta1, -100, 100, log = TRUE)
  prior.theta2 <- dunif(theta2, -100, 100, log = TRUE)
  
  return(prior.theta0 + prior.theta1 + prior.theta2)
}

log.posterior <- function(theta, t, experiment) {
  log.likelihood(theta, t, experiment) + log.prior(theta)
}