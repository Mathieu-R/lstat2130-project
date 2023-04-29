prior <- function(theta) {
  dunif(theta, 0, 1, log = TRUE)
}

log_posterior <- function(theta, t, experiment) {
  log_likelihood(theta, t, experiment) + prior(theta[0]) + prior(theta[1]) + prior(theta[2])
}