log.mu <- function(alpha, t_j) {
  log(alpha[1]) - alpha[2] * exp(- alpha[3] * t_j)
}

log.likelihood <- function(theta, t, experiment) {
  n <- length(t)
  alpha <- exp(theta)
  
  result <- sum(sapply(1:n, function(j) {
    dpois(x = t[j], lambda = exp(log.mu(alpha, t[j])), log = TRUE)
  }))
  
  return(result)
}