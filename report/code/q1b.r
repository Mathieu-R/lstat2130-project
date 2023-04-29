mu <- function(alpha, t) {
  - alpha[1] * exp(- alpha[2] * exp(- alpha[3] * t))
}

log_likelihood <- function(theta, t, experiment) {
  n <- length(experiment)
  alpha <- exp(theta)
  
  result <- sum(sapply(1:n, function(j) {
    experiment[j] * log(mu(alpha = alpha, t = t[j])^experiment[j] * exp(-mu(alpha = alpha, t = t[j])))
  }))
  
  return(result)
}