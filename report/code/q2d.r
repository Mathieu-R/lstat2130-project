laplace.approximation <- function(log.posterior, inits, n_samples, ...) {
  fit <- optim(
    par = inits,
    fn = log.posterior,
    control = list(fnscale = -1),
    hessian = TRUE,
    ...
  )
  
  mean <- fit$par
  var_cov_matrix <- solve(-fit$hessian)
  samples <- rmvnorm(20000, mean, var_cov_matrix)
  
  return(list(
    mean = mean,
    var_cov_matrix = var_cov_matrix,
    samples = samples
  ))
}

inits <- c(theta0 = 0.001, theta1 = 0.001, theta2 = 0.001)
lapprox <- laplace.approximation(log.posterior, inits, 10000, t = df$day, experiment = df$Exp1)