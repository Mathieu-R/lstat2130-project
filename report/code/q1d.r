laplace_approximation <- function(log_posterior, inits, n_samples, ...) {
  fit <- optim(
    par = inits,
    fn = log_posterior,
    control = list(fnscale = -1),
    hessian = TRUE,
    ...
  )
  
  mean <- fit$param
  var_cov_matrix <- solve(-fit$hessian)
  
  return(list(
    mean = mean,
    var_cov_matrix = var_cov_matrix
  ))
}

inits <- c(theta0 = 0, theta1 = 0, theta2 = 0)
lapprox <- laplace_approximation(log_posterior, inits, 10000, t = df$day, experiment = df$Exp1)

lapprox$mean
lapprox$var_cov_matrix