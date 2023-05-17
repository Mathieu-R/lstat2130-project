metropolis.cw <- function(theta.init, sd.proposals, t, experiment, n_steps) {
  pb <- progress_bar$new(
    format = "metropolis algorithm [:bar] :percent eta: :eta",
    total = n_steps,
    clear = FALSE,
    width = 60
  )
  
  n_parameters <- length(theta.init)
  
  # matrix of dim (n_steps, n_parameters)
  theta <- matrix(nrow = n_steps, ncol = n_parameters)
  colnames(theta) <- c("theta0", "theta1", "theta2")
  
  # initialize
  theta[1, ] <- theta.init
  
  # counter
  n_accept <- rep(0, n_parameters)
  
  pb$tick(0)
  
  for (n in 2:n_steps) {
    pb$tick()
    
    # get all the current parameters for the step n
    theta.current <- theta[n - 1, ]
    
    for (i in 1:n_parameters) {
      # sample a proposal for parameter i from a normal distribution centered at the previous parameter point with a given standard deviation
      theta.proposed_i <- theta.current[i] + rnorm(1, mean = 0, sd = sd.proposals[i])
      
      # update the current parameters vector with the proposal
      theta.proposed <- theta.current
      theta.proposed[i] <- theta.proposed_i
      
      proba <- min(
        1, 
        exp(log.posterior(theta.proposed, t, experiment) - log.posterior(theta.current, t, experiment))
      )
  
      accept <- sample(0:1, 1, prob = c(1 - proba, proba))
      
      if (accept) {
        n_accept[i] <- n_accept[i] + 1
        # update the final vector
        theta[n, i] <- theta.proposed_i
        
        # update the temporary current vector for the next iteration
        theta.current[i] <- theta.proposed_i
      } else {
        # update the final vector
        theta[n, i] <- theta.current[i]
        
        # note: the value of the temporary current vector for this parameter
        # remains the same in that case
      }
    }
  }
  
  # note: n_steps - 1 because there is theta.init "automatically accepted" 
  # (we do n_steps - 1) iterations
  accept.rate <- round(n_accept / ((n_steps - 1)), 2)
  return(list(theta = theta, accept.rate = accept.rate))
}
```

```{r}
#sd.proposals <- c(0.017, 0.28, 0.023)
sd.proposals <- c(0.017, 0.025, 0.023)

metropolis.results <- metropolis.cw(
  theta.init = c(5.5, 0.98, 0.01),
  sd.proposals = sd.proposals,
  t = df$day,
  experiment = df$Exp1,
  n_steps = N_STEPS
)

# goal is to reach an acceptance rate of 0.4
metropolis.results$accept.rate