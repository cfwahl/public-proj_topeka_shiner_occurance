model {

# model with random slopes and intercept 
  
  # Priors
  for (j in 1:N_watshed) {
    beta[j, 1:5] ~ dmnorm(beta_hat[], Tau_beta[,])
  }
  
  for(i in 1:5) {
    beta_hat[i] ~ dnorm(0, 0.01)
  }
  
  Tau_beta[1:5, 1:5] ~ dscaled.wishart(rep(5, 5), 2)
  Sigma_beta[1:5, 1:5] <- inverse(Tau_beta[,])
  
  # Likelihood
  for (i in 1:N_sample) {
    Y[i] ~ dbern(p[i])		# The 'residual' random variable
    logit(p[i]) <- beta[Watshed[i], 1] + 
                   beta[Watshed[i], 2] * Agr[i] + 
                   beta[Watshed[i], 3] * Elv[i] + 
                   beta[Watshed[i], 4] * Area[i] +
                   beta[Watshed[i], 5] * Slop[i] 
  }
  
}