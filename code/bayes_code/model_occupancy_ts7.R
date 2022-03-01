model {

# model with random slopes and intercept 
  
  # Priors
  for (j in 1:5){
    beta[j, 1:5] ~ dmnorm(beta_hat[], Tau_beta[,])
  }
  
  for(i in 1:N_watshed) {
    beta_hat[i] ~ dnorm(0, 0.001)
  }
  
  Tau_beta[1:7,1:7] ~ dwish(W[,], 4)
  Sigma_beta[1:7, 1:7] <- inverse(Tau_beta[,])
  
  tau ~ dscaled.gamma(2.5, 3)
  sigma <- sqrt(1 / tau)
  
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