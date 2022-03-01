model {
  
  # Priors
  for (j in 1:5){
    beta[j, 1:5] ~ dmnorm(beta.hat[], Tau.beta[,])
  }
  
  for(i in 1:7) {
    beta.hat[i] ~ dnorm(0, 0.001)
  }
  
  Tau.beta[1:7,1:7] ~ dwish(W[,], 4)
  Sigma.beta[1:7, 1:7] <- inverse(Tau.beta[,])
  
  tau ~ dscaled.gamma(2.5, 3)
  sigma <- sqrt(1 / tau)
  
  # Likelihood
  for (i in 1:n) {
    y[i] ~ dbern(p[i])		# The 'residual' random variable
    logit(p[i]) <- beta[Watshed[i], 1] + 
                   beta[Watshed[i], 2] * Agr[i] + 
                   beta[Watshed[i], 3] * Elv[i] + # Expectation
                   beta[Watshed[i], 4] * Agr[i] +
                   beta[Watshed[i], 5] * Slop[i] 
  }
  
}