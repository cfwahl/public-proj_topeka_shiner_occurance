
model {
  
  # Priors and linear models
  alpha ~ dnorm(0, 0.1)                        # Intercept on logit link scale 
  beta1 ~ dnorm(0, 0.1)                        # Slope on logit link scale
  beta2 ~ dnorm(0, 0.1) 
  
  # Likelihood of the Bernoulli GLM
  for (i in 1:n){
      y[i] ~ dbern(theta[i])                        # Stochastic part of response
      logit(theta[i]) <- alpha + 
        beta1 * agr[i] + 
        beta2 * elv[i] 
    }
}