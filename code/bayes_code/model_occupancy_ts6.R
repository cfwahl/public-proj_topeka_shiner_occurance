model {
    # Priors and linear models
  for (j in 1:N_watshed){		
    alpha[j] ~ dnorm(mu_alpha, tau_alpha)	# # Random effects
  }
  
  # Priors for hyper-parameters
  mu_alpha ~ dnorm(0, 1.0E-06)                          # Hyperprior for mean hyperparam
  tau_alpha <- pow(sd_alpha, -2)
  sd_alpha ~ dunif(0, 100)                              # Hyperprior for sd hyperparam
  
  # Other priors
  for (j in 1:4){
  beta[j] ~ dnorm(0, 0.01)                              # Slope of beta
  }
  
  tau <- pow(sd, -2)
  sd ~ dunif(0, 1000)                                   # 1/residual variance
  
  
  # Binomial likelihood
  for (i in 1:N_sample) {
    Y[i] ~ dbern(p[i],tau)
    logit(p[i]) <- alpha[Watshed[i]] + 
                    beta[1]* Agr[i] +
                    beta[2] * Elv[i] +
                    beta[3] * Area[i] +
                    beta[4] * Slop[i]
  }
}
