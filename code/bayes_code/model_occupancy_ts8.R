model {
  
  # random intercept model
  
  # Priors and linear models
  for (j in 1:N_watshed){		
    alpha[j] ~ dnorm(mu_alpha, tau_alpha)	# Random effects
  }
  
  # Priors for hyper-parameters
  mu_alpha ~ dnorm(0, 1.0E-06)                          # Hyperprior for mean hyperparam
  tau_alpha <- pow(sd_alpha, -2)
  sd_alpha ~ dunif(0, 100)                              # Hyperprior for sd hyperparam
  
  # Other priors
  for (j in 1:5){
    beta[j] ~ dnorm(0, 0.01)                              # Slope of beta
  }
  
  tau <- pow(sd, -2)
  sd ~ dunif(0, 1000)                                   # 1/residual variance
  
  
  # Binomial likelihood
  for (i in 1:N_sample) {
    Y[i] ~ dbern(p[i])
    logit(p[i]) <- alpha[Watshed[i]] + 
      beta[1]* Agr[i] +
      beta[2] * Elv[i] +
      beta[3] * Area[i] +
      beta[4] * Slop[i] +
      beta[5] * s[i]
    
    # connectivity summed over j
    # subtract c[i,Segment_id[i]] from the sum; self-connection removal
    s[i] <- sum(c[i,]) - c[i, Segment_id[i]]
    
    # connectivity measure for a specific pair of i and j
    for(j in 1:N_site) {
      c[i,j] <- exp(u[i,j] + d[i,j])
      
      u[i,j] <- -alpha[1] * U[Segment_id[i], j]
      d[i,j] <- -alpha[2] * D[Segment_id[i], j]
    }
  }
}
