model {
  
  ninfo <- 0.01 # precision value for vague priors
  scale <- 2.5 # scale parameter for dscaled.gamma
  df <- 3 # degree of freedom
  
  
  # prior -------------------------------------------------------------------
  
  # random effect
  for (j in 1:N_watshed){		
    r[j] ~ dnorm(mu_r, tau_r)	# random effects
  }
  
  # priors for hyper-parameters
  mu_r ~ dnorm(0, ninfo) # prior for hyper-mean
  tau_r ~ dscaled.gamma(scale, df)
  sd_r <- sqrt(1 / tau_r)  # half-t distribution for sd prior
  
  # prior for fixed effects
  for (j in 1:5){
    beta[j] ~ dnorm(0, ninfo)
  }
  
  # prior for connectivity
  # truncated normal distribution for alpha ("T(,)" defines lower and upper limits)
  alpha[1] ~ dnorm(0, ninfo)T(0, 50)
  alpha[2] ~ dnorm(0, ninfo)T(0, alpha[1]) # assume alpha[2] < alpha[1]
  
  
  # likelihood --------------------------------------------------------------
  
  # Binomial likelihood
  for (i in 1:N_sample) {
    
    ld[i] <- logdensity.bern(Y[i], p[i])
    
    Y[i] ~ dbern(p[i])
    logit(p[i]) <- 
      r[Watshed[i]] + 
      beta[1]* Agr[i] +
      beta[2] * Gras[i] +
      beta[3] * Area[i] +
      beta[4] * Slop[i] +
      beta[5] * s[i]
    
    # connectivity summed over j
    # subtract c[i,] from the sum; self-connection removal
    s[i] <- sum(c[i,] * M[i,])
    
    # connectivity measure for a specific pair of i and j
    for(j in 1:N_site) {
      c[i,j] <- exp(u[i,j] + d[i,j]) * Incidence[j]
      
      u[i,j] <- -alpha[1] * U[i, j]
      d[i,j] <- -alpha[2] * D[i, j]
    }
  }
}
