
model {
  
  # Priors and linear models
  alpha <- 1 / delta
  delta ~ dnorm(0, 0.1)T(0, )
  
  ## if all the slopes and intercept follow the same prior, you can write in this way
  ## if you call "beta" in R, it returns all beta's [1-4]
  for(k in 1:4) {
    beta[k] ~ dnorm(0, 0.1)
  }
  
  # Likelihood of the Bernoulli GLM
  for (i in 1:N_sample){
      
      Y[i] ~ dbern(theta[i])
      logit(theta[i]) <- 
        beta[1] + 
        beta[2] * Agr[i] + 
        beta[3] * Elv[i] +
        beta[4] * s[i]
      
      # connectivity summed over j
      # subtract c[i,Segment_id[i]] from the sum; self-connection removal
      s[i] <- sum(c[i,]) - c[i, Segment_id[i]]
      
      # connectivity measure for a specific pair of i and j
      for(j in 1:N_site) {
        c[i, j] <- exp(-alpha * M[Segment_id[i], j])
      }
      
  }
  
}