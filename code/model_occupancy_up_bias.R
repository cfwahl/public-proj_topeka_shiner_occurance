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
    b[j] ~ dnorm(0, ninfo)
  }
  
  # prior for connectivity
  # truncated normal distribution for alpha ("T(,)" defines lower and upper limits)
  alpha[1] ~ dnorm(0, ninfo)T(0, alpha[2]) # assume alpha[2] > alpha[1]
  alpha[2] ~ dnorm(0, ninfo)T(0, 50)
  
  
  # likelihood --------------------------------------------------------------
  
  # Binomial likelihood
  for (i in 1:N_sample) {
    
    ld[i] <- logdensity.bern(Y[i], p[i])
    
    Y[i] ~ dbern(p[i])
    logit(p[i]) <- 
      r[Watshed[i]] + 
      b[1] * Agr[i] +
      b[2] * Temp[i] +
      b[3] * Area[i] +
      b[4] * Precp_wet[i] +
      b[5] * s[i]
    
    # connectivity summed over j
    # subtract c[i,] from the sum; self-connection removal
    s[i] <- sum(c[i,] * M[i,])
    
    # connectivity measure for a specific pair of i and j
    for(j in 1:N_sample) {
      c[i,j] <- exp(u[i,j] + d[i,j]) * Incidence[j]
      
      u[i,j] <- -alpha[1] * U[i, j]
      d[i,j] <- -alpha[2] * D[i, j]
    }
  }
  
  # dummy site model
  for (i in 1:N_dummy) {
    y[i] ~ dbern(p_hat[i])
    
    logit(p_hat[i]) <- 
      r[Watshed_hat[i]] + 
      b[1] * Agr_hat[i] +
      b[2] * Temp_hat[i] +
      b[3] * Area_hat[i] +
      b[4] * Precp_wet_hat[i]
    
    s_hat[i] <- sum(c_hat[i,] * M_hat[i,])
    
    # connectivity measure for a specific pair of i and j
    for(j in 1:N_dummy) {
      c_hat[i,j] <- exp(u_hat[i,j] + d_hat[i,j]) * y[j]
      
      u_hat[i, j] <- -alpha[1] * U_hat[i, j]
      d_hat[i, j] <- -alpha[2] * D_hat[i, j]
    }
  }
  
  
  # parameter conversion ----------------------------------------------------
  
  beta[1] <- b[1] / sd(Agr[])
  beta[2] <- b[2] / sd(Temp[])
  beta[3] <- b[3] / sd(Area[])
  beta[4] <- b[4] / sd(Precp_wet[])
  beta[5] <- b[5] / sd(s[])
  
#  b[1] <- beta[1] * sd(Agr[])  
#  b[2] <- beta[2] * sd(Gras[])  
#  b[3] <- beta[3] * sd(Area[])  
#  b[4] <- beta[4] * sd(Slop[])  
#  b[5] <- beta[5] * sd(s[])  
  
}

data {
  for (n in 1:Ndim) {
    U[Row[n], Col[n]] <- V_U[n]
    D[Row[n], Col[n]] <- V_D[n]
  }
  
  for (n in 1:Ndim_hat) {
    U_hat[Row_hat[n], Col_hat[n]] <- V_U_hat[n]
    D_hat[Row_hat[n], Col_hat[n]] <- V_D_hat[n]
  }
}
