model {
  
  # Specify priors
  psi1 ~ dunif(0, 1)
  for (j in 1:(nyear-1)){
    phi[j] ~ dunif(0, 1)
    gamma[j] ~ dunif(0, 1)
  }
  p ~ dunif(0, 1)
  
  # Both models at once
  for (i in 1:nsite){
    z[i,1] ~ dbern(psi1)     # State model 1: Initial state
    muy[i,1] <- z[i,1]*p
    y[i,1] ~ dbin(muy[i,1], 2)
    for (k in 2:nyear){      # State model 2: State dynamics
      muZ[i,j] <- z[i,j-1]*phi[j-1] + (1-z[i,j-1])*gamma[j-1]
      z[i,j] ~ dbern(muZ[i,j])
      
      # Observation model
      muy[i,j] <- z[i,j]*p
      y[i,j] ~ dbin(muy[i,j], 2)
    } #j
  } #i
  
  # Derived parameters: Sample and population occupancy, growth rate and turnover
  psi[1] <- psi1
  n.occ[1] <- sum(z[1:nsite,1])
  for (k in 2:nyear){
    psi[k] <- psi[k-1]*phi[k-1] + (1-psi[k-1])*gamma[k-1]
    n.occ[k] <- sum(z[1:nsite,k])
    growthr[k-1] <- psi[k]/psi[k-1]
    turnover[k-1] <- (1 - psi[k-1]) * gamma[k-1]/psi[k]
  }
}