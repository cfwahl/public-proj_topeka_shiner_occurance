model {
  
  # Specify priors
  psi1 ~ dunif(0, 1)
  for (j in 1:(nyear-1)){
    phi[j] ~ dunif(0, 1)
    gamma[j] ~ dunif(0, 1)
    p[j] ~ dunif(0, 1) 
  }
  p[nyear] ~ dunif(0, 1)
  
  # Ecological submodel: Define state conditional on parameters
  for (i in 1:nsite){
    z[i,1] ~ dbern(psi1)
    for (j in 2:nyear){
      muZ[i,j]<- z[i,j-1]*phi[j-1] + (1-z[i,j-1])*gamma[j-1]
      z[i,j] ~ dbern(muZ[i,j])
    } #j
  } #i
  
  # Observation model
  for (i in 1:nsite){
    for (j in 1:nyear){
     muy[i,j] <- z[i,j]*p[j]
        y[i,j] ~ dbern(muy[i,j])
    } #j
  } #i
  
  # Derived parameters: Sample and population occupancy, growth rate and turnover
  psi[1] <- psi1
  n.occ[1]<-sum(z[1:nsite,1])
  for (j in 2:nyear){
    psi[j] <- psi[j-1]*phi[j-1] + (1-psi[j-1])*gamma[j-1]
    n.occ[j] <- sum(z[1:nsite,j])
    growthr[j-1] <- psi[j]/psi[j-1]                         
    turnover[j-1] <- (1 - psi[j-1]) * gamma[j-1]/psi[j]
  }
}