model{
  
  # Likelihood
  for(i in 1:N) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- b0 + bagr * agr[i] + belv * elv[i]
  }
  
  # Priors
  p0 ~ dbeta(1, 1)
  b0 <- logit(p0)
  bagr ~ dunif(-5, 5)
  belv ~ dunif(-5, 5)
}