
# Load packages
library(runjags)
library(tidyverse)
library(MCMCvis) 
library(mcmcOutput)


# Read in the data
data <- read.csv("MN_Topeka_Shiner_Occurance_sites_removed.csv", header = TRUE)
str(data)

# Collect the data into suitable structures
y <- as.matrix(data[,2:16])         # as.matrix essential for WinBUGS

# jags --------------------------------------------------------------------

## data ####
# 
d_jags <- list(y = y,
               nsite = dim(y)[1],
               nyear = dim(y)[2])


## parameters ####
para <- c("psi",
          "phi",
          "gamma",
          "p",
          "n.occ",
          "growthr",
          "turnover")

## model file ####
m <- read.jagsfile("model_occupancy_ts2.R")

## mcmc setup ####
n_ad <- 100 
n_iter <- 1.0E+4 #number of draws
n_thin <- max(3, ceiling(n_iter / 500)) #number of thins
n_burn <- ceiling(max(10, n_iter/2)) # number of draws to burn
n_sample <- ceiling(n_iter / n_thin)

# Initial values
zst <- apply(y, 1, max, na.rm = TRUE)	# Good starting values crucial

inits <- replicate(3,
                   list(.RNG.name = "base::Mersenne-Twister",
                        z = zst,
                        .RNG.seed = NA),
                   simplify = FALSE)

for (i in 1:3) inits[[i]]$.RNG.seed <- i

# run jags ----------------------------------------------------------------

post <- run.jags(m$model,
                 monitor = para,
                 data = d_jags,
                 n.chains = 3,
                 inits = inits,
                 method = "parallel",
                 burnin = n_burn,
                 sample = n_sample,
                 adapt = n_ad,
                 thin = n_thin,
                 n.sims = 3,
                 module = "glm")

# summarize outputs
post

mcmc_summary <- MCMCsummary(post$mcmc)  
mcmc_summary   # Bayesian analysis

