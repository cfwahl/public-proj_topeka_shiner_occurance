

# setup --------------------------------------------------------------------

# Load packages
pacman::p_load(runjags,
               tidyverse,
               MCMCvis,
               mcmcOutput)

# set working directory, 
setwd()

# Read in the data
data <- read.csv("data_raw/MN_TS_occurance_watershed_stats.csv", header = TRUE)
str(data)

# assign variables
y <- data$occurrence
agr <- data$frac_agri
elv <- data$elevationmean

# jags --------------------------------------------------------------------

## data ####
# 
d_jags <- list(y = y,
               agr = agr,
               elv = elv,
               n = length(y))

d_jags # check to make sure its correct
str(d_jags)

## parameters ####
para <- c("alpha",
          "beta1",
          "beta2",
          "mean.theta")

## model file ####
m <- read.jagsfile("code/model_occupancy_ts.R")

## mcmc setup ####
n_ad <- 100 
n_iter <- 1.0E+4 #number of draws
n_thin <- max(3, ceiling(n_iter / 500)) #number of thins
n_burn <- ceiling(max(10, n_iter/2)) # number of draws to burn
n_sample <- 10000

inits <- replicate(3,
                   list(.RNG.name = "base::Mersenne-Twister",
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

diagPlot(post)

# convert to mcmcOutput for plots of marginal probabilities
occ_post <- mcmcOutput(post, default='psi')
plot(occ_post)

# Get frequentist MLEs for comparison
summary(glm(y ~ agr + elv, family=binomial(link = "logit")))

        