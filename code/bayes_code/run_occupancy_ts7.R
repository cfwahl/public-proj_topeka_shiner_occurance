
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(runjags,
               tidyverse,
               MCMCvis,
               mcmcOutput)

# Read in the data

df1 <- read_csv("data_fmt/MN_TS_watershed_stats.csv")
df2 <- read_csv("data_fmt/data_mn_fmt.csv")


# assign variables
# capitalize "data" in Jags codes to distinguish from parameters
Y <- df2$occurrence
Agr <- c(scale(df1$frac_agri))
Elv <- c(scale(df1$elevationmean))
Area <- c(scale(df1$area))
Slop <- c(scale(df1$slopemean))
Watshed <- df1$watershed


# jags --------------------------------------------------------------------

## data ####
d_jags <- list(Y = Y,
               Agr = Agr,
               Elv = Elv,
               Area = Area,
               Slop = Slop,
               Watshed = Watshed,
               N_sample = length(Y),
               N_watshed = n_distinct(Watshed),
               W = diag(7))

d_jags # check to make sure its correct
str(d_jags)

## parameters ####
para <- c("beta_hat",
          "beta",
          "Sigma_beta",
          "sigma")

## model file ####
m <- read.jagsfile("code/bayes_code/model_occupancy_ts7.R")

## mcmc setup ####
n_ad <- 100 
n_iter <- 1.0E+4 #number of draws
n_thin <- max(3, ceiling(n_iter / 500)) #number of thins
n_burn <- ceiling(max(10, n_iter/2)) # number of draws to burn
n_sample <- ceiling(n_iter / n_thin)

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
mcmc_summary <- MCMCsummary(post$mcmc)  
mcmc_summary   # Bayesian analysis

MCMCtrace(post$mcmc)

