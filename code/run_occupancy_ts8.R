
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(runjags,
               tidyverse,
               MCMCvis,
               mcmcOutput,
               foreach)

# Read in the data
# upstream distance matrix 

load("data_fmt/distance_matrix.RData")
load("data_fmt/data_mn_fmt.RData")

data <- read_csv("data_fmt/mn_fmt_watershed_landuse.csv")  


# assign variables
# capitalize "data" in Jags codes to distinguish from parameters
Y <- data$occurrence
Agr <- c(scale(data$frac_agri))
Elv <- c(scale(data$elv_mean))
Area <- c(scale(data$area))
Slop <- c(scale(data$slope_mean))
U <- m_u
D <- m_d
Watshed <- data$watershed_num

TD <- U + D
M <- foreach(i = seq_len(nrow(TD)), .combine = rbind) %do% {
  x <- TD[i,]
  y <- ifelse(x == 0 | x > 25,
              yes = 0,
              no = 1)
  return(y)
}


# jags --------------------------------------------------------------------

## data ####
d_jags <- list(Y = Y,
               Agr = Agr,
               Elv = Elv,
               Area = Area,
               Slop = Slop,
               Watshed = Watshed,
               U = data.matrix(U),
               D = data.matrix(D),
               N_sample = length(Y),
               N_watshed = n_distinct(Watshed),
               N_site = nrow(U),
               M = M)

d_jags # check to make sure its correct
str(d_jags)

## parameters ####
para <- c("alpha",
          "beta",
          "mu_r",
          "sd_r")

## model file ####
m <- read.jagsfile("code/model_occupancy_ts8.R")

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

save(mcmc_summary, file = "data_fmt/mcmc_summary.RData")
