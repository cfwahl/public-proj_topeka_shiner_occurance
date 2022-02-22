
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(runjags,
               tidyverse,
               MCMCvis,
               mcmcOutput)

# Read in the data
data_dmat <- read_csv("data_raw/siteXdist_ws1.csv")
names(data_dmat) <- c("segment_id", paste0("j", 1:11))

data_dmat <- data_dmat %>% 
  mutate(segment_id = str_remove(segment_id, "X"),
         segment_id = as.numeric(segment_id))

segment_set <- unique(data_dmat$segment_id)

data_dmat <- data_dmat %>% 
  mutate(segment_id = as.numeric(as.factor(segment_id)))

data <- read_csv("data_raw/MN_TS_occurance_watershed_stats.csv") %>% 
  filter(segment %in% segment_set) %>% 
  mutate(segment_id = as.numeric(as.factor(segment)))

str(data)

# assign variables
# capitalize "data" in Jags codes to distinguish from parameters
Y <- data$occurrence
Agr <- c(scale(data$frac_agri))
Elv <- c(scale(data$elevationmean))
M <- dplyr::select(data_dmat, -segment_id)
Segment_id <- data$segment_id

# jags --------------------------------------------------------------------

## data ####
d_jags <- list(Y = Y,
               Agr = Agr,
               Elv = Elv,
               M = data.matrix(M),
               Segment_id = Segment_id,
               N_sample = length(Y),
               N_site = nrow(M))

d_jags # check to make sure its correct
str(d_jags)

## parameters ####
para <- c("alpha",
          "beta",
          "s")

## model file ####
m <- read.jagsfile("code/model_occupancy_ts3.R")

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

