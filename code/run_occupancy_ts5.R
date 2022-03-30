
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(runjags,
               tidyverse,
               MCMCvis,
               mcmcOutput)

# Read in the data
# upstream distance matrix 
up_dmat <- read_csv("data_fmt/watersheds/distance_matrices/siteXup_dist_ws5.csv")
names(up_dmat) <- c("segment_id", paste0("j", 1:30))

up_dmat <- up_dmat %>% 
  mutate(segment_id = str_remove(segment_id, "X"),
         segment_id = as.numeric(segment_id))

segment_set <- unique(up_dmat$segment_id)

up_dmat <- up_dmat %>% 
  mutate(segment_id = as.numeric(as.factor(segment_id)))

# down stream distance matrix
down_dmat <- read_csv("data_fmt/watersheds/distance_matrices/siteXdown_dist_ws5.csv")
names(down_dmat) <- c("segment_id", paste0("j", 1:30))

down_dmat <- down_dmat %>% 
  mutate(segment_id = str_remove(segment_id, "X"),
         segment_id = as.numeric(segment_id))

segment_set <- unique(down_dmat$segment_id)

down_dmat <- down_dmat %>% 
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
Area <- c(scale(data$area))
Slop <- c(scale(data$slopemean))
U <- dplyr::select(up_dmat, -segment_id)
D <- dplyr::select(down_dmat, -segment_id)
Segment_id <- data$segment_id

# jags --------------------------------------------------------------------

## data ####
d_jags <- list(Y = Y,
               Agr = Agr,
               Elv = Elv,
               Area = Area,
               Slop = Slop,
               U = data.matrix(U),
               D = data.matrix(D),
               Segment_id = Segment_id,
               N_sample = length(Y),
               N_site = nrow(U))

d_jags # check to make sure its correct
str(d_jags)

## parameters ####
para <- c("alpha",
          "beta",
          "s")

## model file ####
m <- read.jagsfile("code/bayes_code/model_occupancy_ts5.R")

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

