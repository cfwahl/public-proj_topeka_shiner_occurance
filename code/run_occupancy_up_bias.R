
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(runjags,
               tidyverse,
               MCMCvis,
               mcmcOutput,
               foreach)

f2v <- function(x) {
  tibble(value = c(x),
         col = rep(1:ncol(x), each = nrow(x)),
         row = rep(1:nrow(x), times = ncol(x)))
}

# data --------------------------------------------------------------------

# Read in the data
# upstream distance matrix 
load("data_fmt/distance_matrix_dummy.RData")

df_landuse <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_5km2.gpkg") %>% 
  as_tibble() %>%
  arrange(siteid) %>% 
  mutate(dummy = ifelse(is.na(occurrence), 1, 0)) %>% 
  relocate(watershed)

df_test <- df_landuse %>% 
  group_by(dummy, watershed) %>% 
  sample_frac(size = 0.2)

# assign variables
# capitalize "data" in Jags codes to distinguish from parameters
df_data <- filter(df_test, !is.na(occurrence))
df_dummy <- filter(df_test, is.na(occurrence))

## for actual sites
U <- m_u[df_data$siteid, df_data$siteid]
D <- m_d[df_data$siteid, df_data$siteid]

TD <- U + D
M <- foreach(i = seq_len(nrow(TD)), .combine = rbind) %do% {
  x <- TD[i,]
  y <- ifelse(x == 0 | x > 25,
              yes = 0,
              no = 1)
  return(y)
}

list_d <- lapply(list(U, D, TD), FUN = f2v)
names(list_d) <- c("U", "D", "TD")

## for dummy sites
U_hat <- m_u[df_dummy$siteid, df_dummy$siteid]
D_hat <- m_d[df_dummy$siteid, df_dummy$siteid]

TD_hat <- U_hat + D_hat
M_hat <- foreach(i = seq_len(nrow(TD_hat)),
                 .combine = rbind) %do% {
  x <- TD_hat[i,]
  y <- ifelse(x == 0 | x > 25,
              yes = 0,
              no = 1)
  return(y)
}

list_d_hat <- lapply(list(U_hat, D_hat, TD_hat), FUN = f2v)
names(list_d_hat) <- c("U", "D", "TD")


# jags --------------------------------------------------------------------

## data ####
d_jags <- list(# actual data
               Y = df_data$occurrence,
               Agr = df_data$frac_agri,
               Gras = df_data$frac_grass,
               Area = df_data$area,
               Slop = df_data$slope,
               Watshed = df_data$watershed,
               N_sample = n_distinct(df_data$siteid),
               N_watshed = n_distinct(df_data$watershed),
               M = M,
               Incidence = df_data$occurrence,
               V_U = list_d$U$value,
               V_D = list_d$D$value,
               Col = list_d$U$col,
               Row = list_d$U$row,
               Ndim = length(list_d$U$value),
               
               # dummy data
               Agr_hat = df_dummy$frac_agri,
               Gras_hat = df_dummy$frac_grass,
               Area_hat = df_dummy$area,
               Slop_hat = df_dummy$slope,
               Watshed_hat = df_dummy$watershed,
               N_dummy = n_distinct(df_dummy$siteid),
               M_hat = M_hat,
               V_U_hat = list_d_hat$U$value,
               V_D_hat = list_d_hat$D$value,
               Col_hat = list_d_hat$U$col,
               Row_hat = list_d_hat$U$row,
               Ndim_hat = length(list_d_hat$U$value))

d_jags # check to make sure its correct
str(d_jags)

## parameters ####
para <- c("alpha",
          "beta",
          "b",
          "mu_r",
          "sd_r")

## model file ####
m <- read.jagsfile("code/model_occupancy_up_bias.R")

## mcmc setup ####
n_ad <- 100 
n_iter <- 100#2.0E+4 #number of draws
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
mcmc_summary_up2 <- MCMCsummary(post$mcmc)  
# mcmc_summary_up2   # Bayesian analysis
# 
# # waic --------------------------------------------------------------------
# 
# ## get mcmc samples of log likelihood
# loglik <- sapply(1:length(Y),
#                  function(i) unlist(post$mcmc[, paste0("ld[", i, "]")]))
# 
# waic_hat_up2 <- loo::waic(loglik)
# 
# 
# # export ------------------------------------------------------------------
# 
# ## save mcmc trace plot to "output/"
# MCMCtrace(post$mcmc,
#           wd = "output/",
#           filename = "mcmc_trace_up2")
# 
# ## save mcmc_summary & waic
# save(mcmc_summary_up2, waic_hat_up2,
#      file = "output/mcmc_summary_up2.RData")
