
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

f2v <- function(x) {
  tibble(value = c(x),
         col = rep(1:ncol(x), each = nrow(x)),
         row = rep(1:nrow(x), times = ncol(x)))
}

# data --------------------------------------------------------------------

# Read in the data
# upstream distance matrix 
datalist <- readRDS(file = "data_fmt/data_minnesota_distance_matrix_dummy_real.rds") 
m_u <- datalist$m_u
m_d <- datalist$m_d

df_landuse <- readRDS(file ="data_fmt/data_minnesota_stream_landuse_dummy_real.rds") %>%
  as_tibble() %>%
  arrange(siteid) %>% 
  mutate(dummy = ifelse(is.na(occurrence), 1, 0)) %>% 
  relocate(watershed)

df_test <- df_landuse %>% 
  group_by(dummy, watershed) %>% 
  sample_frac(size = 1) %>% # sample fraction of total data
  ungroup() %>% 
  mutate(site0 = as.numeric(factor(siteid))) %>% 
  arrange(site0) %>% 
  relocate(site0)

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
  Temp = df_data$temp_season,
  Area = df_data$area,
  Precp_wet = df_data$precip_wet,
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
  Temp_hat = df_dummy$temp_season,
  Area_hat = df_dummy$area,
  Precp_wet_hat = df_dummy$precip_wet,
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
          "sd_r",
          "s_hat")

## model file ####
m <- read.jagsfile("code/model_occupancy_up_bias.R")

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
mcmc_summary_up_full_test <- MCMCsummary(post$mcmc)
mcmc_summary_up_full_test   # Bayesian analysis

# # export ------------------------------------------------------------------

# ## save mcmc trace plot to "output/"
MCMCtrace(post$mcmc,
          wd = "output/",
          filename = "mcmc_trace_up_full")

# save mcmc output
saveRDS(mcmc_summary_up_full_test, file = "output/mcmc_summary_up_full.rds")

# save jags output
saveRDS(post, file = "output/post_summary_up_full.rds")

# # waic --------------------------------------------------------------------
# 
# ## get mcmc samples of log likelihood
# loglik <- sapply(1:length(Y),
#                  function(i) unlist(post$mcmc[, paste0("ld[", i, "]")]))
# 
# waic_hat_up2 <- loo::waic(loglik)
# 
# 
# ## save mcmc_summary & waic
# save(mcmc_summary_up2, waic_hat_up2,
#      file = "output/mcmc_summary_up2.RData")
# 