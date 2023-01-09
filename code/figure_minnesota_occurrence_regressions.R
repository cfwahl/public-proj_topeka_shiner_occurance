# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

## read in mcmc summary output
mcmc_summary_up_full <- readRDS(here::here("output/mcmc_summary_up_full.rds"))

# common data -------------------------------------------------------------

## extract estimated connectivity values
df_s <- mcmc_summary_up_full %>% 
  as_tibble() %>% 
  filter(str_detect(param, "s_hat\\[.{1,}\\]")) %>% # remove alpha/beta, only want s_hat
  mutate(siteid = as.numeric(str_extract(param, "\\d{1,}"))) %>% 
  dplyr::select(siteid,
                s = `50%`) # select only siteid and connectivity value, or s

## data used for analysis
df_actual_occurrence <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_landuse_dummy_real.gpkg") %>% 
  as_tibble() %>%
  filter(!is.na(occurrence)) %>%  # filter out NA data for occurrence 
  left_join(df_s,
            by = "siteid")

## regression slopes  
df_beta <- mcmc_summary_up_full %>% 
  dplyr::select(param,
                median = `50%`) %>% # select param and median columns 
  filter(str_detect(string = param,
                    pattern = "mu_r|beta\\[.{1,}\\]")) %>% # keep only beta and mu values
  mutate(param = factor(param,
                        levels = c("mu_r",
                                   paste0("beta[", 1:5, "]")))) %>% 
  arrange(param) # move mu_r to the top

# predictor 
df_prediction <- df_actual_occurrence %>% 
  summarize(across(.cols = c(frac_agri, temp_season, area, precip_wet, s),
                   .fns = function(x) seq(from = min(x, na.rm = T),
                                          to = max(x, na.rm = T),
                                          length = 100))) %>% # dplyr::select these variables
  mutate(mean_agri = mean(df_actual_occurrence$frac_agri), # add columns with mean values for these variables 
         mean_temp = mean(df_actual_occurrence$temp_season),
         mean_area = mean(df_actual_occurrence$area),
         mean_precip = mean(df_actual_occurrence$precip_wet),
         mean_s = mean(df_actual_occurrence$s))

x_name <- df_prediction %>% 
  dplyr::select(!starts_with("mean")) %>% 
  colnames() # dplyr::select the 9 column names that do not start with "mean"


# prediction --------------------------------------------------------------

## extract mean values for each predictor
df_y <- foreach(i = 1:length(x_name),
                .combine = bind_rows) %do% {
                  
                  X <- df_prediction %>% 
                    dplyr::select(starts_with("mean")) %>% # extract mean value for all rows combined
                    data.matrix()
                  
                  ## replace the column of the predictor of interest
                  x_focus <- df_prediction %>% 
                    dplyr::select(x_name[i]) %>% # select specific column for each loop
                    pull() # make vector
                  
                  X[, i] <- x_focus 
                  M <- model.matrix(~ X)
                  
                  y <- c(boot::inv.logit(M %*% pull(df_beta, median)))
                  
                  df_y0 <- tibble(y = y,
                                  x = x_focus,
                                  focus = colnames(df_prediction)[i])
                  
                  return(df_y0)    
                }

## reformat to long-form
df_data_l <- df_actual_occurrence %>% 
  pivot_longer(cols = x_name,
               values_to = "x",
               names_to = "focus")


# regression plot ---------------------------------------------------------

df_y %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_point(data = df_data_l,
             aes(y = occurrence),
             alpha = 0.2) +
  facet_wrap(facets = ~ focus,
             scales = "free_x",
             strip.position = "bottom",
             labeller = labeller(focus = c(`area` = "Watershed area",
                                           `frac_agri` = "Prop. of agriculture",
                                           `precip_wet` = "Precipitation",
                                           `s` = "Connectivity",
                                           `temp_season` = "Temperature"))) +
  labs(y = "Occurrence prob.") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.x = element_blank())



# caterpillar bar plot -----------------------------------------------------

## read in jags output
post <- readRDS("output/post_summary_up_full.rds")

# connectivity, alpha only
MCMCplot(post$mcmc, 
         params = 'alpha', 
         horiz = FALSE,
         rank = TRUE,
         ref_ovl = FALSE,
         xlab = 'My x-axis label', 
         main = 'MCMCvis plot', 
)

# alpha and beta 
#MCMCplot(post$mcmc, 
#         params = c('alpha', 'beta'),
#         ci = c(50, 95),
#         HPD = TRUE,
#         ref_ovl = TRUE,
#         labels = c('upstream conn.', 'downstream conn.', '% agriculture', 
#                    'temp', 'drainage area', 'precip', 
#                    'tot. connectivity'))

# alpha and b 
MCMCplot(post$mcmc, 
         params = c('alpha', 'b'),
         ci = c(50, 95),
         HPD = TRUE,
         ref_ovl = TRUE,
         labels = c('upstream conn.', 'downstream conn.', '% agriculture', 
                    'temp', 'drainage area', 'precip', 
                    'tot. connectivity'))

