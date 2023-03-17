
# this script produces prediction plots between stream occurrence and 
# connectivity from the Bayes model output. 

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# read in mcmc summary output
mcmc_summary_up_full <- readRDS(here::here("output/mcmc_summary_up_full.rds"))

# common data -------------------------------------------------------------

mcmc_summary_up_full <- tibble::rownames_to_column(mcmc_summary_up_full, "param") 


## extract estimated connectivity values
df_s <- mcmc_summary_up_full %>% 
  as_tibble() %>% 
  filter(str_detect(param, "s_hat\\[.{1,}\\]")) %>% # remove alpha/beta, only want s_hat
  mutate(siteid = as.numeric(str_extract(param, "\\d{1,}"))) %>% 
  dplyr::select(siteid,
                s = `50%`) # select only siteid and connectivity value, or s

## data used for analysis
df_actual_occurrence <- readRDS(file = "data_fmt/data_minnesota_stream_landuse_dummy_real.rds") %>% 
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
               names_to = "focus") %>%
  filter(focus=="s")

# regression plot ---------------------------------------------------------

df_y0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line(aes(x = x,
                y = y)) +
  geom_point(data = df_data_l,
             aes(y = occurrence),
             alpha = 0.2) +
  facet_wrap(facets = ~ focus,
             scales = "free_x",
             strip.position = "bottom",
             labeller = labeller(focus = c(`s` = "Connectivity"))) +
  labs(y = "Prob. Stream Occurrence") +
  theme_minimal() +
  theme(strip.placement = "outside",
        axis.title.x = element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=12),
        strip.text.x = element_text(size = 12)) 
