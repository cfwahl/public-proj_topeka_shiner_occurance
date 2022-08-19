# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(runjags,
               tidyverse,
               MCMCvis,
               mcmcOutput,
               foreach)

load("output/mcmc_summary_up_full.RData")


# common data -------------------------------------------------------------

## extract estimated connectivity values
df_s <- mcmc_summary_up_full %>% 
  as_tibble(rownames = "param") %>% 
  filter(str_detect(param, "s\\[.{1,}\\]")) %>% 
  mutate(siteid = as.numeric(str_extract(param, "\\d{1,}"))) %>% 
  select(siteid,
         s = `50%`)

## data used for analysis
df_data <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_2_5km2.gpkg") %>% 
  as_tibble() %>%
  filter(!is.na(occrrnc)) %>% 
  left_join(df_s,
            by = "siteid")

## regression slopes  
df_beta <- mcmc_summary_up_full %>% 
  as_tibble(rownames = "param") %>% 
  select(param,
         median = `50%`) %>% 
  filter(str_detect(string = param,
                    pattern = "mu_r|beta\\[.{1,}\\]")) %>% 
  mutate(param = factor(param,
                        levels = c("mu_r",
                                   paste0("beta[", 1:5, "]")))) %>% 
  arrange(param)

df_pred <- df_data %>% 
  summarize(across(.cols = c(starts_with("frac"), tmp_ssn, area, prcp_wt, s),
                   .fns = function(x) seq(from = min(x, na.rm = T),
                                          to = max(x, na.rm = T),
                                          length = 100))) %>% 
  mutate(mean_agri = mean(df_data$frac_gr),
         mean_temp = mean(df_data$tmp_ssn),
         mean_area = mean(df_data$area),
         mean_precip = mean(df_data$prcp_wt),
         mean_s = mean(df_data$s))

x_name <- df_pred %>% 
  select(!starts_with("mean")) %>% 
  colnames()


# prediction --------------------------------------------------------------

## extract mean values for each predictor
df_y <- foreach(i = 1:length(x_name),
                .combine = bind_rows) %do% {
                  
                  X <- df_pred %>% 
                    select(starts_with("mean")) %>% 
                    data.matrix()
                  
                  ## replace the column of the predictor of interest
                  x_focus <- df_pred %>% 
                    select(x_name[i]) %>% 
                    pull()
                  
                  X[, i] <- x_focus
                  M <- model.matrix(~ X)
                  
                  y <- c(boot::inv.logit(M %*% pull(df_beta, median)))
                  
                  df_y0 <- tibble(y = y,
                                  x = x_focus,
                                  focus = colnames(df_pred)[i])
                  
                  return(df_y0)    
                }

## reformat to long-form
df_data_l <- df_data %>% 
  pivot_longer(cols = x_name,
               values_to = "x",
               names_to = "focus")


# regression plot ---------------------------------------------------------

df_y %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line() +
  geom_point(data = df_data_l,
             aes(y = occrrnc),
             alpha = 0.2) +
  facet_wrap(facets = ~ focus,
             scales = "free_x",
             strip.position = "bottom",
             labeller = labeller(focus = c(`area` = "Watershed area",
                                           `frac_gr` = "Prop. of agriculture",
                                           `prcp_wt` = "Precipitation",
                                           `s` = "Connectivity",
                                           `tmp_ssn` = "Temperature"))) +
  labs(y = "Occurrence prob.") +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.x = element_blank())
  


# catapiller bar plot -----------------------------------------------------

load(file = "output/post_summary_up_full.Rdata")

MCMCplot(post$mcmc, 
         params = 'alpha', 
         horiz = FALSE,
         rank = TRUE,
         ref_ovl = FALSE,
         xlab = 'My x-axis label', 
         main = 'MCMCvis plot', 
)

MCMCplot(post$mcmc, 
         params = 'beta', 
         ci = c(50, 95),
         HPD = TRUE,
         ref_ovl = TRUE)

MCMCplot(post$mcmc, 
         params = c('alpha', 'beta'),
         ci = c(50, 95),
         HPD = TRUE,
         ref_ovl = TRUE,
         labels = c('upstream conn.', 'downstream conn.', '% agriculture', 
                    'temp', 'drainage area', 'precip', 
                    'tot. connectivity'))
