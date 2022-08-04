
# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(runjags,
               tidyverse,
               MCMCvis,
               mcmcOutput,
               foreach)

load(here::here("output/mcmc_summary_up2.RData"))


# figure ------------------------------------------------------------------

df_data <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_5km2.gpkg") %>% 
  as_tibble() %>%
  filter(!is.na(occurrence)) %>% 
  mutate(across(.cols = c(starts_with("frac"), area, slope),
                .fns = function(x) c(scale(x))))
  
df_pred <- df_data %>% 
  summarize(across(.cols = c(starts_with("frac"), area, slope),
                   .fns = function(x) seq(from = min(x), to = max(x), length = 100))) %>% 
  select(frac_agri, frac_grass, area, slope) %>% 
  mutate(frac_agri = mean(df_data$frac_agri),
         frac_grass = mean(df_data$frac_grass),
         slope = mean(df_data$slope),
         s = 0)

m <- model.matrix(~ frac_agri + frac_grass + area + slope + s, data = df_pred)

df_beta <- mcmc_summary_up2 %>% 
  mutate(param = rownames(.)) %>% 
  as_tibble() %>% 
  select(param,
         median = `50%`) %>% 
  filter(str_detect(string = param,
                    pattern = "mu_r|beta")) %>% 
  mutate(param = factor(param,
                        levels = c("mu_r",
                                   paste0("beta[", 1:5, "]")))) %>% 
  arrange(param)

df_y <- m %>% 
  as_tibble() %>% 
  mutate(y = c(boot::inv.logit(m %*% pull(df_beta, median))))

df_y %>% 
  ggplot(aes(x = area,
             y = y)) +
  geom_line()


