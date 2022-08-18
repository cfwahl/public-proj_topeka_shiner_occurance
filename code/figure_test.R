
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


# figure area ------------------------------------------------------------------

df_data <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_2_5km2.gpkg") %>% 
  as_tibble() %>%
  filter(!is.na(occrrnc)) %>% 
  mutate(across(.cols = c(starts_with("frac"), area, tmp_ssn, prcp_wt),
                .fns = function(x) c(scale(x))))

# code is written for AREA to be plotted
df_pred <- df_data %>% 
  summarize(across(.cols = c(starts_with("frac"), area, tmp_ssn, prcp_wt),
                   .fns = function(x) seq(from = min(x), to = max(x), length = 373))) %>% 
  select(frac_gr, tmp_ssn, area, prcp_wt) %>% 
  mutate(agri = mean(df_data$frac_gr),
         temp = mean(df_data$tmp_ssn),
         precip = mean(df_data$prcp_wt),
         s = 0)

m <- model.matrix(~ frac_gr + tmp_ssn + area + prcp_wt + s, data = df_pred)

df_beta <- mcmc_summary_up_full %>% 
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
  geom_point() + 
  geom_line() 


# fraction agriculture figure ---------------------------------------------------------------

df_pred <- df_data %>% 
  summarize(across(.cols = c(starts_with("frac"), area, tmp_ssn, prcp_wt),
                   .fns = function(x) seq(from = min(x), to = max(x), length = 373))) %>% 
  select(frac_gr, tmp_ssn, area, prcp_wt) %>% 
  mutate(area = mean(df_data$area),
         tmp_ssn = mean(df_data$tmp_ssn),
         prcp_wt = mean(df_data$prcp_wt),
         s = 0)

m <- model.matrix(~ frac_gr + tmp_ssn + area + prcp_wt + s, data = df_pred)

df_beta <- mcmc_summary_up_full %>% 
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
  ggplot(aes(x = frac_gr,
             y = y)) +
  geom_line() +
  geom_point()



# Temp figure ---------------------------------------------------------------


df_pred <- df_data %>% 
  summarize(across(.cols = c(starts_with("frac"), area, tmp_ssn, prcp_wt),
                   .fns = function(x) seq(from = min(x), to = max(x), length = 373))) %>% 
  select(frac_gr, tmp_ssn, area, prcp_wt) %>% 
  mutate(area = mean(df_data$area),
         agri = mean(df_data$frac_gr),
         precip = mean(df_data$prcp_wt),
         s = 0)

m <- model.matrix(~ frac_gr + tmp_ssn + area + prcp_wt + s, data = df_pred)

df_beta <- mcmc_summary_up_full %>% 
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
  ggplot(aes(x = tmp_ssn,
             y = y)) +
  geom_line() + 
  geom_point()


# Precipitation figure ---------------------------------------------------------------


df_pred <- df_data %>% 
  summarize(across(.cols = c(starts_with("frac"), area, tmp_ssn, prcp_wt),
                   .fns = function(x) seq(from = min(x), to = max(x), length = 373))) %>% 
  select(frac_gr, tmp_ssn, area, prcp_wt) %>% 
  mutate(area = mean(df_data$area),
         temp = mean(df_data$tmp_ssn),
         agri = mean(df_data$frac_gr),
         s = 0)

m <- model.matrix(~ frac_gr + tmp_ssn + area + prcp_wt + s, data = df_pred)

df_beta <- mcmc_summary_up_full %>% 
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
  ggplot(aes(x = prcp_wt,
             y = y)) +
  geom_line() +
  geom_point()


# s figure ---------------------------------------------------------------


df_pred <- df_data %>% 
  summarize(across(.cols = c(starts_with("frac"), area, slope),
                   .fns = function(x) seq(from = min(x), to = max(x), length = 100))) %>% 
  select(frac_agri, frac_grass, area, slope) %>% 
  mutate(area = mean(df_data$area),
         frac_grass = mean(df_data$frac_grass),
         frac_agri = mean(df_data$frac_agri),
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
  ggplot(aes(x = slope,
             y = y)) +
  geom_line()



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
