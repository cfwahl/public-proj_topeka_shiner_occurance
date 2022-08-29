

### load mcmc_summary
load("output/mcmc_summary_up_full.RData")

### load jags output 
load("output/post_summary_up_full.RData")


df_landuse <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_2_5km2.gpkg") %>% 
  as_tibble() %>%
  arrange(siteid) %>% 
  mutate(dummy = ifelse(is.na(occrrnc), 1, 0)) %>% 
  relocate(watrshd)

df_test <- df_landuse %>% 
  group_by(dummy, watrshd) %>% 
  sample_frac(size = 1) %>% # sample fraction of total data
  ungroup() %>% 
  mutate(site0 = as.numeric(factor(siteid))) %>% 
  arrange(site0) %>% 
  relocate(site0)

# assign variables
# capitalize "data" in Jags codes to distinguish from parameters
df_data <- filter(df_test, !is.na(occrrnc))
df_dummy <- filter(df_test, is.na(occrrnc))



df_s <- mcmc_summary_up_full %>% 
  mutate(param = rownames(.)) %>% 
  as_tibble() %>% 
  mutate(param_id = str_extract(param, pattern = "\\[\\d{1,}\\]"),
         param_id = str_remove_all(param_id, pattern = "\\[|\\]"),
         site0 = ifelse(str_detect(param, pattern = "s\\[.{1,}\\]"),
                        as.numeric(param_id),
                        NA)) %>% 
  filter(str_detect(param, pattern = "s\\[.{1,}\\]"))

m_beta <- MCMCvis::MCMCchains(post$mcmc, param = c("mu_r", "beta"))
X <- model.matrix(~ rep(mean(df_data$frac_gr), 373) +
                    rep(mean(df_data$tmp_ssn), 373) +
                    rep(mean(df_data$area), 373) +
                    rep(mean(df_data$prcp_wt), 373) +
                    seq(min(df_s$`50%`), max(df_s$`50%`), length = 373))  

Y <- boot::inv.logit(X %*% t(m_beta))

df_pred <- apply(Y, MARGIN = 1, FUN = quantile, c(0.025, 0.5, 0.975)) %>% 
  t() %>% 
  as_tibble() %>% 
  rename(lower = `2.5%`,
         median = `50%`,
         higher = `97.5%`) %>% 
  mutate(s = X[, 6])

df_plot <- df_s %>%
  left_join(df_data,
            by = "site0")

ggplot(data = df_plot,
       aes(x = `50%`,
           y = occrrnc)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = median,
                x = s)) +
  geom_ribbon(data = df_pred,
              aes(x = s,
                  y = median,
                  ymin = lower,
                  ymax = higher),
              color = NA,
              fill = "steelblue",
              alpha = 0.2) +
  theme_bw()



# remove NA only if actual data exist -------------------------------------

pacman::p_load(sf,
               tidyverse)

## data
df0 <- st_read("data_fmt/vector/epsg4326_mn_dnr_fws_dummy_real_occurrence.shp") %>%
  as_tibble() %>% 
  group_by(line_id) %>% 
  mutate(type = ifelse(all(is.na(occurrence)),
                       "dummy",
                       "actual"))

df_a <- df0 %>% 
  filter(type == "actual") %>% 
  drop_na(occurrence) %>% 
  bind_rows(filter(df0, type == "dummy"))


## stream line
sf_line <- list.files("data_fmt/vector",
           pattern = "connectivity_dummy_5km2.shp",
           full.names = T) %>% 
  st_read()

## 
load("output/mcmc_summary_up_full.RData")


