df_s <- mcmc_summary_up2 %>% 
  mutate(param = rownames(.)) %>% 
  as_tibble() %>% 
  mutate(param_id = str_extract(param, pattern = "\\[\\d{1,}\\]"),
         param_id = str_remove_all(param_id, pattern = "\\[|\\]"),
         site0 = ifelse(str_detect(param, pattern = "s\\[.{1,}\\]"),
                        as.numeric(param_id),
                        NA)) %>% 
  filter(str_detect(param, pattern = "s\\[.{1,}\\]"))

m_beta <- MCMCvis::MCMCchains(post$mcmc, param = c("mu_r", "beta"))
X <- model.matrix(~ rep(mean(df_data$frac_agri), 100) +
                    rep(mean(df_data$frac_grass), 100) +
                    rep(mean(df_data$area), 100) +
                    rep(mean(df_data$slope), 100) +
                    seq(min(df_s$`50%`), max(df_s$`50%`), length = 100))  

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
           y = occurrence)) +
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


