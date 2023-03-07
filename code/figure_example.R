
# this script does a series of generalized linear mixed models examining 
# stream and oxbow occurrence, betweenness, and how they are explained by
# landuse and local vairbales

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data -----------------------------------------------------------------

# import oxbow data
df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")

# analysis ----------------------------------------------------------------

# glmm occurrence --------------------------------------------------------------------

df_fit <- df_mn_ox_cent %>% 
  drop_na(oxbow_occurrence,
          connectivity,
          turb,
          temp,
          do_mgl,
          ph)

# oxbow_occurrence and connectivity
fit <- glmer(oxbow_occurrence ~  connectivity +
               scale(turb) +
               scale(temp) +
               scale(do_mgl) +
               scale(ph) + (1|watershed),
             data = df_fit, family = "binomial")


# figure example ----------------------------------------------------------

df_pred <-  tibble(x = seq(min(df_fit$connectivity),
                           max(df_fit$connectivity),
                           length = 100)) %>% 
  mutate(y = boot::inv.logit(fit@beta[1] + fit@beta[2] * x))

df_fit %>% 
  ggplot(aes(x = connectivity,
             y = oxbow_occurrence)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(x = x,
                y = y))
