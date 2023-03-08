
# prediction regression of Minnesota oxbow occurrence and betweeness

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# import oxbow rds
df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")

# remove NAs --------------------------------------------------------------------

# number of NAs
colSums(is.na(df_mn_ox_cent))

df_fit <- df_mn_ox_cent %>% 
  drop_na(oxbow_occurrence,
          turb,
          #temp,
          #do_mgl,
          dopercent,
          ph)

# glmm --------------------------------------------------------------------

fit <- glmer(oxbow_occurrence ~  between  + scale(dopercent) + 
                scale(turb) + scale(ph) + (1|watershed), data = df_fit, 
                family = "binomial")

summary(fit)

# figure ------------------------------------------------------------------

df_pred <-  tibble(x = seq(min(df_fit$between),
                           max(df_fit$between),
                           length = 100)) %>% 
  mutate(y = boot::inv.logit(fit@beta[1] + fit@beta[2] * x))

df_fit %>% 
  ggplot(aes(x = between,
             y = oxbow_occurrence)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(x = x,
                y = y)) +
  theme_minimal() +
  xlab("Betwenness") + ylab("Oxbow Occurrence")

