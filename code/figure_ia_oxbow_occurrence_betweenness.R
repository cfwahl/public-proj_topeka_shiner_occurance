
# prediction regression between Iowa oxbow occurrence (Iowa) and betweenness 

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data -----------------------------------------------------------------

# import oxbow data
df_iowa_oxbow <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds") %>%
  filter(habitat=='Restored_Oxbow')

# remove NAs --------------------------------------------------------------------

# number of NAs
colSums(is.na(df_iowa_oxbow))

df_fit <- df_iowa_oxbow %>% 
  drop_na(oxbow_occurrence,
          temp,
          do_mgl,
          ph)

# glmm --------------------------------------------------------------------

fit <- glmer(oxbow_occurrence ~ between + scale(ph) + scale(cond) +
                                (1|line_id) + scale(do_mgl) + scale(temp) +
                                scale(cond) + scale(turb),
                              data = df_iowa_oxbow, family = "binomial")

summary(fit)

# figure ----------------------------------------------------------

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
