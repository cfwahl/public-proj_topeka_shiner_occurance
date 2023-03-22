
# prediction regression between Iowa oxbow occurrence and betweenness 

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data -----------------------------------------------------------------

# import oxbow data
df_iowa_oxbow <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds")

# remove NAs --------------------------------------------------------------------

# number of NAs
colSums(is.na(df_iowa_oxbow))

# drop NAs
df_fit <- df_iowa_oxbow %>% 
  drop_na(oxbow_occurrence,
          temp,
          turb)

# glmm --------------------------------------------------------------------

fit <- glmer(oxbow_occurrence ~ between + scale(temp) + scale(turb) + 
               (1|watershed), data = df_fit, family = "binomial")

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
  xlab("Betwenness") + ylab("Prob. of Oxbow Occurrence") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))
