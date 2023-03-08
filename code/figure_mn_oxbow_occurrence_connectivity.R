
# prediction regression of Minnesota oxbow occurrence and stream reach connectivity

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# read in oxbow data
df_oxbow_snap <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds") 

# remove NAs --------------------------------------------------------------------

# number of NAs
colSums(is.na(df_oxbow_snap))

df_fit <- df_oxbow_snap %>% 
  drop_na(oxbow_occurrence,
          turb,
          #temp,
          #do_mgl,
          dopercent,
          ph)

# glmm --------------------------------------------------------------------

fit <- glmer(oxbow_occurrence ~  connectivity + scale(turb) +
                           scale(dopercent) +  scale(ph) + (1|line_id),
                           data = df_fit, family = "binomial")

summary(fit)


# figure ------------------------------------------------------------------

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
                y = y)) +
  theme_minimal() +
  xlab("Connectivity") + ylab("Oxbow Occurrence")
