
# prediction regression of Minnesota stream occurrence and betweeness

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# import stream rds
df_mn_strm_cent <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# read stream landuse
sf_stream_point <- readRDS(file = "data_fmt/data_minnesota_stream_landuse_dummy_real.rds")

# join for analysis -------------------------------------------------------

sf_covar <- df_mn_strm_cent %>%
  left_join(sf_stream_point,
            by = "siteid") %>%
  dplyr::select(-c(line_id.y, slope.y, geometry.y, watershed.y)) %>%
  rename(line_id = line_id.x,
         slope = slope.x,
         geometry = geometry.x,
         watershed = watershed.x)

# remove NAs --------------------------------------------------------------

# drop NAs
df_fit <- sf_covar %>% 
  drop_na(stream_occurrence,
          between,
          frac_agri,
          temp_mean)

# glmm --------------------------------------------------------------------

fit <- glmer(stream_occurrence ~ between + scale(frac_agri) + scale(temp_mean) +
               (1|watershed), data = df_fit, family = "binomial")

summary(fit)

# figure ----------------------------------------------------------

df_pred <-  tibble(x = seq(min(sf_covar$between),
                           max(sf_covar$between),
                           length = 100)) %>% 
  mutate(y = boot::inv.logit(fit@beta[1] + fit@beta[2] * x))

sf_covar %>% 
  ggplot(aes(x = between,
             y = stream_occurrence)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(x = x,
                y = y)) +
  theme_minimal() +
  xlab("Betwenness") + ylab("Prob. of Stream Occurrence") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))

# save figure ----------------------------------------------------------------

ggsave(file = "figure/figure_stream_occurrence_X_betweenness.pdf",
       width = 7,
       height = 5)
