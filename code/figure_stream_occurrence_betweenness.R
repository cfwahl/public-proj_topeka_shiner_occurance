
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

# glmm --------------------------------------------------------------------

fit <- glmer(stream_occurrence ~ between + scale(area) + scale(frac_agri) + 
               scale(seg_length) + scale(temp_season) + scale(precip_wet) + 
               scale(slope) +  (1|watershed), data = sf_covar, family = "binomial")

summary(fit)

# figure ----------------------------------------------------------

df_pred <-  tibble(x = seq(min(sf_covar$between),
                           max(sf_covar$between),
                           length = 113)) %>% 
  mutate(y = boot::inv.logit(fit@beta[1] + fit@beta[2] * x))

sf_covar %>% 
  ggplot(aes(x = between,
             y = stream_occurrence)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(x = x,
                y = y)) +
  theme_minimal()
