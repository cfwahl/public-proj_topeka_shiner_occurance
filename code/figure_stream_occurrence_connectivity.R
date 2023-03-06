
# this script produces figures examining stream and oxbow occurrence against
# connectivity scores, and histograms of stream connectivity, oxbow connectivity
# and occurrence connectivity

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# read in stream data 
df_stream_conn <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# read stream landuse
sf_stream_point <- readRDS(file = "data_fmt/data_minnesota_stream_landuse_dummy_real.rds")

# join for analysis -------------------------------------------------------

sf_covar <- df_stream_conn %>%
  left_join(sf_stream_point,
            by = "siteid") %>%
  dplyr::select(-c(line_id.y, slope.y, geometry.y, watershed.y)) %>%
  rename(line_id = line_id.x,
         slope = slope.x,
         geometry = geometry.x,
         watershed = watershed.x)
# glmm --------------------------------------------------------------------

fit <- glmer(stream_occurrence ~ connectivity + scale(area) + 
                               #scale(frac_agri) + scale(temp_season) + scale(seg_length) +
                               scale(precip_wet) + scale(slope) + 
                               (1|watershed),
                             data = sf_covar, family = "binomial")

summary(fit)

# figure ----------------------------------------------------------

df_pred <-  tibble(x = seq(min(sf_covar$connectivity),
                           max(sf_covar$connectivity),
                           length = 100)) %>% 
  mutate(y = boot::inv.logit(fit@beta[1] + fit@beta[2] * x))

sf_covar %>% 
  ggplot(aes(x = connectivity,
             y = stream_occurrence)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(x = x,
                y = y)) +
  theme_minimal()
