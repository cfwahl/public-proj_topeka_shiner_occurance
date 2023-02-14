
# this script does a series of generalized linear mixed models examining 
# stream and oxbow occurrence, betweenness, and how they are explained by
# landuse and local vairbales

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data -----------------------------------------------------------------

# import stream data
df_mn_strm_cent <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# import oxbow data
df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")

# read stream landuse
sf_stream_point <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_landuse_dummy_real.gpkg")

## stream point with covariates for GLMM
sf_covar <- df_mn_strm_cent %>%
  left_join(sf_stream_point,
            by = "siteid") %>%
  dplyr::select(-c(line_id.y, slope.y, geom, watershed.y)) %>%
  rename(line_id = line_id.x,
         slope = slope.x,
         geometry = geometry.x,
         watershed = watershed.x)

# glmm occurrence --------------------------------------------------------------------

# oxbow_occurrence and connectivity
glmer_ox_occ_conn <- glmer(oxbow_occurrence ~  connectivity + scale(turb) +
                             scale(temp) + scale (do_mgl) + scale(dopercent) +
                             scale(ph) + (1|watershed),
                           data = df_mn_ox_cent, family = "binomial")

summary(glmer_ox_occ_conn)

# oxbow_occurrence and betweenness
glmer_ox_occ_betw <- glmer(oxbow_occurrence ~  between + 
                             scale(dopercent) + scale(ph) + (1|watershed),
                           data = df_mn_ox_cent, family = "binomial")

summary(glmer_ox_occ_betw)

# betweenness and stream_occurrence
glmer_strm_between_occ <- glmer(stream_occurrence ~ between + scale(area) + 
                                  frac_agri + scale(temp_season) + scale(seg_length) +
                                  scale(precip_wet) + scale(slope) + 
                                  (1|watershed),
                                data = sf_covar, family = "binomial")

summary(glmer_strm_between_occ)

# connectivity and stream_occurrence
glmer_strm_conn_occ <- glmer(stream_occurrence ~ connectivity + scale(area) + 
                                  frac_agri + scale(temp_season) + scale(seg_length) +
                                  scale(precip_wet) + scale(slope) + 
                                  (1|watershed),
                                data = sf_covar, family = "binomial")

summary(glmer_strm_conn_occ)

# stream connectivity and betweenness
glmer_strm_between_conn <- glmer(connectivity ~ between + (1|watershed),
                                 data = sf_covar, family = Gamma(link = "log"))

summary(glmer_strm_between_conn)

### oxbow connectivity and betweenness
glmer_ox_between_conn <- glmer(connectivity ~ between + (1|watershed),
                               data = df_mn_ox_cent, family = Gamma(link = "log"))

summary(glmer_ox_between_conn)

# correlation  ------------------------------------------------------------

# correlation between stream connectivity and betweenness 
corr <- cor.test(x=df_mn_strm_cent$connectivity, y=df_mn_strm_cent$between, 
                  method = 'spearman')
corr



# IOWA --------------------------------------------------------------------
# data -----------------------------------------------------------------

# import oxbow data
df_iowa_oxbow <- readRDS(file = "data_fmt/data_iowa_stream_network_centrality.rds")


# glmm --------------------------------------------------------------------

### iowa oxbow occurrence and betweenness 
glmer_str_eigen_conn <- glmer(oxbow_occurrence ~ between + scale(ph) +
                                (1|watershed) + scale(do_mgl) + scale(temp),
                              data = df_iowa_oxbow, family = "binomial")

summary(glmer_str_eigen_conn)




# MN & IA COMBINED --------------------------------------------------------
# data --------------------------------------------------------------------

df_mn_ia_oxbow <- readRDS(file = "data_fmt/data_ia_mn_oxbow_join.rds")

# analyze -----------------------------------------------------------------

# betweenness and join oxbow_occurrence
glmer_strm_between_occ <- glmer(oxbow_occurrence ~ between + scale(do_mgl) + 
                                  scale(turbidity) + scale(do_percent) + scale(ph) +
                                  scale(temperature) +  (1|watershed),
                                data = df_mn_ia_oxbow, family = "binomial")

summary(glmer_strm_between_occ)

