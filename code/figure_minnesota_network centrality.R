
# this code examines relationships among stream occurrence and betweenness, 
# and oxbow occurrence and betweenness

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# readRDS -----------------------------------------------------------------

# import stream rds
df_mn_strm_cent <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# import oxbow rds
df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")

## stream polyline
sf_line2 <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_connectivity.shp") %>%
  dplyr::select(-c(STRM_VA)) %>% # remove slope variable
  rename(connectivity = connectivi)

# visualize connectivity betweenness relationship ----------------------------------------------------------

# stream connectivity x betweenness 
ggplot(df_mn_strm_cent,
       aes(x = between,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

# oxbow connectivity x betweenness
ggplot(df_mn_ox_cent,
       aes(x = between,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()


# visualize occurrence betweenness relationship -------------------------

# stream betweenness x occurrence
ggplot(df_mn_strm_cent,
       aes(x = between,
           y = stream_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

# oxbow betweenness x occurrence
ggplot(df_mn_ox_cent,
       aes(x = between,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()



# IOWA --------------------------------------------------------------------
# data -----------------------------------------------------------------

# import oxbow data
df_iowa_oxbow <- readRDS(file = "data_fmt/data_iowa_stream_network_centrality.rds")

# visualize relationship ----------------------------------------------------------

# betweenness X oxbow occurrence
ggplot(df_iowa_oxbow,
       aes(x = between,
           y = oxbow_occurrence)) +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

# MN & IA COMBINED --------------------------------------------------------
# data --------------------------------------------------------------------

df_mn_ia_oxbow <- readRDS(file = "data_fmt/data_ia_mn_oxbow_join.rds")

# visualize ---------------------------------------------------------------


# MN + IA oxbow betweenness x occurrence
ggplot(df_mn_ia_oxbows_join,
       aes(x = between,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

# MN + IA oxbow eigenvector x occurrence
ggplot(df_mn_ia_oxbows_join,
       aes(x = eigen,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

