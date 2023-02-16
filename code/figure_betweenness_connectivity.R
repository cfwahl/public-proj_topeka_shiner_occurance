
# this code examines relationships among connectivity and betweenness in Minnesota
# streams and oxbows

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# MINNESOTA ---------------------------------------------------------------
# data --------------------------------------------------------------------

# import stream rds
df_mn_strm_cent <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# import oxbow rds
df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")

# visualize connectivity-betweenness ----------------------------------------------------------

# stream connectivity x betweenness 
ggplot(df_mn_strm_cent,
       aes(x = between,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

# oxbow connectivity x betweenness
#ggplot(df_mn_ox_cent,
#       aes(x = between,
#           y = connectivity))  +
#  geom_smooth(method = 'glm', se = TRUE,
#              method.args = list(Gamma(link = 'log'))) + 
#  geom_point()
