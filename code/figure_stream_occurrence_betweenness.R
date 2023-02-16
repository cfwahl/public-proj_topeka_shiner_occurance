
# this code examines relationships among stream and oxbow occurrence and 
# betweenness (Minnesota)

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

# visualize occurrence-betweenness relationship -------------------------

# stream betweenness x occurrence
ggplot(df_mn_strm_cent,
       aes(x = between,
           y = stream_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

# oxbow betweenness x occurrence
#ggplot(df_mn_ox_cent,
#       aes(x = between,
#           y = oxbow_occurrence))  +
#  geom_smooth(method = 'glm', se = TRUE,
#              method.args = list(binomial(link = 'logit'))) + 
#  geom_point()
