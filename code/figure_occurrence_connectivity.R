
# this script produces figures examining stream and oxbow occurrence against
# connectivity scores, and histograms of stream connectivity, oxbow connectivity
# and occurrence connectivity

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# MINNESOTA ---------------------------------------------------------------
# data --------------------------------------------------------------------

# read in stream data 
df_stream_conn <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# read in oxbow data
df_oxbow_snap <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds") 

# read stream netwrok with connectivity scores
sf_line <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_connectivity.shp") %>%
  rename(connectivity = connectivi)

# occurrence, connectivity ------------------------------------------------

# plot of stream occurrence and connectivity
ggplot(df_stream_conn,
       aes(x = connectivity,
           y = stream_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point() +
  theme(rect = element_blank()) 

# plot of oxbow occurrence and connectivity
ggplot(df_oxbow_snap,
       aes(x = connectivity,
           y = oxbow_occurrence)) +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial"))+
  geom_point() 

# histograms -----------------------------------------------------------------

# histogram of streamline connectivity
ggplot(sf_line, aes(x=connectivity)) + 
  geom_histogram(binwidth=0.2, color="black", fill="grey") +
  theme(rect = element_blank()) # remove grid from figure

# histogram of sampling site connectivity
ggplot(df_stream_conn, aes(x=connectivity)) + 
  geom_histogram(binwidth=0.2, color="black", fill="grey") +
  theme(rect = element_blank())

# histogram of oxbow connectivity 
ggplot(df_oxbow_snap, aes(x=connectivity)) + 
  geom_histogram(binwidth=0.2, color="black", fill="grey") +
  theme(rect = element_blank())
