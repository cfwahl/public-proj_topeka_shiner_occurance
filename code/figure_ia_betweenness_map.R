
#this script crates a map of betweenness in Iowa

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# IOWA --------------------------------------------------------------------
# data --------------------------------------------------------------------

## stream polyline
sf_line <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_stream_network_5km2.shp") 

# network centrality ---------------------------------------------------------

# betweenness
df_b <- lapply(X = 1:n_distinct(sf_line$watershed),
               FUN = function(x) {
                 df_subset <- sf_line %>% 
                   filter(watershed == x)
                 
                 y <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   betweenness(normalized = TRUE)
                 
                 out <- df_subset %>% 
                   mutate(between = y) %>% 
                   relocate(between)
                 
                 return(out)
               }) %>% 
  bind_rows() 

# maps --------------------------------------------------------------------

# map of betweenness scores 
ggplot(df_b) + # base map of stream lines
  geom_sf(aes(color = between),
          size = 1)+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Betweenness") + # label legend 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), # remove lat/long from map
        axis.text.y = element_blank())
