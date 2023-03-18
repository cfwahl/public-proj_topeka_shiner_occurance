
# this script crates a map of centrality betweenness in Iowa

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

## stream polyline
sf_line <- readRDS(file = "data_fmt/data_iowa_stream_network_5km2.rds") 

# import oxbow data
df_iowa_oxbow <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds")

# data prep for oxbow sites ---------------------------------------------------------------

# occurrence present (1) only
sf_oxbow_snapped_1 <- df_iowa_oxbow %>% 
  filter(oxbow_occurrence == "1") %>%
  st_as_sf() %>% # use snapped coordinates
  st_transform(3722) # ensure define CRS again

# occurrence absent (0) only
sf_oxbow_snapped_0 <- df_iowa_oxbow %>% 
  filter(oxbow_occurrence == "0") %>%
  st_as_sf() %>% # use snapped coordinates
  st_transform(3722) # ensure define CRS again

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
          size = 1.2)+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Betweenness") + # label legend 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), # remove lat/long from map
        axis.text.y = element_blank()) #+
  # geom_sf(data = sf_oxbow_snapped_0,
  #         shape = 16, size = 1.5, color = 'red') + 
  # geom_sf(data = sf_oxbow_snapped_1,
  #          shape = 16, size = 1.5, color = 'green') 
