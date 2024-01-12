
# this script crates a map of centrality betweenness in Iowa

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

## stream polyline
df_b <- readRDS(file = "data_fmt/data_iowa_stream_betweenness.rds") %>%
  st_as_sf()

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

# maps --------------------------------------------------------------------

# map of betweenness scores 
ggplot(df_b) + # base map of stream lines
  geom_sf(aes(color = between),
          linewidth = 1.2)+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Betweenness") + # label legend 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), # remove lat/long from map
        axis.text.y = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))

# save map ----------------------------------------------------------------

ggsave(file = "figure/figure_map_iowa_betweenness.pdf",
       width = 7,
       height = 5)
