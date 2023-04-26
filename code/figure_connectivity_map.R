
# this script produces figures of stream connectivity with stream and oxbow
# sites added if desired. 

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# read stream netwrok with connectivity scores
sf_line <- readRDS(file = "data_fmt/data_minnesota_stream_connectivity.rds")

# import stream rds
df_stream_occ_snap <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# import oxbow rds
df_oxbow <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")

# data prep for stream sites ---------------------------------------------------------------

# occurrence present (1) only
sf_stream_snapped_1 <- df_stream_occ_snap %>% 
  filter(stream_occurrence == "1") %>%
  st_as_sf() %>% # use snapped coordinates
  st_set_crs(st_crs(3722)) # ensure define CRS again

# occurrence absent (0) only
sf_stream_snapped_0 <- df_stream_occ_snap %>% 
  filter(stream_occurrence == "0") %>%
  st_as_sf() %>% # use snapped coordinates
  st_set_crs(st_crs(3722)) # ensure define CRS again

# data prep for oxbow sites ---------------------------------------------------------------

# occurrence present (1) only
sf_oxbow_snapped_1 <- df_oxbow %>% 
  filter(oxbow_occurrence == "1") %>%
  st_as_sf() %>% # use snapped coordinates
  st_transform(3722) # ensure define CRS again

# occurrence absent (0) only
sf_oxbow_snapped_0 <- df_oxbow %>% 
  filter(oxbow_occurrence == "0") %>%
  st_as_sf() %>% # use snapped coordinates
  st_transform(3722) # ensure define CRS again

# map connectivity with stream, oxbow locations ---------------------------------------------------------------------

# create heat map of connectivity (s)
plot1 <- ggplot(sf_line) + # base map of stream lines
  geom_sf(aes(color = connectivity),
          linewidth = 1.2) + # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Connectivity") + # label legend 
  theme_minimal() +
  theme(axis.text.x = element_blank(), # remove lat/long from map
        axis.text.y = element_blank()) +
  labs(tag = "A") #+ 
# geom_sf(data = sf_stream_snapped_1, 
#          shape = 16, size = 1.5, color = 'green') 
# geom_sf(data = sf_stream_snapped_0, 
#          shape = 16, size = 1.5, color = 'red') + 
# geom_sf(data = sf_oxbow_snapped_1,
#          shape = 16, size = 1.5, color = 'green') +
# geom_sf(data = sf_oxbow_snapped_0,
#          shape = 16, size = 1.5, color = 'red')

# save map ----------------------------------------------------------------

ggsave(file = "figure/figure_map_stream_connectivity.pdf",
       width = 7,
       height = 9)
