
# this script produces figures of stream connectivity with stream and oxbow
# sites added if desired. 

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 


# data --------------------------------------------------------------------

# read real and dummy sites
sf_all <- sf::st_read(dsn = "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp") %>%
  st_transform(crs = 3722)

# read oxbow data
sf_oxbow <- st_read("data_fmt/vector/epsg4326_minnesota_oxbow_sites.shp") %>%
  mutate(oxbow_id = row_number())%>% # add unique identifier for oxbows
  st_transform(oxbow, crs = 3722) # transform to utm

# read stream netwrok with connectivity scores
sf_line <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_connectivity.shp") %>%
  rename(connectivity = connectivi)

# snap function ---------------------------------------------------------

st_snap_points = function(f1, f2) {
  
  pacman::p_load(tidyverse,
                 sf)
  
  if (inherits(f1, "sf")) n = nrow(f1)
  if (inherits(f1, "sfc")) n = length(f1)
  
  out = foreach(i = seq_len(n), .combine = bind_rows) %do% {
    
    nrst = sf::st_nearest_points(st_geometry(f1)[i], f2)
    nrst_len = sf::st_length(nrst)
    nrst_mn = which.min(nrst_len)
    
    df0 <- sf::st_cast(nrst[nrst_mn], "POINT")[2] %>%
      sf::st_coordinates() %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(X1 = st_coordinates(f1[i,])[1],
                    Y1 = st_coordinates(f1[i,])[2],
                    distance = nrst_len[nrst_mn],
                    f1[i,]) %>% 
      dplyr::rename(X2 = X,
                    Y2 = Y) %>% 
      dplyr::select(-geometry)
    
    return(df0)
  }
  
  return(out)
}

# data prep for stream sites ---------------------------------------------------------------

# filter real sites only
sf_stream_occ <- sf_all %>%
  filter(!is.na(occurrence)) %>%
  st_transform(crs = 3722)

# snap stream sites to stream line
# X1/Y1 are original locations, X2/Y2 are snapped points
df_stream_occ_snap <- st_snap_points(sf_stream_occ, sf_line)

# occurrence present (1) only
sf_stream_snapped_1 <- df_stream_occ_snap %>% 
  filter(occurrence == "1") %>%
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(sf_stream_occ)) # ensure define CRS again

# occurrence absent (0) only
sf_stream_snapped_0 <- df_stream_occ_snap %>% 
  filter(occurrence == "0") %>%
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(sf_stream_occ)) # ensure define CRS again

# data prep for oxbow sites ---------------------------------------------------------------

# snap oxbows to stream line, original output will be `tibble`
df_oxbow <- st_snap_points(sf_oxbow, sf_line) 

## convert it to sf
sf_point_snapped <- df_oxbow %>% 
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(sf_oxbow)) # ensure define CRS again

# occurrence present (1) only
sf_oxbow_snapped_1 <- df_oxbow %>% 
  filter(occurrence == "1") %>%
  st_as_sf(coords = c("X1", "Y1")) %>% # use snapped coordinates
  st_set_crs(st_crs(sf_oxbow)) # ensure define CRS again

# occurrence absent (0) only
sf_oxbow_snapped_0 <- df_oxbow %>% 
  filter(occurrence == "0") %>%
  st_as_sf(coords = c("X1", "Y1")) %>% # use snapped coordinates
  st_set_crs(st_crs(sf_oxbow)) # ensure define CRS again

# map connectivity with stream, oxbow locations ---------------------------------------------------------------------

# create heat map of connectivity (s)
ggplot(sf_line) + # base map of stream lines
  geom_sf(aes(color = connectivity)) + # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Immigration potential") + # label legend 
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) + # remove lat/long from map
#geom_point(data = sf_stream_snapped_1, aes(x = X1, y = Y1), # stream sites (present)
#           shape = 16, size = 1, color = 'green') + # define point shape and color
#geom_point(data = sf_stream_snapped_0, aes(x = X1, y = Y1), # stream sites (absent)
#           shape = 16, size = 1, color = 'red') + # define point shape and color
#geom_point(data = sf_oxbow_snapped_1, aes(x = X2, y = Y2), # stream sites (present)
#           shape = 16, size = 1, color = 'purple') + # define point shape and color
#geom_point(data = sf_oxbow_snapped_0, aes(x = X2, y = Y2), # stream sites (absent)
#           shape = 16, size = 1, color = 'black') + # define point shape and color
xlab("") + ylab("")

# save map ----------------------------------------------------------------

ggsave(file = "output/figure_map_stream_oxbows.pdf",
       width = 7,
       height = 9)

# join --------------------------------------------------------------------

# join stream connectivity and stream occurrence attributes
df_stream_conn <- sf_stream_occ %>%
  as_tibble() %>%
  left_join(sf_line,
            by = "line_id") %>%
  dplyr::select(-c(STRM_VA, siteid.y, geometry.y)) %>%
  rename(siteid = siteid.x,
         geometry = geometry.x)


# join stream connectivity and oxbow occurrence attributes
df_oxbow_snap <- st_join(x = sf_point_snapped,
                         y = sf_line,
                         join = st_is_within_distance,
                         dist = 10) %>% # join two features, then make as tibble data frame
  st_transform(crs = 3722)

# export data -------------------------------------------------------------

# save stream occurrence and connectivity RDS
#saveRDS(df_stream_conn, file = "data_fmt/data_minnesota_stream_occur_connect.RDS")

# save oxbow occurrence and connectivity RDS
#saveRDS(df_oxbow_snap, file = "data_fmt/data_minnesota_oxbow_occur_connect.RDS")

# stream sites with connectivity scores shapefile
#st_write(df_oxbow_snap,
#         dsn = "data_fmt/vector/epsg3722_minnesota_oxbow_snap.shp",
#         append = FALSE)

# oxbow sites with connectivity scores shapefile
#st_write(df_oxbow_snap,
#         dsn = "data_fmt/vector/epsg3722_minnesota_oxbow_snap.shp",
#         append = FALSE)
