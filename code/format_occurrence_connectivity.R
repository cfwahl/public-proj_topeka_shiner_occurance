
# this script produces figures of stream connectivity with stream and oxbow
# sites added if desired. 

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# read real and dummy sites
sf_all <- readRDS(file = "data_fmt/data_minnesota_stream_dummy_real_occurrence.rds") %>%
  st_transform(crs = 3722)

# read oxbow data
sf_oxbow <- readRDS(file = "data_fmt/data_minnesota_fmt_oxbows.rds") %>%
  mutate(oxbow_id = row_number()) %>% # add unique identifier for oxbows
  st_transform(oxbow, crs = 3722) # transform to utm

# read stream netwrok with connectivity scores
sf_line <- readRDS(file = "data_fmt/data_minnesota_stream_connectivity.rds") 

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

# data prep for oxbow sites ---------------------------------------------------------------

# snap oxbows to stream line, original output will be `tibble`
df_oxbow <- st_snap_points(sf_oxbow, sf_line) 

## convert it to sf
sf_point_snapped <- df_oxbow %>% 
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(sf_oxbow)) # ensure define CRS again

# join --------------------------------------------------------------------

# join stream connectivity and stream occurrence attributes
df_stream_conn <- sf_stream_occ %>%
  as_tibble() %>%
  left_join(sf_line,
            by = "line_id") %>%
  dplyr::select(-c(STRM_VAL, siteid.y, geometry.y)) %>%
  rename(siteid = siteid.x,
         geometry = geometry.x)

# join stream connectivity and oxbow occurrence attributes
df_oxbow_snap <- st_join(x = sf_point_snapped,
                         y = sf_line,
                         join = st_is_within_distance,
                         dist = 10) %>% # join two features, then make as tibble data frame
  st_transform(crs = 4326)

# export data -------------------------------------------------------------

# save stream occurrence and connectivity RDS
saveRDS(df_stream_conn, file = "data_fmt/data_minnesota_stream_occur_connect.rds")

# save oxbow occurrence and connectivity RDS
saveRDS(df_oxbow_snap, file = "data_fmt/data_minnesota_oxbow_occur_connect.rds")
