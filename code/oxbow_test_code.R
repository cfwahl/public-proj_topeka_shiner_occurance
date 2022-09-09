# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               tmap,
               sf,
               mapview, 
               foreach)


# data --------------------------------------------------------------------

## stream polyline
sf_line <- list.files("data_fmt/vector",
                      pattern = "connectivity_dummy_5km2.shp",
                      full.names = T) %>% 
  st_read()

## connectivity estimate
df_s <- readRDS("output/mcmc_summary_up_full.rds") %>% 
  filter(str_detect(param, "s_hat")) %>% 
  select(connectivity = `50%`,
         site0,
         siteid) %>% 
  mutate(siteid = siteid + 373)

df_x <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_2_5km2.gpkg") %>% 
  as_tibble() %>%
  filter(is.na(occrrnc)) %>% 
  select(siteid, line_id) %>% 
  arrange(siteid) %>% 
  left_join(df_s,
            by = "siteid")

## combine with stream polyline
sf_line <- sf_line %>% 
  left_join(df_x,
            by = c("line_id" = "line_id")) %>%
  st_transform(sf_line, crs = 3722)


oxbow <- st_read("data_fmt/vector/epsg4326_oxbow_sites.shp") %>%
  st_transform(oxbow, crs = 3722) %>% 
  mutate(oxbow_id = row_number())


## snap function
st_snap_points = function(oxbow, sf_line) {
  
  pacman::p_load(tidyverse,
                 sf)
  
  if (inherits(oxbow, "sf")) n = nrow(oxbow)
  if (inherits(oxbow, "sfc")) n = length(oxbow)
  
  out = foreach(i = seq_len(n), .combine = bind_rows) %do% {
    
    nrst = sf::st_nearest_points(st_geometry(oxbow)[i], sf_line)
    nrst_len = sf::st_length(nrst)
    nrst_mn = which.min(nrst_len)
    
    df0 <- sf::st_cast(nrst[nrst_mn], "POINT")[2] %>%
      sf::st_coordinates() %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(X1 = st_coordinates(oxbow[i,])[1],
                    Y1 = st_coordinates(oxbow[i,])[2],
                    distance = nrst_len[nrst_mn],
                    oxbow[i,]) %>% 
      dplyr::rename(X2 = X,
                    Y2 = Y) %>% 
      dplyr::select(-geometry)
    
    return(df0)
  }
  
  return(out)
}


## example code

### original output will be `tibble`
df1 <- st_snap_points(oxbow, sf_line)

### convert it to sf
sf_point_snapped <- df1 %>% 
  st_as_sf(coords = c("X2", "Y2")) %>% 
  st_set_crs(st_crs(oxbow)) # ensure define CRS again

  
  
### to link to line feature, use st_join()

oxbow.info <- sf_point_snapped %>% 
  mutate(site0 = st_nearest_feature(., sf_line)) %>%
  left_join(as_tibble(sf_line), 
          by = c("site0" = "site0"))


# write shapefiles --------------------------------------------------------

# sf_line, stream connectivity
st_write(sf_line,
         dsn = "data_fmt/vector/epsg4326_stream_connectivity.shp",
         append = FALSE)

# oxbow info, snapped oxbows and stream line info added
st_write(oxbow.info,
         dsn = "data_fmt/vector/epsg4326_oxbow_snap.shp",
         append = FALSE)


# map ---------------------------------------------------------------------

ggplot(sf_line) +
  geom_sf(aes(color = connectivity)) +
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Immigration potential") +
  theme_minimal() +
  ggtitle("Minnesota watersheds")

ggsave(file = "output/figure_map.pdf",
       width = 7,
       height = 9)
