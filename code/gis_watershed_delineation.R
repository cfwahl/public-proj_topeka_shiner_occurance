
# This script creates watersheds for the dummy and real sites. 

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# unnested watershed delineation --------------------------------------------------------

# generate multiple rasterfiles, one for each site
wbt_unnest_basins(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                  pour_pts= "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp",
                  output = "data_fmt/wsraster/dummy/unnestedws.tif")

# read result of delineated watershed raster files
# list.files() calls file names in the folder
# lapply() applies function to each element
wgs84_list_raster <- list.files("data_fmt/wsraster/dummy",
                                full.names = TRUE) %>%
  lapply(FUN = raster)

# convert raster to shapefile
wgs84_sf_ws_polygon <- lapply(wgs84_list_raster,
                              function(x) {
                                st_as_stars(x) %>% 
                                  st_as_sf(merge = TRUE,
                                           as_points = FALSE) %>% 
                                  rename(siteid = starts_with("unnested"))
                              }) %>% 
  bind_rows() %>% 
  st_transform(crs = 3722) %>% 
  mutate(area = units::set_units(st_area(.), "km^2")) %>% 
  filter(area > units::set_units(5, "km^2")) %>% 
  st_transform(crs = 4326)

# import stream and occurrence data ------------------------------------------------------------------

# read stream occurrence data
stream_occurrence <- readRDS(file = "data_fmt/data_minnesota_stream_dummy_real_occurrence.rds")

# read in steam network
stream_network <- readRDS(file = "data_fmt/data_minnesota_stream_network_5km2.rds")

# join attributes --------------------------------------------------------------------

# join occurrence and stream attributes to the watershed polygons
join <- wgs84_sf_ws_polygon %>%
  left_join(as_tibble(stream_occurrence),
            by = c("siteid" = "siteid")) %>%
  dplyr::select(-c(geometry.y)) %>%
  left_join(as_tibble(stream_network),
            by = c("line_id" = "line_id")) %>%
  dplyr::select(-c(STRM_VAL, geometry)) %>%
  mutate(area = as.numeric(area)) %>%
  rename(geometry = geometry.x)

# export data -------------------------------------------------------------

saveRDS(join, file = "data_fmt/data_minnesota_stream_watersheds_dummy_real.rds")
