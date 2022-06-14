
# watershed delineation using whitebox package

# setup -------------------------------------------------------------------

pacman::p_load(sf,
               raster,
               stars,
               whitebox,
               mapview,
               tidyverse)

rm(list = ls())

# DEM setup ---------------------------------------------------------------
# - Read flow direction raster
wgs84_dir <- raster("data_fmt/raster/epsg4326_n40w100_dir_clipped.tif")


# - convert arc format to d8 format
source("code/function_arc2d8.R")
wgs84_dir_d8 <- arc2d8(wgs84_dir)

writeRaster(wgs84_dir_d8,
            "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass_5km2.tif",
            options = c('TFW = YES'),
            overwrite = TRUE)


# gis ---------------------------------------------------------------------

# ## Extract stream network
# - the function wbt_extract_streams was used to extract stream network
# - threshold is 1 = 1 km^2
wbt_extract_streams(flow_accum = "data_fmt/raster/epsg4326_n40w100_upa_clipped.tif",
                    output = "data_fmt/raster/epsg4326_mn_fmt_stream_3km2.tif",
                    threshold = 3) # Based on MERIT hydro, 1 indicates 1 km^2

# ## Snap pour point
# - the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
# - This function snap the spoint to stream network and it wored better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_mn_fmt_sites_relocated2.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_mn_fmt_stream_3km2.tif",
                            output = "data_fmt/vector/epsg4326_mn_fmt_sites_snap_3km2.shp",
                            snap_dist = 1)


# ## Unnested watershed delineation
# - This function generate multiple rasterfiles, thus I have made different folders to save the file
wbt_unnest_basins(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass_5km2.tif",
                  pour_pts= "data_fmt/vector/epsg4326_mn_fmt_sites_snap_3km2.shp", # GPKG file is not accepted. Used SHP file.
                  output = "data_fmt/wsraster/unnestedws.tif")

# Assigns a unique identifier to each link in a stream network
wbt_stream_link_identifier(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass_5km2.tif",
                           streams = "data_fmt/raster/epsg4326_mn_fmt_stream_3km2.tif",
                           output = "data_fmt/raster/epsg4326_mn_fmt_stream_id_3km2.tif")

# Estimates the average slope of each link (or tributary) in a stream network
wbt_stream_link_slope(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass_5km2.tif",
                      linkid = "data_fmt/raster/epsg4326_mn_fmt_stream_id_3km2.tif",
                      dem = "data_fmt/raster/epsg4326_n40w100_upa_clipped.tif", 
                      output = "data_fmt/raster/epsg4326_mn_fmt_stream_link_slope_3km2.tif")

# Converts a raster stream file into a vector file.
wbt_raster_streams_to_vector(streams = "data_fmt/raster/epsg4326_mn_fmt_stream_link_slope_3km2.tif", 
                             d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass_5km2.tif",
                             output = "data_fmt/vector/epsg4326_str_net_3km2.shp")


# ## Read result of delineated watershed raster files
# list.files() calls file names in the folder
# lapply() applies function to each element
wgs84_list_raster <- list.files("data_fmt/wsraster",
                                full.names = TRUE) %>%
  lapply(FUN = raster)

# ## Convert rater to shape
wgs84_sf_ws_polygon <- lapply(wgs84_list_raster,
                              function(x) {
                                st_as_stars(x) %>% 
                                  st_as_sf(merge = TRUE,
                                           as_points = FALSE) %>% 
                                  rename(site = starts_with("unnested"))
                              }) %>% 
  bind_rows() %>% 
  st_transform(crs = 3722) %>% 
  mutate(area = units::set_units(st_area(.), "km^2")) %>% 
  filter(area > units::set_units(3, "km^2")) %>% 
  st_transform(crs = 4326)


# associate line id -------------------------------------------------------
point <- st_read(dsn = here::here("data_fmt/vector"),
                 layer = "epsg4326_mn_fmt_sites_snap_3km2")

line <- st_read(dsn = here::here("data_fmt/vector"),
                layer = "epsg4326_str_net_3km2") %>% 
  st_set_crs(4326)

point %>% 
  mutate(line_id = st_nearest_feature(., line)) %>% 
  left_join(as_tibble(line),
            by = c("line_id" = "FID"))


# export ------------------------------------------------------------------

st_write(wgs84_sf_ws_polygon,
         dsn = "data_fmt/wgs84_mn_fmt_watersheds_3km2.gpkg",
         append = FALSE)
