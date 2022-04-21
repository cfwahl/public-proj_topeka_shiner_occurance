
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
# flow direction
wgs84_dir <- raster("data_fmt/raster/epsg4326_n40w100_dir_clipped.tif")

# convert arc format to d8 format
source("code/function_arc2d8.R")
wgs84_dir_d8 <- arc2d8(wgs84_dir)

writeRaster(wgs84_dir_d8,
            "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
            options = c('TFW = YES'),
            overwrite = TRUE)


# gis ---------------------------------------------------------------------

# ## Extract stream network
# - the function wbt_extract_streams was used to extract stream network
# - threshold is 1 = 1 km^2
wbt_extract_streams(flow_accum = "data_fmt/raster/epsg4326_n40w100_upa_clipped.tif",
                    output = "data_fmt/raster/epsg4326_mn_fmt_stream.tif",
                    threshold = 1) # Based on MERIT hydro, 1 indicates 1 km^2

# ## Snap pour point
# - the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
# - This function snap the spoint to stream network and it wored better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_mn_fmt_sites.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_mn_fmt_stream.tif",
                            output = "data_fmt/vector/epsg4326_mn_fmt_sites_snap.shp",
                            snap_dist = 1)


# ## Unnested watershed delineation
# - This function generate multiple rasterfiles, thus I have made different folders to save the file
wbt_unnest_basins(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                  pour_pts= "data_fmt/vector/epsg4326_mn_fmt_sites_snap.shp", # GPKG file is not accepted. Used SHP file.
                  output = "data_fmt/wsraster/unnestedws.tif")


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
  group_by(site) %>% 
  slice(which.max(area)) %>% 
  ungroup() %>% 
  st_transform(crs = 4326)


# export ------------------------------------------------------------------

st_write(wgs84_sf_ws_polygon,
         dsn = "data_fmt/vector/wgs84_mn_fmt_watersheds.gpkg",
         append = FALSE)
