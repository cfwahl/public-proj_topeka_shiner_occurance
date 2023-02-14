
# define watersheds in minnesota and iowa based on the pour point for each of the 
# sub-watersheds

# setup -------------------------------------------------------------------

# load libraries
source(here::here("code/library.R")) 

# clean objects
rm(list = ls())

# this line will allow st_make_valid to fix geometries for polygons
sf::sf_use_s2(FALSE)

# MINNESOTA ---------------------------------------------------------------
# gis ---------------------------------------------------------------------

##  Snap pour point
# the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
# This function snap the point to stream network and it worked better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/old/epsg4326_minnesota_watershed_pour_points.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                            output = "data_fmt/vector/epsg4326_minnesota_watershed_points_snap.shp",
                            snap_dist = 1)

# Unnested dummy watershed delineation --------------------------------------------------------

#  This function generate multiple rasterfiles, thus I have made different folders to save the file
wbt_unnest_basins(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                  pour_pts= "data_fmt/vector/epsg4326_minnesota_watershed_points_snap.shp", # GPKG file is not accepted. Used SHP file.
                  output = "data_fmt/wsraster/wspoints/unnestedws.tif")

# ## Read result of delineated watershed raster files
# list.files() calls file names in the folder
# lapply() applies function to each element
wgs84_list_raster <- list.files("data_fmt/wsraster/wspoints",
                                full.names = TRUE) %>%
  lapply(FUN = raster)

# ## Convert rater to shape
wgs84_sf_ws_polygon <- lapply(wgs84_list_raster,
                              function(x) {
                                st_as_stars(x) %>% 
                                  st_as_sf(merge = TRUE,
                                           as_points = FALSE) %>% 
                                  rename(siteid = starts_with("unnested"))
                              }) %>% 
  bind_rows() %>%
  st_set_crs(4326) %>%
  st_make_valid()

# verify geometries are fixed
st_is_valid(wgs84_sf_ws_polygon)

# read data ---------------------------------------------------------------

# read stream network shapefile
sfline <- st_read(dsn = "data_fmt/vector/epsg4326_mn_str_slope_5km2.shp") %>%
  st_set_crs(4326) %>%
  filter(!(FID %in% c("1882", "1640", "1509", "1022",
                       "800", "797", "769"))) # %in% is select these values within site column, these sites fall outside connectivity stream network

# join --------------------------------------------------------------------

join <- st_join(sfline, wgs84_sf_ws_polygon) %>% 
  drop_na(siteid) %>% 
  rename(line_id = FID,
         watershed = siteid) #%>%
  #st_transform(crs = 3722)
  
# export stream network  ------------------------------------------------------------------

# watersheds
st_write(join,
         dsn = "data_fmt/vector/epsg4326_minnesota_stream_network_5km2.shp",
         append = FALSE)

# IOWA --------------------------------------------------------------------
# gis ---------------------------------------------------------------------

##  Snap pour point
# the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
# This function snap the point to stream network and it worked better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_iowa_watershed_pour_points.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_iowa_stream_5km2.tif",
                            output = "data_fmt/vector/epsg4326_iowa_watershed_points_snap.shp",
                            snap_dist = 1)

# Unnested dummy watershed delineation --------------------------------------------------------

#  This function generate multiple rasterfiles, thus I have made different folders to save the file
wbt_unnest_basins(d8_pntr = "data_fmt/raster/epsg4326_iowa_flow_dir_clip_reclass_5km2.tif",
                  pour_pts= "data_fmt/vector/epsg4326_iowa_watershed_points_snap.shp", # GPKG file is not accepted. Used SHP file.
                  output = "data_fmt/wsraster/iowa/wsrasterunnestedws.tif")

# ## Read result of delineated watershed raster files
# list.files() calls file names in the folder
# lapply() applies function to each element
wgs84_list_raster <- list.files("data_fmt/wsraster/iowa",
                                full.names = TRUE) %>%
  lapply(FUN = raster)

# ## Convert rater to shape
wgs84_sf_ws_polygon <- lapply(wgs84_list_raster,
                              function(x) {
                                st_as_stars(x) %>% 
                                  st_as_sf(merge = TRUE,
                                           as_points = FALSE) %>% 
                                  rename(siteid = starts_with("wsrasterunnested"))
                              }) %>% 
  bind_rows() %>%
  st_set_crs(4326) %>%
  st_make_valid()

# verify geometries are fixed
st_is_valid(wgs84_sf_ws_polygon)

# read data ---------------------------------------------------------------

# read stream network shapefile
sfline <- st_read(dsn = "data_fmt/vector/epsg4326_ia_str_network_5km2.shp") %>%
  st_set_crs(4326) 

# join --------------------------------------------------------------------

join <- st_join(sfline, wgs84_sf_ws_polygon) %>% 
  drop_na(siteid) %>% 
  rename(line_id = STRM_VAL,
         watershed = siteid) %>%
  dplyr::select(-c(FID))

# export stream network  ------------------------------------------------------------------

# watersheds
st_write(join,
         dsn = "data_fmt/vector/epsg4326_iowa_stream_network_5km2.shp",
         append = FALSE)
