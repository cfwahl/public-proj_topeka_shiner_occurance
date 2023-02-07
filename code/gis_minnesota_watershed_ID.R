
# define watersheds in minnesota

# setup -------------------------------------------------------------------

# load libraries
source(here::here("code/library.R")) 

# clean objects
rm(list = ls())


# gis ---------------------------------------------------------------------

##  Snap pour point
# the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
# This function snap the point to stream network and it worked better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_minnesota_watershed_pour_points.shp", # GPKG file is not accepted. Used SHP file.
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
  bind_rows() #%>% 
  #st_transform(crs = 3722) %>% 
  #mutate(area = units::set_units(st_area(.), "km^2")) %>% 
  #filter(area > units::set_units(5, "km^2")) %>% 
  #st_transform(crs = 4326)


# export polygon ------------------------------------------------------------------

# watersheds
st_write(wgs84_sf_ws_polygon,
         dsn = "data_fmt/vector/epsg4326_minnesota_wspolygons.shp",
         append = FALSE)


# read data ---------------------------------------------------------------

sfline <- st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_network_5km2.shp") %>%
  st_transform(crs = 4326)

sfwspolygon <- st_read(dsn = "data_fmt/vector/epsg4326_minnesota_wspolygons.shp")

# intersect ---------------------------------------------------------------


out <- st_intersection(sfline, sfwspolygon) %>%
  rename(watershed = siteid) %>%
  st_transform(crs = 3722)


# export stream network  ------------------------------------------------------------------

# watersheds
st_write(out,
         dsn = "data_fmt/vector/old/epsg3722_minnesota_stream_network_5km2.shp",
         append = FALSE)


