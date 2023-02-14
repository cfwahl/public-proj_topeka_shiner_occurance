
# This script creates watersheds for the dummy and real sites. 

# setup -------------------------------------------------------------------

# load libraries
source(here::here("code/library.R")) 

# clean objects
rm(list = ls())

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

# export watersheds ------------------------------------------------------------------

# create shapefile, need this saved for code below
st_write(wgs84_sf_ws_polygon,
         dsn = "data_fmt/vector/epsg4326_minnesota_stream_watersheds_dummy_real.shp",
         append = FALSE)

# join site and watersehd attributes --------------------------------------------------------------------

# add line_id from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_minnesota_stream_watersheds_dummy_real.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp",
                fkey = "siteid",
                import_field = "line_id")

# add slope from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_minnesota_stream_watersheds_dummy_real.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp",
                fkey = "siteid",
                import_field = "slope")

# add occurrence from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_minnesota_stream_watersheds_dummy_real.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp",
                fkey = "siteid",
                import_field = "occurrence")

# add watershed ID from stream line file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_minnesota_stream_watersheds_dummy_real.shp",
                pkey = "line_id",
                input2 = "data_fmt/vector/epsg3722_minnesota_stream_network_5km2.shp",
                fkey = "line_id",
                import_field = "watershed")
