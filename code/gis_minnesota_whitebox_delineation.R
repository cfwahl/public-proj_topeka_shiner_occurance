
# watershed delineation using whitebox package

# setup -------------------------------------------------------------------

pacman::p_load(sf,
               raster,
               stars,
               whitebox,
               mapview,
               tidyverse, 
               sp)

rm(list = ls())


# DEM setup ---------------------------------------------------------------

##  Read flow direction raster
wgs84_dir <- raster("data_fmt/raster/epsg4326_n40w100_dir_clipped.tif")

##  convert arc format to d8 format
source("code/function_arc2d8.R")
wgs84_dir_d8 <- arc2d8(wgs84_dir)

## produce D8 raster file
writeRaster(wgs84_dir_d8,
            "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
            options = c('TFW = YES'),
            overwrite = TRUE)


# gis ---------------------------------------------------------------------

## Extract stream network

# the function wbt_extract_streams was used to extract stream network
# threshold is 1 = 1 km^2
wbt_extract_streams(flow_accum = "data_fmt/raster/epsg4326_n40w100_upa_clipped.tif",
                    output = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                    threshold = 5) # Based on MERIT hydro, 1 indicates 1 km^2

##  Snap pour point
# the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
# This function snap the point to stream network and it worked better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                            output = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap2.shp",
                            snap_dist = 1)

# Assigns a unique identifier to each link in a stream network
wbt_stream_link_identifier(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                           streams = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                           output = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif")

# determine continuous slope for each stream segment
wbt_stream_slope_continuous(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif", 
                            streams = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif", 
                            dem = "data_fmt/raster/epsg4326_n40w100_elv_clipped.tif",
                            output = "data_fmt/raster/epsg4326_mn_fmt_stream_slope_cont_5km2.tif")

# coverts continuous slope into mean for each segment 
wbt_zonal_statistics(input = "data_fmt/raster/epsg4326_mn_fmt_stream_slope_cont_5km2.tif",
                     features = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif",
                     output = "data_fmt/raster/epsg4326_mn_fmt_stream_link_slope_5km2.tif",
                     stat = "mean")

# Converts link slope raster into a vector file
wbt_raster_streams_to_vector(streams = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif", 
                             d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                             output = "data_fmt/vector/epsg4326_mn_str_network_5km2.shp")

# Estimates the average slope of each link (or tributary) in a stream network
#wbt_stream_link_slope(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
#                      linkid = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif",
#                      dem = "data_fmt/raster/epsg4326_n40w100_elv_clipped.tif", 
#                      output = "data_fmt/raster/epsg4326_mn_fmt_stream_link_slope_5km2.tif")
#
#wbt_strahler_stream_order(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
#                          streams = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif",
#                          output = "data_fmt/raster/epsg4326_mn_fmt_stream_order_5km2.tif")
#
# Converts stream order raster into a vector file.
#wbt_raster_streams_to_vector(streams = "data_fmt/raster/epsg4326_mn_fmt_stream_order_5km2.tif", 
#                             d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
#                             output = "data_fmt/vector/epsg4326_mn_stream_order_5km2.shp")


# associate line id -------------------------------------------------------

# read in snapped point file
point <- st_read(dsn = "data_fmt/vector",
                 layer = "epsg4326_mn_dnr_fws_fmt_sites_snap2")

# read stream network with mean slope 
line <- st_read(dsn = "data_fmt/vector",
                layer = "epsg4326_mn_str_slope_5km2") %>% 
  st_set_crs(4326)

# join attribute tables for the point and line files
site_info <- point %>% 
  mutate(line_id = st_nearest_feature(., line)) %>% 
  left_join(as_tibble(line),
            by = c("line_id" = "FID")) %>%
  dplyr::select(occurrence:geometry.x) %>% # remove line geometry 
  rename(slope = STRM_VAL) %>%
  arrange(siteid)

# export ------------------------------------------------------------------

# site info, all individual sites 
st_write(site_info,
         dsn = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_site_link.shp",
         append = FALSE)

# save code in R script
saveRDS(site_info, file = "data_fmt/mn_dnr_fws_fmt_site_link.rds")


# aggregate sites with line_id --------------------------------------------

#line_id <- site.info %>%
#  group_by(line_id)  %>%
#  summarize(occurrence = sum(occurrence),
#            slope = mean(STRM_VAL)) %>%
#  mutate(occurrence = replace(occurrence, occurrence > 0, 1)) 
  
# creates one point that is the centroid on the points along a line segment
#line_centroid <- st_centroid(line_id) 
#snapped_points <- spNetwork::snapPointsToLines2(line_centroid, # snap centroid to stream
#                                     line,
#                                     "line_id") %>% 
#  mutate(siteid = row_number()) 

# export ------------------------------------------------------------------

# line id, sites within a stream segment grouped
#st_write(line_id,
#         dsn = "data_fmt/vector/epsg4326_mn_dnr_fws_line_id.gpkg",
#         append = FALSE)

# site info, one point representing all sites along a stream segment
#st_write(snapped_points,
#         dsn = "data_fmt/vector/epsg4326_mn_dnr_fws_line_centroid.shp",
#         append = FALSE)

# save code in R script
#save(snapped_points, file = "data_fmt/mn_dnr_fws_line_centroid.Rdata")



# Unnested watershed delineation ------------------------------------------

# - This function generate multiple rasterfiles, thus I have made different folders to save the file
wbt_unnest_basins(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                  pour_pts= "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_site_link.shp", # GPKG file is not accepted. Used SHP file.
                  output = "data_fmt/wsrasterunnestedws.tif")

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
                                  rename(siteid = starts_with("unnested"))
                              }) %>% 
  bind_rows() %>% 
  st_transform(crs = 3722) %>% 
  mutate(area = units::set_units(st_area(.), "km^2")) %>% 
  filter(area > units::set_units(5, "km^2")) %>% 
  st_transform(crs = 4326)


# export ------------------------------------------------------------------

# watersheds
st_write(wgs84_sf_ws_polygon,
         dsn = "data_fmt/vector/epsg4326_mn_fmt_watersheds_5km2.shp",
         append = FALSE)


# join --------------------------------------------------------------------

# add line_id from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_mn_fmt_watersheds_5km2.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_site_link.shp",
                fkey = "siteid",
                import_field = "line_id")

# add slope from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_mn_fmt_watersheds_5km2.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_site_link.shp",
                fkey = "siteid",
                import_field = "slope")

# add occurrence from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_mn_fmt_watersheds_5km2.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_site_link.shp",
                fkey = "siteid",
                import_field = "occurrence")


####  DUMMY + REAL OCCURRENCE DATA  ####


# combined data sets ------------------------------------

# read in stream network
stream <- st_read(dsn = "data_fmt/vector",
                 layer = "epsg3722_mn_str_connectivity_5km2")

# create point for each stream segment
dummy <- sf::st_point_on_surface(stream) %>%
  rename (line_id = FID,
          slope = STRM_VAL) %>%
  mutate(occurrence = NA) # create occurrence column with NAs 

# dummy sites, export to remove points outside of study location
#st_write(dummy,
#         dsn = "data_fmt/vector/epsg4326_mn_dnr_fws_dummy.shp",
#         append = FALSE)

# read in shapefile from with removed sites
#dummy <- st_read(dsn = "data_fmt/vector",
#                         layer = "epsg4326_mn_dnr_fws_dummy")

# read line_id sites
site <- st_read(dsn = "data_fmt/vector/",
                 layer = "epsg4326_mn_dnr_fws_fmt_site_link") %>%
  st_transform(crs = 3722)

site$siteid = NULL # remove site_id so columns will match
site$year = NULL # remove year so columns will match
join <- rbind(site, dummy) %>%
  mutate(siteid = row_number()) # create new site_id


# export ------------------------------------------------------------------


# site info, one point representing all sites along a stream segment
st_write(join,
         dsn = "data_fmt/vector/epsg4326_mn_dnr_fws_dummy_real_occurrence.shp",
         append = FALSE)

# save code in R script
saveRDS(join, file = "data_fmt/epsg4326_mn_dnr_fws_dummy_real_occurrence.rds")


# Unnested dummy watershed delineation --------------------------------------------------------


#  This function generate multiple rasterfiles, thus I have made different folders to save the file
wbt_unnest_basins(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                  pour_pts= "data_fmt/vector/epsg4326_mn_dnr_fws_dummy_real_occurrence.shp", # GPKG file is not accepted. Used SHP file.
                  output = "data_fmt/wsraster/dummy/unnestedws.tif")

# ## Read result of delineated watershed raster files
# list.files() calls file names in the folder
# lapply() applies function to each element
wgs84_list_raster <- list.files("data_fmt/wsraster/dummy",
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
  st_transform(crs = 3722) %>% 
  mutate(area = units::set_units(st_area(.), "km^2")) %>% 
  filter(area > units::set_units(5, "km^2")) %>% 
  st_transform(crs = 4326)


# export ------------------------------------------------------------------

# watersheds
st_write(wgs84_sf_ws_polygon,
         dsn = "data_fmt/vector/epsg4326_mn_fmt_watersheds_dummy_5km2.shp",
         append = FALSE)


# join --------------------------------------------------------------------

# add line_id from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_mn_fmt_watersheds_dummy_5km2.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_mn_dnr_fws_dummy_real_occurrence.shp",
                fkey = "siteid",
                import_field = "line_id")

# add slope from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_mn_fmt_watersheds_dummy_5km2.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_mn_dnr_fws_dummy_real_occurrence.shp",
                fkey = "siteid",
                import_field = "slope")

# add occurrence from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_mn_fmt_watersheds_dummy_5km2.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_mn_dnr_fws_dummy_real_occurrence.shp",
                fkey = "siteid",
                import_field = "occurrence")
