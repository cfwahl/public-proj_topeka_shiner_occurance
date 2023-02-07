
# This script creates a point for each stream segment in the network. There
# are no occurrence data for the points, we refer to these as "dummy" sites.
# Sampling points with occurrence data, aka "real" sites, are joined with the dummy sites. Prior
# to creating the dummy sites, "epsg4326_mn_str_slope_5km2.shp" was trimmed to
# stream segments within the study area. The new netowrk is named
# "epsg3722_minnesota_stream_network_5km2.shp". Watershed identidy was manually
# added to connected streams. 

# setup -------------------------------------------------------------------

# load libraries
source(here::here("code/library.R")) 

# clean objects
rm(list = ls())

# join stream sampling sites and stream network attributes -------------------------------------------------------

# read in snapped point file
point <- st_read(dsn = "data_fmt/vector/old",
                 layer = "epsg4326_mn_dnr_fws_fmt_sites_snap2")

# read stream network with mean slope and segment identifier 
line <- st_read(dsn = "data_fmt/vector/old",
                layer = "epsg4326_mn_str_slope_5km2") %>%
  st_set_crs(4326)
  
# join attribute tables for the point and line files
site_info <- point %>% 
  mutate(line_id = st_nearest_feature(., line)) %>% 
  left_join(as_tibble(line),
            by = c("line_id" = "FID")) %>%
  dplyr::select(occurrence:geometry.x) %>% # remove line geometry 
  rename(slope = STRM_VAL,
         geometry = geometry.x) %>%
  arrange(siteid) %>%
  st_transform(3722) %>%
  dplyr::select(-c("year"))

# create dummy sites ------------------------------------

# read in stream network
stream <- st_read(dsn = "data_fmt/vector/old",
                  layer = "epsg3722_minnesota_stream_network_5km2")

# create point for each stream segment
dummy <- sf::st_point_on_surface(stream) %>%
  rename (slope = STRM_VAL) %>%
  mutate(occurrence = NA,
         siteid = row_number()+373) # create occurrence column with NAs 

dummy$watershed = NULL # remove watershed so columns will match for rbind

site_info$siteid = NULL # remove site_id so columns will match
site_info$year = NULL # remove year so columns will match
join <- rbind(site_info, dummy) %>%
  mutate(siteid = row_number()) # create new site_id

# export dummy and real sites ------------------------------------------------------------------

# create shapefile
st_write(join,
         dsn = "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp",
         append = FALSE)

# create RDS file
#saveRDS(join, file = "data_fmt/data_minnesota_stream_dummy_real_occurrence.rds")
