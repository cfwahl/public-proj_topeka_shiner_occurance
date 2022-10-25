
## IOWA STREAM NETWORK AND OXBOW ANALYSIS


# setup -------------------------------------------------------------------

# clean up
rm(list = ls())


pacman::p_load(sf,
               raster,
               stars,
               whitebox,
               mapview,
               tidyverse, 
               sp)

# read culled data ---------------------------------------------------------------

# sites without topeka shiner data were removed, aka culled
df0 <- read_csv("data_fmt/mn_ia_site_env_data_culled.csv") %>% 
  select(SampleID:Topeka_Shiner) # select columns from Site_num to Topeka_Shiner

colnames(df0) <- str_to_lower(colnames(df0)) # make all column names lowercase

# only want Iowa sampling locations
df_ia <- df0 %>% 
  filter(state=="IA")

# only want oxbow sampling locations
df_ia <- df_ia %>% 
  filter(habitat=="oxbow")

# export ------------------------------------------------------------------

# this will recall code in R script
saveRDS(df_ia, file = "data_fmt/data_ia_env.rds")


# shape file export
df_ia %>% 
  st_as_sf(coords = c("longitudewq", "latutudewq"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_ia_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)

# DEM setup for ---------------------------------------------------------------

# flow direction
wgs84_dir_ia <- raster("data_fmt/raster/epsg4326_n40w95_dir_clip.tif")


# convert arc format to d8 format
source("code/function_arc2d8.R")
wgs84_dir_d8_ia <- arc2d8(wgs84_dir_ia)

# create raster
writeRaster(wgs84_dir_d8_ia,
            "data_fmt/raster/epsg4326_ia_flow_dir_clip_reclass_5km2.tif",
            options = c('TFW = YES'),
            overwrite = TRUE)


# gis stream network creation ---------------------------------------------------

# ## Extract stream network
# - the function wbt_extract_streams was used to extract stream network
# - threshold is 1 = 1 km^2
wbt_extract_streams(flow_accum = "data_fmt/raster/epsg4326_n40w95_upa_clip.tif",
                    output = "data_fmt/raster/epsg4326_ia_stream.tif",
                    threshold = 5) # Based on MERIT hydro, 1 indicates 1 km^2

# ## Snap pour point
# - the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
# - This function snap the point to stream network and it worded better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_ia_sites.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_ia_stream.tif",
                            output = "data_fmt/vector/epsg4326_ia_sites_snap.shp",
                            snap_dist = 1)

# Assigns a unique identifier to each link in a stream network
wbt_stream_link_identifier(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                           streams = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                           output = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif")


# Converts a raster stream file into a vector file.
wbt_raster_streams_to_vector(streams = "data_fmt/raster/epsg4326_ia_stream.tif", 
                             d8_pntr = "data_fmt/raster/epsg4326_ia_flow_dir_clip_reclass.tif",
                             output = "data_fmt/vector/epsg4326_str_net.shp")


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




# netwrok centrality ------------------------------------------------------


# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

pacman::p_load(igraph,
               tidyverse,
               sf,
               foreach,
               lme4,
               lmerTest)

# data --------------------------------------------------------------------

## stream polyline
sf_line3 <- sf::st_read(dsn = "data_fmt/vector/espg3722_ia_stream_network.gpkg") 


# network centrality test ---------------------------------------------------------

df_i <- lapply(X = 1:n_distinct(sf_line3$watershed),
               FUN = function(x) {
                 df_subset <- sf_line3 %>% 
                   filter(watershed == x)
                 
                 y <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   eigen_centrality(directed = FALSE)
                 
                 out <- df_subset %>% 
                   mutate(eigen = y$vector) %>% 
                   relocate(eigen)
                 
                 return(out)
               }) %>% 
  bind_rows()

# subset watersheds -------------------------------------------------------

ws1 <- filter(df_i, watershed == "1")
ws2 <- filter(df_i, watershed == "2")


