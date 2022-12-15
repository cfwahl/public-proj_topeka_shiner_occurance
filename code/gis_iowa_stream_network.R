
## IOWA STREAM NETWORK AND OXBOW ANALYSIS


# setup -------------------------------------------------------------------

# clean up
rm(list = ls())

# load libaries
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

# make all column names lowercase
colnames(df0) <- str_to_lower(colnames(df0)) 

# only want Iowa sampling locations
df_ia <- df0 %>% 
  filter(state=="IA")

# only want oxbow sampling locations
df_ia <- df_ia %>% 
  filter(habitat=="oxbow") %>%
  mutate(occurrence = replace(topeka_shiner, topeka_shiner > 0, 1))  # if occurrence is >0 then make 1
         

#df_ia <- df_ia %>% 
#  group_by(latutudewq, longitudewq) %>% # check for double lat longs between data sets 
#  summarize(occurrence = sum(topeka_shiner),
#            year = max(year), # latest year of observation
#            lat = unique(latutudewq),
#            long = unique(longitudewq)) %>% 
#  ungroup() %>%
#  mutate(occurrence = replace(occurrence, occurrence > 0, 1),  # if occurrence is >0 then make 1
#         oxbowid = seq_len(n_distinct(paste0(.$lat, .$long))))


# export ------------------------------------------------------------------

# export geopackage, shapefile changes the column names
df_ia %>% 
  st_as_sf(coords = c("longitudewq", "latutudewq"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_iowa_oxbow_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)

# DEM setup ---------------------------------------------------------------

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
                    output = "data_fmt/raster/epsg4326_ia_stream_5km2.tif",
                    threshold = 5) # Based on MERIT hydro, 1 indicates 1 km^2

# ## Snap pour point
# - the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
# - This function snap the point to stream network and it worded better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_iowa_oxbow_sites.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_ia_stream_5km2.tif",
                            output = "data_fmt/vector/epsg4326_ia_oxbow_snap.shp",
                            snap_dist = 1)

# Assigns a unique identifier to each link in a stream network
wbt_stream_link_identifier(d8_pntr = "data_fmt/raster/epsg4326_ia_flow_dir_clip_reclass_5km2.tif",
                           streams = "data_fmt/raster/epsg4326_ia_stream_5km2.tif",
                           output = "data_fmt/raster/epsg4326_iowa_stream_id_5km2.tif")


# Converts a raster stream file into a vector file.
wbt_raster_streams_to_vector(streams = "data_fmt/raster/epsg4326_iowa_fmt_stream_id_5km2.tif", 
                             d8_pntr = "data_fmt/raster/epsg4326_ia_flow_dir_clip_reclass_5km2.tif",
                             output = "data_fmt/vector/epsg4326_iowa_str_net_5km2.shp")


# associate line id -------------------------------------------------------

# read in snapped point file
point <- st_read(dsn = "data_fmt/vector",
                 layer = "epsg4326_ia_oxbow_snap")

# read stream network with mean slope 
line <- st_read(dsn = "data_fmt/vector",
                layer = "epsg4326_iowa_str_net_5km2") %>% 
  st_set_crs(4326)

# join attribute tables for the point and line files
site_info <- point %>% 
  mutate(line_id = st_nearest_feature(., line)) %>% 
  mutate(oxbowid = row_number()) %>%
  left_join(as_tibble(line),
            by = c("line_id" = "line_id")) %>%
  dplyr::select(sampled:geometry.x) %>% # remove line geometry.y 
  rename(occurrence = tpk_shn) %>%
  mutate(occurrence = replace(occurrence, occurrence > 0, 1)) %>%
  arrange(oxbowid)


# export ------------------------------------------------------------------

# export geopackage, shapefile changes the column names
site_info %>% 
  st_as_sf(coords = c("longitudewq", "latutudewq"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_iowa_oxbow_lineid.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)


# Unnested watershed delineation ------------------------------------------

### create points for each line segment for watersheds

#add in stream network
stream <- st_read(dsn = "data_fmt/vector",
                  layer = "epsg4326_iowa_str_net_5km2_dummy") 

# create point for each stream segment
ia_dummmy <- sf::st_point_on_surface(stream) %>%
  mutate(siteid = row_number())

# export ------------------------------------------------------------------


# site info, one point representing all sites along a stream segment
st_write(ia_dummmy,
         dsn = "data_fmt/vector/epsg4326_ia_dummy_sites.shp",
         append = FALSE)

# - This function generate multiple rasterfiles, thus I have made different folders to save the file
wbt_unnest_basins(d8_pntr = "data_fmt/raster/epsg4326_ia_flow_dir_clip_reclass_5km2.tif",
                  pour_pts= "data_fmt/vector/epsg4326_ia_dummy_sites.shp", # GPKG file is not accepted. Used SHP file.
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
  st_transform(crs = 3722) %>% 
  mutate(area = units::set_units(st_area(.), "km^2")) %>% 
  filter(area > units::set_units(5, "km^2")) %>% 
  st_transform(crs = 4326)


# export ------------------------------------------------------------------

# watersheds
st_write(wgs84_sf_ws_polygon,
         dsn = "data_fmt/vector/epsg4326_ia_watersheds_dummy_5km2.shp",
         append = FALSE)

# join --------------------------------------------------------------------

# add line_id from site point file to watershed polygon attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_ia_watersheds_dummy_5km2.shp",
                pkey = "siteid",
                input2 = "data_fmt/vector/epsg4326_ia_dummy_sites.shp",
                fkey = "siteid",
                import_field = "line_id")

# add area from watershed polygon file to stream line attribute table
wbt_join_tables(input1 = "data_fmt/vector/epsg4326_iowa_str_net_5km2_dummy.shp",
                pkey = "line_id",
                input2 = "data_fmt/vector/epsg4326_ia_watersheds_dummy_5km2.shp",
                fkey = "line_id",
                import_field = "area")


# network centrality ------------------------------------------------------

pacman::p_load(igraph,
               tidyverse,
               sf,
               foreach,
               lme4,
               lmerTest)

# data --------------------------------------------------------------------

## stream polyline
sf_line <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_str_net_5km2_dummy.shp") 

site_info <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_oxbow_lineid.shp") 

# network centrality test ---------------------------------------------------------

df_i <- lapply(X = 1:n_distinct(sf_line$watershed),
               FUN = function(x) {
                 df_subset <- sf_line %>% 
                   filter(watershed == x)
                 
                 y <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   eigen_centrality()
                 
                 #weight <- df_subset$area %>% 
                   #scale(center = min(.), scale = max(. - min(.))) %>% # scales from 0-1
                   #c() # combine into matrix
                   #group_by(watershed) %>%
                   #summarise(area = max(area))
                 
                 #weight2 <- weight$area/df_subset$area 
                   
                 
                 #v_w_cent <- y * weight 
                 
                 out <- df_subset %>% 
                   #mutate(eigen = y) %>% 
                   mutate(eigen = y$vector) %>%
                   relocate(eigen)
                 
                 return(out)
               }) %>% 
  bind_rows()

### betweenness

df_b <- lapply(X = 1:n_distinct(sf_line$watershed),
               FUN = function(x) {
                 df_subset <- sf_line %>% 
                   filter(watershed == x)
                 
                 y <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   betweenness(normalized = TRUE,
                               nobigint = TRUE)
                 
                 #weight <- df_subset$area %>% 
                 #scale(center = min(.), scale = max(. - min(.))) %>% # scales from 0-1
                 #c() # combine into matrix
                 #group_by(watershed) %>%
                 #summarise(area = max(area))
                 
                 #weight2 <- weight$area/df_subset$area 
                 
                 
                 #v_w_cent <- y * weight 
                 
                 out <- df_subset %>% 
                   mutate(between = y) %>% 
                   relocate(between)
                 
                 return(out)
               }) %>% 
  bind_rows()

#  join occurrence with eigen ---------------------------------------------

df_e <- site_info %>%
  as_tibble %>%
 left_join(as_tibble(df_i),
            by = c("line_id"))
  

# visualize relationship ----------------------------------------------------------


## eigenvector X connectivity
## linear model 
ggplot(ws4,
       aes(x = eigen,
           y = occurrence)) +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


# maps --------------------------------------------------------------------

# map of network centrality scores from the  subwatersheds
ggplot(df_b) + # base map of stream lines
  geom_sf(aes(color = between))+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Eigenvector") + # label legend 
  theme_minimal()
