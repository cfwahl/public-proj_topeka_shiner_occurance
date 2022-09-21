# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               tmap,
               sf,
               mapview, 
               foreach)


# data --------------------------------------------------------------------

## whole data frame of real occurrence + dummy data
df_all <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_2_5km2.gpkg")

## stream sites (for the connectivity map)
df_stream_occ <- sf::st_read("data_fmt/vector/epsg4326_mn_dnr_fws_dummy_real_occurrence.shp") %>%
  filter(!is.na(occurrence)) %>%
  st_transform(crs = 3722)
  

## stream polyline
sf_line <- list.files("data_fmt/vector",
                      pattern = "connectivity_dummy_5km2.shp",
                      full.names = T) %>% 
  st_read()

## connectivity estimate
df_s <- readRDS("output/mcmc_summary_up_full.rds") %>% 
  filter(str_detect(param, "s_hat")) %>% #remove alpha and beta parameters 
  select(connectivity = `50%`,
         site0,
         siteid) %>% # select 3 columns, connectivity, site0, and siteid (site0 is dummy site identifier)
  mutate(siteid = siteid + sum(!is.na(df_all$occrrnc))) #reorganize data by occurrence=NA, move NA to the top (! command)

df_x <- df_all %>% 
  as_tibble() %>% #change gpkg to tibble
  filter(is.na(occrrnc)) %>% # filter by NA only
  select(siteid, line_id) %>% # select only these columns
  arrange(siteid) %>% # arrange by siteid
  left_join(df_s,
            by = "siteid") # join data frames by common column, siteid

## combine with stream polyline
sf_line <- sf_line %>% 
  left_join(df_x,
            by = c("line_id" = "line_id")) %>% # join data frames based on commun column, line_id 
  st_transform(sf_line, crs = 3722) # transform to utm

# read oxbow data
sf_oxbow <- st_read("data_fmt/vector/epsg4326_oxbow_sites.shp") %>%
  mutate(oxbow_id = row_number())%>% # add unique identifier for oxbows
  st_transform(oxbow, crs = 3722) # transform to utm

## snap function
st_snap_points = function(f1, f2) {
  
  pacman::p_load(tidyverse,
                 sf)
  
  if (inherits(f1, "sf")) n = nrow(f1)
  if (inherits(f1, "sfc")) n = length(f1)
  
  out = foreach(i = seq_len(n), .combine = bind_rows) %do% {
    
    nrst = sf::st_nearest_points(st_geometry(f1)[i], f2)
    nrst_len = sf::st_length(nrst)
    nrst_mn = which.min(nrst_len)
    
    df0 <- sf::st_cast(nrst[nrst_mn], "POINT")[2] %>%
      sf::st_coordinates() %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(X1 = st_coordinates(f1[i,])[1],
                    Y1 = st_coordinates(f1[i,])[2],
                    distance = nrst_len[nrst_mn],
                    f1[i,]) %>% 
      dplyr::rename(X2 = X,
                    Y2 = Y) %>% 
      dplyr::select(-geometry)
    
    return(df0)
  }
  
  return(out)
}


## example code

### snap oxbows to stream line, original output will be `tibble`
df1 <- st_snap_points(sf_oxbow, sf_line) # X1/Y1 are original locations, X2/Y2 are snapped points

### convert it to sf
sf_point_snapped <- df1 %>% 
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(sf_oxbow)) # ensure define CRS again

### to link to line feature, use st_join()
df_oxbow_snap <- st_join(x = sf_point_snapped,
                y = sf_line,
                join = st_is_within_distance,
                dist = 10) %>% # join two features, then make as tibble data frame
  as_tibble()

##### stream sites #####

## snap stream sites to stream line
df_stream_occ_snap <- st_snap_points(df_stream_occ, sf_line)

### convert it to sf
sf_stream_snapped <- df_stream_occ_snap %>% 
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(df_stream_occ)) # ensure define CRS again

# plot of occurrence and connectivity
ggplot(df_m,
       aes(x = connectivity,
           y = occurrence)) +
  geom_point() 

# write shapefiles --------------------------------------------------------

# sf_line, stream connectivity
st_write(sf_line,
         dsn = "data_fmt/vector/epsg3722_stream_connectivity.shp",
         append = FALSE)

# oxbow info, snapped oxbows and stream line info added
st_write(df_oxbow_snap,
         dsn = "data_fmt/vector/epsg3722_oxbow_snap.shp",
         append = FALSE)


# save oxbow_info as rds --------------------------------------------------

# this will recall code in R script
saveRDS(oxbow_info, file = "data_fmt/oxbow_connectivity.RDS")


# map ---------------------------------------------------------------------

ggplot(sf_line) + # base map of stream lines
  geom_sf(aes(color = connectivity)) + # heat map of connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Immigration potential") +
  theme_minimal() +
  ggtitle("Minnesota watersheds") +
  geom_point(data = sf_point_snapped, aes(x = X1, y = Y1), # oxbow sites
             shape = 16) + 
  geom_point(data = sf_stream_snapped, aes(x = X1, y = Y1), # stream sites
             aes(shape = occurrence)) + # shape based on occurrence
  scale_shape_manual(values=c(0, 21)) 
  

ggsave(file = "output/figure_map.pdf",
       width = 7,
       height = 9)


# plot --------------------------------------------------------------------

## load data
oxbow_info <- readRDS("data_fmt/oxbow_connectivity.RDS")

attach(oxbow_info)


plot(occurrence, connectivity)
abline(lm(occurrence~connectivity))
lines(lowess(connectivity,occurrence), col="blue")   

