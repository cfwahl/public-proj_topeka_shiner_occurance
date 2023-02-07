
# this script extracts the connectivity scores from the bayes output and 
# adds the score to the stream network.

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# read real and dummy sites
df_all <- sf::st_read(dsn = "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp") %>%
  st_transform(crs = 3722)

## read stream network
sf_line <- list.files("data_fmt/vector/old",
                      pattern = "epsg3722_minnesota_stream_network_5km2.shp",
                      full.names = T) %>% 
  st_read()

## read connectivity estimate
df_s <- readRDS("output/mcmc_summary_up_full.rds") %>% 
  filter(str_detect(param, "s_hat")) %>% #remove alpha and beta parameters 
  dplyr::select(connectivity = `50%`,
         site0,
         siteid) %>% # select 3 columns, connectivity, site0, and siteid (site0 is dummy site identifier)
  mutate(siteid = siteid + sum(!is.na(df_all$occurrence))) #reorganize data by occurrence=NA, move NA to the top (! command)

# prep occurrence and connectivity data ---------------------------------------------------------------

# filter dummy sites only, join attributes with connectivity
df_x <- df_all %>% 
  as_tibble() %>% #change gpkg to tibble
  filter(is.na(occurrence)) %>% # filter by NA only
  dplyr::select(siteid, line_id) %>% # select only these columns
  arrange(siteid) %>% # arrange by siteid
  left_join(df_s,
            by = "siteid") # join data frames by common column, siteid

# join shapefiles ---------------------------------------------------------

# combine with stream and dummy site attributes
sf_line <- sf_line %>% 
  left_join(df_x,
            by = c("line_id" = "line_id")) %>% # join data frames based on commun column, line_id 
  st_transform(sf_line, crs = 3722) # transform to utm

# export connectivity stream network--------------------------------------------------------

# write stream connectivity shapefile
st_write(sf_line,
         dsn = "data_fmt/vector/epsg3722_minnesota_stream_connectivity.shp",
         append = FALSE)
