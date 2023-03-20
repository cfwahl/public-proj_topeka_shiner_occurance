
# this script determines stream length for connectivity and betwenness in 
# Minnesota and Iowa

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# MINNESOTA ---------------------------------------------------------------
# data -----------------------------------------------------------------

# import RDS files for scores
df_mn_strm_cent <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

## stream polyline
sf_line2 <- readRDS(file = "data_fmt/data_minnesota_stream_connectivity.rds") %>%
  dplyr::select(-c(STRM_VAL))  # remove slope variable

# segment length ----------------------------------------------------------

# determine length of stream segments
l <- st_length(sf_line2)

# add column
sf_line2 <- sf_line2 %>% 
  add_column(l = l)

# new cloumn with "[m]" removed  
sf_line2$seg_length <- stringr::str_remove(sf_line2$l, '\\[m]')

# remove original l column 
sf_line2 <- sf_line2 %>%
  dplyr::select(-c(l))

# join segment length with the rest of stream point df
df_mn_strm_cent <- merge(x = df_mn_strm_cent, y = sf_line2[ , c("line_id", "seg_length")], 
                         by = "line_id", all.x=TRUE) %>%
  dplyr::select(-c(geometry.y)) %>%
  mutate(seg_length = as.numeric(seg_length)) %>%
  rename(geometry = geometry.x)

# connectivity classes ----------------------------------------------------

# make segment length numeric
sf_line2 <- sf_line2 %>%
  mutate(seg_length = as.numeric(seg_length))
sum(sf_line2$seg_length) # total length

# subset connectivity <=2
low <- sf_line2 %>%
  filter(connectivity <= 2)
sum(low$seg_length) # length 

# subset connectivity between 2-3.9
med <- sf_line2 %>%
  filter(connectivity >= 2 & connectivity <= 3.9)
sum(med$seg_length) # length

# subset connectivity >=4
high <- sf_line2 %>%
  filter(connectivity >= 4)
sum(high$seg_length)

# betweenness classes ----------------------------------------------------

# make segment length numeric
sf_line2 <- sf_line2 %>%
  mutate(seg_length = as.numeric(seg_length))
sum(sf_line2$seg_length) # total length

# subset connectivity <=2
low <- sf_line2 %>%
  filter(between <= 2.2)
sum(low$seg_length) # length 

# subset connectivity between 2-3.9
med <- sf_line2 %>%
  filter(between >= 2.3 & between <= 4.4)
sum(med$seg_length) # length

# subset connectivity >=4
high <- sf_line2 %>%
  filter(between >= 4.5)
sum(high$seg_length)

# export data ------------------------------------------------------------------

# export stream network centrality scores
#saveRDS(df_mn_strm_cent, file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# IOWA --------------------------------------------------------------------
# data -----------------------------------------------------------------

# import RDS files for scores
df_ia_strm_cent <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds")

## stream polyline
# stream polyline
sf_line2 <- readRDS(file = "data_fmt/data_iowa_stream_betweenness.rds") %>%
  st_as_sf()

# segment length ----------------------------------------------------------

# determine length of stream segments
l <- st_length(sf_line2)

# add column
sf_line2 <- sf_line2 %>% 
  add_column(l = l)

# new cloumn with "[m]" removed  
sf_line2$seg_length <- stringr::str_remove(sf_line2$l, '\\[m]')

# remove original l column 
sf_line2 <- sf_line2 %>%
  dplyr::select(-c(l))

# betweenness classes ----------------------------------------------------

# make segment length numeric
sf_line2 <- sf_line2 %>%
  mutate(seg_length = as.numeric(seg_length))
sum(sf_line2$seg_length) # total length

# subset connectivity <=2
low <- sf_line2 %>%
  filter(between <= 0.16)
sum(low$seg_length) # length 

# subset connectivity between 2-3.9
med <- sf_line2 %>%
  filter(between >= 0.17 & between <= 0.32)
sum(med$seg_length) # length

# subset connectivity >=4
high <- sf_line2 %>%
  filter(between >= 0.33)
sum(high$seg_length)
