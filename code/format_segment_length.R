
# this script adds stream segment length to data frame for GLMM

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

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

# export data ------------------------------------------------------------------

# export stream network centrality scores
saveRDS(df_mn_strm_cent, file = "data_fmt/data_minnesota_stream_network_centrality.rds")
