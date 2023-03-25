
# this script determines stream length for connectivity and betweenness measures 
# in Minnesota and Iowa. Includes total distance, distance within equally divided
# subgroups, and proportion within each subgroup.

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# MINNESOTA ---------------------------------------------------------------
# data -----------------------------------------------------------------

# import stream network
df_b <- readRDS(file = "data_fmt/data_minnesota_stream_betweenness.rds") %>%
  st_as_sf()

# Connectivity ------------------------------------------------------------
# segment length ----------------------------------------------------------

# determine length of stream segments
l <- st_length(df_b)

# add column
df_b <- df_b %>% 
  add_column(l = l)

# new cloumn with "[m]" removed  
df_b$seg_length <- stringr::str_remove(df_b$l, '\\[m]')

# remove original l column 
df_b <- df_b %>%
  dplyr::select(-c(l))

# connectivity classes ----------------------------------------------------

# make segment length numeric
df_b <- df_b %>%
  mutate(seg_length = as.numeric(seg_length))
sum(df_b$seg_length) # total length

# subset connectivity <=2.34
low <- df_b %>%
  filter(connectivity <= 2.34)
sum(low$seg_length) # length 
sum(low$seg_length)/sum(df_b$seg_length) # proportion

# subset connectivity between 2.35-4.68
med <- df_b %>%
  filter(connectivity >= 2.34 & connectivity <= 4.68)
sum(med$seg_length) # length
sum(med$seg_length)/sum(df_b$seg_length) # proportion

# subset connectivity >=4.69
high <- df_b %>%
  filter(connectivity >= 4.68)
sum(high$seg_length) # length
sum(high$seg_length)/sum(df_b$seg_length) # proportion

(low$line_id)
# betweenness classes ----------------------------------------------------

# make segment length numeric
df_b <- df_b %>%
  mutate(seg_length = as.numeric(seg_length))
sum(df_b$seg_length) # total length

# subset betweenness <=0.22
low <- df_b %>%
  filter(between <= 0.22)
sum(low$seg_length) # length 
sum(low$seg_length)/sum(df_b$seg_length) # proportion

# subset betweenness between 0.23-0.44
med <- df_b %>%
  filter(between >= 0.23 & between <= 0.44)
sum(med$seg_length) # length
sum(med$seg_length)/sum(df_b$seg_length) # proportion

# subset betweenness >=0.45
high <- df_b %>%
  filter(between >= 0.45)
sum(high$seg_length) # length
sum(high$seg_length)/sum(df_b$seg_length) # proportion

# IOWA --------------------------------------------------------------------
# data -----------------------------------------------------------------

# stream polyline
df_b <- readRDS(file = "data_fmt/data_iowa_stream_betweenness.rds") %>%
  st_as_sf()

# segment length ----------------------------------------------------------

# determine length of stream segments
l <- st_length(df_b)

# add column
df_b <- df_b %>% 
  add_column(l = l)

# new cloumn with "[m]" removed  
df_b$seg_length <- stringr::str_remove(df_b$l, '\\[m]')

# remove original l column 
df_b <- df_b %>%
  dplyr::select(-c(l))

# betweenness classes ----------------------------------------------------

# make segment length numeric
df_b <- df_b %>%
  mutate(seg_length = as.numeric(seg_length))
sum(df_b$seg_length) # total length

# subset connectivity <=0.16
low <- df_b %>%
  filter(between <= 0.16)
sum(low$seg_length) # length 
sum(low$seg_length)/sum(df_b$seg_length) # proportion

# subset connectivity between 0.17-0.32
med <- df_b %>%
  filter(between >= 0.17 & between <= 0.32)
sum(med$seg_length) # length
sum(med$seg_length)/sum(df_b$seg_length) # proportion

# subset connectivity >=0.33
high <- df_b %>%
  filter(between >= 0.33)
sum(high$seg_length)
sum(high$seg_length)/sum(df_b$seg_length) # proportion
