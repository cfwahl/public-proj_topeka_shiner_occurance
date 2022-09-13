# this code creates the segment identifier based on lat/long

# setup -------------------------------------------------------------------

## loading packages
pacman::p_load(tidyverse,
               sf)

rm(list = ls())



 ####  MN DNR STREAM DATA  ####

# read mn dnr data ---------------------------------------------------------------

df0 <- read_csv("data_raw/mn_topeka_shiner_occurrence.csv") %>% 
  select(Site_Num:Topeka_Shiner) # select columns from Site_num to Topeka_Shiner

colnames(df0) <- str_to_lower(colnames(df0)) # make all column names lowercase


# format ------------------------------------------------------------------

df_mn <- df0 %>% 
  separate(site_num,
           into = c("segment", "transect"),
           sep = "_") %>% # separate `site_num` into `segment` and `transect` columns
  mutate(segment = as.numeric(segment),
         transect = as.numeric(transect)) %>% # tunrn into numeric data
  group_by(segment, year) %>%  # grouping by segment and year columns for group operation
  summarize(occurrence = sum(topeka_shiner), # take sum of topeka shiner for each group
            lat = round(lat[1], 3), # take the first element of lat for each group
            long = round(long[1], 3)) %>%  # take the first element of long for each group
  ungroup() %>% 
  group_by(lat, long) %>% 
  summarize(occurrence = sum(occurrence),
            year = max(year), # latest year of observation
            lat = unique(lat),
            long = unique(long)) %>% 
  ungroup() %>% 
  mutate(occurrence = replace(occurrence, occurrence > 0, 1),  # if occurrence is >0 then make 1
         site = seq_len(n_distinct(paste0(.$lat, .$long))))


# error check -------------------------------------------------------------

## check if each segment has unique segment coordinates
df_coords <- df_mn %>% 
  distinct(site, lat, long) %>% 
  group_by(site) %>% 
  summarize(n_coord = n_distinct(paste0(lat, long)))

## segment with multiple unique coords  
unique(df_coords$n_coord)

## count number of observations for each segment
df_n_obs <- df_mn %>% 
  group_by(site) %>% 
  summarize(n_year = n_distinct(year)) # count unique year ID for each group

# export ------------------------------------------------------------------

# this will recall code in R script
save(df_mn, file = "data_fmt/data_mn_dnr_fmt.RData")

# shape file export
df_mn %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_mn_dnr_fmt_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)


   ####  FWS & ISU OXBOW/STREAM DATA ####

# read fws data ---------------------------------------------------------------

# fws and isu topeka shiner occurrence and env data
df1 <- read_csv("data_fmt/tksn_env_fws_isu.csv") %>% 
  select(SampleID:Topeka_Shiner) # select columns from Site_num to Topeka_Shiner

colnames(df1) <- str_to_lower(colnames(df1)) # make all column names lowercase


# format fws data ------------------------------------------------------------------

df_mn_fws <- df1 %>% 
  filter(habitattype == "stream") %>% # select only stream samples, not oxbows
  group_by(site, year) %>%  # grouping by site and year 
  summarize(occurrence = sum(topeka_shiner), # take sum of topeka shiner for each group
            lat = round(lat[1], 3), # take the first element of lat for each group
            long = round(long[1], 3)) %>% # take the first element of long for each group
  ungroup() %>% 
  group_by(lat, long) %>% 
  summarize(occurrence = sum(occurrence),
            year = max(year), # latest year of observation
            lat = unique(lat),
            long = unique(long)) %>% 
  ungroup() %>% 
  mutate(occurrence = replace(occurrence, occurrence > 0, 1),  # if occurrence is >0 then make 1
         site = seq_len(n_distinct(paste0(.$lat, .$long))))

## check if each segment has unique segment coordinates
df_coords <- df_mn_fws %>% 
  distinct(site, lat, long) %>% 
  group_by(site) %>% 
  summarize(n_coord = n_distinct(paste0(lat, long)))

## segment with multiple unique coords  
unique(df_coords$n_coord)

## count number of observations for each segment
df_n_obs <- df_mn_fws %>% 
  group_by(site) %>% 
  summarize(n_year = n_distinct(year)) # count unique year ID for each group


# export fws data------------------------------------------------------------------

# this will recall code in R script
save(df_mn_fws, file = "data_fmt/data_mn_fws_fmt.RData")

# shape file export
df_mn_fws %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_mn_fws_fmt_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)


# join DNR and FWS data frames ---------------------------------------------------------

# load the MN DNR occurrence data
# load(file = "data_fmt/data_mn_dnr_fmt.Rdata")

# join data and create a new column with unique site IDs
df2 <- full_join(df_mn, df_mn_fws) %>%
  mutate(siteid = row_number())


# format ------------------------------------------------------------------

df_mn_dnr_fws <- df2 %>% 
  group_by(lat, long) %>% # check for double lat longs between data sets 
  summarize(occurrence = sum(occurrence),
            year = max(year), # latest year of observation
            lat = unique(lat),
            long = unique(long)) %>% 
  ungroup() %>%
  mutate(occurrence = replace(occurrence, occurrence > 0, 1),  # if occurrence is >0 then make 1
         siteid = seq_len(n_distinct(paste0(.$lat, .$long)))) %>%
  slice(-c(72, 117, 147, 162, 207, 211, 232, 239, 268)) %>% # these sites fall outside 5km2 stream network
  mutate(siteid = seq_len(n_distinct(paste0(.$lat, .$long)))) # assign new siteid

# export combined fws and dnr data------------------------------------------------------------------

# this will recall code in R script
save(df_mn_dnr_fws, file = "data_fmt/data_mn_dnr_fws_fmt.RData")

# shape file export
df_mn_dnr_fws %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)



######  extract oxbow data  ######

# read fws data ---------------------------------------------------------------

# fws and isu topeka shiner occurrence and env data
df1 <- read_csv("data_fmt/tksn_env_fws_isu.csv") %>% 
  select(SampleID:Topeka_Shiner) # select columns from Site_num to Topeka_Shiner

colnames(df1) <- str_to_lower(colnames(df1)) # make all column names lowercase

# format fws data ------------------------------------------------------------------

df_mn_fws <- df1 %>% 
  filter(habitat == "oxbow") %>% # select only stream samples, not oxbows
  group_by(site, year) %>%  # grouping by site and year 
  summarize(occurrence = sum(topeka_shiner), # take sum of topeka shiner for each group
            lat = round(lat[1], 3), # take the first element of lat for each group
            long = round(long[1], 3)) %>% # take the first element of long for each group
  ungroup() %>% 
  group_by(lat, long) %>% 
  summarize(occurrence = sum(occurrence),
            year = max(year), # latest year of observation
            lat = unique(lat),
            long = unique(long)) %>% 
  ungroup() %>% 
  mutate(occurrence = replace(occurrence, occurrence > 0, 1),  # if occurrence is >0 then make 1
         site = seq_len(n_distinct(paste0(.$lat, .$long)))) %>%
  slice(-c(1, 2, 3, 4, 5, 6)) # these sites fall outside connectivity stream network
  


# export oxbow data------------------------------------------------------------------

# this will recall code in R script
saveRDS(df_mn_fws, file = "data_fmt/data_mn_fws_fmt_oxbows.RDS")

# shape file export
df_mn_fws %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_oxbow_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)

