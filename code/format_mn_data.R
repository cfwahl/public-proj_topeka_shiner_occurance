
# this code creates the segment identifier based on lat/long

# setup -------------------------------------------------------------------

## loading packages
pacman::p_load(tidyverse,
               sf)


# read data ---------------------------------------------------------------

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
save(df_mn, file = "data_fmt/data_mn_fmt.RData")

# need a csv to display points in qgis
write_csv(df_mn, "data_fmt/data_mn_fmt.csv")


# mapping -----------------------------------------------------------------

# sf is the package for shape file operations
# st_as_sf is to convert dataframe to sf object (i.e., points/polygons with coordinates)

# error in row 314?
st_df1 <- df1 %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) # crs 4326 = WGS84

mapview::mapView(st_df1)

## error in row 406 and 1335?
st_df0 <- st_as_sf(df0,
                   coords = c("long", "lat"),
                   crs = 4326) # crs 4326 = WGS84

mapview::mapView(st_df0$geometry)

