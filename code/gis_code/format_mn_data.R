
# this code creates the segment identifer based on lat/long

# setup -------------------------------------------------------------------

## loading packages
pacman::p_load(tidyverse,
               sf)


# read data ---------------------------------------------------------------

df0 <- read_csv("data_raw/mn_topeka_shiner_occurrence_2.csv") %>% 
  select(Site_Num:Topeka_Shiner) # select columns from Site_num to Topeka_Shiner

colnames(df0) <- str_to_lower(colnames(df0)) # make all column names lowercase


# format ------------------------------------------------------------------

df1 <- df0 %>% 
  separate(site_num,
           into = c("segment", "transect"),
           sep = "_") %>% # separate `site_num` into `segment` and `transect` columns
  mutate(segment = as.numeric(segment),
         transect = as.numeric(transect)) %>% # tunrn into numeric data
  group_by(segment, year) %>%  # grouping by segment and year columns for group operation
  summarize(occurrence = sum(topeka_shiner), # take sum of topeka shiner for each group
            lat = round(lat[1], 3), # take the first element of lat for each group
            long = round(long[1], 3), # take the first element of long for each group
            lat_long = paste(lat, long)) # make unique site_id (combination of long-lat)


# error check -------------------------------------------------------------

## check if each segment has unique segment coordinates
df_coords <- df1 %>% 
  group_by(segment) %>% 
  summarize(n_coords = n_distinct(lat_long))

## segment with multiple unique coords  
df_coords %>% 
  filter(n_coords > 1) %>% # select segment with multiple coordinates
  pull(segment) # pull segment ID

## count number of observations for each segment
df_n_obs <- df1 %>% 
  group_by(segment) %>% 
  summarize(n_year = n_distinct(year)) # count unique year ID for each group

## There were sites where more than one shiner was caught, we want binary
## if occurrence is >0 then make 1
df1 <-  df1 %>% mutate(occurrence = replace(occurrence, occurrence>0, 1))

# export ------------------------------------------------------------------

write_csv(df1, "data_fmt/data_mn_fmt.csv")


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

