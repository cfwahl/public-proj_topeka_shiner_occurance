
# this code use the raw data and creates unique stream and oxbow identifier 
# based on segment, most recent sampling year, lat/long. Count data summed in 
# groups then converted to occurrence. Uses stream data from Minnesota Department 
# of Natural Resources (MNDNR) and US Fish and Wildlife Service (FWS), and 
# oxbow data from FWS and Iowa State University (ISU).

# setup -------------------------------------------------------------------

# clean up
rm(list = ls())

## loading libaries
source(here::here("code/library.R")) 

# MINNESOTA ---------------------------------------------------------------
# STREAMS -----------------------------------------------------------------

# read MNDNR stream data ---------------------------------------------------------------

df_dnr <- read_csv("data_raw/mn_topeka_shiner_occurrence.csv") %>% 
  dplyr::select(Site_Num:Topeka_Shiner) # select columns from Site_num to Topeka_Shiner

colnames(df_dnr) <- str_to_lower(colnames(df_dnr)) # make all column names lowercase

# format MNDNR data ------------------------------------------------------------------

df_mn_dnr <- df_dnr %>% 
  separate(site_num,
           into = c("segment", "transect"),
           sep = "_") %>% # separate `site_num` into `segment` and `transect` columns
  mutate(segment = as.numeric(segment),
         transect = as.numeric(transect)) %>% # turn into numeric data
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

# read FWS and ISU stream/oxbow data ---------------------------------------------------------------

# fws and isu occurrence and environmental data
df_fws <- read_csv("data_fmt/tksn_env_fws_isu.csv") %>% 
  dplyr::select(SampleID:Topeka_Shiner) # select columns from Site_num to Topeka_Shiner

colnames(df_fws) <- str_to_lower(colnames(df_fws)) # make all column names lowercase

# format FWS stream data ------------------------------------------------------------------

df_mn_fws <- df_fws %>% 
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

# join MNDNR and FWS data ---------------------------------------------------------

# join data and create a new column with unique site IDs
df_dnr_fws <- full_join(df_mn_dnr, df_mn_fws) %>%
  mutate(siteid = row_number())

# format MNDNR and FWS data ------------------------------------------------------------------

df_mn_dnr_fws <- df_dnr_fws %>% 
  group_by(lat, long) %>% # check for double lat longs between data sets 
  summarize(occurrence = sum(occurrence),
            year = max(year), # latest year of observation
            lat = unique(lat),
            long = unique(long)) %>% 
  ungroup() %>%
  mutate(occurrence = replace(occurrence, occurrence > 0, 1),  # if occurrence is >0 then make 1
         siteid = seq_len(n_distinct(paste0(.$lat, .$long)))) %>%
  filter(!(siteid %in% c("72", "117", "147", "162", "207", 
                         "211", "232", "239", "268"))) %>% # these sites fall outside 5km2 stream network
  mutate(siteid = seq_len(n_distinct(paste0(.$lat, .$long)))) # assign new siteid

# export MNDNR and FWS data------------------------------------------------------------------

# this will recall code in R script
saveRDS(df_mn_dnr_fws, file = "data_fmt/data_minnesota_fmt_streams.rds")

# shape file export
df_mn_dnr_fws %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)

# OXBOWS ------------------------------------------------------------------
# read FWS and ISU data ---------------------------------------------------------------

# fws and isu occurrence and environmental data
df_all <- read_csv("data_fmt/tksn_env_fws_isu.csv") %>% 
  dplyr::select(SampleID:pH) %>% # select columns from Site_num to pH
  mutate_at(c('WaterTemp_C', 'DOPercent', 'DO_mgL', 'Turbidity_NTU',
                'Conductivity_Scm', 'pH'), as.numeric) # make these columns numeric 

colnames(df_all) <- str_to_lower(colnames(df_all)) # make all column names lowercase

# format Minnesota FWS and ISU oxbow data ------------------------------------------------------------------

df_oxbow <- df_all %>% 
  filter(habitat == "oxbow") %>%  # select only oxbows
  filter(!habitattype=="Unrestored_oxbow") %>%
  group_by(site, year) %>%  # grouping by site and year 
  summarize(occurrence = sum(topeka_shiner), # take sum of topeka shiner for each group
            temp = mean(watertemp_c, na.rm = TRUE), # get mean values for these variables and remove NAs
            dopercent = mean(dopercent, na.rm = TRUE),
            cond = mean(conductivity_scm, na.rm = TRUE),
            do_mgl = mean(do_mgl, na.rm = TRUE),
            turb = mean(turbidity_ntu, na.rm = TRUE),
            ph = mean(ph, na.rm = TRUE), 
            lat = round(lat[1], 4), # take the first element of lat for each group
            long = round(long[1], 4)) %>% # take the first element of long for each group) 
  ungroup() %>% 
  mutate(occurrence = replace(occurrence, occurrence > 0, 1)) # if occurrence is >0 then make 1
  
# export oxbow data------------------------------------------------------------------

# set coordinates for oxbows
df_oxbow <- df_oxbow %>% 
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

# this will recall code in R script
saveRDS(df_oxbow, file = "data_fmt/data_minnesota_fmt_oxbows.rds")

# shape file export
df_oxbow %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>%
  st_write(dsn = "data_fmt/vector/epsg4326_minnesota_oxbow_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)

# IOWA  -------------------------------------------------------------
# read FWS and ISU oxbow and stream data ---------------------------------------------------------------

# sites without topeka shiner data were removed, aka culled
df0 <- read_csv("data_fmt/mn_ia_site_env_data_culled.csv") %>% 
  dplyr::select(SampleID:Topeka_Shiner) %>% # select columns from SampleID to Topeka_Shiner
  mutate_at(c('WaterTemp_C', 'DOPercent', 'DO_mgL', 'Turbidity_NTU',
              'Conductivity_Scm', 'pH'), as.numeric) 

# make all column names lowercase
colnames(df0) <- str_to_lower(colnames(df0)) 

# only want Iowa sampling locations
df_ia <- df0 %>% 
  filter(state=="IA",
         habitattype=="Restored_Oxbow")

# only want oxbow sampling locations
df_ia <- df_ia %>% 
  filter(habitat=="oxbow") %>%
  group_by(siteid, year) %>%
  summarize(occurrence = sum(topeka_shiner), # take sum of topeka shiner for each group
            temp = mean(watertemp_c, na.rm = TRUE), # get mean values for these variables and remove NAs
            dopercent = mean(dopercent, na.rm = TRUE),
            do_mgl = mean(do_mgl, na.rm = TRUE),
            turb = mean(turbidity_ntu, na.rm = TRUE),
            ph = mean(ph, na.rm = TRUE),
            cond = mean(conductivity_scm, na.rm = TRUE),
            lat = round(latutudewq[1], 4), # take the first element of lat for each group
            long = round(longitudewq[1], 4),
            year = max(year)) %>%
  mutate(occurrence = replace(occurrence, occurrence > 0, 1))  # if occurrence is >0 then make 1

# export ------------------------------------------------------------------

# this will recall code in R script
saveRDS(df_ia, file = "data_fmt/data_iowa_fmt_owbows.rds")

# export geopackage, shapefile changes the column names
df_ia %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4326) %>%
  st_write(dsn = "data_fmt/vector/epsg4326_iowa_oxbow_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)
