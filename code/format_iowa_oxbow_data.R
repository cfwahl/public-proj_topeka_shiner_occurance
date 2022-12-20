
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


# export ------------------------------------------------------------------

# export geopackage, shapefile changes the column names
df_ia %>% 
  st_as_sf(coords = c("longitudewq", "latutudewq"),
           crs = 4326) %>% 
  st_write(dsn = "data_fmt/vector/epsg4326_iowa_oxbow_sites.shp",
           drivers = "ESRI Shapefile",
           append = FALSE)