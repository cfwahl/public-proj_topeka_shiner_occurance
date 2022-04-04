
# Minnesota DNR Topeka shiner analysis. This code will identify sites based
# on lat/long and assign as segment ID. Then the script does watershed delineation 
# for each site, determines land use within each watershed and other variables 
# of interest. Finally it calculates in-stream distance among sites. Broken into
# upstream, downstream and total disatnce matrices

# setup -------------------------------------------------------------------

pacman::p_load(tidyverse,
               sf,
               raster,
               stars,
               whitebox, 
               foreach,
               exactextractr,
               riverdist)

# clean objects
rm(list = ls())

##### segment identifier --------------------------------------------------

# this section of code creates the segment identifier based on lat/long

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



##### watershed analysis ---------------------------------------------------

# this section of code will create the watershed for each site.


# extract stream grid -----------------------------------------------------

## read raster - upstream catchment area
wgs84_upa <- raster("data_raw/n40w100_upa.tif")

## extract stream grids (>= 1 sq-km)
stream_grid_1sqkm <- calc(wgs84_upa,
                          fun = function(x) ifelse(x >= 1, 1, NA))

## save the raster file
writeRaster(stream_grid_1sqkm,
            filename = "epsg4326_stream_grid_1sqkm",
            format = "GTiff",
            overwrite = TRUE)  


# snapping ----------------------------------------------------------------

## read channel
wgs84_channel <- st_read(dsn = "vector",
                         layer = "StrNet2",
                         drivers = "ESRI Shapefile")

utm_channel <- st_transform(wgs84_channel, crs = 3722)

## read outlet points
wgs84_outlet <- st_read(dsn = "data_fmt/epsg4326_sites_relocated.gpkg")
utm_outlet <- st_transform(wgs84_outlet, crs = 3722)

## snap points to the nearest polyline
source("code/function_st_snap_points.R")
utm_outlet_snap <- st_snap_points(x = utm_outlet,
                                  y = utm_channel) %>% 
  st_as_sf()

wgs84_outlet_snap <- st_transform(utm_outlet_snap,
                                  crs = 4326)

## export to check snapped file in QGIS
st_write(wgs84_outlet_snap,
         dsn = "data_fmt/epsg4326_outlet_sample_snap2.gpkg",
         append = FALSE)


# watershed delineation ---------------------------------------------------

# this example runs watershed delineation for each sampling site separately
# whitebox functions cannot take path including whitespace
# gis files for whitebox is saved in temporary directory

## read flow direction raster
wgs84_dir_arc <- raster("data_raw/n40w100_dir.tif")

## convered arc format to d8 format
source("code/function_arc2d8.R")
wgs84_dir_d8 <- arc2d8(wgs84_dir_arc)
temp_d8 <- paste(tempdir(), "epsg4326_dir_d8.tif", sep = "\\") # temporary file name

## looping watershed delineation for each sampling site
wgs84_wsd_shape_comb <- foreach(i = seq_len(nrow(wgs84_outlet_snap)),
                                .combine = bind_rows) %do% {
                                  ## save gis files to temporary directory
                                  ## choose one outlet point from the data frame
                                  st_write(wgs84_outlet_snap[i,],
                                           dsn = tempdir(),
                                           "outlet",
                                           driver = "ESRI Shapefile",
                                           append = FALSE)
                                  
                                  writeRaster(wgs84_dir_d8,
                                              temp_d8,
                                              overwrite = TRUE)
                                  
                                  ## watershed raster file name
                                  wsd <- paste(tempdir(), "epsg4326_watershed.tif", sep = "\\")
                                  
                                  wbt_watershed(d8_pntr = temp_d8,
                                                pour_pts = paste(tempdir(), "outlet.shp", sep = "\\"), 
                                                output = wsd)
                                  
                                  ## watershed raster to watershed polygon
                                  wgs84_wsd_shape <- NULL
                                  wgs84_wsd_shape <- raster(wsd) %>% 
                                    st_as_stars() %>% 
                                    st_as_sf(merge = TRUE,
                                             as_points = FALSE) %>% 
                                    mutate(watershed_id = i)
                                  
                                  return(wgs84_wsd_shape)
                                }

st_write(wgs84_wsd_shape_comb,
         dsn = "data_fmt/wgs84_watershed2.gpkg",
         append = FALSE)



##### calculate land use in watersheds ------------------------------------

# this section of code will calculate percent land use for agriculture, 
# forest, and urban, plus climate variables. 

# read data ---------------------------------------------------------------

# raster extraction -------------------------------------------------------

## watershed polygon delineated in gis_watershed_example.R
## transform to projected crs for extraction - extaction needs to be performed with projected crs
wgs84_sf_wsd <- st_read(dsn = "data_fmt/wgs84_watershed2.gpkg")
albers_sf_wsd <- wgs84_sf_wsd %>% 
  st_transform(crs = 5070)


## land use ####

## example landuse data - Copernics 100m resolution global data
## transform to projected crs for extraction - extaction needs to be performed with projected crs
## "discrete" raster must be reprojected with "ngb" (nearest neigbor)
wgs84_rs_lu <- raster("raster/epsg4326_nlcd_2019_study_area.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer

albers_rs_lu <- projectRaster(from = wgs84_rs_lu,
                              crs = 5070,
                              method = 'ngb',
                              res = 100)

## create binary raster for each land use type (1,0 entry for each cell)
## land use code must be adapted
albers_rs_urban <- calc(albers_rs_lu,
                         fun = function(x) ifelse(dplyr::between(x, 21, 24), 1, 0))

albers_rs_forest <- calc(albers_rs_lu,
                        fun = function(x) ifelse(dplyr::between(x, 41, 52), 1, 0))

albers_rs_agri <- calc(albers_rs_lu,
                       fun = function(x) ifelse(dplyr::between(x, 81, 82), 1, 0))

albers_rs_grass <- calc(albers_rs_lu,
                       fun = function(x) ifelse(x == 71, 1, 0))

albers_rs_wet <- calc(albers_rs_lu,
                       fun = function(x) ifelse(dplyr::between(x, 90, 95), 1, 0))


albers_rs_fua <- raster::stack(albers_rs_forest,
                               albers_rs_urban,
                               albers_rs_agri) # combine 3 layers into one "stack"

names(albers_rs_fua) <- c("forest", "urban", "agri")


## climate ####

## example climate data (annual mean temperature) - CHELSA ver 2.1 (30 arcsecond ~ 1km)
## "continuous" raster must be reprojected with "bilinear"
## CHELSA layer from "https://chelsa-climate.org/downloads/"
wgs84_rs_temp <- raster("data_raw/CHELSA_bio1_1981-2010_V.2.1.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer

albers_rs_temp <- projectRaster(from = wgs84_rs_temp,
                                crs = 5070,
                                method = 'bilinear',
                                res = 1000)


# extraction by polygon ---------------------------------------------------

## land use
df_lu <- exact_extract(albers_rs_fua, # raster layer for extraction
                       albers_sf_wsd, # masking layer
                       "mean") %>% # take mean for each polygon
  rename(frac_forest = mean.forest,
         frac_urban = mean.urban,
         frac_agri = mean.agri) %>% 
  mutate(watershed_id = seq_len(nrow(.)))

## climate
df_temp <- exact_extract(albers_rs_temp, # raster layer for extraction
                         albers_sf_wsd, # masking layer
                         "mean") %>%   # take mean for each polygon
  as_tibble() %>% 
  rename(temp = value) %>% 
  mutate(watershed_id = seq_len(nrow(.)))


# merge data --------------------------------------------------------------

albers_sf_wsd <- albers_sf_wsd %>% 
  left_join(df_lu, by = "watershed_id") %>% 
  left_join(df_temp, by = "watershed_id") %>% 
  relocate(watershed_id) %>% 
  mutate(area = st_area(.)) # add watershed area

st_write(albers_sf_wsd,
         dsn = "data_fmt/espg5070_Watersheds_LU.gpkg",
         append = FALSE)



##### distance matrices ---------------------------------------------------

# this section of code creates the upstream, downstream, and total distance matrices


# data --------------------------------------------------------------------

# read sites
utm_sf_outlet <- st_read(dsn = "data_fmt/gis/watersheds/watershed6",
                         layer = "epsg3722_sites_ws6",
                         drivers = "ESRI Shapefile")

# data frame for sampling sites
df_coord <- utm_sf_outlet %>% 
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>% 
  as_tibble() %>% 
  distinct(segment, X, Y) # remove duplicates

X <- df_coord$X
Y <- df_coord$Y

# read in the network
strnet <- line2network(path = "data_fmt/gis/watersheds/watershed6", 
                       layer = "epsg3722_StrNet_ws6")


# prep stream network and sites --------------------------------------------------

# plot of all the segments
plot(strnet)  

# check topology (nodes)
topologydots(strnet)

# do a clean up: dissolve - y, insert vertices - y, distance - 1, 
# examine figure for mouth questions, remove ad4ditional segments - n, 
# build segment routes - y
strnet_fixed <- cleanup(rivers = strnet)

# snap sites to stream network
site_snap <- xy2segvert(x = X,
                        y = Y,
                        rivers = strnet_fixed)


# distance matrix  --------------------------------------------------------

# distance matrix: total, m_x
m_x <- riverdistancemat(site_snap$seg,
                        site_snap$vert,
                        strnet_fixed, 
                        ID = site_snap$segment) %>%
  data.matrix()

m_x <- round(m_x / 1000, 2) # convert to km


# distance matrix: net upstream, m_y
m_y <- upstreammat(seg = site_snap$seg,
                   vert = site_snap$vert, 
                   rivers = strnet_fixed, 
                   ID = site_snap$segment,
                   net = TRUE) %>%
  data.matrix()

m_y <- round(m_y / 1000, 2) # convert to km

# distance matrix: up and downstream distance (m_u & m_d)
m_u <- (m_x + m_y) / 2
m_d <- (m_x - m_y) / 2

m_td <- round(m_u + m_d, 2)

# check if m_td = m_x
identical(m_td, m_x)

# export
save(m_u, file = "data_fmt/m_u_ws6.RData")
save(m_d, file = "data_fmt/m_d_ws6.RData")
