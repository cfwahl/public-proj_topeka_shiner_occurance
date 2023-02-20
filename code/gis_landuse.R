
# this code will calculate percent land use for agriculture, 
# forest, urban, grassland, wetlands, plus climate variables 

# setup -------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# read watershed and landuse data ---------------------------------------------------------------

# watershed polygon
wgs84_sf_wsd <- readRDS(file = "data_fmt/data_minnesota_stream_watersheds_dummy_real.rds")
#wgs84_sf_wsd <- st_read(dsn = "data_fmt/vector/epsg4326_minnesota_stream_watersheds_dummy_real.shp")
utm_sf_wsd <- wgs84_sf_wsd %>% 
  st_transform(crs = 3722)

# land use
wgs84_rs_lu <- raster("data_fmt/raster/epsg4326_nlcd_2019_study_area.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer

# raster extraction -------------------------------------------------------

# "discrete" raster must be reprojected with "ngb" (nearest neigbor)
utm_rs_lu <- projectRaster(from = wgs84_rs_lu,
                           crs = st_crs(3722)$proj4string,
                           method = 'ngb',
                           res = 30)

## create binary raster for each land use type (1,0 entry for each cell)
## land use code must be adapted from source
utm_rs_urban <- calc(utm_rs_lu,
                     fun = function(x) ifelse(dplyr::between(x, 21, 24), 1, 0))

utm_rs_forest <- calc(utm_rs_lu,
                      fun = function(x) ifelse(dplyr::between(x, 41, 52), 1, 0))

utm_rs_agri <- calc(utm_rs_lu,
                    fun = function(x) ifelse(dplyr::between(x, 81, 82), 1, 0))

utm_rs_grass <- calc(utm_rs_lu,
                     fun = function(x) ifelse(x == 71, 1, 0))

utm_rs_wet <- calc(utm_rs_lu,
                   fun = function(x) ifelse(dplyr::between(x, 90, 95), 1, 0))

utm_rs_fua <- raster::stack(utm_rs_forest,
                            utm_rs_urban,
                            utm_rs_agri, 
                            utm_rs_grass,
                            utm_rs_wet) # combine 5 layers into one "stack"

names(utm_rs_fua) <- c("forest", "urban", "agri", "grass", "wetland")

# read climate data -------------------------------------------------------

# CHELSA ver 2.1 (30 arcsecond ~ 1km), "https://chelsa-climate.org/downloads/"
## "continuous" raster must be reprojected with "bilinear"
# precipitation seasonality 
wgs84_rs_precip_season <- raster("data_fmt/raster/chelsa_clip_precip_seasonality.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer

utm_rs_precip_season <- projectRaster(from = wgs84_rs_precip_season,
                                      crs = st_crs(3722)$proj4string,
                                      method = 'bilinear',
                                      res = 1000)

# precipitation during wettest month 
wgs84_rs_precip_wettest <- raster("data_fmt/raster/chelsa_clip_precip_wettest.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer

utm_rs_precip_wettest <- projectRaster(from = wgs84_rs_precip_wettest,
                                       crs = st_crs(3722)$proj4string,
                                       method = 'bilinear',
                                       res = 1000)

# temp seasonality
wgs84_rs_temp_seasonality <- raster("data_fmt/raster/chelsa_clip_temp_seasonality.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer

utm_rs_temp_seasonality <- projectRaster(from = wgs84_rs_temp_seasonality,
                                         crs = st_crs(3722)$proj4string,
                                         method = 'bilinear',
                                         res = 1000)

# extraction by polygon ---------------------------------------------------

## land use extraction 
df_lu <- exact_extract(utm_rs_fua, # raster layer for extraction
                       utm_sf_wsd, # masking layer
                       "mean",
                       append_cols = "siteid") %>% # take mean for each polygon
  as_tibble() %>% 
  rename(frac_forest = mean.forest,
         frac_urban = mean.urban,
         frac_agri = mean.agri,
         frac_grass = mean.grass,
         frac_wetland = mean.wetland)

# combine climate layers
utm_rs_clim <- raster::stack(utm_rs_precip_season,
                            utm_rs_precip_wettest,
                            utm_rs_temp_seasonality) # combine 3 layers into one "stack"

names(utm_rs_clim) <- c("precip_season", "precip_wet", "temp_season")

## climate extraction
df_clim <- exact_extract(utm_rs_clim, # raster layer for extraction
                       utm_sf_wsd, # masking layer
                       "mean",
                       append_cols = "siteid") %>% # take mean for each polygon
  as_tibble() %>% 
  rename(precip_season = mean.precip_season,
         precip_wet = mean.precip_wet,
         temp_season = mean.temp_season)

# join land use and climate data --------------------------------------------------------------

# join shapefiles based on "siteid" column
utm_sf_wsd <- utm_sf_wsd %>% 
  left_join(df_lu, by = "siteid") %>% 
  left_join(df_clim, by = "siteid") %>% 
  mutate(area = units::set_units(st_area(.), "km^2")) %>% # add watershed area
  arrange(siteid)

# save shapefile as geopackage, saving as shapefile will change column names
# st_write(utm_sf_wsd,
#          dsn = "data_fmt/vector/epsg3722_minnesota_stream_landuse_dummy_real.gpkg",
#          append = FALSE)

saveRDS(utm_sf_wsd, file ="data_fmt/data_minnesota_stream_landuse_dummy_real.rds")
