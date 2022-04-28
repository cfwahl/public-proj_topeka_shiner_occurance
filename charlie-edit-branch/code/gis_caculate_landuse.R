
##### calculate land use in watersheds ------------------------------------

# this section of code will calculate percent land use for agriculture, 
# forest, urban, grassland, wetlands, plus climate variables 


# setup -------------------------------------------------------

# clean objects
rm(list = ls())

pacman::p_load(raster,
               tidyverse,
               sf,
               exactextractr)  


# read data ---------------------------------------------------------------

## watershed polygon ##

# transform to projected crs for extraction - 
# extraction needs to be performed with projected crs
wgs84_sf_wsd <- st_read(dsn = "data_fmt/vector/wgs84_mn_fmt_watersheds.gpkg")
utm_sf_wsd <- wgs84_sf_wsd %>% 
  st_transform(crs = 3722)


## land use ##

## transform to projected crs for extraction - extaction needs to be performed with projected crs
## "discrete" raster must be reprojected with "ngb" (nearest neigbor)
wgs84_rs_lu <- raster("data_fmt/raster/epsg4326_nlcd_2019_study_area.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer


# raster extraction -------------------------------------------------------

utm_rs_lu <- projectRaster(from = wgs84_rs_lu,
                           crs = st_crs(3722)$proj4string,
                           method = 'ngb',
                           res = 30)

## create binary raster for each land use type (1,0 entry for each cell)
## land use code must be adapted
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


## climate ####

## example climate data (annual mean temperature) - CHELSA ver 2.1 (30 arcsecond ~ 1km)
## "continuous" raster must be reprojected with "bilinear"
## CHELSA layer from "https://chelsa-climate.org/downloads/"
wgs84_rs_temp <- raster("data_fmt/raster/chelsa_clip.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer

utm_rs_temp <- projectRaster(from = wgs84_rs_temp,
                             crs = st_crs(3722)$proj4string,
                             method = 'bilinear',
                             res = 1000)


# extraction by polygon ---------------------------------------------------

## land use
df_lu <- exact_extract(utm_rs_fua, # raster layer for extraction
                       utm_sf_wsd, # masking layer
                       "mean",
                       append_cols = "site") %>% # take mean for each polygon
  as_tibble() %>% 
  rename(frac_forest = mean.forest,
         frac_urban = mean.urban,
         frac_agri = mean.agri,
         frac_grass = mean.grass,
         frac_wetland = mean.wetland)

## climate
df_temp <- exact_extract(utm_rs_temp, # raster layer for extraction
                         utm_sf_wsd, # masking layer
                         "mean",
                         append_cols = "site") %>%   # take mean for each polygon
  as_tibble() %>% 
  rename(temp = mean)


# merge data --------------------------------------------------------------

utm_sf_wsd <- utm_sf_wsd %>% 
  left_join(df_lu, by = "site") %>% 
  left_join(df_temp, by = "site") %>% 
  mutate(area = units::set_units(st_area(.), "km^2")) # add watershed area

st_write(utm_sf_wsd,
         dsn = "data_fmt/vector/espg3722_watersheds_landuse.gpkg",
         append = FALSE)

