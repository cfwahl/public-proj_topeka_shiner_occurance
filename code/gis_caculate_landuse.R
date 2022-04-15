
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
wgs84_sf_wsd <- st_read(dsn = "vector/wgs84_mn_fmt_watershed.gpkg")
albers_sf_wsd <- wgs84_sf_wsd %>% 
  st_transform(crs = 5070)


## land use ##

## example landuse data - Copernics 100m resolution global data
## transform to projected crs for extraction - extaction needs to be performed with projected crs
## "discrete" raster must be reprojected with "ngb" (nearest neigbor)
wgs84_rs_lu <- raster("raster/epsg4326_nlcd_2019_study_area.tif") %>% 
  crop(extent(wgs84_sf_wsd)) # crop by extent of watershed layer


# raster extraction -------------------------------------------------------

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
                               albers_rs_agri, 
                               albers_rs_grass,
                               albers_rs_wet) # combine 5 layers into one "stack"

names(albers_rs_fua) <- c("forest", "urban", "agri", "grass", "wetland")


## climate ####

## example climate data (annual mean temperature) - CHELSA ver 2.1 (30 arcsecond ~ 1km)
## "continuous" raster must be reprojected with "bilinear"
## CHELSA layer from "https://chelsa-climate.org/downloads/"
wgs84_rs_temp <- raster("raster/CHELSA_bio1_1981-2010_V.2.1.tif") %>% 
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
         frac_agri = mean.agri,
         frac_grass = mean.grass,
         frac_wetland = mean.wetland) %>% 
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

albers_sf_wsd <- st_transform(albers_sf_wsd, crs = 3722)

st_write(albers_sf_wsd,
         dsn = "data_fmt/espg3722_watersheds_landuse.gpkg",
         append = FALSE)

