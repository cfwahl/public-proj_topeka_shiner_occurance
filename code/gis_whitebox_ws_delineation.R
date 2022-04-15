
# watershed delineation using whitebox package

# setup -------------------------------------------------------------------

pacman::p_load(sf,
               raster,
               stars,
               whitebox,
               foreach,
               tidyverse)


# DEM setup ---------------------------------------------------------------

# read in DEM
wgs84_ups <- raster("raster/epsg4326_n40w100_upa_clipped.tif")

## read outlet points
wgs84_sites <- st_read(dsn = "vector/epsg4326_mn_fmt_sites.gpkg") 

#'- Read flow direction raster
wgs84_dir <- raster("raster/epsg4326_n40w100_dir_clipped.tif")


#'- convert arc format to d8 format
source("code/function_arc2d8.R")
wgs84_dir_d8 <- arc2d8(wgs84_dir)

writeRaster(wgs84_dir_d8,'raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif',
            options=c('TFW=YES'), overwrite = TRUE)


# gis ---------------------------------------------------------------------

#'## Extract stream network
#'- the function wbt_extract_streams was used to extract stream network
#'- threshold is 1 = 1 km^2
wbt_extract_streams(flow_accum = "raster/epsg4326_n40w100_upa_clipped.tif",
                    output = "raster/epsg4326_mn_fmt_stream.tif",
                    threshold = 1) # Based on MERIT hydro, 1 indicates 1 km^2

#'## Snap pour point
#'- the function wbt_jenson_snap_pour_points rather than wbt_snap_pour_points
#'- This function snap the spoint to stream network and it wored better than snapping into catchment area
wbt_jenson_snap_pour_points(pour_pts = "vector/epsg4326_sites_snap.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "raster/epsg4326_mn_fmt_stream.tif",
                            output = "data_fmt/epsg4326_mn_fmt_sites_snap.shp",
                            snap_dist = 1)


#'## Unnested watershed delineation
#'- This function generate multiple rasterfiles, thus I have made different folders to save the file
wbt_unnest_basins(d8_pntr = "raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                  pour_pts= "vector/epsg4326_mn_fmt_sites_snap.shp", # GPKG file is not accepted. Used SHP file.
                  output = "wsraster/unnestedws.tif")


#'## Read result of delineated watershed raster files
#'- There should be better way to read multiple files at once. I will figure it out
wgs84_ws01 <- raster("wsraster/unnestedws_1.tif")
wgs84_ws02 <- raster("wsraster/unnestedws_2.tif")
wgs84_ws03 <- raster("wsraster/unnestedws_3.tif")
wgs84_ws04 <- raster("wsraster/unnestedws_4.tif")
wgs84_ws05 <- raster("wsraster/unnestedws_5.tif")
wgs84_ws06 <- raster("wsraster/unnestedws_6.tif")
wgs84_ws07 <- raster("wsraster/unnestedws_7.tif")
wgs84_ws08 <- raster("wsraster/unnestedws_8.tif")
wgs84_ws09 <- raster("wsraster/unnestedws_9.tif")
wgs84_ws10 <- raster("wsraster/unnestedws_10.tif")
wgs84_ws11 <- raster("wsraster/unnestedws_11.tif")
wgs84_ws12 <- raster("wsraster/unnestedws_12.tif")
wgs84_ws13 <- raster("wsraster/unnestedws_13.tif")
wgs84_ws14 <- raster("wsraster/unnestedws_14.tif")
wgs84_ws15 <- raster("wsraster/unnestedws_15.tif")
wgs84_ws16 <- raster("wsraster/unnestedws_16.tif")
wgs84_ws17 <- raster("wsraster/unnestedws_17.tif")
wgs84_ws18 <- raster("wsraster/unnestedws_18.tif")
wgs84_ws19 <- raster("wsraster/unnestedws_19.tif")
wgs84_ws20 <- raster("wsraster/unnestedws_20.tif")
wgs84_ws21 <- raster("wsraster/unnestedws_21.tif")
wgs84_ws22 <- raster("wsraster/unnestedws_22.tif")
wgs84_ws23 <- raster("wsraster/unnestedws_23.tif")
wgs84_ws24 <- raster("wsraster/unnestedws_24.tif")
wgs84_ws25 <- raster("wsraster/unnestedws_25.tif")
wgs84_ws26 <- raster("wsraster/unnestedws_26.tif")
wgs84_ws27 <- raster("wsraster/unnestedws_27.tif")
wgs84_ws28 <- raster("wsraster/unnestedws_28.tif")
wgs84_ws29 <- raster("wsraster/unnestedws_29.tif")
wgs84_ws30 <- raster("wsraster/unnestedws_30.tif")
wgs84_ws31 <- raster("wsraster/unnestedws_31.tif")
wgs84_ws32 <- raster("wsraster/unnestedws_32.tif")
wgs84_ws33 <- raster("wsraster/unnestedws_33.tif")
wgs84_ws34 <- raster("wsraster/unnestedws_34.tif")
wgs84_ws35 <- raster("wsraster/unnestedws_35.tif")


#'## Convert raster watershed to polygon shapefile
#'if error code appears then turn off raster package: "unable to find an inherited method for function 
#'# 'select' for signature '"sf""
wgs84_ws_shp01 <- st_as_stars(wgs84_ws01) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_1,geometry)
wgs84_ws_shp02 <- st_as_stars(wgs84_ws02) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_2,geometry)
wgs84_ws_shp03 <- st_as_stars(wgs84_ws03) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_3,geometry)
wgs84_ws_shp04 <- st_as_stars(wgs84_ws04) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_4,geometry)
wgs84_ws_shp05 <- st_as_stars(wgs84_ws05) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_5,geometry)
wgs84_ws_shp06 <- st_as_stars(wgs84_ws06) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_6,geometry)
wgs84_ws_shp07 <- st_as_stars(wgs84_ws07) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_7,geometry)
wgs84_ws_shp08 <- st_as_stars(wgs84_ws08) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_8,geometry)
wgs84_ws_shp09 <- st_as_stars(wgs84_ws09) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_9,geometry)
wgs84_ws_shp10 <- st_as_stars(wgs84_ws10) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_10,geometry)
wgs84_ws_shp11 <- st_as_stars(wgs84_ws11) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_11,geometry)
wgs84_ws_shp12 <- st_as_stars(wgs84_ws12) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_12,geometry)
wgs84_ws_shp13 <- st_as_stars(wgs84_ws13) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_13,geometry)
wgs84_ws_shp14 <- st_as_stars(wgs84_ws14) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_14,geometry)
wgs84_ws_shp15 <- st_as_stars(wgs84_ws15) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_15,geometry)
wgs84_ws_shp16 <- st_as_stars(wgs84_ws16) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_16,geometry)
wgs84_ws_shp17 <- st_as_stars(wgs84_ws17) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_17,geometry)
wgs84_ws_shp18 <- st_as_stars(wgs84_ws18) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_18,geometry)
wgs84_ws_shp19 <- st_as_stars(wgs84_ws19) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_19,geometry)
wgs84_ws_shp20 <- st_as_stars(wgs84_ws20) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_20,geometry)
wgs84_ws_shp21 <- st_as_stars(wgs84_ws21) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_21,geometry)
wgs84_ws_shp22 <- st_as_stars(wgs84_ws22) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_22,geometry)
wgs84_ws_shp23 <- st_as_stars(wgs84_ws23) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_23,geometry)
wgs84_ws_shp24 <- st_as_stars(wgs84_ws24) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_24,geometry)
wgs84_ws_shp25 <- st_as_stars(wgs84_ws25) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_25,geometry)
wgs84_ws_shp26 <- st_as_stars(wgs84_ws26) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_26,geometry)
wgs84_ws_shp27 <- st_as_stars(wgs84_ws27) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_27,geometry)
wgs84_ws_shp28 <- st_as_stars(wgs84_ws28) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_28,geometry)
wgs84_ws_shp29 <- st_as_stars(wgs84_ws29) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_29,geometry)
wgs84_ws_shp30 <- st_as_stars(wgs84_ws30) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_30,geometry)
wgs84_ws_shp31 <- st_as_stars(wgs84_ws31) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_31,geometry)
wgs84_ws_shp32 <- st_as_stars(wgs84_ws32) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_32,geometry)
wgs84_ws_shp33 <- st_as_stars(wgs84_ws33) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_33,geometry)
wgs84_ws_shp34 <- st_as_stars(wgs84_ws34) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_34,geometry)
wgs84_ws_shp35 <- st_as_stars(wgs84_ws35) %>% st_as_sf(merge = T) %>% select(unwsID = unnestedws_35,geometry)


#'## Combine shapefiles 
wgs84_ws_shp <- rbind(wgs84_ws_shp01, wgs84_ws_shp02, wgs84_ws_shp03, wgs84_ws_shp04, wgs84_ws_shp05,
                      wgs84_ws_shp06, wgs84_ws_shp07, wgs84_ws_shp08, wgs84_ws_shp09, wgs84_ws_shp10,
                      wgs84_ws_shp11, wgs84_ws_shp12, wgs84_ws_shp13, wgs84_ws_shp14, wgs84_ws_shp15,
                      wgs84_ws_shp16, wgs84_ws_shp17, wgs84_ws_shp18, wgs84_ws_shp19, wgs84_ws_shp20,
                      wgs84_ws_shp21, wgs84_ws_shp22, wgs84_ws_shp23, wgs84_ws_shp24, wgs84_ws_shp25,
                      wgs84_ws_shp26, wgs84_ws_shp27, wgs84_ws_shp28, wgs84_ws_shp29, wgs84_ws_shp30,
                      wgs84_ws_shp31, wgs84_ws_shp32, wgs84_ws_shp33, wgs84_ws_shp34, wgs84_ws_shp35)

st_write(wgs84_ws_shp,
         dsn = "data_fmt/wgs84_mn_fmt_watersheds.gpkg",
         append = FALSE)
