
# This script creates stream network from raster files and snaps stream sampling
# locations to the channel

# setup -------------------------------------------------------------------

# load libraries
source(here::here("code/library.R")) 

# clean objects
rm(list = ls())

# DEM setup ---------------------------------------------------------------

##  Read flow direction raster
wgs84_dir <- raster("data_fmt/raster/epsg4326_n40w100_dir_clipped.tif")

##  convert arc format to d8 format
source("code/function_arc2d8.R")
wgs84_dir_d8 <- arc2d8(wgs84_dir)

## produce D8 raster file
writeRaster(wgs84_dir_d8,
            "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
            options = c('TFW = YES'),
            overwrite = TRUE)

# gis extract stream network ---------------------------------------------------------------------

# extract stream network from rasters
wbt_extract_streams(flow_accum = "data_fmt/raster/epsg4326_n40w100_upa_clipped.tif",
                    output = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                    threshold = 5) # Based on MERIT hydro, 1 = 1 km^2

##  Snap pour point
# snap stream sampling locations to stream network
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                            output = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap.shp",
                            snap_dist = 1)

# manually moved locations not properly snaped and re-did snapping to stream network
wbt_jenson_snap_pour_points(pour_pts = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap.shp", # GPKG file is not accepted. Used SHP file.
                            streams = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                            output = "data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap2.shp",
                            snap_dist = 1)

# Assigns a unique identifier to each link in a stream network
wbt_stream_link_identifier(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                           streams = "data_fmt/raster/epsg4326_mn_fmt_stream_5km2.tif",
                           output = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif")

# determine continuous slope for each stream segment
wbt_stream_slope_continuous(d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif", 
                            streams = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif", 
                            dem = "data_fmt/raster/epsg4326_n40w100_elv_clipped.tif",
                            output = "data_fmt/raster/epsg4326_mn_fmt_stream_slope_cont_5km2.tif")

# coverts continuous slope into mean for each segment 
wbt_zonal_statistics(input = "data_fmt/raster/epsg4326_mn_fmt_stream_slope_cont_5km2.tif",
                     features = "data_fmt/raster/epsg4326_mn_fmt_stream_id_5km2.tif",
                     output = "data_fmt/raster/epsg4326_mn_fmt_stream_link_slope_5km2.tif",
                     stat = "mean")

# Converts link slope raster into a vector file
wbt_raster_streams_to_vector(streams = "data_fmt/raster/epsg4326_mn_fmt_stream_link_slope_5km2.tif", 
                             d8_pntr = "data_fmt/raster/epsg4326_mn_fmt_flow_dir_clip_reclass.tif",
                             output = "data_fmt/vector/epsg4326_mn_str_slope_5km2.shp")

