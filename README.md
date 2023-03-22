# public-proj_topeka_shiner_occurance

Here is the order in which to run the analysis, and a brief description of the code content and additional info for 

Coding order
format_stream_oxbow_data*# > gis_stream_network*#^ > gis_watershed_ID*  > format_dummy_real_sites*# > gis_watershed_delineation > gis_landuse > gis_distance_matrix_riverdist@ > run_occupancy_up_bias > gis_stream_connectivity > format_occurrence_connectivity# > gis_network_centrailty*# > format_segment_length  > format_mn_ia_oxbow_join

* = Minnesota and Iowa in the same file
# = Stream and oxbows in the same file
^ = Manually moved sites because 1) DEM streamline was off for six stream sites (siteid = 160-164, 167), 2) sites were exactly the same location with dummy site thus not producing a watershed (siteid = 19, 242). File with manually moved points is “data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap2.shp"
@ = used a stream network file that manually removed unnecessary streams outside of study network. This allows for faster computation when estimating distance among sites. File is "data_fmt/vector/epsg3722_minnesota_stream_dispersal_5km2.shp"

 
