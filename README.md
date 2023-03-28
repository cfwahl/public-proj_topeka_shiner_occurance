# public-proj_topeka_shiner_occurance

Here is the order in which to run the analysis, and a brief description of the code content and additional info for manual steps.

Coding order
format_stream_oxbow_data*# > gis_stream_network*#^ > gis_watershed_ID*  > format_dummy_real_sites*# > gis_watershed_delineation > gis_landuse > gis_distance_matrix_riverdist@ > run_occupancy_up_bias > gis_stream_connectivity > format_occurrence_connectivity# > gis_network_centrailty*# > format_segment_length  > format_mn_ia_oxbow_join

* = Minnesota and Iowa in the same file
# = Stream and oxbows in the same file
^ = Manually moved sites because 1) DEM streamline was off for six stream sites (siteid = 160-164, 167), 2) sites were exactly the same location with dummy site thus not producing a watershed (siteid = 19, 242, 357). File with manually moved points is “data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap2.shp"
@ = used a stream network file that manually removed unnecessary streams outside of study network. This allows for faster computation when estimating distance among sites. File is "data_fmt/vector/epsg3722_minnesota_stream_dispersal_5km2.shp"

Figures
figure_bayes_caterpillar > figure_connectivity_map > figure_stream_occurrence_connectivity > figure_stream_occurrence_drainage_area > figure_mn_oxbow_occurrence_connectivity >   figure_mn_betweenness_map > figure_betweenness_connectivity > figure_stream_occurrence_betweenness > figure_mn_oxbow_occurrence_betweenness > figure_ia_betweenness_map > figure_ia_oxbow_occurrence_betweenness > figure_oxbow_occurrence_betweenness

Shapefiles
data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites.shp
data_fmt/vector/epsg4326_minnesota_oxbow_sites.shp
data_fmt/vector/epsg4326_iowa_oxbow_sites.shp
data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap.shp
data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap2.shp
data_fmt/vector/epsg4326_mn_dnr_fws_fmt_sites_snap3.shp
data_fmt/vector/epsg4326_mn_str_slope_5km2.shp
data_fmt/vector/epsg4326_iowa_oxbow_snap.shp
data_fmt/vector/epsg4326_ia_str_network_5km2.shp
data_fmt/vector/epsg4326_minnesota_watershed_pour_points.shp
data_fmt/vector/epsg4326_minnesota_watershed_points_snap.shp
data_fmt/vector/epsg4326_iowa_watershed_pour_points.shp
data_fmt/vector/epsg4326_iowa_watershed_points_snap.shp
data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp

RDS files
data_fmt/data_minnesota_fmt_oxbows.rds
data_fmt/data_iowa_fmt_owbows.rds
data_fmt/data_minnesota_stream_network_5km2.rds
data_fmt/data_iowa_stream_network_5km2.rds
data_fmt/data_minnesota_stream_dummy_real_occurrence.rds
data_fmt/data_iowa_oxbow_lineid.rds
data_fmt/data_minnesota_stream_watersheds_dummy_real.rds
data_fmt/data_minnesota_stream_landuse_dummy_real.rds
data_fmt/riverdist_minnesota_stream_network.rds
data_fmt/data_minnesota_distance_matrix_dummy_real.rds
data_fmt/data_minnesota_stream_connectivity.rds
data_fmt/data_minnesota_stream_occur_connect.rds
data_fmt/data_minnesota_oxbow_occur_connect.rds
data_fmt/data_minnesota_stream_network_centrality.rds
data_fmt/data_minnesota_stream_betweenness.rds
data_fmt/data_minnesota_oxbow_network_centrality.rds
data_fmt/data_iowa_network_centrality.rds
data_fmt/data_iowa_stream_betweenness.rds
data_fmt/data_ia_mn_oxbow_join.rds
output/mcmc_summary_up_full.rds
output/post_summary_up_full.rds

