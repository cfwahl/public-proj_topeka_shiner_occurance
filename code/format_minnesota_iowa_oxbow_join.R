
# this script combines Minnesota and Iowa oxbow water quality and occurrence
# data

-----------------------------------------------------------------------------
  # data manipulation -------------------------------------------------


## whole data frame of real occurrence + dummy data
df_all <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_landuse_dummy_real.gpkg") %>%
  filter(!is.na(occurrence)) %>%
  as_tibble()

## stream sites (for the connectivity map)
df_stream_occ <- sf::st_read("data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp") %>%
  filter(!is.na(occurrence)) %>%
  st_transform(crs = 3722) %>%
  as_tibble()

df_join <- df_stream_occ %>%
  left_join(df_all, by = "siteid") %>%
  dplyr::select(-c(line_id.y, slope.y, occurrence.y, geom)) %>%
  rename(occurrence = occurrence.x,
         line_id = line_id.x,
         slope = slope.x)


# export ------------------------------------------------------------------

#st_write(df_join,
#         dsn = "data_fmt/vector/epsg3722_minnesota_stream_site_covariates.shp",
#         append = FALSE)




# data --------------------------------------------------------------------

## stream polyline
#sf_line <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_stream_network_5km2_dummy.shp") %>%
#  as_tibble() 

site_info <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_oxbow_lineid.shp") %>%
  as_tibble() %>%
  filter(hbtttyp == "Restored_Oxbow")

# read minnesota oxbow rds
df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds") %>%
  df_mn_ox_cent  
as_tibble() %>%
  dplyr::select(-c(X1, Y1))

# read iowa oxbow rds
df_iowa_oxbow <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds") %>%
  as_tibble() %>%
  dplyr::select(-c(fid, sampled, siteid, date, year, state,
                   stremnm, habitat, hbtttyp)) %>%
  rename(oxbow_occurrence = occurrence,
         temp = wtrtmp_) %>%
  mutate(oxbow_id = row_number()+142,
         watershed = watershed+7) %>%
  mutate(temp = as.numeric(temp),
         doprcnt = as.numeric(doprcnt),
         do_mgl = as.numeric(do_mgl),
         trbdty_ = as.numeric(trbdty_),
         cndctv_ = as.numeric(cndctv_),
         ph = as.numeric(ph))


df_mn_ia_oxbows_join <- df_iowa_oxbow %>%
  full_join(df_mn_ox_cent, by = "oxbow_id") %>%
  mutate(oxbow_occurrence = coalesce(oxbow_occurrence.x,oxbow_occurrence.y),
         do_mgl = coalesce(do_mgl.x, do_mgl.y),
         dp_percent = coalesce(doprcnt, dopercent),
         turbidity = coalesce(trbdty_, turb),
         ph = coalesce(ph.x, ph.y),
         temperature = coalesce(temp.x, temp.y),
         between = coalesce(between.x, between.y),
         eigen = coalesce(eigen.x, eigen.y),
         watershed = coalesce(watershed.x, watershed.y)) %>%
  dplyr::select(-c(temp.x, temp.y, doprcnt, do_mgl.x, do_mgl.y, trbdty_, turb,
                   ph.x, ph.y, oxbow_occurrence.x, oxbow_occurrence.y, dopercent, 
                   line_id.x, line_id.y, watershed.x, watershed.y, oxbowid, 
                   siteid, site0, eigen.x, eigen.y, between.x, between.y, closeness,
                   hub, authority, geometry, geom))


# visualize ---------------------------------------------------------------


# MN + IA oxbow betweenness x occurrence
ggplot(df_mn_ia_oxbows_join,
       aes(x = between,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

# MN + IA oxbow eigenvector x occurrence
ggplot(df_mn_ia_oxbows_join,
       aes(x = eigen,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()



# analyze -----------------------------------------------------------------


# betweenness and join oxbow_occurrence
glmer_strm_between_occ <- glmer(oxbow_occurrence ~ between + scale(do_mgl) + 
                                  scale(turbidity) + scale(dp_percent) + scale(ph) +
                                  scale(temperature) +  (1|watershed),
                                data = df_mn_ia_oxbows_join, family = "binomial")


summary(glmer_strm_between_occ)



