
# this script is random code for testing script

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# import RDS files for scores
df_mn_strm_cent <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

## stream polyline
sf_line2 <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_connectivity.shp") %>%
  dplyr::select(-c(STRM_VA)) %>% # remove slope variable
  rename(connectivity = connectivi)

# segment length ----------------------------------------------------------

# determine length of stream segments
l <- st_length(sf_line2)

# add column
sf_line2 <- sf_line2 %>% 
  add_column(l = l)

# new cloumn with "[m]" removed  
sf_line2$seg_length <- stringr::str_remove(sf_line2$l, '\\[m]')

# remove original l column 
sf_line2 <- sf_line2 %>%
  dplyr::select(-c(l))

# segment length ----------------------------------------------------------

# connectivity and segment length
ggplot(sf_line2,
       aes(x = connectivity,
           y = seg_length))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(gaussian(link = 'log'))) + 
  geom_point()

######## stream betweenness x occurrence
ggplot(df_mn_strm_cent,
       aes(x = seg_length,
           y = stream_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()



# combine oxbow data ------------------------------------------------------


# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libaries
source(here::here("code/library.R")) 


# data --------------------------------------------------------------------

## stream polyline
sf_line <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_stream_network_5km2_dummy.shp") 

site_info <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_oxbow_lineid.shp") %>%
  dplyr::select(-c(sampled, siteid, date, year, state, stremnm, habitat, fid)) %>%
  rename(temp = wtrtmp_,
         dopercent = doprcnt,
         turb = trbdty_,
         cond = cndctv_,
         habitat = hbtttyp) %>%
  mutate(temp = as.numeric(temp),
         dopercent = as.numeric(dopercent),
         turb = as.numeric(turb),
         cond = as.numeric(cond),
         do_mgl = as.numeric(do_mgl),
         ph = as.numeric(ph))

# import oxbow data
df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds") %>%
  rename(oxbowid = oxbow_id) %>%
  as_tibble() %>%
  dplyr::select(-c(site0, siteid, connectivity))


# network centrality test ---------------------------------------------------------

# betweenness
df_b <- lapply(X = 1:n_distinct(sf_line$watershed),
               FUN = function(x) {
                 df_subset <- sf_line %>% 
                   filter(watershed == x)
                 
                 y <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   betweenness(normalized = TRUE)
                 
                 out <- df_subset %>% 
                   mutate(between = y) %>% 
                   relocate(between)
                 
                 return(out)
               }) %>% 
  bind_rows() %>%
  as_tibble()

#  join occurrence with eigen ---------------------------------------------

df_i <-  merge(x = site_info, y = df_b[ , c("line_id", "between")], 
               by = "line_id", all.x=TRUE)

df_i <-  merge(x = df_i, y = df_b[ , c("line_id", "watershed")], 
        by = "line_id", all.x=TRUE) %>%
  rename(oxbow_occurrence = occurrence) %>%
  dplyr::select(-c(habitat, cond)) %>%
  mutate(oxbowid = row_number()+142,
         watershed = watershed) %>%
  as_tibble()

df_combined <- rbind(df_i, df_mn_ox_cent)


# glmm --------------------------------------------------------------------

### iowa oxbow occurrence and betweenness 
glmer_str_eigen_conn <- glmer(oxbow_occurrence ~ between + (1|watershed) +
                                scale(ph) + scale(dopercent),
                            data = df_combined, family = "binomial")

summary(glmer_str_eigen_conn)

# betweenness X oxbow occurrence
ggplot(df_combined,
       aes(x = between,
           y = oxbow_occurrence)) +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()



# clip attempt ------------------------------------------------------------


# this code reduces the stream network to just the stream reaches within our
# range of sampling sites

# setup -------------------------------------------------------------------

# load libraries
source(here::here("code/library.R")) 

# clean objects
rm(list = ls())


# data --------------------------------------------------------------------

sf_line <- st_read("data_fmt/vector/old/epsg4326_mn_str_slope_5km2.shp") 

sf_poly <- st_read("data_fmt/vector/old/epsg3722_site_range.shp") %>%
  st_transform(crs = 4326)

plot(sf_poly)


# crop --------------------------------------------------------------------

output <-  st_intersection(sf_line, sf_poly)


cropped <- st_crop(sf_line$geometry, sf_poly$geometry)
plot(cropped)


