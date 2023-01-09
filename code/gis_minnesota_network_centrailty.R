
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 


# data --------------------------------------------------------------------

## stream polyline
sf_line2 <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_stream_connectivity.shp") %>%
  dplyr::select(-c(STRM_VA)) %>% # remove slope variable
  rename(connectivity = connectivi)

## oxbow point
sf_ox_point <- sf::st_read(dsn = "data_fmt/vector/epsg3722_minnesota_oxbow_snap.gpkg") %>%
  rename(oxbow_occurrence = occurrence) %>%
  dplyr::select(-c(distance, site, year, STRM_VAL)) # remove unneeded variables

## stream point
sf_stream_point <- sf::st_read(dsn = "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp") %>%
  rename(stream_occurrence = occurrence)


# network centrality eigenvector ---------------------------------------------------------

df_m <- lapply(X = 1:n_distinct(sf_line2$watershed),
       FUN = function(x) {
         df_subset <- sf_line2 %>% 
           filter(watershed == x)
         
         y <- df_subset %>% 
           st_touches() %>% 
           graph.adjlist() %>% 
           eigen_centrality()
         
         out <- df_subset %>% 
           mutate(eigen = y$vector) %>% 
           relocate(eigen)
         
        return(out)
       }) %>% 
  bind_rows()


# network centrality betweenness ---------------------------------------------------------

df_b <- lapply(X = 1:n_distinct(sf_line2$watershed),
               FUN = function(x) {
                 df_subset <- sf_line2 %>% 
                   filter(watershed == x)
                 
                 b <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   betweenness(normalized = TRUE)
                 
                 out <- df_subset %>% 
                   mutate(between = b) %>% 
                   relocate(between)
                 
                 return(out)
               }) %>% 
  bind_rows()


# network centrality closeness ---------------------------------------------------------

df_c <- lapply(X = 1:n_distinct(sf_line2$watershed),
               FUN = function(x) {
                 df_subset <- sf_line2 %>% 
                   filter(watershed == x)
                 
                 c <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   closeness(normalized = TRUE)
                 
                 out <- df_subset %>% 
                   mutate(closeness = c) %>% 
                   relocate(closeness)
                 
                 return(out)
               }) %>% 
  bind_rows()

# network centrality hub ---------------------------------------------------------

df_h <- lapply(X = 1:n_distinct(sf_line2$watershed),
               FUN = function(x) {
                 df_subset <- sf_line2 %>% 
                   filter(watershed == x)
                 
                 h <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   hub_score()
                 
                 out <- df_subset %>% 
                   mutate(hub = h$vector) %>% 
                   relocate(hub)
                 
                 return(out)
               }) %>% 
  bind_rows()


# network centrality authority ---------------------------------------------------------

df_a <- lapply(X = 1:n_distinct(sf_line2$watershed),
               FUN = function(x) {
                 df_subset <- sf_line2 %>% 
                   filter(watershed == x)
                 
                 a <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   authority_score()
                 
                 out <- df_subset %>% 
                   mutate(authority = a$vector) %>% 
                   relocate(authority)
                 
                 return(out)
               }) %>% 
  bind_rows()

#  join occurrence with centrality measures ---------------------------------------------

df_ox_cent <- sf_ox_point %>%
  as_tibble %>%
  left_join(as_tibble(df_m),
            by = c("line_id")) %>% 
  dplyr::select(-c(geometry, watershed.y, site0.y, connectivity.y, siteid.y)) %>%
  
  left_join(as_tibble(df_b),
            by = c("line_id")) %>%
  dplyr::select(-c(geometry, watershed.x, site0.x, connectivity.x, siteid.x)) %>%
  
  left_join(as_tibble(df_c),
            by = c("line_id")) %>%
  dplyr::select(-c(geometry, watershed.y, site0.y, connectivity.y, siteid.y)) %>%
  
  left_join(as_tibble(df_h),
            by = c("line_id")) %>%
  dplyr::select(-c(geometry, watershed.x, site0.x, connectivity.x, siteid.x)) %>%
  
  left_join(as_tibble(df_a),
            by = c("line_id")) %>%
  dplyr::select(-c(geometry, watershed.y, site0.y, connectivity.y, siteid.y)) %>%
  rename(siteid = siteid.x,
         connectivity = connectivity.x,
         site0 = site0.x,
         watershed = watershed.x)

df_strm_cent <- sf_stream_point %>%
  as_tibble %>%
  left_join(as_tibble(df_m),
            by = c("line_id")) %>%
  dplyr::select(-c(siteid.y, geometry.y)) %>%
  
  left_join(as_tibble(df_b),
            by = c("line_id")) %>%
  dplyr::select(-c(geometry, watershed.y, site0.y, connectivity.y, siteid.x)) %>%
  
  left_join(as_tibble(df_c),
            by = c("line_id")) %>%
  dplyr::select(-c(geometry, watershed.x, site0.x, connectivity.x, siteid.x)) %>%
  
  left_join(as_tibble(df_h),
            by = c("line_id")) %>%
  dplyr::select(-c(geometry, watershed.y, site0.y, connectivity.y, siteid.y)) %>%
  
  left_join(as_tibble(df_a),
            by = c("line_id")) %>%
  dplyr::select(-c(geometry.x, watershed.x, site0.x, connectivity.x, siteid.y)) %>%
  rename(siteid = siteid.x)


# visualize relationship ----------------------------------------------------------

### eigenvector
# MODEL df
ggplot(df_m,
       aes(x = eigen,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

# STREAM df
ggplot(df_strm_cent,
       aes(x = eigen,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

# OXBOW df
ggplot(df_ox_cent,
       aes(x = eigen,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

### betweenness
# MODEL df
ggplot(df_b,
       aes(x = between,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

# STREAM df
ggplot(df_strm_cent,
       aes(x = between,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

# OXBOW df
ggplot(df_ox_cent,
       aes(x = between,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()


### linear model
# betweenness MODEL
#ggplot(df_b,
#       aes(x = between,
#           y = connectivity))  +
#  geom_smooth(method = 'lm', se = TRUE) + 
#  geom_point()

#ggplot(df_strm_cent,
#       aes(x = between,
#           y = connectivity))  +
#  geom_smooth(method = 'lm', se = TRUE) + 
#  geom_point()

#ggplot(df_ox_cent,
#       aes(x = between,
#           y = connectivity))  +
#  geom_smooth(method = 'lm', se = TRUE) + 
#  geom_point()


### occurrence

# stream occurrence X eigenvector
ggplot(df_strm_cent,
       aes(x = eigen,
           y = stream_occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()

# oxbow occurrence X eigenvector
ggplot(df_ox_cent,
       aes(x = eigen,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()

# stream occurrence X betweenness
ggplot(df_strm_cent,
       aes(x = between,
           y = stream_occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()

# oxbow occurrence X betweenness
ggplot(df_ox_cent,
       aes(x = between,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


# glmm --------------------------------------------------------------------

### glmm (lme4) connectivity and eigenvector
mod_glmer_conn <- glmer(connectivity ~ eigen + (1|watershed),
                        data = df_m, family = Gamma(link = "log"))

summary(mod_glmer_conn)

## lmer
mod_lmer_conn <- lmer(connectivity ~ eigen + (1|watershed),
                      data = df_m)

summary(mod_lmer_conn)


### glmm (lme4) connectivity and betweenness
mod_glmer_conn <- glmer(connectivity ~ between + (1|watershed),
                        data = df_b, family = Gamma(link = "log"))

summary(mod_glmer_conn)

## lmer
mod_lmer_conn <- lmer(connectivity ~ between + (1|watershed),
                        data = df_b)

summary(mod_lmer_conn)



# maps --------------------------------------------------------------------

# map of eigen scores 
ggplot(df_m) + # base map of stream lines
  geom_sf(aes(color = eigen))+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Eigenvector") + # label legend 
  theme_minimal()

# map of betweenness scores 
ggplot(df_b) + # base map of stream lines
  geom_sf(aes(color = between))+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Betweenness") + # label legend 
  theme_minimal()

# map of closeness scores
#ggplot(df_c) + # base map of stream lines
#  geom_sf(aes(color = closeness))+ # heat map for connectivity 
#  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
#  labs(color = "Eigenvector") + # label legend 
#  theme_minimal()


# export data ------------------------------------------------------------------

# export stream network centrality scores
saveRDS(df_strm_cent, file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# export oxbow network centrality scores
saveRDS(df_ox_cent, file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")


# readRDS -----------------------------------------------------------------

# import RDS files for scores
# network centrality scores
#df_mn_strm_cent <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# import oxbow network centrality scores
#df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")
