
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

df_mn_ox_cent <- sf_ox_point %>%
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

df_mn_strm_cent <- sf_stream_point %>%
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
# stream connectivity x eigen
ggplot(df_m,
       aes(x = eigen,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

# oxbow connectivity x eigen
ggplot(df_mn_ox_cent,
       aes(x = eigen,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

### betweenness
# stream connectivity x betweenness 
ggplot(df_b,
       aes(x = between,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()

# oxbow connectivity x betweenness
ggplot(df_mn_ox_cent,
       aes(x = between,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log'))) + 
  geom_point()


### occurrence plots
# stream eigenvector x occurrence
ggplot(df_mn_strm_cent,
       aes(x = eigen,
           y = stream_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

# stream betweenness x occurrence
ggplot(df_mn_strm_cent,
       aes(x = between,
           y = stream_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

# oxbow eigenvector x occurrence
ggplot(df_mn_ox_cent,
       aes(x = eigen,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

# oxbow betweenness x occurrence
ggplot(df_mn_ox_cent,
       aes(x = between,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()


# glmm and lmm --------------------------------------------------------------------

# connectivity x eigenvector

### stream connectivity and eigenvector
glmer_str_eigen_conn <- glmer(connectivity ~ eigen + (1|watershed),
                        data = df_m, family = Gamma(link = "log"))

summary(glmer_str_eigen_conn)

### oxbow connectivity and eigenvector
glmer_ox_eigen_conn <- glmer(connectivity ~ eigen + (1|watershed),
                         data = df_mn_ox_cent, family = Gamma(link = "log"))

summary(glmer_ox_eigen_conn)


### eigen occurrence

# connectivity and stream_occurrence
glmer_strm_occ_conn <- glmer(stream_occurrence ~ connectivity + (1|watershed),
                         data = df_mn_strm_cent, family = "binomial")

summary(glmer_strm_occ_conn)

# eigenvector and stream_occurrence
glmer_strm_occ_eigen <- glmer(stream_occurrence ~ eigen + (1|watershed),
                           data = df_mn_strm_cent, family = "binomial")

summary(glmer_strm_occ_eigen)

# connectivity and oxbow_occurrence
glmer_ox_occ_conn <- glmer(oxbow_occurrence ~ connectivity + (1|watershed),
                           data = df_mn_ox_cent, family = "binomial")

summary(glmer_ox_occ_conn)

# eigenvector and oxbow_occurrence
glmer_ox_occ_conn <- glmer(oxbow_occurrence ~ eigen + (1|watershed),
                            data = df_mn_ox_cent, family = "binomial")

summary(glmer_ox_occ_conn)


### connectivity and betweenness

### stream connectivity and betweenness
glmer_strm_between_conn <- glmer(connectivity ~ between + (1|watershed),
                           data = df_b, family = Gamma(link = "log"))

summary(glmer_strm_between_conn)

### oxbow connectivity and betweenness
glmer_ox_between_conn <- glmer(connectivity ~ between + (1|watershed),
                           data = df_mn_ox_cent, family = Gamma(link = "log"))

summary(glmer_ox_between_conn)


### betweenness occurrence

# betweenness and stream_occurrence
glmer_strm_between_occ <- glmer(stream_occurrence ~ between + (1|watershed),
                           data = df_mn_strm_cent, family = "binomial")

summary(glmer_strm_between_occ)

# betweenness and oxbow_occurrence
glmer_ox_between_occ <- glmer(oxbow_occurrence ~ between + (1|watershed),
                           data = df_mn_ox_cent, family = "binomial")

summary(glmer_ox_between_occ)


# correlation  ------------------------------------------------------------

# correlation between stream connectivity and eigenvector centrality
corr <- cor.test(x=df_mn_strm_cent$connectivity, y=df_mn_strm_cent$eigen, 
                 method = 'spearman')
corr

# correlation between stream connectivity and betweenness 
corr2 <- cor.test(x=df_mn_strm_cent$connectivity, y=df_mn_strm_cent$between, 
                  method = 'spearman')
corr2

# correlation between stream connectivity and eigenvector centrality
corr3 <- cor.test(x=df_mn_ox_cent$connectivity, y=df_mn_ox_cent$eigen,
                  method = 'spearman')
corr3

# correlation between stream connectivity and betweenness 
corr4 <- cor.test(x=df_mn_ox_cent$connectivity, y=df_mn_ox_cent$between, 
                  method = 'spearman')
corr4

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


# export data ------------------------------------------------------------------

# export stream network centrality scores
#saveRDS(df_mn_strm_cent, file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# export oxbow network centrality scores
#saveRDS(df_mn_ox_cent, file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")


# readRDS -----------------------------------------------------------------

# import RDS files for scores
# network centrality scores
#df_mn_strm_cent <- readRDS(file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# import oxbow network centrality scores
#df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")
