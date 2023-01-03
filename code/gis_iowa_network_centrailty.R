

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

pacman::p_load(igraph,
               tidyverse,
               sf,
               foreach,
               lme4,
               lmerTest)

# data --------------------------------------------------------------------

## stream polyline
sf_line <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_str_net_5km2_dummy.shp") 

site_info <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_oxbow_lineid.shp") 

# network centrality test ---------------------------------------------------------

df_e <- lapply(X = 1:n_distinct(sf_line$watershed),
               FUN = function(x) {
                 df_subset <- sf_line %>% 
                   filter(watershed == x)
                 
                 y <- df_subset %>% 
                   st_touches() %>% 
                   graph.adjlist() %>% 
                   eigen_centrality()
                 
                 #weight <- df_subset$area %>% 
                 #scale(center = min(.), scale = max(. - min(.))) %>% # scales from 0-1
                 #c() # combine into matrix
                 #group_by(watershed) %>%
                 #summarise(area = max(area))
                 
                 #weight2 <- weight$area/df_subset$area 
                 
                 
                 #v_w_cent <- y * weight 
                 
                 out <- df_subset %>% 
                   #mutate(eigen = y) %>% 
                   mutate(eigen = y$vector) %>%
                   relocate(eigen)
                 
                 return(out)
               }) %>% 
  bind_rows()

### betweenness

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
  bind_rows()

#  join occurrence with eigen ---------------------------------------------

df_i <- site_info %>%
  as_tibble %>%
  left_join(as_tibble(df_e),
            by = c("line_id")) %>%
  left_join(as_tibble(df_b),
            by = c("line_id"))


# visualize relationship ----------------------------------------------------------


## eigenvector X connectivity
## linear model 
ggplot(df_i,
       aes(x = eigen,
           y = occurrence)) +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()

## betweenness X connectivity
## linear model 
ggplot(df_i,
       aes(x = between,
           y = occurrence)) +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()



# cleanup dataframe --------------------------------------------------------------

df_i <- df_i %>%
  select(, -c(fid.x, geometry.x, fid.y, geometry.y, area.y, watershed.y)) %>%
  rename(watershed = watershed.x,
         area = area.x)

# maps --------------------------------------------------------------------

# map of eigen scores from the sub-watersheds
ggplot(df_e) + # base map of stream lines
  geom_sf(aes(color = eigen))+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Eigenvector") + # label legend 
  theme_minimal()

# map of eigen scores from the sub-watersheds
ggplot(df_b) + # base map of stream lines
  geom_sf(aes(color = between))+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Betweenness") + # label legend 
  theme_minimal()

# export oxbow data------------------------------------------------------------------

# this will recall code in R script
saveRDS(df_i, file = "data_fmt/data_iowa_network_centrality.rds")

df_i <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds")
