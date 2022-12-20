# network centrality ------------------------------------------------------

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

df_i <- lapply(X = 1:n_distinct(sf_line$watershed),
               FUN = function(x) {
                 df_subset <- sf_line %>% 
                   filter(watershed == 1)
                 
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
                   betweenness(normalized = TRUE,
                               nobigint = TRUE)
                 
                 #weight <- df_subset
                 $area %>% 
                   #scale(center = min(.), scale = max(. - min(.))) %>% # scales from 0-1
                   #c() # combine into matrix
                   #group_by(watershed) %>%
                   #summarise(area = max(area))
                   
                   #weight2 <- weight$area/df_subset$area 
                   
                   
                   #v_w_cent <- y * weight2 
                   
                   out <- df_subset %>% 
                   mutate(between = y) %>% 
                   relocate(between)
                 
                 return(out)
               }) %>% 
  bind_rows()

#  join occurrence with eigen ---------------------------------------------

df_e <- site_info %>%
  as_tibble %>%
  left_join(as_tibble(df_i),
            by = c("line_id"))


# visualize relationship ----------------------------------------------------------


## eigenvector X connectivity
## linear model 
ggplot(ws4,
       aes(x = eigen,
           y = occurrence)) +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


# maps --------------------------------------------------------------------

# map of network centrality scores from the  subwatersheds
ggplot(df_b) + # base map of stream lines
  geom_sf(aes(color = between))+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Eigenvector") + # label legend 
  theme_minimal()
