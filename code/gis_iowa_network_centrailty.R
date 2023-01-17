

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libaries
source(here::here("code/library.R")) 


# data --------------------------------------------------------------------

## stream polyline
sf_line <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_stream_network_5km2_dummy.shp") 

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
  dplyr::select(-c(fid.y, geometry.y)) %>%
  left_join(as_tibble(df_b),
            by = c("line_id")) %>%
  dplyr::select(-c(area.y, watershed.y, fid.x, geometry)) %>%
  rename(geometry = geometry.x,
         area = area.x,
         watershed = watershed.x)


# visualize relationship ----------------------------------------------------------


## eigenvector X oxbow occurrence
## linear model 
ggplot(df_i,
       aes(x = eigen,
           y = occurrence)) +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()

## betweenness X oxbow occurrence
## linear model 
ggplot(df_i,
       aes(x = between,
           y = occurrence)) +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()


# glmm --------------------------------------------------------------------

# connectivity x eigenvector

### iowa oxbow occurrence and eigenvector
glmer_str_eigen_conn <- glm(occurrence ~ eigen ,
                              data = df_i, family = "binomial")

summary(glmer_str_eigen_conn)

### iowa oxbow occurrence and betweenness 
glmer_str_eigen_conn <- glm(occurrence ~ between,
                              data = df_i, family = "binomial")

summary(glmer_str_eigen_conn)

# maps --------------------------------------------------------------------

## snap function
st_snap_points = function(f1, f2) {
  
  pacman::p_load(tidyverse,
                 sf)
  
  if (inherits(f1, "sf")) n = nrow(f1)
  if (inherits(f1, "sfc")) n = length(f1)
  
  out = foreach(i = seq_len(n), .combine = bind_rows) %do% {
    
    nrst = sf::st_nearest_points(st_geometry(f1)[i], f2)
    nrst_len = sf::st_length(nrst)
    nrst_mn = which.min(nrst_len)
    
    df0 <- sf::st_cast(nrst[nrst_mn], "POINT")[2] %>%
      sf::st_coordinates() %>% 
      dplyr::as_tibble() %>% 
      dplyr::mutate(X1 = st_coordinates(f1[i,])[1],
                    Y1 = st_coordinates(f1[i,])[2],
                    distance = nrst_len[nrst_mn],
                    f1[i,]) %>% 
      dplyr::rename(X2 = X,
                    Y2 = Y) %>% 
      dplyr::select(-geometry)
    
    return(df0)
  }
  
  return(out)
}


### snapping code for oxbows

## snap oxbows to stream line, original output will be `tibble`
df_oxbow <- st_snap_points(site_info, sf_line)


## convert it to sf
sf_point_snapped <- df_oxbow %>% 
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(site_info)) # ensure define CRS again

## to link to line feature, use st_join()
df_oxbow_snap <- st_join(x = sf_point_snapped,
                         y = sf_line,
                         join = st_is_within_distance,
                         dist = 10) %>% # join two features, then make as tibble data frame
  as_tibble()


### occurrence present (1) only
# convert to sf
sf_oxbow_1 <- df_oxbow %>% 
  filter(occurrence == "1") %>%
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(site_info)) # ensure define CRS again

### occurrence absent (0) only
# convert it to sf
sf_oxbow_0 <- df_oxbow %>% 
  filter(occurrence == "0") %>%
  st_as_sf(coords = c("X2", "Y2")) %>% # use snapped coordinates
  st_set_crs(st_crs(site_info)) # ensure define CRS again


# map of eigen scores from the sub-watersheds
ggplot(df_e) + # base map of stream lines
  geom_sf(aes(color = eigen))+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Eigenvector") + # label legend 
  theme_minimal() +
  geom_point(data = sf_oxbow_0, aes(x = X1, y = Y1), # stream sites (absent)
             shape = 16, size = 1, color = 'red') + # define point shape and color
  geom_point(data = sf_oxbow_1, aes(x = X1, y = Y1), # stream sites (present)
             shape = 16, size = 1, color = 'green') + # define point shape and color
  xlab("") + ylab("")           

# map of eigen scores from the sub-watersheds
ggplot(df_b) + # base map of stream lines
  geom_sf(aes(color = between))+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Betweenness") + # label legend 
  theme_minimal() + 
  geom_point(data = sf_oxbow_0, aes(x = X1, y = Y1), # stream sites (absent)
             shape = 16, size = 1, color = 'red') + # define point shape and color
  geom_point(data = sf_oxbow_1, aes(x = X1, y = Y1), # stream sites (present)
             shape = 16, size = 1, color = 'green') + # define point shape and color
  xlab("") + ylab("")

# export oxbow data------------------------------------------------------------------

# this will recall code in R script
saveRDS(df_i, file = "data_fmt/data_iowa_network_centrality.rds")

# readRDS -----------------------------------------------------------------

#df_i <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds")

