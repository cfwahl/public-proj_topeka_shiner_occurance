
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

pacman::p_load(igraph,
               tidyverse,
               tidygraph,
               sf,
               foreach,
               lme4,
               lmerTest)

# data --------------------------------------------------------------------

## stream polyline
sf_line2 <- sf::st_read(dsn = "data_fmt/vector/epsg4326_iowa_str_net_5km2_dummy.shp") 

## example watershed
df_subset <- sf_line2 %>% 
  filter(watershed == 1)

## graph object
y <- df_subset %>% 
  st_touches() %>% 
  graph.adjlist()

## eigen vector
v_cent <- y %>% 
  eigen_centrality(directed = FALSE)

## assign weight
## NOTE! - runif() returns random values bw 0-1. replace the code with the variable representing stream size
## also, make sure the order of weight values is compatible with the order of eigen vectors!
## scale(center = min(.), scale = max(. - min(.))...this code normalizes the weight to be 0-1
weight <- df_subset$area %>% 
  scale(center = min(.), scale = max(. - min(.))) %>% 
  c()

## multiply weight
v_w_cent <- v_cent$vector * weight
