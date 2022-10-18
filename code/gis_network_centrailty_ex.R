
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

pacman::p_load(igraph,
               tidygraph, 
               tidyverse,
               tmap,
               sf,
               mapview, 
               foreach)

# data --------------------------------------------------------------------

## stream polyline
sf_line2 <- sf::st_read(dsn = "data_fmt/vector/epsg3722_stream_connectivity.shp") 


# network centrality test ---------------------------------------------------------

df_m <- lapply(X = 1:n_distinct(sf_line2$watershed),
       FUN = function(x) {
         df_subset <- sf_line2 %>% 
           filter(watershed == x)
         
         y <- df_subset %>% 
           st_touches() %>% 
           graph.adjlist() %>% 
           eigen_centrality(directed = FALSE)
         
         out <- df_subset %>% 
           mutate(eigen = y$vector) %>% 
           relocate(eigen)
         
         return(out)
       }) %>% 
  bind_rows()

