
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
sf_line <- list.files("data_fmt/vector",
                      pattern = "connectivity_dummy_5km2.shp",
                      full.names = T) %>% 
  st_read()

# network centrality test ---------------------------------------------------------


### st_touches

# examine if line segments touch, true/false
#mat_touch <- st_touches(sf_line, sparse = FALSE) 

# change T/F to 1/0
#mat_touch <- 1* mat_touch


### network with igraph
# source - https://r-spatial.org/r/2019/09/26/spatial-networks.html

# identify edges in the stream network
edges <- sf_line %>%
  mutate(edgeID = c(1:n())) # assign identifer


# create nodes at start and end of each line segment
nodes <- edges %>%
  st_coordinates() %>% # get start and end location for each segment
  as_tibble() %>%
  rename(edgeID = L1) %>% # ID edges
  group_by(edgeID) %>% # group by edge index
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

# give each node a unique index
nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy) # some nodes/edges will have same coords so assign them the same index

# Combine the node indices with the edges
source_nodes <- nodes %>%
  filter(start_end == 'start') %>% # ID start node
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>% # ID end node
  pull(nodeID)

edges <- edges %>%
  mutate(from = source_nodes, to = target_nodes) # ID each edge with node starts or ends

# Remove duplicate nodes
nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>% # remove duplicates
  st_as_sf(coords = c('X', 'Y')) %>% # convert tibble to sf
  st_set_crs(st_crs(edges))

# Convert to tbl_graph
graph <- tbl_graph(nodes = nodes, edges = as_tibble(edges), 
                  directed = FALSE)

# put everything together into a single function
# this combines all the steps above into a single function
sf_to_tidygraph = function(x, directed = TRUE) {
  
  edges <- x %>%
    mutate(edgeID = c(1:n()))
  
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    select(-xy)
  
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  
}


# make graph
graph <- graph %>%
  activate(edges) %>% # manipulate edges 
  mutate(length = st_length(geometry))

# make into sf object
graph %>%
  activate(edges) %>% 
  as_tibble() %>%
  st_as_sf() %>% # turn edges back into sf object
  group_by(line_id) %>%
  summarise(length = sum(length))

# plot
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()) + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), size = 0.5)

# view interactive map
tmap_mode('view')

tm_shape(graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()) +
  tm_lines() +
  tm_shape(graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()) +
  tm_dots() +
  tmap_options(basemaps = 'OpenStreetMap')


# centrality measurements -------------------------------------------------

# describes it importance of nodes in the network

# betweenness centrality measurements
# specifies the number of shortest paths that pass through an edge
graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length))


# plot graph of betweenness for nodes
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))
