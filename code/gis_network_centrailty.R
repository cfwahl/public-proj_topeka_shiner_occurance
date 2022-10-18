
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


#sf_line <- list.files("data_fmt/vector",
#                      pattern = "connectivity_dummy_5km2.shp",
#                      full.names = T) %>% 
#  st_read()

## whole data frame of real occurrence + dummy data
#df_ws <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_5km2_fixed_geom.gpkg") 


# create sub-watersheds -----------------------------------------------------

ws1 <- subset(sf_line2, watershed=="1")
ws2 <- subset(sf_line2, watershed=="2") 
ws3 <- subset(sf_line2, watershed=="3") 
ws4 <- subset(sf_line2, watershed=="4") 
ws5 <- subset(sf_line2, watershed=="5") 
ws6 <- subset(sf_line2, watershed=="6") 
ws7 <- subset(sf_line2, watershed=="7") 

# insestion of watershed polygon with stream segments
#ws2_int <- st_intersection(sf_line, ws3) 

# network centrality test ---------------------------------------------------------


### st_touches

# examine if line segments touch, true/false
mat_touch <- st_touches(ws1) 

# change T/F to 1/0
mat_touch <- 1* mat_touch

g = graph.adjlist(mat_touch)
c = components(g)
table(c$membership)
ws1$groups = c$membership




### network with igraph
# source - https://r-spatial.org/r/2019/09/26/spatial-networks.html

# identify edges in the stream network
edges <- ws6 %>%
  mutate(edgeID = c(1:n())) # assign identifier


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
  mutate(degree = centrality_degree()) %>% # determine centrality degree for nodes
  mutate(eigen = centrality_eigen(weights = NULL)) %>% # eigenvector for nodes
  mutate(betweenness = centrality_betweenness(weights = length)) %>% # determine betweenness for nodes
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length)) # determine betweenness for edges


# plot graph of betweenness for nodes
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))

# plot graph of eigen and betweenness for nodes
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey50') + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = eigen, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))

# graph for edges
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0,4))



# centrality measurements --------------------------------------------------------

# eigen centrality, same as graph
eigen_centrality(graph, 
                 directed = FALSE, 
                 scale = TRUE, 
                 weights = NULL, 
                 options = arpack_defaults )

# extract only the centrality scores
eig  <- evcent(graph)$vector

# degree or number of edges connecting to a node
degree(graph, v = V(graph), 
       mode = c("all", "out", "in", "total"), 
       loops = TRUE, normalized = FALSE)

# produces same node scores as above
degree <- degree(graph)

# hubs, similar to eigen
hub <- hub.score(graph)$vector
authority <- authority.score(graph)$vector

# closeness  centrality indicates how close a node is to all other nodes in the network
closeness <- closeness(graph)

# betweenness centrality measures how often a node occurs on all 
# shortest paths between two nodes
betweenness <- betweenness(graph)

## Reach at k=2
reach_2 <- (ego_size(graph, 2)-1)/(vcount(graph)-1)

## Reach at k=3
reach_3 <- (ego_size(graph, 3)-1)/(vcount(graph)-1)

# compare different centrality scores
centralities <- cbind(degree, 
                      eig, 
                      hub,
                      authority,
                      closeness,
                      reach_2, 
                      reach_3, 
                      betweenness)

# examine how correlated the different degrees are to each other
round(cor(centralities), 2)



centr_eigen(graph,
  directed = FALSE,
  scale = TRUE,
  options = arpack_defaults,
  normalized = TRUE)

alpha_centrality(graph, 
                 nodes = V(graph), 
                 alpha = 1, 
                 loops = FALSE, 
                 exo = 1, 
                 weights = NULL, 
                 tol = 1e-07, 
                 sparse = TRUE )

