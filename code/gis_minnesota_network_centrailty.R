
# this code extracts the network centrality measure (betweenness) for Minnesota 
# stream networks, and creates data frames for figures and glmm analyses

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

sf_ox_point <- readRDS(file = "data_fmt/data_minnesota_oxbow_occur_connect.RDS") %>%
  rename(oxbow_occurrence = occurrence) %>%
  dplyr::select(-c(distance, site, year, STRM_VA))

sf_stream_point <- readRDS(file = "data_fmt/data_minnesota_stream_occur_connect.RDS") %>%
  rename(stream_occurrence = occurrence)

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
  bind_rows() %>%
  as_tibble()

#  join occurrence with centrality measures ---------------------------------------------

# add betweenness scores column to oxbow data
df_mn_ox_cent <-  merge(x = sf_ox_point, y = df_b[ , c("line_id", "between")], 
        by = "line_id", all.x=TRUE) %>%
  dplyr::select(-c(X1, Y1))

# add betweenness scores column to stream data
df_mn_strm_cent <-  merge(x = sf_stream_point, y = df_b[ , c("line_id", "between")], 
                        by = "line_id", all.x=TRUE)

# export data ------------------------------------------------------------------

# export stream network centrality scores
saveRDS(df_mn_strm_cent, file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# export oxbow network centrality scores
saveRDS(df_mn_ox_cent, file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")
