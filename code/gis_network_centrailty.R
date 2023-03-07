
# this code extracts the network centrality measure (betweenness) for Minnesota 
# and Iowa stream networks and creates data frames for figures and glmm analyses

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# MINNESOTA ---------------------------------------------------------------
# data --------------------------------------------------------------------

# stream polyline
sf_line2 <- readRDS(file = "data_fmt/data_minnesota_stream_connectivity.rds") %>%
  dplyr::select(-c(STRM_VAL)) # remove slope variable

# read oxbows with connectivity value  
sf_ox_point <- readRDS(file = "data_fmt/data_minnesota_oxbow_occur_connect.RDS") %>%
  rename(oxbow_occurrence = occurrence) %>%
  dplyr::select(-c(distance, site, year, STRM_VA))

# read stream sites with connectivity value
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
  dplyr::select(-c(X1, Y1)) %>%
  rename(watershed = watrshd)

# add betweenness scores column to stream data
df_mn_strm_cent <-  merge(x = sf_stream_point, y = df_b[ , c("line_id", "between")], 
                        by = "line_id", all.x=TRUE) %>%
  rename(watershed = watrshd)

# export data ------------------------------------------------------------------

# export stream network centrality scores
saveRDS(df_mn_strm_cent, file = "data_fmt/data_minnesota_stream_network_centrality.rds")

# export stream network with betweenness scores
saveRDS(df_b, file = "data_fmt/data_minnesota_stream_betweenness.rds")

# export oxbow network centrality scores
saveRDS(df_mn_ox_cent, file = "data_fmt/data_minnesota_oxbow_network_centrality.rds")

# IOWA --------------------------------------------------------------------
# data --------------------------------------------------------------------

# stream polyline
sf_line <- readRDS(file = "data_fmt/data_iowa_stream_network_5km2.rds")

# read oxbow sites with line_id
site_info <- readRDS("data_fmt/data_iowa_oxbow_lineid.rds") %>%
  dplyr::select(-c(sampled, siteid, date, year, state, stremnm, habitat, STRM_VAL,
                   occrrnc)) %>%
  rename(temp = wtrtmp_,
         dopercent = doprcnt,
         turb = trbdty_,
         cond = cndctv_,
         habitat = hbtttyp) %>%
  mutate(temp = as.numeric(temp),
         dopercent = as.numeric(dopercent),
         turb = as.numeric(turb),
         cond = as.numeric(cond),
         do_mgl = as.numeric(do_mgl),
         ph = as.numeric(ph)) 

# network centrality ---------------------------------------------------------

# betweenness
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
  bind_rows() %>%
  as_tibble()

#  join occurrence with eigen ---------------------------------------------

df_i <- site_info %>% 
  as_tibble %>%
  left_join(df_b,
            by = "line_id") %>%
  dplyr::select(-c(geometry.y)) %>%
  rename(geometry = geometry.x,
         oxbow_occurrence = occurrence)

# export  -----------------------------------------------------------------

# export stream network centrality scores
saveRDS(df_i, file = "data_fmt/data_iowa_network_centrality.rds")
