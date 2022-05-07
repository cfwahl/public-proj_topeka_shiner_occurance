# setup -------------------------------------------------------------------

pacman::p_load(riverdist,
               sf, 
               tidyverse)

rm(list = ls())

# data --------------------------------------------------------------------

# read sites
utm_sf_wsd <- st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse.gpkg")

wgs_sf_outlet <- st_read(dsn = "data_fmt/vector",
                         layer = "epsg4326_mn_fmt_sites_snap",
                         drivers = "ESRI Shapefile") %>% 
  filter(site %in% utm_sf_wsd$site) # select sites with watershed delineation

utm_sf_outlet <- st_transform(wgs_sf_outlet, crs = 3722)

# data frame for sampling sites
df_coord <- utm_sf_outlet %>% 
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>% 
  as_tibble()

X <- df_coord$X
Y <- df_coord$Y

# read in the network
strnet <- line2network(path = "data_fmt/vector", 
                       layer = "epsg3722_strnet")


# prep stream network and sites --------------------------------------------------

# # plot of all the segments
# plot(strnet)  
# 
# # check topology (nodes)
# topologydots(strnet)
# 
# # do a clean up: dissolve - y, insert vertices - y, distance - 1, 
# # examine figure for mouth questions, remove additional segments - n, 
# # build segment routes - y
# strnet_fixed <- cleanup(rivers = strnet)
# 
# # save file if you do not want to re-run cleanup()
# save(strnet_fixed, file = "data_fmt/strnet_fixed.RData")
load(file = "data_fmt/strnet_fixed.RData")

# snap sites to stream network
site_snap <- xy2segvert(x = X,
                        y = Y,
                        rivers = strnet_fixed)


# distance matrix  --------------------------------------------------------

# distance matrix: total, m_x
m_x <- riverdistancemat(site_snap$seg,
                        site_snap$vert,
                        strnet_fixed, 
                        ID = site_snap$segment) %>%
  data.matrix()

m_x <- round(m_x / 1000, 2) # convert to km


# distance matrix: net upstream, m_y
m_y <- upstreammat(seg = site_snap$seg,
                   vert = site_snap$vert, 
                   rivers = strnet_fixed, 
                   ID = site_snap$segment,
                   net = TRUE) %>%
  data.matrix()

m_y <- round(m_y / 1000, 2) # convert to km

# distance matrix: up and downstream distance (m_u & m_d)
m_u <- (m_x + m_y) / 2
m_d <- (m_x - m_y) / 2

m_td <- round(m_u + m_d, 2)

# check if m_td = m_x
identical(m_td, m_x)

# export
save(m_u, m_d, file = "data_fmt/distance_matrix.RData")
