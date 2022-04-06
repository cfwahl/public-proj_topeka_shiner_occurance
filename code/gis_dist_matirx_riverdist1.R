
# setup -------------------------------------------------------------------

pacman::p_load(riverdist,
               sf, 
               tidyverse)


# data --------------------------------------------------------------------

# clean objects
rm(list = ls())

# read sites
utm_sf_outlet <- st_read(dsn = "data_fmt/gis/watersheds/watersheds_combined",
                         layer = "epsg3722_sites_relocated",
                         drivers = "ESRI Shapefile")

# data frame for sampling sites
df_coord <- utm_sf_outlet %>% 
  mutate(X = round(st_coordinates(.)[,1], 6),
         Y = round(st_coordinates(.)[,2], 6)) %>% 
  as_tibble() %>% 
  distinct(segment, X, Y) # remove duplicates

X <- df_coord$X
Y <- df_coord$Y

# read in the network
strnet <- line2network(path = "data_fmt/gis/watersheds/watersheds_combined", 
                       layer = "epsg3722_strnet")


# prep stream network and sites --------------------------------------------------

# plot of all the segments
plot(strnet)  

# check topology (nodes)
topologydots(strnet)

# do a clean up: dissolve - y, insert vertices - y, distance - 1, 
# examine figure for mouth questions, remove ad4ditional segments - n, 
# build segment routes - y
strnet_fixed <- cleanup(rivers = strnet)

save(strnet_fixed, file = "data_fmt/strnet_fixed.RData")

# snap sites to stream network
site_snap <- xy2segvert(x = X,
                        y = Y,
                        rivers = strnet_fixed)

# distance matrix  --------------------------------------------------------

# distance matrix: total, m_x
m_x <- riverdistancemat(site_snap$seg,
                        site_snap$vert,
                        strnet_fixed, 
                        ID = df_coord$segment) %>%
  data.matrix()

m_x <- round(m_x / 1000, 2) # convert to km

# distance matrix: net upstream, m_y
m_y <- upstreammat(seg = site_snap$seg,
                   vert = site_snap$vert, 
                   rivers = strnet_fixed, 
                   ID = df_coord$segment,
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
save(m_u, file = "data_fmt/m_u_ws6.RData")
save(m_d, file = "data_fmt/m_d_ws6.RData")
