
# create river network and measure distances among sampling locations. Stream 
# network "epsg3722_minnesota_stream_dispersal_5km2.shp" was created to connect
# all sites with corridor for dispersal

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libaries
source(here::here("code/library.R")) 

# read site and stream data --------------------------------------------------------------------

# read occurrence data
wgs_sf_outlet <- readRDS(file = "data_fmt/data_minnesota_stream_dummy_real_occurrence.rds")

# transform to utm
utm_sf_outlet <- st_transform(wgs_sf_outlet, crs = 3722)

# data frame for sampling site coordinates
df_coord <- utm_sf_outlet %>% 
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>% 
  as_tibble()

X <- df_coord$X
Y <- df_coord$Y

# read in stream network
strnet <- line2network(path = "data_fmt/vector", 
                       layer = "epsg3722_minnesota_stream_dispersal_5km2")

# prep stream network and sites --------------------------------------------------

# clean up network: dissolve - y, split segment - n, insert vertices - y, 
# distance - 1, examine figure for mouth questions, remove additional segments - n, 
# build segment routes - y
strnet_fixed <- cleanup(rivers = strnet)
 
# snap sites to stream network
site_snap <- xy2segvert(x = X,
                        y = Y,
                        rivers = strnet_fixed)

# export fixed stream network ---------------------------------------------

# save file 
#saveRDS(strnet_fixed, file = "data_fmt/riverdist_minnesota_stream_network.rds")

# create distance matrices  --------------------------------------------------------

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

# total distance
m_td <- round(m_u + m_d, 2)

# check if m_td = m_x
identical(m_td, m_x)

# make data list to save multipule objects on one RDS file
datalist <- list(m_u = m_u, m_d = m_d)

# export
saveRDS(datalist, file = "data_fmt/data_minnesota_distance_matrix_dummy_real.rds")
