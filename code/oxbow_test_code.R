# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               tmap,
               sf,
               mapview)


# data --------------------------------------------------------------------

## stream polyline
sf_line <- list.files("data_fmt/vector",
                      pattern = "connectivity_dummy_5km2.shp",
                      full.names = T) %>% 
  st_read()

## connectivity estimate
df_s <- readRDS("output/mcmc_summary_up_full.rds") %>% 
  filter(str_detect(param, "s_hat")) %>% 
  select(connectivity = `50%`,
         site0,
         siteid) %>% 
  mutate(siteid = siteid + 373)

df_x <- sf::st_read(dsn = "data_fmt/vector/espg3722_watersheds_landuse_dummy_2_5km2.gpkg") %>% 
  as_tibble() %>%
  filter(is.na(occrrnc)) %>% 
  select(siteid, line_id) %>% 
  arrange(siteid) %>% 
  left_join(df_s,
            by = "siteid")

## combine with stream polyline
sf_line <- sf_line %>% 
  left_join(df_x,
            by = c("line_id" = "line_id")) %>%
  st_transform(sf_line, crs = 3722)

# site info, one point representing all sites along a stream segment
st_write(sf_line,
         dsn = "data_fmt/vector/epsg4326_stream_connectivity.shp",
         append = FALSE)

oxbow <- st_read("data_fmt/vector/epsg4326_oxbow_sites.shp") %>%
  st_transform(oxbow, crs = 3722)

#cut_dist = 200 # meters 
#oxbow_snap <- st_nearest_points(oxbow, sf_line, tolerance = cut_dist) 

st_snap_points = function(oxbow, sf_line, max_dist = 1000) {
  
  if (inherits(oxbow, "sf")) n = nrow(oxbow)
  if (inherits(oxbow, "sfc")) n = length(oxbow)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(oxbow)[i], sf_line)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(oxbow)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}

brew = st_transform(oxbow, st_crs(sf_line))

tst = st_snap_points(brew, sf_line, 500)

mapview(oxbow) + mapview(tst, col.regions = "red") + sf_line



# site info, one point representing all sites along a stream segment
st_write(tst,
         dsn = "data_fmt/vector/epsg4326_oxbow_snap.shp",
         append = FALSE)


# map ---------------------------------------------------------------------

ggplot(sf_line) +
  geom_sf(aes(color = connectivity)) +
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Immigration potential") +
  theme_minimal() +
  ggtitle("Minnesota watersheds")

ggsave(file = "output/figure_map.pdf",
       width = 7,
       height = 9)
