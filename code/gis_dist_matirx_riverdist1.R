
# setup -------------------------------------------------------------------

pacman::p_load(riverdist,
               dplyr,
               sf, 
               tidyverse,
               stringr)

# set working directory 

# data --------------------------------------------------------------------

# clean objects
rm(list = ls())

# read sites
utm_sf_outlet <- st_read(dsn = "data_fmt/gis/watersheds/watershed6",
                         layer = "epsg3722_sites_ws6",
                         drivers = "ESRI Shapefile")

# create dataframe 
df <- utm_sf_outlet %>% 
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>% 
  as_tibble()

# coordinates  from utm_sf_outlet 
X <- df$X
Y <- df$Y

# read in the network
strnet <- line2network(path="data_fmt/gis/watersheds/watershed6", 
                       layer="epsg3722_StrNet_ws6")


# prep stream network and sites --------------------------------------------------

# plot of all the segments
plot(strnet)  

# check topology (nodes)
topologydots(strnet)

# do a clean up: dissolve - y, insert vertices - y, distance - 1, 
#     examine figure for mouth questions, remove additional segments - n, 
#     build segment routes - y
strnet_fixed <- cleanup(rivers = strnet)

# snap sites to stream network
site_snap <- xy2segvert(x=X, y=Y, rivers=strnet_fixed)

# add vertices
site_strnet <- bind_cols(utm_sf_outlet, site_snap)


# distance matrix  --------------------------------------------------------

# create matrix of total distances among sites
dmat <- riverdistancemat(site_strnet$seg, site_strnet$vert, strnet_fixed, 
                         ID = site_strnet$segment ) %>% as.matrix

tot_dist <- dmat/1000 # convert to km

# Save data frame
write.csv(tot_dist,"data_fmt/gis/watersheds/watershed6/total_dist_ws6.csv", row.names = TRUE)

# make total distance between sites into matrix for subtraction/addition 
#TD <- matrix(dmat, nrow = 126, ncol = 126)


# remove duplicate sites --------------------------------------------------

# I tried removing the duplicate sites and columns with coding but I 
# cannot figure it out

# read in distance matrix just created
#site_dists <- read.csv("data_fmt/distance_matrices/siteXdist_combined.csv",
#                       header = TRUE)

# assigning the first column a new name
#colnames(site_dists)[1] <- "segment_id"

# select sites which have "." in segment_id then assign to a df
#SD <- site_dists %>% 
#  filter(str_count(segment_id, fixed(".")) == 1)

# remove SD sites from the other dataframe and create a new df
#site_dists1 <- site_dists[!(site_dists$segment_id %in% SD$segment_id),]

# write the data out
#site_dists1 <- as.data.frame(site_dists1)
#write.csv(site_dists1,"data_fmt/distance_matrices\\siteXdist_combined2.csv", row.names = TRUE)


# flow direction ----------------------------------------------------------

# Mouth of network must be specified, get seg and vert from cleanup() above
#setmouth(seg=1844, vert=4246, rivers=strnet_fixed)
#setmouth(seg=702, vert=73, rivers=strnet_fixed)

# net UPSTREAM distance among sites
# net = TRUE subtracts downstream movement from upstream, otherwise it total dist
updmat <- upstreammat(seg = site_strnet$seg, vert = site_strnet$vert, 
                      rivers = strnet_fixed, 
                      ID = site_strnet$segment, net = TRUE) %>% as.matrix

net_up <- updmat/1000 # convert to km

# Save data frame
write.csv(net_up,"data_fmt/gis/watersheds/watershed6/net_up_dist_ws6.csv", row.names = TRUE)

# make in matrix for subtraction/addition
#net_up <- matrix(net_up, nrow = 126, ncol = 126)


# downstream and upstream distance matrices -----------------------------------

# net down stream should equal TD - net_up, but it makes some values TD instead
# for some reason this changes the column numbers which is not wanted
net_down <- tot_dist - net_up
 
# make negative values, aka more downstream dist than upstream, zero thus only 
# positive net upstream distances remains
U <- pmax(net_up,0) # if negative value, make zero

# make positive values, aka more upstream dist, zero. This negative nummber added
# to the total distance becomes the downstream distance 
updmat1 <- pmin(updmat,0) # if positive value, make zero
U1 <- matrix(updmat1, nrow = 126, ncol = 126) # convert to matrix


# negative values from net upstream distance plus TD equals the downstream dist
# the problem is that the total distance is now cells that were zero. Instead of 
# total dist we want the value from "U" in those cells
D <- U1 + TD 

# write the data out
# there are lots of repeating values, the math is wrong
D <- as.data.frame(D)
write.csv(D,"data_fmt/gis/watersheds/watershed6/net_down_dist_ws6.csv", row.names = TRUE)


