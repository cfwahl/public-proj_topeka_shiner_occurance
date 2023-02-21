
# format mcmc_summry reuslts to extract median connectivity, and add siteid and 
# site0 columns

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libaries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# read mcmc_summary results
mcmc_summary_up_full_test1 <- readRDS("output/mcmc_summary_up_full.rds")  # old results
mcmc_summary_up_full_test <- readRDS("output/mcmc_summary_up_full_test.rds")  # new results

# old results, do not need to do following steps
#mcmc_summary_up_full_test <- readRDS("output/mcmc_summary_up_full.rds")

# make the rows into a column named param
df_s1 <- tibble::rownames_to_column(mcmc_summary_up_full_test, "param") 

# only look at s_hat rows, siteid and site0 columns with row numbers
df_s2 <- df_s1 %>%
  filter(str_detect(param, "s_hat")) %>% #remove alpha and beta parameters
  mutate(siteid = row_number(),
         site0 = row_number()) 

# slice model parameters (alpha, beta, mu, etc) from mcmc_summary   
df_s3 <- slice(df_s1, 1:14)
df_s <-  bind_rows(df_s3, df_s2) # bind parameters and connectiivty values




# CONNECTIVITY -------------------------------------------------------
# data --------------------------------------------------------------------


# read real and dummy sites
df_all <- readRDS(file = "data_fmt/data_minnesota_stream_dummy_real_occurrence.rds") %>% # new rds
  #df_all1 <- sf::st_read(dsn = "data_fmt/vector/epsg4326_minnesota_stream_dummy_real_occurrence.shp") %>% # old shapefile
  st_transform(crs = 3722)

## read stream network
sf_line <- readRDS(file = "data_fmt/data_minnesota_stream_network_5km2.rds") %>%
  st_transform(crs = 3722)



# extract 50% connectivity value and make into date frame
df_s <- df_s %>%
  filter(str_detect(param, "s_hat")) %>% #remove alpha and beta parameters 
  dplyr::select(connectivity = `50%`,
                site0,
                siteid) %>% # select 3 columns, connectivity, site0, and siteid (site0 is dummy site identifier)
  mutate(siteid = siteid + sum(!is.na(df_all$occurrence))) #reorganize data by occurrence=NA, move NA to the top (! command)

df_x <- df_all %>% 
  as_tibble() %>% #change gpkg to tibble
  filter(is.na(occurrence)) %>% # filter by NA only
  dplyr::select(siteid, line_id) %>% # select only these columns
  arrange(siteid) %>% # arrange by siteid
  left_join(df_s,
            by = "siteid") # join data frames by common column, siteid

sf_line2 <- sf_line %>% 
  left_join(df_x,
            by = c("line_id" = "line_id")) %>% # join data frames based on line_id 
  st_transform(sf_line, crs = 3722) # transform to utm



# map ---------------------------------------------------------------------

# create heat map of connectivity (s)
ggplot(sf_line2) + # base map of stream lines
  geom_sf(aes(color = connectivity)) + # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Immigration potential") + # label legend 
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) + 
  xlab("") + ylab("")


# export connectivity stream network--------------------------------------------------------

# write stream connectivity shapefile
st_write(sf_line2,
         dsn = "data_fmt/vector/epsg3722_minnesota_stream_connectivity_test.shp",
         append = FALSE)
