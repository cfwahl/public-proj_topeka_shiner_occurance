
# this code examines relationships among connectivity and betweenness in Minnesota
# streams and oxbows

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# stream network
sf_line2 <- readRDS(file = "data_fmt/data_minnesota_stream_connectivity.rds") %>%
  dplyr::select(-c(STRM_VAL)) # remove slope variable

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

# join --------------------------------------------------------------------

sf_line_test <- sf_line2 %>%
  left_join(df_b,
            by = 'line_id') %>%
  rename(connectivity = connectivity.x)

# plot --------------------------------------------------------------------

# plot of betweenness and connectivity
ggplot(sf_line_test,
       aes(x = between,
           y = connectivity)) +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log')),
              color = "black",
              fill = "grey70")+
  geom_point() + 
  theme_minimal()
