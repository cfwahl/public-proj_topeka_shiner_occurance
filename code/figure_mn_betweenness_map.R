
#this script crates a map of betweenness in Minnesota

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# stream network
df_b <- readRDS(file = "data_fmt/data_minnesota_stream_betweenness.rds") %>%
  st_as_sf()

# maps --------------------------------------------------------------------

# map of betweenness scores 
plot2 <- ggplot(df_b) + # base map of stream lines
  geom_sf(aes(color = between),
          linewidth = 1.2)+ # heat map for connectivity 
  MetBrewer::scale_color_met_c("Hiroshige", direction = -1) +
  labs(color = "Betweenness") + # label legend 
  theme_minimal() +
  theme(axis.text.x = element_blank(), # remove lat/long from map
        axis.text.y = element_blank(),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13), 
        plot.tag = element_text(size = 13)) +
  labs(tag = "B")

# save map ----------------------------------------------------------------

ggsave(file = "figure/figure_map_minnesota_betweenness.pdf",
       width = 7,
       height = 9)
