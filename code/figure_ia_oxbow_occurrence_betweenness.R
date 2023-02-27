
# this code examines relationships among oxbow occurrence (Iowa) and betweenness 

# IOWA --------------------------------------------------------------------
# data -----------------------------------------------------------------

# import oxbow data
df_iowa_oxbow <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds")

# visualize relationship ----------------------------------------------------------

# betweenness X oxbow occurrence
ggplot(df_iowa_oxbow,
       aes(x = between,
           y = oxbow_occurrence)) +
  theme_minimal() +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit')),
              color = "black",
              fill = "grey70") + 
  geom_point()
