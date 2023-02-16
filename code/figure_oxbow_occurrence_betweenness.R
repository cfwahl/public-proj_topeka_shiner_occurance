
# this code examines relationships among oxbow occurrence (Minnesota and Iowa joined)
# and betweenness

# MN & IA COMBINED --------------------------------------------------------
# data --------------------------------------------------------------------

df_mn_ia_oxbow <- readRDS(file = "data_fmt/data_ia_mn_oxbow_join.rds")

# visualize ---------------------------------------------------------------

# MN + IA oxbow betweenness x occurrence
ggplot(df_mn_ia_oxbow,
       aes(x = between,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit'))) + 
  geom_point()
