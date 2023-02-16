
# this code examines relationships among oxbow occurrence (Minnesota and Iowa joined)
# and betweenness

# MN & IA COMBINED --------------------------------------------------------
# data --------------------------------------------------------------------

df_mn_ia_oxbow <- readRDS(file = "data_fmt/data_ia_mn_oxbow_join.rds") %>%
  as_tibble() %>%
  mutate(state = (oxbow_id < 143)) %>%
  mutate(state = replace(state, state == 'FALSE', 'Iowa'),
         state = replace(state, state == 'TRUE', 'Minnesota'))

# visualize ---------------------------------------------------------------

# MN + IA oxbow betweenness x occurrence
sp <- ggplot(df_mn_ia_oxbow,
       aes(x = between,
           y = oxbow_occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(binomial(link = 'logit')),
              color = "black",
              fill = "grey70") + 
  geom_point(aes(colour = state)) +  
  theme_minimal() +
  theme(legend.background = element_rect(fill = FALSE, size = 4, colour = FALSE),
        legend.justification = c(-0.5, 1.75),
        legend.position = c(0, 1),
        legend.title=element_blank()) 

sp + scale_color_manual(values=c("chocolate", "grey39")) 

