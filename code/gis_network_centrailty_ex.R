
# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

pacman::p_load(igraph,
               tidyverse,
               sf,
               foreach,
               lme4,
               lmerTest)

# data --------------------------------------------------------------------

## stream polyline
sf_line2 <- sf::st_read(dsn = "data_fmt/vector/espg3722_stream_connectivity_2.gpkg") 


# network centrality test ---------------------------------------------------------

df_m <- lapply(X = 1:n_distinct(sf_line2$watershed),
       FUN = function(x) {
         df_subset <- sf_line2 %>% 
           filter(watershed == x)
         
         y <- df_subset %>% 
           st_touches() %>% 
           graph.adjlist() %>% 
           eigen_centrality(directed = FALSE)
         
         out <- df_subset %>% 
           mutate(eigen = y$vector) %>% 
           relocate(eigen)
         
         return(out)
       }) %>% 
  bind_rows()

# subset watersheds -------------------------------------------------------

ws1 <- filter(df_m, watershed == "1")
ws2 <- filter(df_m, watershed == "2")
ws3 <- filter(df_m, watershed == "3")
ws4 <- filter(df_m, watershed == "4")
ws5 <- filter(df_m, watershed == "5")
ws6 <- filter(df_m, watershed == "6")
ws7 <- filter(df_m, watershed == "7")
ws8 <- filter(df_m, watershed == "8")

# visualize relationship ----------------------------------------------------------


## eigenvector X connectivity
## linear model 
ggplot(ws8,
       aes(x = eigen,
           y = connectivity))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


## plot of eigenvector X connectivity  
# glm with gamma distribution
ggplot(df_m,
       aes(x = eigen,
           y = connectivity))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(gaussian(link = 'log'))) + 
  geom_point()



# glmm --------------------------------------------------------------------

### glmm (lme4) connectivity and eigen
mod_glmer_conn <- glmer(connectivity ~ eigen + (1|watershed),
                        data = df_m, family = Gamma(link = "log"))

summary(mod_glmer_conn)

## lmer
mod_lmer_conn <- lmer(connectivity ~ eigen + (1|watershed),
                        data = df_m)

summary(mod_lmer_conn)
