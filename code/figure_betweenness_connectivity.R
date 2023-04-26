
# this code examines relationships among connectivity and betweenness in Minnesota
# streams and oxbows

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# stream network
df_b <- readRDS(file = "data_fmt/data_minnesota_stream_betweenness.rds") 

# glmm --------------------------------------------------------------------

fit <- glm(connectivity ~ between, data = df_b, 
             family = Gamma(link = log))

summary(fit)

# plot --------------------------------------------------------------------

# plot of betweenness and connectivity
plot1 <- ggplot(df_b,
       aes(x = between,
           y = connectivity)) +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(Gamma(link = 'log')),
              color = "black",
              fill = "grey70")+
  geom_point() + 
  theme_minimal() +
  xlab("") + ylab("Connectivity") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.x = element_blank())  +
  labs(tag = "A")

# rank correlation --------------------------------------------------------

corr <- cor.test(x=df_b$between, y=df_b$connectivity, method = 'spearman')
corr

# save figure ----------------------------------------------------------------

ggsave(file = "figure/figure_connectivity_X_betweenness.pdf",
       width = 7,
       height = 5)
