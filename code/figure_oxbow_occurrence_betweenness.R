
# prediction regression of oxbow occurrence (Minnesota and Iowa joined)
# and betweenness

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

df_mn_ia_oxbow <- readRDS(file = "data_fmt/data_ia_mn_oxbow_join.rds") %>%
  as_tibble() %>%
  mutate(state = (oxbow_id < 143)) %>%
  mutate(state = replace(state, state == 'FALSE', 'Iowa'),
         state = replace(state, state == 'TRUE', 'Minnesota'))

# remove NAs --------------------------------------------------------------------

# number of NAs
colSums(is.na(df_mn_ia_oxbow))

df_fit <- df_mn_ia_oxbow %>% 
  drop_na(oxbow_occurrence,
          temperature,
          ph)

# glmm --------------------------------------------------------------------

fit <- glmer(oxbow_occurrence ~ between +  scale(temperature)  +
               +scale(ph) + (1|watershed), data = df_fit, 
             family = "binomial")

summary(fit)

# figure ------------------------------------------------------------------

df_pred <-  tibble(x = seq(min(df_fit$between),
                           max(df_fit$between),
                           length = 100)) %>% 
  mutate(y = boot::inv.logit(fit@beta[1] + fit@beta[2] * x))

df_fit %>% 
  ggplot(aes(x = between,
             y = oxbow_occurrence)) +
  geom_point(aes(colour = state)) +
  geom_line(data = df_pred,
            aes(x = x,
                y = y)) +
  theme_minimal() +
  theme(legend.background = element_rect(fill = FALSE, colour = FALSE),
        legend.justification = c(-0.25, 1.30),
        legend.position = c(0, 1),
        legend.title=element_blank()) + 
  scale_color_manual(values=c("chocolate", "grey39")) +
  xlab("Betwenness") + ylab("Prob. of Oxbow Occurrence") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=11))
