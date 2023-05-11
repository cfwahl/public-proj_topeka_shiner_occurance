
# this script is used for model selection of the different GLMMs in analyses

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# insert data file here, varies with glmm
df_oxbow_snap <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds") 

# join for analysis remove NAs ---------------------------------------------

# drop NAs
df_fit <- df_oxbow_snap %>% 
  drop_na(oxbow_occurrence,
          cond,
          ph)

# model selection ---------------------------------------------------------

fit <- glmer(oxbow_occurrence ~  connectivity + scale(cond) + scale(ph) + (1|watershed),
             data = df_fit, family = "binomial")

summary(fit)

# model selection ---------------------------------------------------------

# might need this for selection to work 
options(na.action = "na.fail") 

# automated selection
MuMIn::dredge(global.model = fit)
