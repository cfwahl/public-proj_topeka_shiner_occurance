
# this script is used for model selection of the different GLMMs in analyses

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# insert data file here, varies with glmm
df_mn_ia_oxbow <- readRDS(file = "data_fmt/data_ia_mn_oxbow_join.rds")

# join for analysis remove NAs ---------------------------------------------

# drop NAs
df_fit <- df_mn_ia_oxbow %>% 
  drop_na(oxbow_occurrence,
          cond,
          ph,
          temperature)

# model selection ---------------------------------------------------------

fit <- glmer(oxbow_occurrence ~  between + scale(cond) + scale(temperature) + scale(ph) + (1|watershed),
             data = df_fit, family = "binomial")

summary(fit)

# model selection ---------------------------------------------------------

# might need this for selection to work 
options(na.action = "na.fail") 

# automated selection
MuMIn::dredge(global.model = fit, rank = "AIC")
