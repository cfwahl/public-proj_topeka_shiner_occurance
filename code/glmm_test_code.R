# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               glmm, 
               lme4, 
               nlme, 
               arm)


# data --------------------------------------------------------------------

# read oxbow connectivity 
oxbow_conn <- readRDS("data_fmt/oxbow_connectivity.rds")

# read oxbow local water quality
oxbow_wq <- readRDS("data_fmt/data_mn_fws_fmt_oxbows_2.rds") 

### glmm (lme4) connectivity
mod_glmer_conn<-glmer(occurrence ~ connectivity  +(1|line_id),
                  data = oxbow_conn,
                  family="binomial")
summary(mod_glmer_conn)

# other working method
#glmm <- glmm(occurrence ~ connectivity, 
#             random = siteid, 
#             family.glmm = bernoulli.glmm, varcomps.names = c(siteid),
#             m = 10^4, debug = TRUE, data = oxbow_info)


### glmm water quality
mod_glmer_wq<-glmer(occurrence ~  temp + dopercent + do_mgl + turb + ph + (1|oxbow_id),
                  data = oxbow_conn,
                  family="binomial")
summary(mod_glmer_wq)




