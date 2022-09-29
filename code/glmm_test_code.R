# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               lme4)


# data --------------------------------------------------------------------

# read oxbow connectivity 
oxbow_conn <- readRDS("data_fmt/oxbow_connectivity.rds")


# visualize data ----------------------------------------------------------

## occurrence X connectivity
## prediction like plot  
ggplot(oxbow_conn,
       aes(x = connectivity,
           y = occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


# plot of occurrence and temperature
ggplot(oxbow_conn,
       aes(x = temp,
           y = occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


# plot of occurrence and %DO
ggplot(oxbow_conn,
       aes(x = dopercent,
           y = occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


# plot of occurrence and DO (mg/L)
ggplot(oxbow_conn,
       aes(x = do_mgl,
           y = occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()

# plot of occurrence and turbidity
ggplot(oxbow_conn,
       aes(x = turb,
           y = occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


# plot of occurrence and pH
ggplot(oxbow_conn,
       aes(x = ph,
           y = occurrence))  +
  geom_smooth(method = 'lm', se = TRUE) + 
  geom_point()


# scale variables  --------------------------------------------------------

# scale variable for model convergence 
oxbow_conn$turb <- scale(oxbow_conn$turb)
oxbow_conn$temp <- scale(oxbow_conn$temp)
oxbow_conn$dopercent <- scale(oxbow_conn$dopercent)
oxbow_conn$do_mgl <- scale(oxbow_conn$do_mgl)
oxbow_conn$ph <- scale(oxbow_conn$ph)


# glmm --------------------------------------------------------------------

### glmm (lme4) connectivity
mod_glmer_conn <- glmer(occurrence ~ connectivity + temp + dopercent + 
                        do_mgl + turb + ph + (1|line_id),
                  data = oxbow_conn, family="binomial")

summary(mod_glmer_conn)







