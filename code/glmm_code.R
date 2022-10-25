# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               lme4)


# data --------------------------------------------------------------------

# read oxbow connectivity 
oxbow_conn <- readRDS("data_fmt/oxbow_connectivity.rds")


# prep distance column  ---------------------------------------------------

#oxbow_conn$distance <- gsub("[m]","",as.character(oxbow_conn$distance)) %>%
#  as.numeric(as.character(oxbow_conn$distance)) 

#oxbow_conn2 <- mutate(dist_bin = cut(oxbow_conn$distance, breaks=5))

# visualize data ----------------------------------------------------------

## occurrence X connectivity
## prediction like plot  
ggplot(oxbow_conn,
       aes(x = connectivity,
           y = occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point()


# plot of occurrence and temperature
ggplot(oxbow_conn,
       aes(x = temp,
           y = occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point()


# plot of occurrence and %DO
ggplot(oxbow_conn,
       aes(x = dopercent,
           y = occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point()


# plot of occurrence and DO (mg/L)
ggplot(oxbow_conn,
       aes(x = do_mgl,
           y = occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point()

# plot of occurrence and turbidity
ggplot(oxbow_conn,
       aes(x = turb,
           y = occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point()


# plot of occurrence and pH
ggplot(oxbow_conn,
       aes(x = ph,
           y = occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point()


# plot of occurrence and area
ggplot(oxbow_conn,
       aes(x = area,
           y = occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point()


## plot of occurrence and distance  
ggplot(oxbow_conn,
       aes(x = distance,
           y = occurrence))  +
  geom_smooth(method = 'glm', se = TRUE,
              method.args = list(family = "binomial")) + 
  geom_point()

# scale variables  --------------------------------------------------------

# scale variable for model convergence 
oxbow_conn$turb <- scale(oxbow_conn$turb)
oxbow_conn$temp <- scale(oxbow_conn$temp)
oxbow_conn$dopercent <- scale(oxbow_conn$dopercent)
oxbow_conn$do_mgl <- scale(oxbow_conn$do_mgl)
oxbow_conn$ph <- scale(oxbow_conn$ph)
oxbow_conn$area <- scale(oxbow_conn$area)
#oxbow_conn$distance <- scale(oxbow_conn$distance)


# glmm --------------------------------------------------------------------

### glmm (lme4) connectivity
mod_glmer_conn <- glmer(occurrence ~ connectivity +  temp + dopercent + 
                        do_mgl + turb + ph + area +  (1|line_id),
                  data = oxbow_conn, family="binomial")

summary(mod_glmer_conn)







