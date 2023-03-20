
# this script produces a caterpillar plot from bayes model output

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

## read in jags output
post <- readRDS("output/post_summary_up_full.rds")

# caterpillar bar plot -----------------------------------------------------

# alpha and b 
MCMCplot(post$mcmc, 
         params = c('b'),
         ci = c(50, 95),
         HPD = TRUE,
         ref_ovl = TRUE,
         labels = c('% agriculture', 
                    'temp', 'drainage area', 'precip', 
                    'tot. connectivity'))
