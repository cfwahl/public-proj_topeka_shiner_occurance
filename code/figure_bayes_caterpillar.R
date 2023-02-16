
# this script produces a caterpillar plot from bayes model output

# caterpillar bar plot -----------------------------------------------------

## read in jags output
post <- readRDS("output/post_summary_up_full.rds")

# alpha and b 
MCMCplot(post$mcmc, 
         params = c('alpha', 'b'),
         ci = c(50, 95),
         HPD = TRUE,
         ref_ovl = TRUE,
         labels = c('upstream conn.', 'downstream conn.', '% agriculture', 
                    'temp', 'drainage area', 'precip', 
                    'tot. connectivity'))
