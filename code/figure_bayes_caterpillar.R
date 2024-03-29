
# this script produces a caterpillar plot from bayes model output

# setup -------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

## read in jags output
post <- readRDS("output/post_summary_up_full.rds")

# set up pdf --------------------------------------------------------------

pdf(file = "figure/figure_bayes_caterpillar_plot.pdf",
    width = 7,
    height = 5)

# caterpillar bar plot -----------------------------------------------------

# alpha and b 
MCMCplot(post$mcmc, 
         params = c('b'),
         ci = c(50, 95),
         HPD = TRUE,
         ref_ovl = TRUE,
         labels = c('% agriculture', 
                    'temperature', 'drainage area', 'precipitation', 
                    'connectivity'),
         sz_labels = 1.4, 
         sz_ax_txt = 1.4,
         sz_tick_txt = 1.4)
# print pdf
dev.off()
