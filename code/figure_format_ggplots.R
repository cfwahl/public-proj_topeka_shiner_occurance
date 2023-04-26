
# this code places plot
library(gridExtra)
library(grid)
library(gridtext)

# add one label for both x-axes
bottom <- textGrob("Betwenness", gp = gpar(fontsize = 12))

# side by sdie plot (1 row, 2 columns)
grid.arrange(plot1, plot2, ncol=2, bottom=bottom) 
