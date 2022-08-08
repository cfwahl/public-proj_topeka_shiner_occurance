

# streamstats test code 

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# Load packages
pacman::p_load(streamstats)



# function times out unless we specify the number of seconds
setTimeout(120)

# snapped lat/longs from shapefile
ws1 <- delineateWatershed(xlocation = -96.4767, ylocation = 44.2550,
                          crs = 4326, 
                          includeparameters = "true",
                          includeflowtypes = "true",
                          includefeatures = "true")
ws1$parameters

# interactive map
leafletWatershed(ws1)

# watershed characteristics stats
chars1 <- computeChars(workspaceID = ws1$workspaceID, 
                       rcode = "MN")
chars1$parameters

# additional stats, flow stats
stats1 <- computeFlowStats(workspaceID = ws1$workspaceID, 
                           rcode = "MN", 
                           simplify = TRUE)

stats1






