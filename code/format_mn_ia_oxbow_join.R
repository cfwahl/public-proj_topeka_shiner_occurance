
# this script combines Minnesota and Iowa oxbow water quality and occurrence
# data

# setup --------------------------------------------------------------------

# clean objects
rm(list = ls())

# load libraries
source(here::here("code/library.R")) 

# data --------------------------------------------------------------------

# read minnesota oxbow rds
df_mn_ox_cent <- readRDS(file = "data_fmt/data_minnesota_oxbow_network_centrality.rds") %>%
  as_tibble() %>%
  arrange(oxbow_id)

# read iowa oxbow rds
df_iowa_oxbow <- readRDS(file = "data_fmt/data_iowa_network_centrality.rds") %>%
  as_tibble() %>%
  mutate(oxbow_id = row_number()+140,
         watershed = watershed+7) 

# join --------------------------------------------------------------------

df_mn_ia_oxbows_join <- df_iowa_oxbow %>%
  full_join(df_mn_ox_cent, by = "oxbow_id") %>%
  mutate(oxbow_occurrence = coalesce(oxbow_occurrence.x,oxbow_occurrence.y),
         do_mgl = coalesce(do_mgl.x, do_mgl.y),
         do_percent = coalesce(dopercent.x, dopercent.y),
         cond = coalesce(cond.x, cond.y),
         turbidity = coalesce(turb.x, turb.y),
         ph = coalesce(ph.x, ph.y),
         temperature = coalesce(temp.x, temp.y),
         between = coalesce(between.x, between.y),
         watershed = coalesce(watershed.x, watershed.y),
         geometry = coalesce(geometry.x, geometry.y)) %>%
  dplyr::select(-c(temp.x, temp.y, dopercent.x, do_mgl.x, do_mgl.y, turb.x, turb.y,
                   ph.x, ph.y, oxbow_occurrence.x, oxbow_occurrence.y, dopercent.y, 
                   line_id.x, line_id.y, watershed.x, watershed.y, cond.x, cond.y, 
                   siteid, site0, between.x, between.y, geometry.y, geometry.x)) 

# export ------------------------------------------------------------------

saveRDS(df_mn_ia_oxbows_join, file = "data_fmt/data_ia_mn_oxbow_join.rds")
