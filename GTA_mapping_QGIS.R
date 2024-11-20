### use QGIS to process the data

library('qgisprocess')
#install.packages("sf", configure.args = "--with-proj-lib=/usr/local/lib/")
library(sf)
library(dplyr)
library(tidyverse)
library(tigris)
library(leaflet)
library(mapview)
library(ggplot2)

gispath = 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\Correct_shapefiles_with_note_from_Matt'

## load emme link shapefile
gta_gis = st_read(paste(gispath, '\\Base Shapefiles from Emily\\Base\\AMRoad\\emme_links.shp',sep = ''),
                     stringsAsFactors = FALSE)

## join the emme link shapefile with the count&emission table by attribute 'ID'
gta_am_gis <- merge(gta_gis, gtamodel_amlink, by.x = 'ID', by.y = 'ID')
#gta_am_gis = gta_am_gis[,-c(seq(16,46,1),48,49,50,51)] #run everytime regenerate the files
gta_md_gis <- merge(gta_gis, gtamodel_mdlink, by.x = 'ID', by.y = 'ID')
#gta_md_gis = gta_md_gis[,-c(seq(16,46,1),48,49,50,51)] #run everytime regenerate the files
gta_pm_gis <- merge(gta_gis, gtamodel_pmlink, by.x = 'ID', by.y = 'ID')
#gta_pm_gis = gta_pm_gis[,-c(seq(16,46,1),48,49,50,51)] #run everytime regenerate the files
gta_ev_gis <- merge(gta_gis, gtamodel_evlink, by.x = 'ID', by.y = 'ID')
#gta_ev_gis = gta_ev_gis[,-c(seq(16,46,1),48,49,50,51)] #run everytime regenerate the files

### prepare for the grid (1km*1km)
## create fishnet
gta_emme_bounding = st_bbox(gta_am_gis)
cell_size = 1000
# Create a sequence of coordinates
x_seq <- seq(from = gta_emme_bounding['xmin'], to = gta_emme_bounding['xmax'], by = cell_size)
y_seq <- seq(from = gta_emme_bounding['ymin'], to = gta_emme_bounding['ymax'], by = cell_size)
# Create grid of polygons
grid <- expand.grid(x = x_seq, y = y_seq) %>%
  mutate(xend = x + cell_size, yend = y + cell_size) %>%
  rowwise() %>%
  mutate(
    geometry = list(
      st_polygon(list(matrix(c(
        x, y,
        xend, y,
        xend, yend,
        x, yend,
        x, y  # Close the polygon
      ), ncol = 2, byrow = TRUE)))
    )
  ) %>%
  ungroup()
# Convert to an sf object
gta_emme_fishnet <- st_sf(geometry = st_sfc(grid$geometry), crs = st_crs(gta_am_gis))
# write fishnet
st_write(gta_emme_fishnet, paste(gispath,"\\Gridlevel_30_50\\GTA_fishnet_size1000.shp", sep=''))
# read the fishnet from the file, so that the FID can be obtained
gta_emme_fishnet = st_read(paste(gispath,"\\Gridlevel_30_50\\GTA_fishnet_size1000.shp", sep=''))


# intersect fishnet with the shapefile
gta_emme_fishnet_overlap_am <- st_intersection(gta_am_gis, gta_emme_fishnet)
gta_emme_fishnet_overlap_md <- st_intersection(gta_md_gis, gta_emme_fishnet)
gta_emme_fishnet_overlap_pm <- st_intersection(gta_pm_gis, gta_emme_fishnet)
gta_emme_fishnet_overlap_ev <- st_intersection(gta_ev_gis, gta_emme_fishnet)



################run GTA_gridlevel_interaction_weightedaverage next####################################
##################################################archived#############################################
## calculate the weighted emissions
# Add length of intersected line segments
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(length_m = st_length(geometry))

# Calculate weighted attribute
# Example: Weighted by length
#emfac weighted, TP, BWP
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(tp10_emfac = as.numeric(tp10_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(tp25_emfac = as.numeric(tp25_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(bwp10_emfac = as.numeric(bwp10_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(bwp25_emfac = as.numeric(bwp25_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))

#moves weighted, TP, BWP, TWP
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(tp10_moves = as.numeric(tp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(tp25_moves = as.numeric(tp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(bwp10_moves = as.numeric(bwp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(bwp25_moves = as.numeric(bwp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(twp10_moves = as.numeric(twp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_am <- gta_emme_fishnet_overlap_am %>%
  mutate(twp25_moves = as.numeric(twp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))


# Summarize weighted values for each polygon
for (i in 1:nrow(gta_emme_fishnet)) {
  findrows = which(gta_emme_fishnet_overlap_am$FID == gta_emme_fishnet$FID[i])
  gta_emme_fishnet$tp10_e_am[i] = sum(gta_emme_fishnet_overlap_am$tp10_emfac[findrows])
  gta_emme_fishnet$tp25_e_am[i] = sum(gta_emme_fishnet_overlap_am$tp25_emfac[findrows])  
  gta_emme_fishnet$bwp10_e_am[i] = sum(gta_emme_fishnet_overlap_am$bwp10_emfac[findrows])
  gta_emme_fishnet$bwp25_e_am[i] = sum(gta_emme_fishnet_overlap_am$bwp25_emfac[findrows])
  
  gta_emme_fishnet$tp10_m_am[i] = sum(gta_emme_fishnet_overlap_am$tp10_moves[findrows])
  gta_emme_fishnet$tp25_m_am[i] = sum(gta_emme_fishnet_overlap_am$tp25_moves[findrows])  
  gta_emme_fishnet$bwp10_m_am[i] = sum(gta_emme_fishnet_overlap_am$bwp10_moves[findrows])
  gta_emme_fishnet$bwp25_m_am[i] = sum(gta_emme_fishnet_overlap_am$bwp25_moves[findrows])
  gta_emme_fishnet$twp10_m_am[i] = sum(gta_emme_fishnet_overlap_am$twp10_moves[findrows])
  gta_emme_fishnet$twp25_m_am[i] = sum(gta_emme_fishnet_overlap_am$twp25_moves[findrows])
}

# st_write(gta_emme_fishnet, paste(gispath,"\\Gridlevel_30_50\\GTA_fishnet_size1000_emiscal_am.shp", sep=''))





### match tlink with tsegs --> link has virtual roads that may overestimate the emissions
# debug finding: tsegs has different node connection as tlink, and these two cannot match with each other
# try to use gta_am_gis ---> ok, checked. These two files cannot use node to infer with each other...

# use mapview to plot
# Check for duplicate column names
if (any(duplicated(names(gta_am_gis)))) {
  names(gta_am_gis) <- make.unique(names(gta_am_gis))  # Automatically makes unique names
}
mapview(gta_am_gis, zcol = 'tp10_moves', at = as.numeric(quantile(gta_am_gis$tp10_moves, seq(0,1,0.2))))

