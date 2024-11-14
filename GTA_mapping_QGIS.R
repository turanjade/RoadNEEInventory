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

gta_am_gis = st_read(paste(gispath, '\\Base Shapefiles from Emily\\Base\\AMRoad\\emme_links.shp',sep = ''),
                     stringsAsFactors = FALSE)

gta_am_gis <- merge(gta_am_gis, gtamodel_amlink, by.x = 'ID', by.y = 'ID')
#gta_am_gis = gta_am_gis[,-c(seq(16,46,1),48,49,50,51)]

# use mapview to plot
# Check for duplicate column names
if (any(duplicated(names(gta_am_gis)))) {
  names(gta_am_gis) <- make.unique(names(gta_am_gis))  # Automatically makes unique names
}
mapview(gta_am_gis, zcol = 'tp10_moves', at = as.numeric(quantile(gta_am_gis$tp10_moves, seq(0,1,0.2))))



### match tlink with tsegs --> link has virtual roads that may overestimate the emissions
# debug finding: tsegs has different node connection as tlink, and these two cannot match with each other
# try to use gta_am_gis ---> ok, checked. These two files cannot use node to infer with each other...
# gta_gis_tsegs = st_read(paste(gispath, '\\Base Shapefiles from Emily\\Base\\AMRoad\\emme_tsegs.shp',sep = ''),
#                        stringsAsFactors = FALSE)
# mapview(gta_gis_tsegs)

# gta_gis_tsegs$link_match = 0
# gta_gis_tsegs$nodeconnect = paste(gta_gis_tsegs$INODE, gta_gis_tsegs$JNODE, sep='-')
# exclude = 0 #find how many links are excluded from the link-tsegs matching 
# errors = 0 #find how many links have more than one matches
# for (i in 1:nrow(gta_am_gis)) {
#   findrow = which(gta_gis_tsegs$nodeconnect == gta_am_gis$ID[i]) 
#   if (length(findrow) == 1) {
#     gta_gis_tsegs$link_match[findrow] = 1
#    } else {
#      if (length(findrow) == 0) {
#        exclude = exclude + 1
#        } else {
#          errors = errors + 1
#        }
#    }
#   }
# mapview(gta_gis_tsegs[which((gta_gis_tsegs$link_match == 1)),])
