### run this file after GTA_mapping_QGIS for interacting and calculating the weighted average of each grid

## calculate the weighted emissions
# Add length of intersected line segments

## AM
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


### MD 
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(length_m = st_length(geometry))
# Calculate weighted attribute
# Example: Weighted by length
#emfac weighted, TP, BWP
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(tp10_emfac = as.numeric(tp10_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(tp25_emfac = as.numeric(tp25_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(bwp10_emfac = as.numeric(bwp10_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(bwp25_emfac = as.numeric(bwp25_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))

#moves weighted, TP, BWP, TWP
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(tp10_moves = as.numeric(tp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(tp25_moves = as.numeric(tp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(bwp10_moves = as.numeric(bwp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(bwp25_moves = as.numeric(bwp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(twp10_moves = as.numeric(twp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_md <- gta_emme_fishnet_overlap_md %>%
  mutate(twp25_moves = as.numeric(twp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))


### PM 
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(length_m = st_length(geometry))
# Calculate weighted attribute
# Example: Weighted by length
#emfac weighted, TP, BWP
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(tp10_emfac = as.numeric(tp10_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(tp25_emfac = as.numeric(tp25_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(bwp10_emfac = as.numeric(bwp10_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(bwp25_emfac = as.numeric(bwp25_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))

#moves weighted, TP, BWP, TWP
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(tp10_moves = as.numeric(tp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(tp25_moves = as.numeric(tp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(bwp10_moves = as.numeric(bwp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(bwp25_moves = as.numeric(bwp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(twp10_moves = as.numeric(twp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_pm <- gta_emme_fishnet_overlap_pm %>%
  mutate(twp25_moves = as.numeric(twp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))


### EV
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(length_m = st_length(geometry))
# Calculate weighted attribute
# Example: Weighted by length
#emfac weighted, TP, BWP
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(tp10_emfac = as.numeric(tp10_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(tp25_emfac = as.numeric(tp25_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(bwp10_emfac = as.numeric(bwp10_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(bwp25_emfac = as.numeric(bwp25_emfac) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))

#moves weighted, TP, BWP, TWP
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(tp10_moves = as.numeric(tp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(tp25_moves = as.numeric(tp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(bwp10_moves = as.numeric(bwp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(bwp25_moves = as.numeric(bwp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(twp10_moves = as.numeric(twp10_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))
gta_emme_fishnet_overlap_ev <- gta_emme_fishnet_overlap_ev %>%
  mutate(twp25_moves = as.numeric(twp25_moves) * as.numeric(length_m) / (as.numeric(LENGTH)*1000))



# Summarize weighted values for each polygon
for (i in 1:nrow(gta_emme_fishnet)) {
  #AM
  findrows = which(gta_emme_fishnet_overlap_am$FID == gta_emme_fishnet$FID[i])
  gta_emme_fishnet$tp10eam[i] = sum(gta_emme_fishnet_overlap_am$tp10_emfac[findrows], na.rm = T)
  gta_emme_fishnet$tp25eam[i] = sum(gta_emme_fishnet_overlap_am$tp25_emfac[findrows], na.rm = T)  
  gta_emme_fishnet$bwp10eam[i] = sum(gta_emme_fishnet_overlap_am$bwp10_emfac[findrows], na.rm = T)
  gta_emme_fishnet$bwp25eam[i] = sum(gta_emme_fishnet_overlap_am$bwp25_emfac[findrows], na.rm = T)

  gta_emme_fishnet$tp10mam[i] = sum(gta_emme_fishnet_overlap_am$tp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$tp25mam[i] = sum(gta_emme_fishnet_overlap_am$tp25_moves[findrows], na.rm = T)  
  gta_emme_fishnet$bwp10mam[i] = sum(gta_emme_fishnet_overlap_am$bwp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$bwp25mam[i] = sum(gta_emme_fishnet_overlap_am$bwp25_moves[findrows], na.rm = T)
  gta_emme_fishnet$twp10mam[i] = sum(gta_emme_fishnet_overlap_am$twp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$twp25mam[i] = sum(gta_emme_fishnet_overlap_am$twp25_moves[findrows], na.rm = T)
  
  #MD
  findrows = which(gta_emme_fishnet_overlap_md$FID == gta_emme_fishnet$FID[i])
  gta_emme_fishnet$tp10emd[i] = sum(gta_emme_fishnet_overlap_md$tp10_emfac[findrows], na.rm = T)
  gta_emme_fishnet$tp25emd[i] = sum(gta_emme_fishnet_overlap_md$tp25_emfac[findrows], na.rm = T)  
  gta_emme_fishnet$bwp10emd[i] = sum(gta_emme_fishnet_overlap_md$bwp10_emfac[findrows], na.rm = T)
  gta_emme_fishnet$bwp25emd[i] = sum(gta_emme_fishnet_overlap_md$bwp25_emfac[findrows], na.rm = T)
  
  gta_emme_fishnet$tp10mmd[i] = sum(gta_emme_fishnet_overlap_md$tp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$tp25mmd[i] = sum(gta_emme_fishnet_overlap_md$tp25_moves[findrows], na.rm = T)  
  gta_emme_fishnet$bwp10mmd[i] = sum(gta_emme_fishnet_overlap_md$bwp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$bwp25mmd[i] = sum(gta_emme_fishnet_overlap_md$bwp25_moves[findrows], na.rm = T)
  gta_emme_fishnet$twp10mmd[i] = sum(gta_emme_fishnet_overlap_md$twp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$twp25mmd[i] = sum(gta_emme_fishnet_overlap_md$twp25_moves[findrows], na.rm = T)
  
  #PM
  findrows = which(gta_emme_fishnet_overlap_pm$FID == gta_emme_fishnet$FID[i])
  gta_emme_fishnet$tp10epm[i] = sum(gta_emme_fishnet_overlap_pm$tp10_emfac[findrows], na.rm = T)
  gta_emme_fishnet$tp25epm[i] = sum(gta_emme_fishnet_overlap_pm$tp25_emfac[findrows], na.rm = T)  
  gta_emme_fishnet$bwp10epm[i] = sum(gta_emme_fishnet_overlap_pm$bwp10_emfac[findrows], na.rm = T)
  gta_emme_fishnet$bwp25epm[i] = sum(gta_emme_fishnet_overlap_pm$bwp25_emfac[findrows], na.rm = T)
  
  gta_emme_fishnet$tp10mpm[i] = sum(gta_emme_fishnet_overlap_pm$tp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$tp25mpm[i] = sum(gta_emme_fishnet_overlap_pm$tp25_moves[findrows], na.rm = T)  
  gta_emme_fishnet$bwp10mpm[i] = sum(gta_emme_fishnet_overlap_pm$bwp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$bwp25mpm[i] = sum(gta_emme_fishnet_overlap_pm$bwp25_moves[findrows], na.rm = T)
  gta_emme_fishnet$twp10mpm[i] = sum(gta_emme_fishnet_overlap_pm$twp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$twp25mpm[i] = sum(gta_emme_fishnet_overlap_pm$twp25_moves[findrows], na.rm = T)
  
  #EV
  findrows = which(gta_emme_fishnet_overlap_ev$FID == gta_emme_fishnet$FID[i])
  gta_emme_fishnet$tp10eev[i] = sum(gta_emme_fishnet_overlap_ev$tp10_emfac[findrows], na.rm = T)
  gta_emme_fishnet$tp25eev[i] = sum(gta_emme_fishnet_overlap_ev$tp25_emfac[findrows], na.rm = T)  
  gta_emme_fishnet$bwp10eev[i] = sum(gta_emme_fishnet_overlap_ev$bwp10_emfac[findrows], na.rm = T)
  gta_emme_fishnet$bwp25eev[i] = sum(gta_emme_fishnet_overlap_ev$bwp25_emfac[findrows], na.rm = T)
  
  gta_emme_fishnet$tp10mev[i] = sum(gta_emme_fishnet_overlap_ev$tp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$tp25mev[i] = sum(gta_emme_fishnet_overlap_ev$tp25_moves[findrows], na.rm = T)  
  gta_emme_fishnet$bwp10mev[i] = sum(gta_emme_fishnet_overlap_ev$bwp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$bwp25mev[i] = sum(gta_emme_fishnet_overlap_ev$bwp25_moves[findrows], na.rm = T)
  gta_emme_fishnet$twp10mev[i] = sum(gta_emme_fishnet_overlap_ev$twp10_moves[findrows], na.rm = T)
  gta_emme_fishnet$twp25mev[i] = sum(gta_emme_fishnet_overlap_ev$twp25_moves[findrows], na.rm = T)
}


st_write(gta_emme_fishnet, paste(gispath,"\\Gridlevel_30_50\\GTA_fishnet_size1000_emiscal_fullday.shp", sep=''))

