###calculate emissions
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")   

library('sf')
library('arcgisbinding')
library('openxlsx')
library('readxl')
library('ggplot2')
###read road network profile
path = 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports'

roadnetwork = read.csv(paste(path, '\\Correct_shapefiles_with_note_from_Matt\\roadid_withEF_linklength.csv',sep = ''), sep = ',', header = T)
aadt = read.csv(paste(path,'\\EstimationDataInput\\0-final_aadt_2023.csv', sep = ''), sep = ',', header = T)
speed1 = read.csv(paste(path,'\\EstimationDataInput\\0-avg_speed_negative_2023_ran.csv', sep = ''), sep = ',', header = F)
speed2 = read.csv(paste(path,'\\EstimationDataInput\\0-avg_speed_positive_2023_ran.csv', sep = ''), sep = ',', header = F)

speed = rbind(speed1, speed2)
colnames(speed) = c('linkID','var','speed_kmh')

roadnetwork$volume = 0
roadnetwork$speed_kmh = 0

# get volume and speed for both directions of the road
for (i in 1:nrow(roadnetwork)) {
  ## only when both speed and network have the same section, we include the link into the network
  if (length(which(aadt$centreline_id == roadnetwork$geo_id[i])) * 
      length(which(speed$linkID == roadnetwork$geo_id[i])) != 0) {
    roadnetwork$volume[i] = sum(aadt$aadt[which(aadt$centreline_id == roadnetwork$geo_id[i])]) ##aadt for both directions
    roadnetwork$speed_kmh[i] = mean(speed$speed_kmh[which(speed$linkID == roadnetwork$geo_id[i])])
  }
}

## select links with non-empty records for speed and volume
road_recorded = roadnetwork[which(roadnetwork$volume > 0),]
# unique road type
roadtypegis = unique(road_recorded$fcode_desc)
# rename road type to the recognized one
road_recorded$roadtype = 0
for (i in roadtypegis) {
  if (i == "Expressway" | i == 'Expressway Ramp') {
    road_recorded$roadtype[which(road_recorded$fcode_desc == i)] = 4
  }
  else {
    road_recorded$roadtype[which(road_recorded$fcode_desc == i)] = 5
  }

}

# convert to mph
road_recorded$speed_mph = round(road_recorded$speed_kmh/1.6,0)


##use pmef_tor_moves
for (i in 1:nrow(road_recorded)) {
  road_recorded$movesef100[i] = pmef_tor_moves$aggregatedEF[which(pmef_tor_moves$pollutantID == 100 & pmef_tor_moves$linkID == road_recorded$speed_mph[i] & pmef_tor_moves$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$movesef110[i] = pmef_tor_moves$aggregatedEF[which(pmef_tor_moves$pollutantID == 110 & pmef_tor_moves$linkID == road_recorded$speed_mph[i] & pmef_tor_moves$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$movesef106[i] = pmef_tor_moves$aggregatedEF[which(pmef_tor_moves$pollutantID == 106 & pmef_tor_moves$linkID == road_recorded$speed_mph[i] & pmef_tor_moves$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$movesef116[i] = pmef_tor_moves$aggregatedEF[which(pmef_tor_moves$pollutantID == 116 & pmef_tor_moves$linkID == road_recorded$speed_mph[i] & pmef_tor_moves$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$movesef107[i] = pmef_tor_moves$aggregatedEF[which(pmef_tor_moves$pollutantID == 107 & pmef_tor_moves$linkID == road_recorded$speed_mph[i] & pmef_tor_moves$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$movesef117[i] = pmef_tor_moves$aggregatedEF[which(pmef_tor_moves$pollutantID == 117 & pmef_tor_moves$linkID == road_recorded$speed_mph[i] & pmef_tor_moves$roadTypeID == road_recorded$roadtype[i])]
}
# calculate the total mass of pollutants in grams (initial shape length is in meters and EF in g/mi)
road_recorded$moves_100 = road_recorded$volume * road_recorded$movesef100 * road_recorded$Shape_Length/1.6/1000
road_recorded$moves_106 = road_recorded$volume * road_recorded$movesef106 * road_recorded$Shape_Length/1.6/1000
road_recorded$moves_107 = road_recorded$volume * road_recorded$movesef107 * road_recorded$Shape_Length/1.6/1000
road_recorded$moves_110 = road_recorded$volume * road_recorded$movesef110 * road_recorded$Shape_Length/1.6/1000
road_recorded$moves_116 = road_recorded$volume * road_recorded$movesef116 * road_recorded$Shape_Length/1.6/1000
road_recorded$moves_117 = road_recorded$volume * road_recorded$movesef117 * road_recorded$Shape_Length/1.6/1000

##use pmef_tor_emfac
# if the EF database is emfac, round to the nearest five
road_recorded$speed_mph_emfac = round(road_recorded$speed_kmh/1.6/5, 0) * 5
road_recorded$speed_mph_emfac[which(road_recorded$speed_mph_emfac > 90)] = 90
for (i in 1:nrow(road_recorded)) {
  road_recorded$emfacef_100[i] = pmef_tor_emfac$aggregatedEF[which(pmef_tor_emfac$pollutantID == 'RUNEX_PM10' & as.numeric(pmef_tor_emfac$linkID) == road_recorded$speed_mph_emfac[i])]# & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$emfacef_110[i] = pmef_tor_emfac$aggregatedEF[which(pmef_tor_emfac$pollutantID == 'RUNEX_PM2_5' & as.numeric(pmef_tor_emfac$linkID) == road_recorded$speed_mph_emfac[i])]# & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$emfacef_106[i] = pmef_tor_emfac$aggregatedEF[which(pmef_tor_emfac$pollutantID == 'PMBW_PM10' & as.numeric(pmef_tor_emfac$linkID) == road_recorded$speed_mph_emfac[i])]# & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$emfacef_116[i] = pmef_tor_emfac$aggregatedEF[which(pmef_tor_emfac$pollutantID == 'PMBW_PM2_5' & as.numeric(pmef_tor_emfac$linkID) == road_recorded$speed_mph_emfac[i])]# & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  #road_recorded$ef107[i] = pmef_tor_emfac$aggregatedEF[which(pmef_tor_emfac$pollutantID == 'PMTW_PM10' & as.numeric(pmef_tor_emfac$linkID) == road_recorded$speed_mph_emfac[i])]# & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  #road_recorded$ef117[i] = pmef_tor_emfac$aggregatedEF[which(pmef_tor_emfac$pollutantID == 'PMTW_PM2_5' & as.numeric(pmef_tor_emfac$linkID) == road_recorded$speed_mph_emfac[i])]# & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
}
# calculate the total mass of pollutants in grams (initial shape length is in meters and EF in g/mi)
road_recorded$emfac_100 = road_recorded$volume * as.numeric(road_recorded$emfacef_100) * road_recorded$Shape_Length/1.6/1000
road_recorded$emfac_106 = road_recorded$volume * as.numeric(road_recorded$emfacef_106) * road_recorded$Shape_Length/1.6/1000
road_recorded$emfac_107 = road_recorded$volume * as.numeric(pmef_tor_emfac$aggregatedEF[which(pmef_tor_emfac$pollutantID == 'PMTW_PM10')]) * road_recorded$Shape_Length/1.6/1000
road_recorded$emfac_110 = road_recorded$volume * as.numeric(road_recorded$emfacef_110) * road_recorded$Shape_Length/1.6/1000
road_recorded$emfac_116 = road_recorded$volume * as.numeric(road_recorded$emfacef_116) * road_recorded$Shape_Length/1.6/1000
road_recorded$emfac_117 = road_recorded$volume * as.numeric(pmef_tor_emfac$aggregatedEF[which(pmef_tor_emfac$pollutantID == 'PMTW_PM2_5')]) * road_recorded$Shape_Length/1.6/1000

road_recorded_save = cbind(road_recorded$geo_id, 
                           road_recorded$Shape_Length, road_recorded$speed_kmh, road_recorded$volume, 
                           road_recorded$moves_100, road_recorded$moves_106, road_recorded$moves_107,
                           road_recorded$moves_110, road_recorded$moves_116, road_recorded$moves_117,
                           road_recorded$emfac_100, road_recorded$emfac_106, road_recorded$emfac_107,
                           road_recorded$emfac_110, road_recorded$emfac_116, road_recorded$emfac_117)
colnames(road_recorded_save) = c('geo_id', 'length_meter', 'speed_kmh', 'volume',
                                 'moves100', 'moves106', 'moves107', 'moves110', 'moves116', 'moves117',
                                 'emfac100', 'emfac106', 'emfac107', 'emfac110', 'emfac116', 'emfac117')

write.csv(road_recorded_save, 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\ProcessingOutput\\roadid_withEF_MOVES_EMFAC.csv')


####GIS plot
library('arcgisbinding')
library('here')
library('leaflet')
library('leaflet.esri')
library('raster')
library('reticulate')
###read processed EF file
#weightedpmef_sheets = excel_sheets('D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\EF_LDV_LDT.xlsx')
# Initialize a list to store data frames
#weightedpmef <- list()

# Loop over the sheet names and read each sheet into a data frame
#for (sheet in weightedpmef_sheets) {
#  weightedpmef[[sheet]] <- read_excel('D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\EF_LDV_LDT.xlsx', 
#                                      sheet = sheet)
#}

# Access the individual data frames by sheet name
# Example: Accessing the first sheet
#df_first_sheet <- weightedpmef[[weightedpmef_sheets[1]]]

#pmefsheetsval = data.frame(matrix(c(100,5,106,5,107,5,110,5,116,5,117,5,100,4,106,4,107,4,110,4,116,4,117,4),
#                                  nrow = 12, byrow = T))
#colnames(pmefsheetsval) = c('pollutantID','roadtype')


#calculate grid total emissions
gridemis = read.csv("D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\ProcessingOutput\\Toronto_Grid_Street_Intersect.csv",header = T, sep = ',')
#correlate this table with road_recorded
gridemis = data.frame(cbind(gridemis$OBJECTID_1, gridemis$Toronto_Road_EF_Emission_Gri1_PageName, 
                            gridemis$Toronto_Road_EF_Emission_Gri1_PageNumber, 
                            gridemis$geo_id_1, gridemis$volume, gridemis$speed_kmh, gridemis$Shape_Length, 
                            gridemis$moves100, gridemis$moves106, gridemis$moves107,
                            gridemis$moves110, gridemis$moves116, gridemis$moves117,
                            gridemis$emfac100, gridemis$emfac106, gridemis$emfac107,
                            gridemis$emfac110, gridemis$emfac116, gridemis$emfac117))
colnames(gridemis) = c('objectid','pagename','pagenumber','geo_id','volume','speed_kmh','length_meter',
                       'moves100','moves106','moves107','moves110','moves116','moves117',
                       'emfac100','emfac106','emfac107','emfac110','emfac116','emfac117')

#calculate weighted emissions for each row of gridded links
gridemis$moves100_c = 0
gridemis$moves106_c = 0
gridemis$moves107_c = 0
gridemis$moves110_c = 0
gridemis$moves116_c = 0
gridemis$moves117_c = 0

#calculate weighted emissions for each row of gridded links
gridemis$emfac100_c = 0
gridemis$emfac106_c = 0
gridemis$emfac107_c = 0
gridemis$emfac110_c = 0
gridemis$emfac116_c = 0
gridemis$emfac117_c = 0
gridemis$volume_c = 0

# gridemis$emis100 = as.numeric(gridemis$emis100)

for (i in 1:nrow(gridemis)) {
  gridemis$volume_c[i] = as.numeric(gridemis$volume[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$moves100_c[i] = as.numeric(gridemis$moves100[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$moves106_c[i] = as.numeric(gridemis$moves106[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$moves107_c[i] = as.numeric(gridemis$moves107[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$moves110_c[i] = as.numeric(gridemis$moves110[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$moves116_c[i] = as.numeric(gridemis$moves116[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$moves117_c[i] = as.numeric(gridemis$moves117[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  
  #gridemis$emfac100_c[i] = as.numeric(gridemis$emfac100[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$emfac106_c[i] = as.numeric(gridemis$emfac106[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$emfac107_c[i] = as.numeric(gridemis$emfac107[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$emfac110_c[i] = as.numeric(gridemis$emfac110[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$emfac116_c[i] = as.numeric(gridemis$emfac116[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  #gridemis$emfac117_c[i] = as.numeric(gridemis$emfac117[i]) * as.numeric(gridemis$length_meter[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  
}

pagenum = unique(gridemis$pagenumber)
gridtotal = data.frame(matrix(0, nrow = length(pagenum), ncol = 14))
colnames(gridtotal) = c('pagenumber','totalmoves100','totalmoves106','totalmoves107',
                        'totalmoves110','totalmoves116','totalmoves117',
                        'totalemfac100','totalemfac106','totalemfac107',
                        'totalemfac110','totalemfac116','totalemfac117', 'totalvolume')
gridtotal$pagenumber = pagenum
for (i in 1:nrow(gridtotal)) {
  gridtotal$totalmoves100[i] = sum(gridemis$moves100_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalmoves106[i] = sum(gridemis$moves106_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalmoves107[i] = sum(gridemis$moves107_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalmoves110[i] = sum(gridemis$moves110_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalmoves116[i] = sum(gridemis$moves116_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalmoves117[i] = sum(gridemis$moves117_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  
  gridtotal$totalemfac100[i] = sum(gridemis$emfac100_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemfac106[i] = sum(gridemis$emfac106_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemfac107[i] = sum(gridemis$emfac107_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemfac110[i] = sum(gridemis$emfac110_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemfac116[i] = sum(gridemis$emfac116_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemfac117[i] = sum(gridemis$emfac117_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  
  gridtotal$totalvolume[i] = sum(gridemis$volume_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  
}

write.csv(gridtotal, 'Toronto_Gridtotalemissions_MOVES_EMFAC.csv',row.names = F)


###planning boundaries
boundaryemis = read.csv('D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\Correct_shapefiles_with_note_from_Matt\\planningboundary_intersect.csv', header = T, sep = ',')
boundaryemis = data.frame(cbind(boundaryemis$OBJECTID_12, boundaryemis$FID_COMMUNITY_PLANNING_BNDRY_WGS84_2024,
                     boundaryemis$Shape_Length, boundaryemis$geo_id, boundaryemis$emis100, 
                     boundaryemis$emis106, boundaryemis$emis107, boundaryemis$emis110,
                     boundaryemis$emis116, boundaryemis$emis117))

colnames(boundaryemis) = c('objectid','communityfid','length_m','geo_id','emis100','emis106',
                           'emis107','emis110','emis116','emis117')

boundaryemis$emis100_c = 0
boundaryemis$emis106_c = 0
boundaryemis$emis107_c = 0
boundaryemis$emis110_c = 0
boundaryemis$emis116_c = 0
boundaryemis$emis117_c = 0

linkid = unique(boundaryemis$geo_id)
for (i in 1:length(linkid)) {
  if (length(which(boundaryemis$geo_id == linkid[i])) > 1) {
    findrows = which(boundaryemis$geo_id == linkid[i])
    for (j in findrows) {
      boundaryemis$emis100_c[j] = boundaryemis$emis100[j] * boundaryemis$length_m[j]/
        sum(boundaryemis$length_m[which(boundaryemis$geo_id == linkid[i])])
      boundaryemis$emis106_c[j] = boundaryemis$emis106[j] * boundaryemis$length_m[j]/
        sum(boundaryemis$length_m[which(boundaryemis$geo_id == linkid[i])])
      boundaryemis$emis107_c[j] = boundaryemis$emis107[j] * boundaryemis$length_m[j]/
        sum(boundaryemis$length_m[which(boundaryemis$geo_id == linkid[i])])
      boundaryemis$emis110_c[j] = boundaryemis$emis110[j] * boundaryemis$length_m[j]/
        sum(boundaryemis$length_m[which(boundaryemis$geo_id == linkid[i])])
      boundaryemis$emis116_c[j] = boundaryemis$emis116[j] * boundaryemis$length_m[j]/
        sum(boundaryemis$length_m[which(boundaryemis$geo_id == linkid[i])])
      boundaryemis$emis117_c[j] = boundaryemis$emis117[j] * boundaryemis$length_m[j]/
        sum(boundaryemis$length_m[which(boundaryemis$geo_id == linkid[i])])
    }
  }
}

boundaryfid = unique(boundaryemis$communityfid)
boundarytotal = data.frame(matrix(0, nrow = length(boundaryfid), ncol = 7))
colnames(boundarytotal) = c('boundaryfid','totalemis100','totalemis106','totalemis107',
                        'totalemis110','totalemis116','totalemis117')
boundarytotal$boundaryfid = boundaryfid
for (i in 1:nrow(boundarytotal)) {
  boundarytotal$totalemis100[i] = sum(boundaryemis$emis100_c[which(boundaryemis$communityfid == boundarytotal$boundaryfid[i])])
  boundarytotal$totalemis106[i] = sum(boundaryemis$emis106_c[which(boundaryemis$communityfid == boundarytotal$boundaryfid[i])])
  boundarytotal$totalemis107[i] = sum(boundaryemis$emis107_c[which(boundaryemis$communityfid == boundarytotal$boundaryfid[i])])
  boundarytotal$totalemis110[i] = sum(boundaryemis$emis110_c[which(boundaryemis$communityfid == boundarytotal$boundaryfid[i])])
  boundarytotal$totalemis116[i] = sum(boundaryemis$emis116_c[which(boundaryemis$communityfid == boundarytotal$boundaryfid[i])])
  boundarytotal$totalemis117[i] = sum(boundaryemis$emis117_c[which(boundaryemis$communityfid == boundarytotal$boundaryfid[i])])
}

write.csv(boundarytotal, 'boundarytotalemissions.csv',row.names = F)


