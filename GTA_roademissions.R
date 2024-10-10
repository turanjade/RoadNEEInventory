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
aadt = read.csv(paste(path,'\\0-final_aadt_2023.csv', sep = ''), sep = ',', header = T)
speed1 = read.csv(paste(path,'\\0-avg_speed_negative_2023_ran.csv', sep = ''), sep = ',', header = F)
speed2 = read.csv(paste(path,'\\0-avg_speed_positive_2023_ran.csv', sep = ''), sep = ',', header = F)

speed = rbind(speed1, speed2)
colnames(speed) = c('linkID','var','speed_kmh')

roadnetwork$volume = 0
roadnetwork$speed_kmh = 0

for (i in 1:nrow(roadnetwork)) {
  if (length(which(aadt$centreline_id == roadnetwork$geo_id[i])) * 
      length(which(speed$linkID == roadnetwork$geo_id[i])) != 0) {
    roadnetwork$volume[i] = sum(aadt$aadt[which(aadt$centreline_id == roadnetwork$geo_id[i])]) ##aadt for both directions
    roadnetwork$speed_kmh[i] = mean(speed$speed_kmh[which(speed$linkID == roadnetwork$geo_id[i])])
  }
}

road_recorded = roadnetwork[which(roadnetwork$volume > 0),]
roadtypegis = unique(road_recorded$fcode_desc)
road_recorded$roadtype = 0
for (i in roadtypegis) {
  if (i == "Expressway" | i == 'Expressway Ramp') {
    road_recorded$roadtype[which(road_recorded$fcode_desc == i)] = 4
  }
  else {
    road_recorded$roadtype[which(road_recorded$fcode_desc == i)] = 5
  }

}

road_recorded$speed_mph = round(road_recorded$speed_kmh/1.6,0)


##assign EF values to road record
road_recorded$ef100 = 0
road_recorded$ef106 = 0
road_recorded$ef107 = 0
road_recorded$ef110 = 0
road_recorded$ef116 = 0
road_recorded$ef117 = 0


##use pmef_tor

for (i in 1:nrow(road_recorded)) {
  road_recorded$ef100[i] = pmef_tor$aggregatedEF[which(pmef_tor$pollutantID == 100 & pmef_tor$linkID == road_recorded$speed_mph[i] & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$ef110[i] = pmef_tor$aggregatedEF[which(pmef_tor$pollutantID == 110 & pmef_tor$linkID == road_recorded$speed_mph[i] & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$ef106[i] = pmef_tor$aggregatedEF[which(pmef_tor$pollutantID == 106 & pmef_tor$linkID == road_recorded$speed_mph[i] & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$ef116[i] = pmef_tor$aggregatedEF[which(pmef_tor$pollutantID == 116 & pmef_tor$linkID == road_recorded$speed_mph[i] & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$ef107[i] = pmef_tor$aggregatedEF[which(pmef_tor$pollutantID == 107 & pmef_tor$linkID == road_recorded$speed_mph[i] & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
  road_recorded$ef117[i] = pmef_tor$aggregatedEF[which(pmef_tor$pollutantID == 117 & pmef_tor$linkID == road_recorded$speed_mph[i] & pmef_tor$roadTypeID == road_recorded$roadtype[i])]
}

road_recorded$emis100 = road_recorded$volume * road_recorded$ef100 * road_recorded$Shape_Length/1.6
road_recorded$emis106 = road_recorded$volume * road_recorded$ef106 * road_recorded$Shape_Length/1.6
road_recorded$emis107 = road_recorded$volume * road_recorded$ef107 * road_recorded$Shape_Length/1.6
road_recorded$emis110 = road_recorded$volume * road_recorded$ef110 * road_recorded$Shape_Length/1.6
road_recorded$emis116 = road_recorded$volume * road_recorded$ef116 * road_recorded$Shape_Length/1.6
road_recorded$emis117 = road_recorded$volume * road_recorded$ef117 * road_recorded$Shape_Length/1.6

write.csv(road_recorded, 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\roadid_withEF.csv')


####GIS plot
library('arcgisbinding')


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
gridemis = read.csv("D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\Correct_shapefiles_with_note_from_Matt\\roadid_clipped_EF.csv",header = T, sep = ',')
#correlate this table with road_recorded
gridemis = data.frame(cbind(gridemis$OBJECTID_12, gridemis$PageName, gridemis$PageNumber, gridemis$geo_id_1,
                 gridemis$volume, gridemis$speed_mph, gridemis$Shape_Length, gridemis$emis100,
                 gridemis$emis106, gridemis$emis107, gridemis$emis110, gridemis$emis116, 
                 gridemis$emis117))
colnames(gridemis) = c('objectid','pagename','pagenumber','geo_id','volume','speed_mph','length_km',
                       'emis100','emis106','emis107','emis110','emis116','emis117')

#calculate weighted emissions for each row of gridded links
gridemis$emis100_c = 0
gridemis$emis106_c = 0
gridemis$emis107_c = 0
gridemis$emis110_c = 0
gridemis$emis116_c = 0
gridemis$emis117_c = 0

gridemis$emis100 = as.numeric(gridemis$emis100)

for (i in 30603:nrow(gridemis)) {
  gridemis$emis100_c[i] = as.numeric(gridemis$emis100[i]) * as.numeric(gridemis$length_km[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  gridemis$emis106_c[i] = as.numeric(gridemis$emis106[i]) * as.numeric(gridemis$length_km[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  gridemis$emis107_c[i] = as.numeric(gridemis$emis107[i]) * as.numeric(gridemis$length_km[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  gridemis$emis110_c[i] = as.numeric(gridemis$emis110[i]) * as.numeric(gridemis$length_km[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  gridemis$emis116_c[i] = as.numeric(gridemis$emis116[i]) * as.numeric(gridemis$length_km[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  gridemis$emis117_c[i] = as.numeric(gridemis$emis117[i]) * as.numeric(gridemis$length_km[i])/road_recorded$Shape_Length[which(road_recorded$geo_id == gridemis$geo_id[i])]
  
  }

pagenum = unique(gridemis$pagenumber)
gridtotal = data.frame(matrix(0, nrow = length(pagenum), ncol = 7))
colnames(gridtotal) = c('pagenumber','totalemis100','totalemis106','totalemis107',
                        'totalemis110','totalemis116','totalemis117')
gridtotal$pagenumber = pagenum
for (i in 1:nrow(gridtotal)) {
  gridtotal$totalemis100[i] = sum(gridemis$emis100_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemis106[i] = sum(gridemis$emis106_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemis107[i] = sum(gridemis$emis107_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemis110[i] = sum(gridemis$emis110_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemis116[i] = sum(gridemis$emis116_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
  gridtotal$totalemis117[i] = sum(gridemis$emis117_c[which(gridemis$pagenumber == gridtotal$pagenumber[i])])
}

write.csv(gridtotal, 'gridtotalemissions.csv',row.names = F)


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


