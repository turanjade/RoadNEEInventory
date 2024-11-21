#@auto_volu: the number of autos traversing the link in the hour. To convert to period totals, you will need to multiply by these ratios:
#  AM  = 1/0.469
#  MD = 1 / 0.1666667
#  PM = 1 / 0.307
#  EV = 1 / 0.2
#  speed: link speed in km/hr

# morning peak (6-9AM), midday (9AM-3PM), afternoon peak (3PM-7PM), evening (7PM-11PM).

library('sf')
library('arcgisbinding')
library('openxlsx')
library('readxl')
library('ggplot2')
###read road network profile
path = 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports'


rootpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程"
movesefpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\MOVES4_output\\toronto_avgspeeed_pc_allPM" 

inputpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\EstimationDataInput" 
outputpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\ProcessingOutput" 

setwd(outputpath) 

## read gtamodel link data from file
gtamodel_amlink = read.csv(paste(inputpath,'\\GTA_from_Emily\\Base_AMRoad_emme_links_result.csv', sep = ''), header = T)
gtamodel_mdlink = read.csv(paste(inputpath,'\\GTA_from_Emily\\Base_MDRoad_emme_links_result.csv', sep = ''), header = T)
gtamodel_pmlink = read.csv(paste(inputpath,'\\GTA_from_Emily\\Base_PMRoad_emme_links_result.csv', sep = ''), header = T)
gtamodel_evlink = read.csv(paste(inputpath,'\\GTA_from_Emily\\Base_EVRoad_emme_links_result.csv', sep = ''), header = T)

# only store necessary variables
colnameslinks = c('Period','ID','length_km','type','lanes','speeed_kmh','volume_original')
gtamodel_amlink = data.frame(cbind(1, gtamodel_amlink$ID, gtamodel_amlink$LENGTH, gtamodel_amlink$TYPE,
                        gtamodel_amlink$LANES, gtamodel_amlink$speed, gtamodel_amlink$X.auto_volu))
colnames(gtamodel_amlink) = colnameslinks

gtamodel_mdlink = data.frame(cbind(2, gtamodel_mdlink$ID, gtamodel_mdlink$LENGTH, gtamodel_mdlink$TYPE,
                                   gtamodel_mdlink$LANES, gtamodel_mdlink$speed, gtamodel_mdlink$X.auto_volu))
colnames(gtamodel_mdlink) = colnameslinks

gtamodel_pmlink = data.frame(cbind(3, gtamodel_pmlink$ID, gtamodel_pmlink$LENGTH, gtamodel_pmlink$TYPE,
                                   gtamodel_pmlink$LANES, gtamodel_pmlink$speed, gtamodel_pmlink$X.auto_volu))
colnames(gtamodel_pmlink) = colnameslinks

gtamodel_evlink = data.frame(cbind(4, gtamodel_evlink$ID, gtamodel_evlink$LENGTH, gtamodel_evlink$TYPE,
                                   gtamodel_evlink$LANES, gtamodel_evlink$speed, gtamodel_evlink$X.auto_volu))
colnames(gtamodel_evlink) = colnameslinks

## define a function to apply to all the periods
linkef_avgspeed <- function(efdb, efdb_speedcol, efdb_pollutantcol, efdb_roadtypecol, efdb_efcol, roadtype, pollutant, speed, speedbin) { 
  speed_round = round(speed/speedbin,0) * speedbin 
  if (speed_round == 0) {
    speed_round = min(efdb[,efdb_speedcol])
  }
  ef = efdb[which(as.numeric(efdb[,efdb_speedcol]) == speed_round & efdb[,efdb_roadtypecol] == roadtype &
                    efdb[,efdb_pollutantcol] == pollutant), efdb_efcol]
  
  return(as.numeric(ef))
}

# first estimate using EMFAC -- do not distinguish road type
# EMFAC speed bin split per 5 mph
# assign EF
## for AM link
for (i in 1:nrow(gtamodel_amlink)) {
  gtamodel_amlink$tp10ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                              'RUNEX_PM10',as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 5)
  gtamodel_amlink$tp25ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                              'RUNEX_PM2_5',as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 5)
  gtamodel_amlink$bwp10ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                              'PMBW_PM10',as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 5)
  gtamodel_amlink$bwp25ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                              'PMBW_PM2_5',as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 5)
  #gtamodel_amlink$twp10ef[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
  #                                            'PMTW_PM10',as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 5)
  #gtamodel_amlink$twp25ef[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
  #                                            'PMTW_PM2_5',as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 5)
}

# md link
for (i in 1:nrow(gtamodel_mdlink)) {
  gtamodel_mdlink$tp10ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                    'RUNEX_PM10',as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 5)
  gtamodel_mdlink$tp25ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                    'RUNEX_PM2_5',as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 5)
  gtamodel_mdlink$bwp10ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                     'PMBW_PM10',as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 5)
  gtamodel_mdlink$bwp25ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                     'PMBW_PM2_5',as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 5)
  #gtamodel_mdlink$twp10ef[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
  #                                            'PMTW_PM10',as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 5)
  #gtamodel_mdlink$twp25ef[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
  #                                            'PMTW_PM2_5',as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 5)
}

# for pmlink
for (i in 1:nrow(gtamodel_pmlink)) {
  gtamodel_pmlink$tp10ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                    'RUNEX_PM10',as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 5)
  gtamodel_pmlink$tp25ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                    'RUNEX_PM2_5',as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 5)
  gtamodel_pmlink$bwp10ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                     'PMBW_PM10',as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 5)
  gtamodel_pmlink$bwp25ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                     'PMBW_PM2_5',as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 5)
  #gtamodel_pmlink$twp10ef[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
  #                                            'PMTW_PM10',as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 5)
  #gtamodel_pmlink$twp25ef[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
  #                                            'PMTW_PM2_5',as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 5)
}

# for evlink
for (i in 1:nrow(gtamodel_evlink)) {
  gtamodel_evlink$tp10ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                    'RUNEX_PM10',as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 5)
  gtamodel_evlink$tp25ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                    'RUNEX_PM2_5',as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 5)
  gtamodel_evlink$bwp10ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                     'PMBW_PM10',as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 5)
  gtamodel_evlink$bwp25ef_emfac[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
                                                     'PMBW_PM2_5',as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 5)
  #gtamodel_evlink$twp10ef[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
  #                                            'PMTW_PM10',as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 5)
  #gtamodel_evlink$twp25ef[i] = linkef_avgspeed(pmef_tor_emfac,1,2,3,4,5,
  #                                            'PMTW_PM2_5',as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 5)
}

# calculate total emissions per link
#am
gtamodel_amlink$tp10_emfac = gtamodel_amlink$tp10ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
gtamodel_amlink$tp25_emfac = gtamodel_amlink$tp25ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
gtamodel_amlink$bwp10_emfac = gtamodel_amlink$bwp10ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
gtamodel_amlink$bwp25_emfac = gtamodel_amlink$bwp25ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
#md
gtamodel_mdlink$tp10_emfac = gtamodel_mdlink$tp10ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
gtamodel_mdlink$tp25_emfac = gtamodel_mdlink$tp25ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
gtamodel_mdlink$bwp10_emfac = gtamodel_mdlink$bwp10ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
gtamodel_mdlink$bwp25_emfac = gtamodel_mdlink$bwp25ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
#pm
gtamodel_pmlink$tp10_emfac = gtamodel_pmlink$tp10ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
gtamodel_pmlink$tp25_emfac = gtamodel_pmlink$tp25ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
gtamodel_pmlink$bwp10_emfac = gtamodel_pmlink$bwp10ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
gtamodel_pmlink$bwp25_emfac = gtamodel_pmlink$bwp25ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
#ev
gtamodel_evlink$tp10_emfac = gtamodel_evlink$tp10ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2
gtamodel_evlink$tp25_emfac = gtamodel_evlink$tp25ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2
gtamodel_evlink$bwp10_emfac = gtamodel_evlink$bwp10ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2
gtamodel_evlink$bwp25_emfac = gtamodel_evlink$bwp25ef_emfac / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2

################### MOVES speed bin split per 1 mph
# assign EF
## for AM link
for (i in 1:nrow(gtamodel_amlink)) {
  gtamodel_amlink$tp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                    100,as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 1)
  gtamodel_amlink$tp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                    110,as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 1)
  gtamodel_amlink$bwp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     106,as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 1)
  gtamodel_amlink$bwp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     116,as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 1)
  gtamodel_amlink$twp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                              107,as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 1)
  gtamodel_amlink$twp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                              117,as.numeric(gtamodel_amlink$speeed_kmh[i])/1.6, 1)
}

# md link
for (i in 1:nrow(gtamodel_mdlink)) {
  gtamodel_mdlink$tp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                    100,as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 1)
  gtamodel_mdlink$tp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                    110,as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 1)
  gtamodel_mdlink$bwp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     106,as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 1)
  gtamodel_mdlink$bwp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     116,as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 1)
  gtamodel_mdlink$twp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     107,as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 1)
  gtamodel_mdlink$twp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     117,as.numeric(gtamodel_mdlink$speeed_kmh[i])/1.6, 1)
}

# for pmlink
for (i in 1:nrow(gtamodel_pmlink)) {
  gtamodel_pmlink$tp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                    100,as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 1)
  gtamodel_pmlink$tp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                    110,as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 1)
  gtamodel_pmlink$bwp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     106,as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 1)
  gtamodel_pmlink$bwp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     116,as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 1)
  gtamodel_pmlink$twp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     107,as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 1)
  gtamodel_pmlink$twp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     117,as.numeric(gtamodel_pmlink$speeed_kmh[i])/1.6, 1)
}

# for evlink
for (i in 1:nrow(gtamodel_evlink)) {
  gtamodel_evlink$tp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                    100,as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 1)
  gtamodel_evlink$tp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                    110,as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 1)
  gtamodel_evlink$bwp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     106,as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 1)
  gtamodel_evlink$bwp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     116,as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 1)
  gtamodel_evlink$twp10ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     107,as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 1)
  gtamodel_evlink$twp25ef_moves[i] = linkef_avgspeed(pmef_tor_moves,1,2,3,4,5,
                                                     117,as.numeric(gtamodel_evlink$speeed_kmh[i])/1.6, 1)
}

# calculate total emissions per link
#am
gtamodel_amlink$tp10_moves = gtamodel_amlink$tp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
gtamodel_amlink$tp25_moves = gtamodel_amlink$tp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
gtamodel_amlink$bwp10_moves = gtamodel_amlink$bwp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
gtamodel_amlink$bwp25_moves = gtamodel_amlink$bwp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
gtamodel_amlink$twp10_moves = gtamodel_amlink$twp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
gtamodel_amlink$twp25_moves = gtamodel_amlink$twp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_amlink$length_km) * as.numeric(gtamodel_amlink$volume_original)/0.469
#md
gtamodel_mdlink$tp10_moves = gtamodel_mdlink$tp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
gtamodel_mdlink$tp25_moves = gtamodel_mdlink$tp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
gtamodel_mdlink$bwp10_moves = gtamodel_mdlink$bwp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
gtamodel_mdlink$bwp25_moves = gtamodel_mdlink$bwp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
gtamodel_mdlink$twp10_moves = gtamodel_mdlink$twp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
gtamodel_mdlink$twp25_moves = gtamodel_mdlink$twp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_mdlink$length_km) * as.numeric(gtamodel_mdlink$volume_original)/0.1666667
#pm
gtamodel_pmlink$tp10_moves = gtamodel_pmlink$tp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
gtamodel_pmlink$tp25_moves = gtamodel_pmlink$tp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
gtamodel_pmlink$bwp10_moves = gtamodel_pmlink$bwp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
gtamodel_pmlink$bwp25_moves = gtamodel_pmlink$bwp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
gtamodel_pmlink$twp10_moves = gtamodel_pmlink$twp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
gtamodel_pmlink$twp25_moves = gtamodel_pmlink$twp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_pmlink$length_km) * as.numeric(gtamodel_pmlink$volume_original)/0.307
#ev
gtamodel_evlink$tp10_moves = gtamodel_evlink$tp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2
gtamodel_evlink$tp25_moves = gtamodel_evlink$tp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2
gtamodel_evlink$bwp10_moves = gtamodel_evlink$bwp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2
gtamodel_evlink$bwp25_moves = gtamodel_evlink$bwp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2
gtamodel_evlink$twp10_moves = gtamodel_evlink$twp10ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2
gtamodel_evlink$twp25_moves = gtamodel_evlink$twp25ef_moves / 1.6 * # g/mile convert to g/km
  as.numeric(gtamodel_evlink$length_km) * as.numeric(gtamodel_evlink$volume_original)/0.2


#save results to output
write.csv(gtamodel_amlink, paste(outputpath, '\\GTA_TPBWP_EF_Total_AM.csv', sep = ''), col.names = T, row.names = F)
write.csv(gtamodel_mdlink, paste(outputpath, '\\GTA_TPBWP_EF_Total_MD.csv', sep = ''), col.names = T, row.names = F)
write.csv(gtamodel_pmlink, paste(outputpath, '\\GTA_TPBWP_EF_Total_PM.csv', sep = ''), col.names = T, row.names = F)
write.csv(gtamodel_evlink, paste(outputpath, '\\GTA_TPBWP_EF_Total_EV.csv', sep = ''), col.names = T, row.names = F)
