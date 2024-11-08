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

