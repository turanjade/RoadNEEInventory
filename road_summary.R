setwd('D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports')


road = read.csv('roadid_withEF_totEmis.csv', header = T, sep = ',')
road$pm25pm10_ee = road$emis110/road$emis100
road$pm25pm10_bwp = road$emis116/road$emis106
road$pm25pm10_twp = road$emis117/road$emis107

plot(road$pm25pm10_ee)

library('ggplot2')

ratio = rbind(cbind(road$roadtype, road$pm25pm10_ee, 'ee'),
              cbind(road$roadtype, road$pm25pm10_bwp, 'bwp'),
              cbind(road$roadtype, road$pm25pm10_twp, 'twp'))

colnames(ratio) = c('roadtype','ratio','emissiontype')

ggplot(ratio, aes(x = factor(emissiontype), y = as.numeric(ratio))) + geom_boxplot()

median(road$pm25pm10_twp)

find401 = road[grepl('401',road$lf_name),]

find401main = road[grepl('401', road$lf_name) & !grepl('Ramp', road$lf_name),]

###summarize road type volume
roadtype = unique(road$fcode_desc)
roadtypetraffic = data.frame(matrix(0, nrow = length(roadtype), ncol = 14)) 
colnames(roadtypetraffic) = c('type','p0','p1','p2','p3','p4','p5','p6','p7',
                              'p8','p9','p10','avg','std')
for (i in 1:nrow(roadtypetraffic)) {
  roadtypetraffic$type[i] = roadtype[i]
  roadtypetraffic[i,2:12] = quantile(road$volume[which(road$fcode_desc == roadtypetraffic$type[i])], 
                                     seq(0,1,by=0.1))
  roadtypetraffic$avg[i] = mean(road$volume[which(road$fcode_desc == roadtypetraffic$type[i])])
  roadtypetraffic$std[i] = sd(road$volume[which(road$fcode_desc == roadtypetraffic$type[i])])
}

###summarize road type emisssions
roadtype = unique(road$fcode_desc)
roadtypetp10 = data.frame(matrix(0, nrow = length(roadtype), ncol = 14)) 
colnames(roadtypetp10) = c('type','p0','p1','p2','p3','p4','p5','p6','p7',
                              'p8','p9','p10','avg','std')
for (i in 1:nrow(roadtypetp10)) {
  roadtypetp10$type[i] = roadtype[i]
  roadtypetp10[i,2:12] = quantile(road$emis100[which(road$fcode_desc == roadtypetp10$type[i])], 
                                     seq(0,1,by=0.1))
  roadtypetp10$avg[i] = mean(road$emis100[which(road$fcode_desc == roadtypetp10$type[i])])
  roadtypetp10$std[i] = sd(road$emis100[which(road$fcode_desc == roadtypetp10$type[i])])
}

roadtypebwp10 = data.frame(matrix(0, nrow = length(roadtype), ncol = 14)) 
colnames(roadtypebwp10) = c('type','p0','p1','p2','p3','p4','p5','p6','p7',
                              'p8','p9','p10','avg','std')
for (i in 1:nrow(roadtypebwp10)) {
  roadtypebwp10$type[i] = roadtype[i]
  roadtypebwp10[i,2:12] = quantile(road$emis106[which(road$fcode_desc == roadtypebwp10$type[i])], 
                                     seq(0,1,by=0.1))
  roadtypebwp10$avg[i] = mean(road$emis106[which(road$fcode_desc == roadtypebwp10$type[i])])
  roadtypebwp10$std[i] = sd(road$emis106[which(road$fcode_desc == roadtypebwp10$type[i])])
}

roadtypetwp10 = data.frame(matrix(0, nrow = length(roadtype), ncol = 14)) 
colnames(roadtypetwp10) = c('type','p0','p1','p2','p3','p4','p5','p6','p7',
                              'p8','p9','p10','avg','std')
for (i in 1:nrow(roadtypetwp10)) {
  roadtypetwp10$type[i] = roadtype[i]
  roadtypetwp10[i,2:12] = quantile(road$emis107[which(road$fcode_desc == roadtypetwp10$type[i])], 
                                     seq(0,1,by=0.1))
  roadtypetwp10$avg[i] = mean(road$emis107[which(road$fcode_desc == roadtypetwp10$type[i])])
  roadtypetwp10$std[i] = sd(road$emis107[which(road$fcode_desc == roadtypetwp10$type[i])])
}

roadtypetp25 = data.frame(matrix(0, nrow = length(roadtype), ncol = 14)) 
colnames(roadtypetp25) = c('type','p0','p1','p2','p3','p4','p5','p6','p7',
                              'p8','p9','p10','avg','std')
for (i in 1:nrow(roadtypetp25)) {
  roadtypetp25$type[i] = roadtype[i]
  roadtypetp25[i,2:12] = quantile(road$emis110[which(road$fcode_desc == roadtypetp25$type[i])], 
                                     seq(0,1,by=0.1))
  roadtypetp25$avg[i] = mean(road$emis110[which(road$fcode_desc == roadtypetp25$type[i])])
  roadtypetp25$std[i] = sd(road$emis110[which(road$fcode_desc == roadtypetp25$type[i])])
}

roadtypebwp25 = data.frame(matrix(0, nrow = length(roadtype), ncol = 14)) 
colnames(roadtypebwp25) = c('type','p0','p1','p2','p3','p4','p5','p6','p7',
                              'p8','p9','p10','avg','std')
for (i in 1:nrow(roadtypebwp25)) {
  roadtypebwp25$type[i] = roadtype[i]
  roadtypebwp25[i,2:12] = quantile(road$emis116[which(road$fcode_desc == roadtypebwp25$type[i])], 
                                     seq(0,1,by=0.1))
  roadtypebwp25$avg[i] = mean(road$emis116[which(road$fcode_desc == roadtypebwp25$type[i])])
  roadtypebwp25$std[i] = sd(road$emis116[which(road$fcode_desc == roadtypebwp25$type[i])])
}

roadtypetwp25 = data.frame(matrix(0, nrow = length(roadtype), ncol = 14)) 
colnames(roadtypetwp25) = c('type','p0','p1','p2','p3','p4','p5','p6','p7',
                              'p8','p9','p10','avg','std')
for (i in 1:nrow(roadtypetwp25)) {
  roadtypetwp25$type[i] = roadtype[i]
  roadtypetwp25[i,2:12] = quantile(road$emis117[which(road$fcode_desc == roadtypetwp25$type[i])], 
                                     seq(0,1,by=0.1))
  roadtypetwp25$avg[i] = mean(road$emis117[which(road$fcode_desc == roadtypetwp25$type[i])])
  roadtypetwp25$std[i] = sd(road$emis117[which(road$fcode_desc == roadtypetwp25$type[i])])
}

####write processed results
# Load the workbook
wb = loadWorkbook("RoadNEEInventory/SummarybyRoadType.xlsx")
# Add a new sheet (e.g., "NewSheet") to the workbook
addWorksheet(wb, "volume")
writeData(wb, sheet = "volume", x =roadtypetraffic)

addWorksheet(wb, "taipipe PM10")
writeData(wb, sheet = "taipipe PM10", x =roadtypetp10)

addWorksheet(wb, "taipipe PM2.5")
writeData(wb, sheet = "taipipe PM2.5", x =roadtypetp25)

addWorksheet(wb, "brakewear PM10")
writeData(wb, sheet = "brakewear PM10", x =roadtypebwp10)

addWorksheet(wb, "brakewear PM2.5")
writeData(wb, sheet = "brakewear PM2.5", x =roadtypebwp25)

addWorksheet(wb, "tirewear PM10")
writeData(wb, sheet = "tirewear PM10", x =roadtypetwp10)

addWorksheet(wb, "tirewear PM2.5")
writeData(wb, sheet = "tirewear PM2.5", x =roadtypetwp25)

# Save the workbook
saveWorkbook(wb, "RoadNEEInventory/SummarybyRoadType.xlsx", overwrite = TRUE)

