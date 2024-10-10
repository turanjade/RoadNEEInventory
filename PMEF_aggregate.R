#file_nj = "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\非尾气-刹车磨损排放\\0-研究进程\\非尾气排放数据\\南京市网约车数据\\2019-10-17.csv"
#vehtraj = read.csv(file_nj, header = T, sep = ',')

####
#1) different vehicle types (LDV, LD-Truck, buses, HDV), 
#2) different speed bins on varied road types (arterial vs expressway),
#3) exhaust vs NEE (including BWP and TWP), for both PM2.5 and PM10. 
#4) EF among different models, MOVES, EMFAC, COPERT, HBEFA, and a range of EFs. 
#5) Road emission estimation using the range of EFs. 
#6) Compare the modeling result with measurement to check the alignment and derive the insufficiency of current models. 

#colnames(pmef)
#[1] "MOVESRunID"         "iterationID"        "yearID"             "monthID"           
#[5] "dayID"              "hourID"             "stateID"            "countyID"          
#[9] "zoneID"             "linkID"             "pollutantID"        "processID"         
#[13] "sourceTypeID"       "regClassID"         "fuelTypeID"         "fuelSubTypeID"     
#[17] "modelYearID"        "roadTypeID"         "SCC"                "engTechID"         
#[21] "sectorID"           "hpID"               "emissionQuant"      "emissionQuantMean" 
#[25] "emissionQuantSigma"

library('openxlsx')

path = 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\MOVES4_output\\toronto_avgspeeed_pc_allPM'
setwd(paste(path,'\\0_averagespeed_output_allPM', sep = ''))
agedist = read.xlsx(paste(path, '\\source21\\agedist_21_31yearsto2020.xlsx', sep = ''))
agedist$age = agedist$yearID - agedist$ageID

filelist = list.files()
pmef = data.frame(matrix(0, nrow = 0, ncol = 7))
colnames(pmef) = c('linkID','pollutantID', 'sourceTypeID', 'fuelTypeID', 'modelYearID', 'roadTypeID', 'emissionQuant')
#linkID corresponds to vehicle speed, will assign the value later after
#emissionQuant equals to the emissionQuant / ratio(vehicle in that age) / 10 (10 is the length of each link, miles)
for (i in 1:length(filelist)) {
  if (grepl('.csv', filelist[i]) == TRUE) {
    pmef_i = read.csv(filelist[i])
    vehage = unique(pmef_i$modelYearID)
    for (j in 1:length(vehage)) {
      pmef_i$emissionQuant[which(pmef_i$modelYearID == vehage[j])] = 
        pmef_i$emissionQuant[which(pmef_i$modelYearID == vehage[j])]/10/agedist$ageFraction[which(agedist$age == vehage[j])]
    }
    pmef_i = pmef_i[,c(10,11,13,15,17,18,23)]
    pmef_i$sourceTypeID = strsplit(filelist[i],'_')[[1]][2]
    colnames(pmef_i) = c('linkID','pollutantID', 'sourceTypeID', 'fuelTypeID', 'modelYearID', 'roadTypeID', 'emissionQuant')
    pmef = rbind(pmef, pmef_i)
  }
}

### choose 100, 106, 107, 110, 116, 117, for road type 4 and 5, choose year 2010 onward 
pmef_final = pmef[which(pmef$pollutantID == 100 | pmef$pollutantID == 106 | pmef$pollutantID == 107 | 
                    pmef$pollutantID == 110 | pmef$pollutantID == 116 | pmef$pollutantID == 117),]
pmef_final = pmef_final[which(pmef_final$roadTypeID > 1),]
pmef_final = pmef_final[which(pmef_final$modelYearID >= 2010),]
pmef_final = pmef_final[which(pmef_final$fuelTypeID != 5 & pmef_final$fuelTypeID != 3),]

#####take the share of Ontario vehicle 
agedist_on = read.csv('D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\0-vehshare_Ontario.csv',header = T)
year = unique(agedist_on$REF_DATE)
ldvshare = data.frame(matrix(0, nrow = length(year), ncol = 2))
colnames(ldvshare) = c('yearID','percentage')
ldtshare = data.frame(matrix(0, nrow = length(year), ncol = 2))
colnames(ldtshare) = c('yearID','percentage')
hdvshare = data.frame(matrix(0, nrow = length(year), ncol = 2))
colnames(hdvshare) = c('yearID','percentage')
busshare = data.frame(matrix(0, nrow = length(year), ncol = 2))
colnames(busshare) = c('yearID','percentage')

for (i in 1:length(year)) {
  ldvshare$yearID[i] = year[i]
  ldvshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == 'Passenger cars' & 
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == 'Passenger cars')])
  ldtshare$yearID[i] = year[i]
  ldtshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == 'Light trucks' & 
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == 'Light trucks')])
  hdvshare$yearID[i] = year[i]
  hdvshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == 'Heavy trucks' & 
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == 'Heavy trucks')])
  busshare$yearID[i] = year[i]
  busshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == 'Buses' & 
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == 'Buses')])
}

vehtypeshare = data.frame(matrix(0, nrow = 4, ncol = 2))
colnames(vehtypeshare) = c('vehtype', 'percentage')
vehtype = unique(agedist_on$Vehicle.type)
for (i in 1:length(vehtype)) {
  vehtypeshare$vehtype[i] = vehtype[i]
  vehtypeshare$percentage[i] = sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == vehtype[i])])/sum(agedist_on$VALUE)
}

#write down the shares
write.csv(busshare, 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\0-busshare.csv')
write.csv(ldvshare, 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\0-ldvshare.csv')
write.csv(ldtshare, 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\0-ldtshare.csv')
write.csv(hdvshare, 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\0-hdvshare.csv')
write.csv(vehtypeshare, 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\0-typeshare.csv')

##the fuel share of Ontario. Only consider gasoline, diesel, and electricity (refer to stats canada)
fuelshare = data.frame(matrix(c(0.320020481, 2, 0.002560164, 9, 0.677419355, 1), nrow =3, byrow = T))
colnames(fuelshare) = c('fuelshare','fueltype') #1: gasoline, 2: diesel, 9: electricity


##for each vehicle type, fuel type, pollutant, road type, and link, calculated a weighted emission factor
source = unique(pmef_final$sourceTypeID)#veh = unique(pmef_final$sourceTypeID)
veh = unique(pmef_final$vehtype)#veh = unique(pmef_final$sourceTypeID)
fuel = unique(pmef_final$fuelTypeID)
road = unique(pmef_final$roadTypeID)
age = unique(pmef_final$modelYearID)
link = unique(pmef_final$linkID)
pollutant = unique(pmef_final$pollutantID)

pmef_final$vehtype = 0
pmef_final$vehtype[which(pmef_final$sourceTypeID == 21)] = 'Passenger cars'
pmef_final$vehtype[which(pmef_final$sourceTypeID <=32 & pmef_final$sourceTypeID > 21)] = 'Light trucks'
pmef_final$vehtype[which(pmef_final$sourceTypeID <=43 & pmef_final$sourceTypeID > 32)] = 'Buses'
pmef_final$vehtype[which(pmef_final$sourceTypeID <=62 & pmef_final$sourceTypeID > 43)] = 'Heavy trucks'

###create a new matrix to store aggregated emission factor -- weighted by vehicle type and fuel type
pmef_tor = data.frame(matrix(0, nrow = length(road)*length(link)*length(pollutant), ncol = 4))
colnames(pmef_tor) = c('linkID','pollutantID','roadTypeID','aggregatedEF')

row = 0 #redirect to the specific row to fill in the pmef
for (p in pollutant) {
  for (r in road) {
    for (l in link) {
      weightef = 0
      row = row + 1
      for (s in source) {
        #define the share of vehicle
        if (s == 21) {
          vehshare_v = ldvshare
          v = 'Passenger cars'
        } else if (s > 21 & s <= 32) {
          vehshare_v = ldtshare
          vehshare_v$percentage = vehshare_v$percentage/2 #two vehicle types, 31,32
          v = 'Light trucks'
        } else if (s > 32 & s <= 43) {
          vehshare_v = busshare
          vehshare_v$percentage = vehshare_v$percentage/3 #3 vehicle types, 41,42,43
          v = 'Buses'
        } else {
          vehshare_v = hdvshare
          vehshare_v$percentage = vehshare_v$percentage/5 #5 vehicle types, 51, 52, 53, 61, 62
          v = 'Heavy trucks'
        }
        #print('vehtype assigned')
        for (f in fuel) {
            for (a in age) {
              findrow = which(pmef_final$linkID == l & 
                                pmef_final$pollutantID == p &
                                pmef_final$sourceTypeID == s &
                                pmef_final$fuelTypeID == f &
                                pmef_final$roadTypeID == r &
                                pmef_final$modelYearID == a)
              if (length(findrow) == 0) {
                weightef = weightef
                } else {
                weightef = weightef +  vehshare_v$percentage[which(vehshare_v$yearID == a)]*
                  fuelshare$fuelshare[which(fuelshare$fueltype == f)] * 
                  vehtypeshare$percentage[which(vehtypeshare$vehtype == v)] * 
                  pmef_final$emissionQuant[findrow]
              }
            }
        }
      }
      pmef_tor[row, ] = cbind(l,p,r, weightef)
      if (row %% 100 == 0) {
        print(paste('This is row:', row, "speed, pollutant, road, ef are:", l, p, r, weightef))
      } 
    }
  }
}


write.xlsx(pmef_tor, paste(path,'\\0_averagespeed_output_allPM\\0_averagespeed_torontovehshare_allPM.xlsx', sep = ''))


#pmef_2020 = pmef_final[which(pmef_final$modelYearID>=2020),]
vehtype_source = list('Passenger cars' = 21, 'Light trucks' = c(31,32), 'Heavy trucks' = c(51,52,53,61,62), 'Buses' = c(41,42,43))

