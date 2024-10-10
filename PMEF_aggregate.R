#file_nj = "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\非尾气-刹车磨损排放\\0-研究进程\\非尾气排放数据\\南京市网约车数据\\2019-10-17.csv" # nolint # nolint
#vehtraj = read.csv(file_nj, header = T, sep = ",") # nolint

####
#1) different vehicle types (LDV, LD-Truck, buses, HDV),
#2) different speed bins on varied road types (arterial vs expressway),
#3) exhaust vs NEE (including BWP and TWP), for both PM2.5 and PM10.
#4) EF among different models, MOVES, EMFAC, COPERT, HBEFA, and a range of EFs
#5) Road emission estimation using the range of EFs.  # nolint
#6) Compare the modeling result with measurement to check the alignment and derive the insufficiency of current models. # nolint

#colnames(pmef) from MOVES output # nolint
#[1] "MOVESRunID"         "iterationID"        "yearID"             "monthID"
#[5] "dayID"              "hourID"             "stateID"            "countyID"
#[9] "zoneID"             "linkID"             "pollutantID"        "processID"          # nolint
#[13] "sourceTypeID"       "regClassID"         "fuelTypeID"         "fuelSubTypeID"      # nolint
#[17] "modelYearID"        "roadTypeID"         "SCC"                "engTechID"          # nolint
#[21] "sectorID"           "hpID"               "emissionQuant"      "emissionQuantMean"  # nolint
#[25] "emissionQuantSigma"

library("openxlsx") # nolint

rootpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程"
movesefpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\MOVES4_output\\toronto_avgspeeed_pc_allPM" # nolint

inputpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\EstimationDataInput" # nolint
outputpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\ProcessingOutput" # nolint

setwd(outputpath) # nolint

#### 1. Data preparation
##### List all the emission rate data that are retrieved from MOVES database
filelist <- list.files(paste(movesefpath, "\\0_averagespeed_output_allPM", sep = "")) #nolint

#### agedist is the vehicle age distribution initially set in MOVES. In MOVES, we specify the output of different vehicle ages, and the result is proportional to the actual result from that year. An ajustment is required (see the code in the following loop) # nolint
agedist = read.xlsx(paste(movesefpath, "\\source21\\agedist_21_31yearsto2020.xlsx", sep = "")) # nolint # we use the same age distribution in MOVES execution
agedist$age <- agedist$yearID - agedist$ageID

#### read all the CSV files from MOVES and merge them into one file
pmef <- data.frame(matrix(0, nrow = 0, ncol = 7))
colnames(pmef) <- c("linkID","pollutantID", "sourceTypeID", "fuelTypeID", "modelYearID", "roadTypeID", "emissionQuant") # nolint
# LinkID corresponds to vehicle speed, will assign the value later after
# EmissionQuant equals to the emissionQuant / ratio(vehicle in that age) / 10 (10 is the length of each link, miles) # nolint
for (i in 1:length(filelist)) { # nolint
  if (grepl(".csv", filelist[i]) == TRUE) { # nolint
    pmef_i <- read.csv(paste(movesefpath, "\\0_averagespeed_output_allPM\\", filelist[i], sep = "")) # nolint #pmef_i is the ith ef file
    vehage <- unique(pmef_i$modelYearID)
    for (j in 1:length(vehage)) { # nolint
      pmef_i$emissionQuant[which(pmef_i$modelYearID == vehage[j])] <-
        pmef_i$emissionQuant[which(pmef_i$modelYearID == vehage[j])]/10/agedist$ageFraction[which(agedist$age == vehage[j])] # nolint #adjust emissions based on vehicle age proportion and link length (10 miles)
    }
    pmef_i <- pmef_i[, c(10, 11, 13, 15, 17, 18, 23)]
    pmef_i$sourceTypeID <- strsplit(filelist[i], "_")[[1]][2]
    colnames(pmef_i) = c("linkID","pollutantID", "sourceTypeID", "fuelTypeID", "modelYearID", "roadTypeID", "emissionQuant") # nolint
    pmef <- rbind(pmef, pmef_i)
  }
}

### choose 100, 106, 107, 110, 116, 117, for road type 4 and 5, choose year 2010 onward  # nolint
pmef_final = pmef[which(pmef$pollutantID == 100 | pmef$pollutantID == 106 | pmef$pollutantID == 107 |  # nolint #three types of PM10
                    pmef$pollutantID == 110 | pmef$pollutantID == 116 | pmef$pollutantID == 117),] # nolint #three types of PM2.5
pmef_final <- pmef_final[which(pmef_final$roadTypeID > 1), ] #only select road type, discard off-road network #nolint
pmef_final <- pmef_final[which(pmef_final$modelYearID >= 2010), ] #only select 2010 and onward because of the availability of actual vehicle share #nolint
pmef_final <- pmef_final[which(pmef_final$fuelTypeID != 5 & pmef_final$fuelTypeID != 3),] # nolint #only consider 1-gasoline, 2-diesel, and 9-electric #nolint

#####take the share of Ontario vehicle
agedist_on = read.csv(paste(inputpath, "\\0-vehshare_Ontario.csv", sep = ""), header = T) # nolint
year <- unique(agedist_on$REF_DATE)

ldvshare <- data.frame(matrix(0, nrow = length(year), ncol = 3))
colnames(ldvshare) <- c("vehTyep", "yearID", "percentage")
ldvshare$vehType <- "Passenger cars"

ldtshare <- data.frame(matrix(0, nrow = length(year), ncol = 3))
colnames(ldtshare) <- c("vehTyep", "yearID", "percentage")
ldtshare$vehType <- "Light trucks"

hdvshare <- data.frame(matrix(0, nrow = length(year), ncol = 3))
colnames(hdvshare) <- c("vehTyep", "yearID", "percentage")
hdvshare$vehType <- "Heavy trucks"

busshare <- data.frame(matrix(0, nrow = length(year), ncol = 3))
colnames(busshare) <- c("vehTyep", "yearID", "percentage")
busshare$vehType <- "Buses"

## Calculate age distribution for four types of vehicles
for (i in 1:length(year)) { # nolint
  ldvshare$yearID[i] <- year[i]
  ldvshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == "Passenger cars" &  # nolint
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == "Passenger cars")]) # nolint
  ldtshare$yearID[i] <- year[i]
  ldtshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == "Light trucks" &  # nolint
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == "Light trucks")]) # nolint
  hdvshare$yearID[i] <- year[i]
  hdvshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == "Heavy trucks" &  # nolint
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == "Heavy trucks")]) # nolint
  busshare$yearID[i] <- year[i]
  busshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == "Buses" &  # nolint
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == "Buses")]) # nolint
}
vehageshare <- rbind(ldvshare, ldtshare, hdvshare, busshare)
colnames(vehageshare) <- c("vehTyep", "yearID", "percentage")

### Calculate vehicle type distribution, four types
vehtypeshare <- data.frame(matrix(0, nrow = 4, ncol = 2))
colnames(vehtypeshare) = c("vehtype", "percentage") # nolint # nolint
vehtype <- unique(agedist_on$Vehicle.type)
for (i in 1:length(vehtype)) { # nolint
  vehtypeshare$vehtype[i] <- vehtype[i]
  vehtypeshare$percentage[i] = sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == vehtype[i])])/sum(agedist_on$VALUE) # nolint
}

# Write down the shares
write.csv(busshare, paste(outputpath, "\\0-busshare.csv", sep = "")) # nolint
write.csv(ldvshare, paste(outputpath, "\\0-ldvshare.csv", sep = "")) # nolint # nolint
write.csv(ldtshare, paste(outputpath, "\\0-ldtshare.csv", sep = "")) # nolint
write.csv(hdvshare, paste(outputpath, "\\0-hdvshare.csv", sep = "")) # nolint # nolint
write.csv(vehtypeshare, paste(outputpath, "\\0-typeshare.csv", sep = "")) # nolint

## The fuel share of Ontario. Only consider gasoline, diesel, and electricity (refer to stats canada) # nolint
fuelshare = data.frame(matrix(c(0.320020481, 2, 0.002560164, 9, 0.677419355, 1), nrow =3, byrow = T)) # nolint
colnames(fuelshare) = c("fuelshare", "fueltype") #1: gasoline, 2: diesel, 9: electricity # nolint

# Categorize MOVES veh type to Ontario veh type
pmef_final$vehtype <- 0
pmef_final$vehtype[which(pmef_final$sourceTypeID == 21)] <- "Passenger cars"
pmef_final$vehtype[which(pmef_final$sourceTypeID <=32 & pmef_final$sourceTypeID > 21)] <- "Light trucks" # nolint
pmef_final$vehtype[which(pmef_final$sourceTypeID <=43 & pmef_final$sourceTypeID > 32)] <- "Buses" # nolint
pmef_final$vehtype[which(pmef_final$sourceTypeID <=62 & pmef_final$sourceTypeID > 43)] <- "Heavy trucks" # nolint

## For each vehicle type, fuel type, pollutant, road type, and link, calculated a weighted emission factor # nolint
# Get unique value of those listed variables
source <- unique(pmef_final$sourceTypeID) # Veh type from MOVES
veh <- unique(pmef_final$vehtype) # Veh type by Ontario vehicle type
fuel <- unique(pmef_final$fuelTypeID) # Fuel type
road <- unique(pmef_final$roadTypeID) # Road type
age <- unique(pmef_final$modelYearID) # Vehicle age
link <- unique(pmef_final$linkID) # Link, or speed in mph
pollutant <- unique(pmef_final$pollutantID) # Pollutant type

### 2. Calculate accumulated emissions, weighted by vehicle, fuel, and age ratio
veh_fuel_age_weighted_ef <- function(road, link, pollutant, vehageshare, vehtypeshare, fuelshare, efdb, moves, emfac) { # nolint
  ### Create a new matrix to store aggregated emission factor -- weighted by vehicle type and fuel type # nolint
  pmef_tor = data.frame(matrix(0, nrow = length(road)*length(link)*length(pollutant), ncol = 4)) # nolint
  colnames(pmef_tor) <- c("linkID", "pollutantID", "roadTypeID", "aggregatedEF") # nolint

  if (moves == 1) {
    vehageshare$percentage[which(vehageshare$vehType == "Light trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/2 # nolint
    vehageshare$percentage[which(vehageshare$vehType == "Heavy trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/5 #nolint
    vehageshare$percentage[which(vehageshare$vehType == "Buses")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/3 #nolint
  }
  row <- 0 #redirect to the specific row to fill in the pmef
  for (p in pollutant) { # nolint
    for (r in road) {
      for (l in link) {
        weightef <- 0
        row <- row + 1
        if (moves == 1) {
          source <- unique(efdb$sourceTypeID)
          fuel <- unique(pmef_final$fuelTypeID)
          for (s in source) {
            #define the share of vehicle
            if (s == 21) {
              v <- "Passenger cars"
            } else if (s > 21 && s <= 32) {
              v <- "Light trucks"
            } else if (s > 32 && s <= 43) {
              v <- "Buses"
            } else {
              v <- "Heavy trucks"
            }
          }
        }
        for (f in fuel) {
          for (a in age) {
            findrow <- which(pmef_final$linkID == l &
                              pmef_final$pollutantID == p & # nolint
                              pmef_final$sourceTypeID == s &
                              pmef_final$fuelTypeID == f &
                              pmef_final$roadTypeID == r &
                              pmef_final$modelYearID == a)
            if (length(findrow) == 0) {
              weightef <- weightef
            } else {
            weightef <- weightef +  vehageshare$percentage[which(vehageshare$yearID == a & vehageshare$vehType == v)]* # nolint # age ratio
            fuelshare$fuelshare[which(fuelshare$fueltype == f)] *  # nolint # fuel ratio
            vehtypeshare$percentage[which(vehtypeshare$vehtype == v)] * pmef_final$emissionQuant[findrow] # nolint # type ratio
            }
          }
        }
      }
      pmef_tor[row, ] <- cbind(l, p, r, weightef)
      if (row %% 100 == 0) {
        print(paste("This is row:", row, "speed, pollutant, road, ef are:", l, p, r, weightef)) # nolint
      }
    }
  }
  return(pmef_tor)
}

## Apply the function
pmef_tor <- veh_fuel_age_weighted_ef(road, link, pollutant, vehageshare, vehtypeshare, fuelshare, pmef_final, 1, 0) #nolint

# Write down the aggregated emission factor
write.xlsx(pmef_tor, paste(outputpath, "\\0_averagespeed_torontovehshare_allPM.xlsx", sep = "")) # nolint

#pmef_2020 = pmef_final[which(pmef_final$modelYearID>=2020),] # nolint
#vehtype_source = list("Passenger cars" = 21, "Light trucks" = c(31,32), "Heavy trucks" = c(51,52,53,61,62), "Buses" = c(41, 42, 43)) # nolint