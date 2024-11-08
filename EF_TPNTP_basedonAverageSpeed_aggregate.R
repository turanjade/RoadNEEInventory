#file_nj = "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\非尾气-刹车磨损排放\\0-研究进程\\非尾气排放数据\\南京市网约车数据\\2019-10-17.csv" #   #  
#vehtraj = read.csv(file_nj, header = T, sep = ",") #  

####
#1) different vehicle types (LDV, LD-Truck, buses, HDV),
#2) different speed bins on varied road types (arterial vs expressway),
#3) exhaust vs NEE (including BWP and TWP), for both PM2.5 and PM10.
#4) EF among different models, MOVES, EMFAC, COPERT, HBEFA, and a range of EFs
#5) Road emission estimation using the range of EFs.  #  
#6) Compare the modeling result with measurement to check the alignment and derive the insufficiency of current models. #  

#colnames(pmef) from MOVES output #  
#[1] "MOVESRunID"         "iterationID"        "yearID"             "monthID"
#[5] "dayID"              "hourID"             "stateID"            "countyID"
#[9] "zoneID"             "linkID"             "pollutantID"        "processID"          #  
#[13] "sourceTypeID"       "regClassID"         "fuelTypeID"         "fuelSubTypeID"      #  
#[17] "modelYearID"        "roadTypeID"         "SCC"                "engTechID"          #  
#[21] "sectorID"           "hpID"               "emissionQuant"      "emissionQuantMean"  #  
#[25] "emissionQuantSigma"

library("openxlsx") #  

rootpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程"
movesefpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\MOVES4_output\\toronto_avgspeeed_pc_allPM" #  

inputpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\EstimationDataInput" #  
outputpath <- "D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程\\2024-UofT_NEE reports\\ProcessingOutput" #  

setwd(outputpath) #  

############################################################################
############################################################################
############################################################################

### 1. Calculate accumulated emissions, weighted by vehicle, fuel, and age ratio
veh_fuel_age_weighted_ef <- function(road = NULL, link, pollutant, vehageshare, vehtypeshare, fuelshare, efdb, moves, emfac) { #  
  ### Create a new matrix to store aggregated emission factor -- weighted by vehicle type and fuel type #  
  pmef_local = data.frame(matrix(0, nrow = length(road)*length(link)*length(pollutant), ncol = 4)) #  
  colnames(pmef_local) <- c("linkID", "pollutantID", "roadTypeID", "aggregatedEF") #  
  
  # Only MOVES distinguish road type
  if (moves == 1 && is.null(road)) {
    stop("road category is required when moves equals 1.")
  }
  
  fuel <- unique(efdb$fuelTypeID)
  age <- unique(efdb$modelYearID)
  source <- unique(efdb$sourceTypeID)
  
  if (emfac == 1) {
   road <- 5
#    vehageshare$percentage[which(vehageshare$vehType == "Light trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/length(which(vehlookup$vehLocal == "Light trucks")) #  
#    vehageshare$percentage[which(vehageshare$vehType == "Heavy trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Heavy trucks")]/length(which(vehlookup$vehLocal == "Heavy trucks")) # 
#    vehageshare$percentage[which(vehageshare$vehType == "Buses")] <- vehageshare$percentage[which(vehageshare$vehType == "Buses")]/length(which(vehlookup$vehLocal == "Buses")) # 
  }
#  
#  if (moves == 1) {
#    vehageshare$percentage[which(vehageshare$vehType == "Light trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/2 #  
#    vehageshare$percentage[which(vehageshare$vehType == "Heavy trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/5 # 
#    vehageshare$percentage[which(vehageshare$vehType == "Buses")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/3 # 
#  }
  
  row <- 0 #redirect to the specific row to fill in the pmef
  for (p in pollutant) { #  
    for (r in road) {
      for (l in link) {
        weightef <- 0
        row <- row + 1
        for (s in veh) {
          #lookup the local vehicle category
          # v <- unique(vehlookup$vehLocal[which(vehlookup$vehDB == s)])
          for (f in fuel) {
            for (a in age) {
              findrow <- which(efdb$linkID == l &
                                 efdb$pollutantID == p &
                                 efdb$vehtype == s &
                                 efdb$fuelTypeID == f &
                                 efdb$roadTypeID == r &
                                 efdb$modelYearID == a)
              if (length(findrow) == 0) {
                weightef <- weightef
              } else {
                weightef <- weightef +  vehageshare$percentage[which(vehageshare$yearID == a & vehageshare$vehType == s)]* #   # age ratio
                  fuelshare$fuelshare[which(fuelshare$fueltype == f)] *  #   # fuel ratio
                  vehtypeshare$percentage[which(vehtypeshare$vehType == s)] * mean(efdb$emissionQuant[findrow]) #   # type ratio
              }
            }
          }
        }
        pmef_local[row, ] <- cbind(l, p, r, weightef)
        print(paste("This is row:", row, "speed, pollutant, road, ef are:", l, p, r, weightef)) #  
      }
    }
  }
  
  ### calculate PMTW specifically
  if (emfac == 1) {
    # v <- unique(vehlookup$vehLocal[which(vehlookup$vehDB == s)])
    for (p in pollutant) {
      weightef <- 0
      row <- row + 1
      for (s in veh) {
        for (a in age) {
          for (f in fuel) {
            findrow <- which(efdb$process == 'PMTW' &
                               efdb$pollutantID == p &
                               efdb$vehtype == s &
                               efdb$fuelTypeID == f &
                               efdb$roadTypeID == r &
                               efdb$modelYearID == a)
            if (length(findrow) == 0) {
              weightef <- weightef
            } else {
              weightef <- weightef +  vehageshare$percentage[which(vehageshare$yearID == a & vehageshare$vehType == s)]* #   # age ratio
                fuelshare$fuelshare[which(fuelshare$fueltype == f)] *  #   # fuel ratio
                vehtypeshare$percentage[which(vehtypeshare$vehType == s)] * mean(efdb$emissionQuant[findrow]) #   # type ratio
            }
          }
        }
      }
      pmef_local[row, ] <- cbind(l, p, r, weightef)
      print(paste("This is row:", row, "speed, pollutant, road, ef are:", l, p, r, weightef)) #  
    }
  }
  pmef_local = pmef_local[which(pmef_local$aggregatedEF > 0),]
  return(pmef_local)
}


############################################################################
############################################################################
############################################################################

#### 2. Data preparation
### 2.1 Ontario specific
### Take the share of Ontario vehicle
agedist_on = read.csv(paste(inputpath, "\\0-vehshare_Ontario.csv", sep = ""), header = T) #  
year <- unique(agedist_on$REF_DATE)

ldvshare <- data.frame(matrix(0, nrow = length(year), ncol = 3))
colnames(ldvshare) <- c("vehType", "yearID", "percentage")
ldvshare$vehType <- "Passenger cars"
ldtshare <- data.frame(matrix(0, nrow = length(year), ncol = 3))
colnames(ldtshare) <- c("vehType", "yearID", "percentage")
ldtshare$vehType <- "Light trucks"
hdvshare <- data.frame(matrix(0, nrow = length(year), ncol = 3))
colnames(hdvshare) <- c("vehType", "yearID", "percentage")
hdvshare$vehType <- "Heavy trucks"
busshare <- data.frame(matrix(0, nrow = length(year), ncol = 3))
colnames(busshare) <- c("vehType", "yearID", "percentage")
busshare$vehType <- "Buses"

## Calculate age distribution for four types of vehicles
for (i in 1:length(year)) { #  
  ldvshare$yearID[i] <- year[i]
  ldvshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == "Passenger cars" &  #  
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == "Passenger cars")]) #  
  ldtshare$yearID[i] <- year[i]
  ldtshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == "Light trucks" &  #  
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == "Light trucks")]) #  
  hdvshare$yearID[i] <- year[i]
  hdvshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == "Heavy trucks" &  #  
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == "Heavy trucks")]) #  
  busshare$yearID[i] <- year[i]
  busshare$percentage[i] = agedist_on$VALUE[which(agedist_on$Vehicle.type == "Buses" &  #  
                                                    agedist_on$REF_DATE == year[i])]/sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == "Buses")]) #  
}
vehageshare <- rbind(ldvshare, ldtshare, hdvshare, busshare)
colnames(vehageshare) <- c("vehType", "yearID", "percentage")

### Calculate vehicle type distribution, four types
vehtypeshare <- data.frame(matrix(0, nrow = 4, ncol = 2))
colnames(vehtypeshare) = c("vehType", "percentage") #  
vehtype <- unique(agedist_on$Vehicle.type)
for (i in 1:length(vehtype)) { #  
  vehtypeshare$vehType[i] <- vehtype[i]
  vehtypeshare$percentage[i] = sum(agedist_on$VALUE[which(agedist_on$Vehicle.type == vehtype[i])])/sum(agedist_on$VALUE) #  
}

### Write down the shares
write.csv(busshare, paste(outputpath, "\\0-busshare.csv", sep = "")) #  
write.csv(ldvshare, paste(outputpath, "\\0-ldvshare.csv", sep = "")) #  
write.csv(ldtshare, paste(outputpath, "\\0-ldtshare.csv", sep = "")) #  
write.csv(hdvshare, paste(outputpath, "\\0-hdvshare.csv", sep = "")) #  
write.csv(vehtypeshare, paste(outputpath, "\\0-typeshare.csv", sep = "")) #  

## The fuel share of Ontario. Only consider gasoline, diesel, and electricity (refer to stats canada) #  
fuelshare = data.frame(matrix(c(0.320020481, 2, 0.002560164, 9, 0.677419355, 1), nrow =3, byrow = T)) #  
colnames(fuelshare) = c("fuelshare", "fueltype") #1: gasoline, 2: diesel, 9: electricity #  

############################################################################
############################################################################
############################################################################
### 2.2 MOVES EF
##### List all the emission rate data that are retrieved from MOVES database
filelist <- list.files(paste(movesefpath, "\\0_averagespeed_output_allPM", sep = "")) # 

#### agedist is the vehicle age distribution initially set in MOVES. In MOVES, we specify the output of different vehicle ages, and the result is proportional to the actual result from that year. An ajustment is required (see the code in the following loop) #  
agedist = read.xlsx(paste(movesefpath, "\\source21\\agedist_21_31yearsto2020.xlsx", sep = "")) #   # we use the same age distribution in MOVES execution
agedist$age <- agedist$yearID - agedist$ageID

#### read all the CSV files from MOVES and merge them into one file
pmef <- data.frame(matrix(0, nrow = 0, ncol = 7))
colnames(pmef) <- c("linkID","pollutantID", "sourceTypeID", "fuelTypeID", "modelYearID", "roadTypeID", "emissionQuant") #  
# LinkID corresponds to vehicle speed, will assign the value later after
# EmissionQuant equals to the emissionQuant / ratio(vehicle in that age) / 10 (10 is the length of each link, miles) #  
for (i in 1:length(filelist)) { #  
  if (grepl(".csv", filelist[i]) == TRUE) { #  
    pmef_i <- read.csv(paste(movesefpath, "\\0_averagespeed_output_allPM\\", filelist[i], sep = "")) #   #pmef_i is the ith ef file
    vehage <- unique(pmef_i$modelYearID)
    for (j in 1:length(vehage)) { #  
      pmef_i$emissionQuant[which(pmef_i$modelYearID == vehage[j])] <-
        pmef_i$emissionQuant[which(pmef_i$modelYearID == vehage[j])]*31/10/ # original emissionQuant is an average of 31 year models of vehicles; 10 is the link length (miles)
        agedist$ageFraction[which(agedist$age == vehage[j])] #   #adjust emissions based on vehicle age proportion and link length (10 miles)
    }
    pmef_i <- pmef_i[, c(10, 11, 13, 15, 17, 18, 23)]
    pmef_i$sourceTypeID <- strsplit(filelist[i], "_")[[1]][2]
    colnames(pmef_i) = c("linkID","pollutantID", "sourceTypeID", "fuelTypeID", "modelYearID", "roadTypeID", "emissionQuant") #  
    pmef <- rbind(pmef, pmef_i)
  }
}

### Choose 100, 106, 107, 110, 116, 117, for road type 4 and 5, choose year 2010 onward  #  
pmef_moves = pmef[which(pmef$pollutantID == 100 | pmef$pollutantID == 106 | pmef$pollutantID == 107 |  #   #three types of PM10
                    pmef$pollutantID == 110 | pmef$pollutantID == 116 | pmef$pollutantID == 117),] #   #three types of PM2.5
pmef_moves <- pmef_moves[which(pmef_moves$roadTypeID > 1), ] #only select road type, discard off-road network # 
pmef_moves <- pmef_moves[which(pmef_moves$modelYearID >= 2010), ] #only select 2010 and onward because of the availability of actual vehicle share # 
pmef_moves <- pmef_moves[which(pmef_moves$fuelTypeID != 5 & pmef_moves$fuelTypeID != 3),] #   #only consider 1-gasoline, 2-diesel, and 9-electric # 

# Categorize MOVES veh type to Ontario veh type
pmef_moves$vehtype <- 0
pmef_moves$vehtype[which(pmef_moves$sourceTypeID == 21)] <- "Passenger cars"
pmef_moves$vehtype[which(pmef_moves$sourceTypeID <=32 & pmef_moves$sourceTypeID > 21)] <- "Light trucks" #  
pmef_moves$vehtype[which(pmef_moves$sourceTypeID <=43 & pmef_moves$sourceTypeID > 32)] <- "Buses" #  
pmef_moves$vehtype[which(pmef_moves$sourceTypeID <=62 & pmef_moves$sourceTypeID > 43)] <- "Heavy trucks" #  

veh <- unique(pmef_moves$sourceTypeID)
vehlookup <- data.frame(matrix(0, nrow = length(veh), ncol = 2))
colnames(vehlookup) <- c("vehLocal", "vehDB")
for (i in 1:length(veh)) { # 
  if (veh[i] == 21) {
    vehlookup$vehLocal[i] <- "Passenger cars"
    vehlookup$vehDB[i] <- veh[i]
  } else if (veh[i] <= 32 && veh[i] > 21) {
    vehlookup$vehLocal[i] <- "Light trucks"
    vehlookup$vehDB[i] <- veh[i]
  } else if (veh[i] <= 43 && veh[i] > 32) {
    vehlookup$vehLocal[i] <- "Buses"
    vehlookup$vehDB[i] <- veh[i]
  } else if (veh[i] <= 62 && veh[i] > 43) {
    vehlookup$vehLocal[i] <- "Heavy trucks"
    vehlookup$vehDB[i] <- veh[i]
  }
}
## For each vehicle type, fuel type, pollutant, road type, and link, calculated a weighted emission factor #  
# Get unique value of those listed variables
# source <- unique(pmef_moves$sourceTypeID) # Veh type from MOVES # 
# veh <- unique(pmef_moves$vehtype) # Veh type by Ontario vehicle type # 
# fuel <- unique(pmef_moves$fuelTypeID) # Fuel type # 
road <- unique(pmef_moves$roadTypeID) # Road type
# age <- unique(pmef_moves$modelYearID) # Vehicle age # 
link <- unique(pmef_moves$linkID) # Link, or speed in mph
pollutant <- unique(pmef_moves$pollutantID) # Pollutant type

## Apply the function
pmef_tor_moves <- veh_fuel_age_weighted_ef(road, link, pollutant, vehageshare, vehtypeshare, fuelshare, pmef_moves, 1, 0) # 

# Write down the aggregated emission factor
write.xlsx(pmef_tor_moves, paste(outputpath, "\\0_averagespeed_torontovehshare_allPMEF_MOVES.xlsx", sep = "")) #  

############################################################################
############################################################################
############################################################################
### 2.3 EMFAC EF: take LA as a sample
pmef_emfac <- read.xlsx(paste(rootpath, "\\EMFAC2021_output\\PL_Los Angeles (SC)_2023_November_20241011044227.xlsx", sep = "")) # 

### Only consider gasoline, diesel, and electric
pmef_emfac <- pmef_emfac[which(pmef_emfac$fuel == "Gas" | pmef_emfac$fuel == "Dsl" | pmef_emfac$fuel == "Elec"), ] # 
## convert gasoline=1, diesel=2, electric=9
pmef_emfac$fuel[which(pmef_emfac$fuel == "Gas")] <- 1
pmef_emfac$fuel[which(pmef_emfac$fuel == "Dsl")] <- 2
pmef_emfac$fuel[which(pmef_emfac$fuel == "Elec")] <- 9

### Convert vehicle type to Toronto type
# see PDF for detailed categorization in California: https://ww2.arb.ca.gov/sites/default/files/2021-01/EMFAC202x_Users_Guide_01112021_final.pdf # 
# delete undesired categories
pmef_emfac <- pmef_emfac[which(pmef_emfac$vehicle_class != "MH" & pmef_emfac$vehicle_class != "MCY"), ] # 
pmef_emfac$veh <- 0
pmef_emfac$veh[which(pmef_emfac$vehicle_class == "SBUS" | pmef_emfac$vehicle_class == "UBUS" | pmef_emfac$vehicle_class == "Motor Coach" | # 
                    pmef_emfac$vehicle_class == "OBUS" | pmef_emfac$vehicle_class == "All Other Buses")] <- "Buses" # 
pmef_emfac$veh[which(pmef_emfac$vehicle_class == "LDA")] <- "Passenger cars" # 
pmef_emfac$veh[which(pmef_emfac$vehicle_class == "LDT1" | pmef_emfac$vehicle_class == "LDT2" | pmef_emfac$vehicle_class == "MDV" | # 
                    pmef_emfac$vehicle_class == "LHD1" | pmef_emfac$vehicle_class == "LHD2")] <- "Light trucks" # 
pmef_emfac$veh[which(pmef_emfac$veh == 0)] <- "Heavy trucks"

### Too many types, add a look-up table
veh <- unique(pmef_emfac$vehicle_class)
vehlookup <- data.frame(matrix(0, nrow = length(veh), ncol = 2))
colnames(vehlookup) <- c("vehLocal", "vehDB")
for (i in 1:length(veh)) { # 
  if (veh[i] == "LDA") {
    vehlookup$vehLocal[i] <- "Passenger cars"
    vehlookup$vehDB[i] <- veh[i]
  } else if (veh[i] == "LDT1" | veh[i] == "LDT2" | veh[i] == "MDV" |
               veh[i] == "LHD1" | veh[i] == "LHD2") {
    vehlookup$vehLocal[i] <- "Light trucks"
    vehlookup$vehDB[i] <- veh[i]
  } else if (veh[i] == "SBUS" | veh[i] == "UBUS" | veh[i] == "Motor Coach" |
               veh[i] == "OBUS" | veh[i] == "All Other Buses") {
    vehlookup$vehLocal[i] <- "Buses"
    vehlookup$vehDB[i] <- veh[i]
  } else {
    vehlookup$vehLocal[i] <- "Heavy trucks"
    vehlookup$vehDB[i] <- veh[i]
  }
}

# Only include PM2.5 and PM10
pmef_emfac <- pmef_emfac[which(pmef_emfac$pollutant != "PM"), ]
# Only include TP, BWP, TWP
pmef_emfac <- pmef_emfac[which(pmef_emfac$process == "RUNEX" | pmef_emfac$process == "PMBW" | pmef_emfac$process == "PMTW"), ] # 

# Calculate average EF for vehicles within one type
age <- unique(pmef_emfac$model_year)
fuel <- unique(pmef_emfac$fuel)
speed <- unique(pmef_emfac$speed_time)
speed = speed[!is.na(speed)]
# pmef_emfac_avg <- data.frame(matrix(0, nrow, ncol = 7)) #  
pmef_emfac$process_pollutant <- paste(pmef_emfac$process, pmef_emfac$pollutant, sep = "_") #  
pollutant <- unique(pmef_emfac$process_pollutant)

# Add a synthetic road type 5 for all the EFs from EMFAC
pmef_emfac$roadTypeID <- 5

# rename emfac table
colnames(pmef_emfac) <- c("calendar_year",	"season_month",	"sub_area",
                       "sourceTypeID",	"fuelTypeID",	"modelYearID",
                       "temperature",	"relative_humidity",
                       "process",	"linkID",	"pollutant",	"emissionQuant",
                       "vehtype", "pollutantID", "roadTypeID")

## Apply the function
pmef_tor_emfac <- veh_fuel_age_weighted_ef(5, speed, pollutant, vehageshare, vehtypeshare, fuelshare, pmef_emfac, 0, 1) # 

# Write down the aggregated emission factor
write.xlsx(pmef_tor_emfac, paste(outputpath, "\\0_averagespeed_torontovehshare_allPMEF_EMFAC.xlsx", sep = "")) #  

#pmef_2020 = pmef_moves[which(pmef_moves$modelYearID>=2020),] #  
#vehtype_source = list("Passenger cars" = 21, "Light trucks" = c(31,32), "Heavy trucks" = c(51,52,53,61,62), "Buses" = c(41, 42, 43)) #  