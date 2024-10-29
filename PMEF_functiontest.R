### Create a new matrix to store aggregated emission factor -- weighted by vehicle type and fuel type # nolint
pmef_local = data.frame(matrix(0, nrow = length(road)*length(link)*length(pollutant), ncol = 4)) # nolint
colnames(pmef_local) <- c("linkID", "pollutantID", "roadTypeID", "aggregatedEF") # nolint

# Only MOVES distinguish road type
if (moves == 1 && is.null(road)) {
  stop("road category is required when moves equals 1.")
}

fuel <- unique(efdb$fuelTypeID)
age <- unique(efdb$modelYearID)
veh <- unique(efdb$vehtype)

#if (emfac == 1) {
#  road <- 5
#  vehageshare$percentage[which(vehageshare$vehType == "Light trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")])#/length(which(vehlookup$vehLocal == "Light trucks")) # nolint
#  vehageshare$percentage[which(vehageshare$vehType == "Heavy trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Heavy trucks")])#/length(which(vehlookup$vehLocal == "Heavy trucks")) #nolint
#  vehageshare$percentage[which(vehageshare$vehType == "Buses")] <- vehageshare$percentage[which(vehageshare$vehType == "Buses")])#/length(which(vehlookup$vehLocal == "Buses")) #nolint
#}

#if (moves == 1) {
#  vehageshare$percentage[which(vehageshare$vehType == "Light trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/2 # nolint
#  vehageshare$percentage[which(vehageshare$vehType == "Heavy trucks")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/5 #nolint
#  vehageshare$percentage[which(vehageshare$vehType == "Buses")] <- vehageshare$percentage[which(vehageshare$vehType == "Light trucks")]/3 #nolint
#}

row <- 0 #redirect to the specific row to fill in the pmef
for (p in pollutant) { # nolint
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
              weightef <- weightef +  vehageshare$percentage[which(vehageshare$yearID == a & vehageshare$vehType == s)]* # nolint # age ratio
                fuelshare$fuelshare[which(fuelshare$fueltype == f)] *  # nolint # fuel ratio
                vehtypeshare$percentage[which(vehtypeshare$vehType == s)] * mean(efdb$emissionQuant[findrow]) # nolint # type ratio
            }
          }
        }
      }
      pmef_local[row, ] <- cbind(l, p, r, weightef)
      print(paste("This is row:", row, "speed, pollutant, road, ef are:", l, p, r, weightef)) # nolint
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
            weightef <- weightef +  vehageshare$percentage[which(vehageshare$yearID == a & vehageshare$vehType == s)]* # nolint # age ratio
              fuelshare$fuelshare[which(fuelshare$fueltype == f)] *  # nolint # fuel ratio
              vehtypeshare$percentage[which(vehtypeshare$vehType == s)] * mean(efdb$emissionQuant[findrow]) # nolint # type ratio
          }
        }
      }
    }
    pmef_local[row, ] <- cbind(l, p, r, weightef)
    print(paste("This is row:", row, "speed, pollutant, road, ef are:", l, p, r, weightef)) # nolint
  }
}
