#### calculate aggregated emission rate
# preparation file: OpMode Distribution and linkdata
# through comparison, we found no matter how the link average speed changes, if provided a comprehensive opmode distribution file, the result will be in g/hr
# what I found different with previous versions: maybe I should add all the emissions in different polProcess?
# GWP: GHG(90): 1; CH4(5): 25; N2O(6): 298
library('openxlsx')

emissionratepath = 'D:\\微云同步助手\\332667113\\2025-省基金申请-NEE\\0-研究进程'
er_opmode = read.xlsx(paste(emissionratepath, '\\MOVES4_output\\toronto_opmode_pc_CO2NOxPM\\',
                            'opmode_nov_95_pc_crosschecked.xlsx', sep=''), sheet = 'added_opmode')

# emission factor is a separate value of each year model (31 year models)
er_opmode$emissionQuant = er_opmode$emissionQuant * 31

# emission rate table should be: different model year, 23 operating mode, CO2 (90-5-6), NOx (3), PM2.5 (112+118+115+118)
# delete link 208 which corresponds to nonroad
er_opmode = er_opmode[which(er_opmode$linkID < 208),]
year = unique(er_opmode$modelYearID)
opmode = unique(er_opmode$OpMode)
er_aggregate = data.frame(matrix(0, nrow = length(year)*length(opmode), ncol = 5))
colnames(er_aggregate) = c('VehYear', 'OpMode', 'CO2eq', 'NOx', 'PM2_5')


row = 0
for (i in 1:length(year)) {
  for (j in 1:length(opmode)) {
    row = row + 1
    er_aggregate$VehYear[row] = year[i]
    er_aggregate$OpMode[row] = opmode[j]
    er_aggregate$CO2eq[row] = (sum(er_opmode$emissionQuant[which(er_opmode$modelYearID == year[i] &
                                                                  er_opmode$OpMode == opmode[j] &
                                                                  er_opmode$pollutantID == 90)]) +
      25 * sum(er_opmode$emissionQuant[which(er_opmode$modelYearID == year[i] &
                                          er_opmode$OpMode == opmode[j] &
                                          er_opmode$pollutantID == 5)]) + 
      298 * sum(er_opmode$emissionQuant[which(er_opmode$modelYearID == year[i] &
                                               er_opmode$OpMode == opmode[j] &
                                               er_opmode$pollutantID == 6)]))/3600 # convert to g/s
    er_aggregate$NOx[row] =  sum(er_opmode$emissionQuant[which(er_opmode$modelYearID == year[i] &
                                                                      er_opmode$OpMode == opmode[j] &
                                                                      er_opmode$pollutantID == 3)])/3600 
    er_aggregate$PM2_5[row] =  (sum(er_opmode$emissionQuant[which(er_opmode$modelYearID == year[i] &
                                                                        er_opmode$OpMode == opmode[j] &
                                                                        er_opmode$pollutantID == 112)]) +
      sum(er_opmode$emissionQuant[which(er_opmode$modelYearID == year[i] &
                                               er_opmode$OpMode == opmode[j] &
                                               er_opmode$pollutantID == 115)]) + 
      sum(er_opmode$emissionQuant[which(er_opmode$modelYearID == year[i] &
                                          er_opmode$OpMode == opmode[j] &
                                          er_opmode$pollutantID == 118)]) +
      sum(er_opmode$emissionQuant[which(er_opmode$modelYearID == year[i] &
                                          er_opmode$OpMode == opmode[j] &
                                          er_opmode$pollutantID == 119)]))/3600
  }
}

write.xlsx(er_aggregate, file = paste(emissionratepath, '\\MOVES4_output\\toronto_opmode_pc_CO2NOxPM\\emissionrate_g_s_passengercar.xlsx', sep=''))
