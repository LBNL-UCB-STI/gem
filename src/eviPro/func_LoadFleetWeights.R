# Author: Schatz Energy Research Center
# Original Version: Jerome Carman
# Edits: Jerome Carman and/or Doug Saucedo
# Version: 1.1
# Date: August 6, 2018
# Description: load .csv file containing weights of each fleet characteristic
# Input Variables
#   weights_file: location of .csv file containing weights for each fleet characteristic. The .csv file must have the following structure:
#       Must have 12 columns of data, 2 columns for each of six fleet characteristics in the following order from left to right
#           pev_type, preferred_loc, power_home, power_work, power_public, schedule_vmt_bin
#       The two columns of each fleet characteristic must have the following structure
#           Header row: "name", "weight"
#           All other rows:
#               Name for each option in the first column of each fleet characteristic
#                   With the exception of schedule_vmt_bin, must be of type character. Choose something short but descriptive.
#                       The "name" column for schedule_vmt_bin must be of type integer, with the integer representing the right bin edge in units of miles
#               Associated weight in the second column of each fleet characteristic
#                   Must be a decimal number.
#                   Cannot have a blank value. If an option has a weight of zero, a zero must be present.
#                   The sum of all weights must equal 1.
#       The number of rows of information do not need to be equal across all fleet characteristics. Each can have a different number of options and associated weights.
# Version Changes
#   1.1: jkc: added vmt_weights

load_fleet_weights <- function(weights_file) {
  fleet_weights <- as.list(fread(weights_file))
  
  for(i in 1:length(fleet_weights)) {
    fleet_weights[[i]] <- fleet_weights[[i]][fleet_weights[[i]]!=""]
    fleet_weights[[i]] <- fleet_weights[[i]][!is.na(fleet_weights[[i]])]
  }
  
  pev_weights <- as.data.table(fleet_weights[1:2])
  pref_weights <- as.data.table(fleet_weights[3:4])
  home_weights <- as.data.table(fleet_weights[5:6])
  work_weights <- as.data.table(fleet_weights[7:8])
  public_weights <- as.data.table(fleet_weights[9:10])
  vmt_weights <- as.data.table(fleet_weights[11:12])
  
  return(list(pev_weights=pev_weights,pref_weights=pref_weights,home_weights=home_weights,work_weights=work_weights,public_weights=public_weights,vmt_weights=vmt_weights))
}
