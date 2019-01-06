#==================================================================================================
# FUNCTION measureFleetMetrics
# Calculate all weights of interest for a generated fleet.
#
# VERSION - AUTHOR: 
#   BETA 20180914 - DRS
#   2018-09-24 - JKC added code to calculate all weight variables for EVI, and added code to weightVMTBins for BEAM and EVI.
#                Confirmed working. The structure of EVI weights allows for returning weights of zero, and works if there are
#                no zero weights for a variable. The vmt weight code was greatly simplified using table()
#   2018-10-19 - ARH archived previous version of measureFleetWeights. New version does not have the "BEAM" option and the 
#                modelSource input.
#
#==================================================================================================
measureFleetWeights <- function(activity){
  
  result <- list(vmt_weights = weightVMTBins(activity))
  
  # Characterize the Weekday and Weekend Fleet Separately
  result$weekday <- measureEVIWeights_DoW(activity,'weekday')
  result$weekend <- measureEVIWeights_DoW(activity,'weekend')
  
  return(result)
}

#======================================================================================================
# FUNCTION weightVMTBins
# For the fleet activity, create the weighting data table for the schedule_vmt_bin
weightVMTBins <- function(activity){
  temp_dt <- as.data.table(activity[!duplicated(fleet_id) & (day_of_week == 'weekday'),table(schedule_vmt_bin)])
  temp_dt[,weight:= N/sum(N)][,N:=NULL]
  setnames(temp_dt,'schedule_vmt_bin','name')
  return(temp_dt)
}

#======================================================================================================
# FUNCTION weightVMTBins
measureEVIWeights_DoW <- function(activity,DoW){
  result <- list(fleet_size = activity[!duplicated(fleet_id) & (day_of_week == DoW),.N])
  result$nrg_cons = activity[(day_of_week == DoW),sum(kwh)]
  result$elec_cons_avg <- 0.325
  result$pev_weights <- as.data.table(activity[!duplicated(fleet_id) & (day_of_week == DoW),table(pev_type)])
  result$pev_weights <- result$pev_weights[,weight:= N/sum(N)][,N:=NULL]
  setnames(result$pev_weights,'pev_type','name')
  
  result$pref_weights <- as.data.table(activity[!duplicated(fleet_id) & (day_of_week == DoW),table(preferred_loc)])
  result$pref_weights <- result$pref_weights[,weight:= N/sum(N)][,N:=NULL]
  setnames(result$pref_weights,'preferred_loc','name')
  
  result$home_weights <- as.data.table(activity[!duplicated(fleet_id) & (day_of_week == DoW),table(power_home)])
  result$home_weights <- result$home_weights[,weight:= N/sum(N)][,N:=NULL]
  setnames(result$home_weights,'power_home','name')
  
  result$work_weights <- as.data.table(activity[!duplicated(fleet_id) & (day_of_week == DoW),table(power_work)])
  result$work_weights <- result$work_weights[,weight:= N/sum(N)][,N:=NULL]
  setnames(result$work_weights,'power_work','name')
  
  result$public_weights <- as.data.table(activity[!duplicated(fleet_id) & (day_of_week == DoW),table(power_public)])
  result$public_weights <- result$public_weights[,weight:= N/sum(N)][,N:=NULL]
  setnames(result$public_weights,'power_public','name')
  
  result$vmt_weights <- as.data.table(activity[!duplicated(fleet_id) & (day_of_week == DoW),table(schedule_vmt_bin)])
  result$vmt_weights <- result$vmt_weights[,weight:= N/sum(N)][,N:=NULL]
  setnames(result$vmt_weights,'schedule_vmt_bin','name')
  
  return(result)
}
