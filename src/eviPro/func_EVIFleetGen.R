# Author: Schatz Energy Research Center
# Original Version: Jerome Carman
# Edits: Jerome Carman and/or Daug Saucedo and/or Andy Harris
# Version: 1.4
# Date: November 20, 2018
# Description: Loads a data table containing EVIPro model data from NREL, applies user defined weights of fleet characteristics, and generates a fleet of vids
# Required Variables
#   evi: data table created using the loadEVIPro() function. Must have the following columns
#       "schedule_vmt_bin","power_public","power_work","power_home","preferred_loc","pev_type","day_of_week","vid"
#       Values in these columns MUST equal the values in the "name" column in the weights data tables listed below
#   fleet_size: integer specifying the size of the desired fleet
#   pev_weights: data table of two columns ("name" and "weight") and 4 rows containing names and associated decimals (with a sum of 1) that represent the fraction of fleet vehicles comprised of each pev type
#   pref_weights: data table of two columns ("name" and "weight") and 2 rows containing names and associated decimals (with a sum of 1) that represent the fraction of fleet vehicles that prefer home or work charging
#   home_weights: data table of two columns ("name" and "weight") and 3 rows containing names and associated decimals (with a sum of 1) that represent the fraction of fleet vehicles that use each each type of home power
#   work_weights: data table of two columns ("name" and "weight") and 2 rows containing names and associated decimals (with a sum of 1) that represent the fraction of fleet vehicles that have access to each type of work power
#   public_weights: data table of two columns ("name" and "weight") and 2 rows containing names and associated decimals (with a sum of 1) that represent the fraction of fleet vehicles that have access to each type of public DCFC power
# Optional Variables
#   vmt_weights: data table of two columns ("name" and "weight") and N rows containing names (representing vmt bins) and associated decimals (with a sum of 1) that represent the fraction of fleet vehicles that travel x-miles in a 24-hour period
#       The bin width in vmt_weights MUST be equal to the bin width used in evi[,schedule_vmt_bin].
#       The number of bins in vmt_weights can be variable
#       The bin widths of all bins MUST be equal
#       The "name" column must be comprised of integers (in units of miles) representing the right bin edge (i.e. 10 captures from 0 - 9.99999 miles)
#       The VMT used is total daily VMT, NOT electric VMT (eVMT).
# Version History
#   1.1: JKC added the "rbind" approach to creating the weighted fleet, as opposed to the binary search appraoch. This creates a much closer
#         match to the desired fleet characteristics weights at the expense of not matching the fleet size exactly.
#   1.2: JKC added vmt_weights. Also added if statement to choose between "rbind" and binary search fleet creation approach depending on the
#         error between the resulting fleet size and the target fleet size.
#   1.3: JKC addressed issue with returning NA when looking for a vid in evi[] that matched all vehicle characteristics
#   1.4: JKC added comments, confirmed that use of vmt_weights applies to total daily vmt, not electric vmt.

evi_fleetGen <- function(evi,fleet_size,pev_weights,pref_weights,home_weights,work_weights,public_weights,vmt_weights=NULL) {
  
  # To confirm: we are in version 1.3
  
  #Must re-cast vmt_weights$name as character. Will re-cast back to integer when merging with evi[]
  if(!is.null(vmt_weights)) { vmt_weights[,name:=as.character(name)] }
  
  ################################################################################################################################
  #Create a data table of all potential permutations of fleet characteristics whose weights are user defined above
  ################################################################################################################################
  # First, data table of all weights and groups combined
  all_weights <- rbind(pev_weights,pref_weights,home_weights,work_weights,public_weights,vmt_weights)
  
  # Use expand.grid to create a data table of all permutations of groups to consider
  if(!is.null(vmt_weights)) {
    all_perms <- as.data.table(expand.grid(list(pev_weights[,name],pref_weights[,name],home_weights[,name],work_weights[,name],public_weights[,name],vmt_weights[,name])))
    colnames(all_perms) <- c("pev_type","preferred_loc","power_home","power_work","power_public","schedule_vmt_bin")
  } else {
    all_perms <- as.data.table(expand.grid(list(pev_weights[,name],pref_weights[,name],home_weights[,name],work_weights[,name],public_weights[,name])))
    colnames(all_perms) <- c("pev_type","preferred_loc","power_home","power_work","power_public")
  }
  
  #Calculate total weight for each permutation of groups
  # Iteratively join all_weights to all_perms, calculate the total weight for each permutation, then remove the joined weight column.
  setkey(all_weights,name)
  all_perms_names <- colnames(all_perms)
  all_perms[,stat_weight:=1]
  for(i in 1:length(all_perms_names)) {
    setkeyv(all_perms,all_perms_names[[i]])
    all_perms <- all_weights[all_perms]
    all_perms[,stat_weight:=stat_weight*weight][,weight:=NULL]
    setnames(all_perms,"name",all_perms_names[[i]])
  }
  
  #Factor values in each column. The corresponding factor numeric values correspond to the integer values used by NREL's
  #     file naming convention.
  # We don't factor schedule_vmt_bin (if created) as this will be re-cast back to integer
  all_perms[, power_public := factor(power_public, levels=c("Public50kW","Public150kW","Public400kW"))]
  all_perms[, power_work := factor(power_work, levels=c("WorkL1","WorkL2"))]
  all_perms[, power_home := factor(power_home, levels=c("HomeL1","HomeL2","HomeNone"))]
  all_perms[, preferred_loc := factor(preferred_loc, levels=c("PrefHome","PrefWork"))]
  all_perms[, pev_type := factor(pev_type, c("PHEV20","PHEV50","BEV100","BEV250"))]
  
  #Create a fleet where each row is associated with a vehicle. Aggregated distribution matches weights - the number of
  # vehicles matching a particular description = fleet size * stat_weight; so if the combined weight for a vmt/public power/
  # work power/home power/preference/pev type combinatuion is 0.02 and the fleet size is 100, there will be 2 vehciles
  # in the fleet with that specific combination.
  # Fleet size may be slightly off for large fleets, and substantially off for small fleets when
  #     coupled with small stat_weight values because round(stat_weight*fleet_size,0) will return a lot of zeros.
  if(!is.null(vmt_weights)) {
    fleet <- all_perms[stat_weight!=0,
                       do.call("rbind",replicate(round(stat_weight*fleet_size,0),.SD,simplify=FALSE)),
                       by=c("schedule_vmt_bin","power_public","power_work","power_home","preferred_loc","pev_type")]
  } else {
    fleet <- all_perms[stat_weight!=0,
                       do.call("rbind",replicate(round(stat_weight*fleet_size,0),.SD,simplify=FALSE)),
                       by=c("power_public","power_work","power_home","preferred_loc","pev_type")]
  }
  
  #If the difference between the resulting fleet size from the above approach and the target fleet size is larger than 0.1%,
  #     use binary search fleet creation approach that ensures the correct fleet size at the expense of not obtaining the exact weights desired
  #     else apply small correction to obtain target fleet size if off by <= 0.1% of target fleet size
  fleet_size_error <- abs((nrow(fleet)-fleet_size)/fleet_size)
  if( fleet_size_error > 0.001 ) {
    #print("LARGE ERROR IN FLEET SIZE")
    
    #Add cdf column to all_perms
    setkey(all_perms,stat_weight)
    all_perms[,cdf:=cumsum(stat_weight)]
    setkey(all_perms,cdf)
    setattr(all_perms,"sorted","cdf")
    
    #Generate array of random numbers between 0 and 1 equal to the fleet size.
    fleet <- data.table(random_num=runif(fleet_size)) #runif is a flat distribution between 0 and 1
    setkey(fleet,random_num)
    
    #Using binary search, select the row index in all_perms[,cdf] that is the nearest value for each random number in the
    #     fleet of random numbers between 0 and 1. This is our fleet.
    # Uses method described in https://stackoverflow.com/questions/20133344/find-closest-value-in-a-vector-with-binary-search
    fleet_perm_index <- all_perms[,.(cdf)][J(fleet[,random_num]), .I, roll=-Inf, by=.EACHI]
    
    #Create fleet of group characteristics. This will always be the correct size, but weights will be slightly off,
    #   especially for smaller fleets
    fleet <- all_perms[fleet_perm_index[,I],]
    
  } else {
    #print("SMALL ERROR IN FLEET SIZE")
    #If too small, add vehicles by randomly duplicating existing vehicles in fleet[]
    if( nrow(fleet) < fleet_size ) {
      index <- fleet[,sample(.I,fleet_size-.N)]
      fleet <- rbind(fleet,fleet[index])
    
    #If too large, randomly delete vehicles from fleet[]
    } else if( nrow(fleet) > fleet_size ) {
      index <- fleet[,sample(.I,.N-fleet_size)]
      fleet <- fleet[-index]
    }
  }
  
  #Add day of week column, and double the size of the data table to have a full weekday fleet and full weekend fleet
  fleet[,day_of_week:="weekday"]
  fleet <- rbind(copy(fleet),fleet[,day_of_week:="weekend"])
  fleet[,day_of_week:=factor(day_of_week, levels=c("weekday","weekend"))]
  
  #Re-cast fleet[,schedule_vmt_bin] values back to integer
  if(!is.null(vmt_weights)) { fleet[,schedule_vmt_bin:=as.integer(schedule_vmt_bin)] }
  
  #Randomly pull and append a vid that has the characteristics specified in the fleet data table.
  # Identify the subset of vids in evi that are associated with each row in your fleet. Randomly pull one of these vids
  #     and associate it with each entry in your fleet. You now have a list of vids equal in size to your fleet.
  # Note that the number of unique vids may be less than the fleet size. This is because one vid can apply to more than
  #     one group characteristic permutation
  if(!is.null(vmt_weights)) {
    setkeyv(evi,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin"))
    setkeyv(fleet,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin"))
  } else {
    setkeyv(evi,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type"))
    setkeyv(fleet,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type"))
  }
  fleet <- evi[fleet,sample(unique_vid,1,replace=TRUE),by=.EACHI] #with replacement
  setnames(fleet,"V1","unique_vid")
  
  #Create a specific fleet ID number for each vehicle in the fleet. The unique_vid value can be chosen more than once, so cannot be
  # relied upon to be a truly unique identifier for each vehicle.
  fleet[,fleet_id:=1:.N]
  
  #Address issue with a unique_vid not existing in evi[] for particular permutations of characteristics. This is seen by NA values
  #   for unique_vid in fleet[]. What triggers this issue is the schedule_vmt_bin. Usually a high vmt is assigned to a low battery capacity
  #   pev_type. Therefore we iteratively change the pev_types to the next battery capacity size, then look for a match.
  # This is addressed with a while loop that continues while NAs exist. There is also a hard break after 3 iterations to catch instances
  #   where we never find a solution (this method only facilitates 3 iterations). In the case where NAs remain, they are simply
  #   removed. This has the added affect of a reduced fleet size from that which was specified.
  # The result of this approach is that we stray a bit from the specified pev_type distribution, retaining the specified distribution
  #   for all other characteristics. This approach implicitly prioritizes vmt distribution over the pev_type distribution.
  i <- 1
  while( (i<=4) & (nrow(fleet[is.na(unique_vid)]) > 0) ) {
    if(i==1) {
      print(paste0("EVI Fleet Generation: correcting ",as.character(nrow(fleet[is.na(unique_vid)]))," NAs from fleet."))
    }
    
    print(paste0("  EVI Fleet Generation: NA correction iteration ",as.character(i)))
    
    #Change pev type to see if we can get a match for the next largest battery capacity
    row_to_match <- copy(fleet[is.na(unique_vid)])
    if( nrow(row_to_match[pev_type=="BEV100"])>0 ) {
      print(paste0("    EVI Fleet Generation: converting ",as.character(nrow(row_to_match[pev_type=="BEV100"]))," BEV100s to BEV250s."))
      row_to_match[pev_type=="BEV100",pev_type:="BEV250"]
    }
    if( nrow(row_to_match[pev_type=="PHEV50"])>0 ) {
      print(paste0("    EVI Fleet Generation: converting ",as.character(nrow(row_to_match[pev_type=="PHEV50"]))," PHEV50s to BEV100s."))
      row_to_match[pev_type=="PHEV50",pev_type:="BEV100"]
    }
    if( nrow(row_to_match[pev_type=="PHEV20"])>0 ) {
      print(paste0("    EVI Fleet Generation: converting ",as.character(nrow(row_to_match[pev_type=="PHEV20"]))," PHEV20s to PHEV50s"))
      row_to_match[pev_type=="PHEV20",pev_type:="PHEV50"]
    }
    
    #Find unique_vid for the rows in row_to_match
    setkeyv(evi,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin"))
    setkeyv(row_to_match,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin"))
    corrected_rows <- evi[row_to_match,sample(unique_vid,1,replace=TRUE),by=.EACHI]
    setnames(corrected_rows,"V1","unique_vid")
    
    #Merge new unique_vid onto row_to_match. Create a temporary key with which to merge
    setkeyv(row_to_match,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin"))
    setkeyv(corrected_rows,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin"))
    row_to_match[,tmpkey:=1:.N]
    corrected_rows[,tmpkey:=1:.N]
    setkeyv(row_to_match,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin","tmpkey"))
    setkeyv(corrected_rows,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin","tmpkey"))
    row_to_match <- row_to_match[corrected_rows,unique_vid:=i.unique_vid][,tmpkey:=NULL]
    
    #Merge found solutions back into fleet_activity
    setkeyv(fleet,"fleet_id")
    setkeyv(row_to_match,"fleet_id")
    fleet <- fleet[row_to_match[,.(fleet_id,unique_vid)],unique_vid:=i.unique_vid]
    fleet <- fleet[row_to_match[,.(fleet_id,pev_type)],pev_type:=i.pev_type]
    
    if( (i==4) & (nrow(fleet[is.na(unique_vid)]) != 0) ) {
      print(paste0("  EVI Fleet Generation: ",as.character(nrow(fleet[is.na(unique_vid)]))," NAs remaining in fleet that could not be corrected. Filtering out and reducing fleet size.\n"))
      fleet <- fleet[!is.na(unique_vid)]
    } else{
      print(paste0("  EVI Fleet Generation: ",as.character(nrow(corrected_rows[!is.na(unique_vid)]))," NAs corrected. ",as.character(nrow(fleet[is.na(unique_vid)]))," still remaining."))
    }
    
    i <- i + 1
  }
  
  #Create the final charging activity itinerary for the full fleet. This pulls all charging events for each unique_vid.
  # Note: when using vmt_weights, this only works if there are matching labels for evi[,schedule_vmt_bin] and fleet[,schedule_vmt_bin]. This
  #     only makes sense if the bin widths used for the two data table are equal.
  if(!is.null(vmt_weights)) {
    setkeyv(evi,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin","unique_vid"))
    setkeyv(fleet,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","schedule_vmt_bin","unique_vid"))
  } else {
    setkeyv(evi,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","unique_vid"))
    setkeyv(fleet,c("day_of_week","power_public","power_work","power_home","preferred_loc","pev_type","unique_vid"))
  }
  fleet_activity <- evi[fleet]
  
  #Return results
  return(fleet_activity)
  
}
