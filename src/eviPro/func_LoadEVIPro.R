# Author: Schatz Energy Research Center
# Original Version: Peter Alstone, July 2018
# Edits: Jerome Carman and/or Doug Saucedo 
# Version: 1.5
# Date: Nov 20, 2018
# Description: Loads NREL's EVIPro data into a data table
# Variables
#   target_dir: location of data files. This directory must only contain data files to be loaded. It cannot contain anything else.
#   dvmt_file: Location and name of additional daily vmt data file provide seperately from the main data set
#   vmt_bin_width: width of vmt bin for creating schedule_vmt_bin column
# Version Changes
#   v1.1: jkc: added columns start_hour, start_min, start_hour_late, start_min_late, end_hour, end_min, end_hour_late, end_min_late
#   v1.2: jkc: added unique_vid, session_vmt, schedule_vmt, and schedule_vmt_bin columns. Commented out kwh_check, kwh_bias, and row_id columns.
#   v1.3: jkc: added vmt_bin_width input variable. Default value is 10 miles for backwards compatability. Changed schedule_vmt_bin to right edge.
#   v1.4: arh: renamed vmt columns to specify eVMT, added dVMT from file.
#   v1.5: jkc: Corrected merge of evi data with dvmt data. Added schedule_vmt and schedule_vmt_bin, now associating them with dmvt.
#               Note, we need to correct session_Evmt and schedule_Evmt based on dvmt data.
#               If evmt is less that dvmt, leave untouched. Otherwise, scale evmt down. Reason for this issue is because
#               evmt is calculated using a static 0.325 kWh/mile, while actual vehicle efficiency is allowed to vary in EVI-Pro model.
#               this is left to a future version.

load_EVIPro <- function(target_dir,dvmt_file,vmt_bin_width=10) {
  #Load list of files in target directory
  file_list <- list.files(target_dir)
  
  # First file to initialize
  filename <- file_list[1]
  evi <- fread(paste0(target_dir, filename), # construct the file name
               col.names = c("vid","start_time", "end_time_chg", "end_time_prk", "dest_type", "dest_chg_level", "kwh", "avg_kw")) # manually define the columns
  evi[,filename := filename]
  
  #Create unique vid for full aggregated EVI data set for first file. Label this "unique_vid"
  setkey(evi,vid)
  evi_unique <- evi[,.(vid=unique(vid))][,unique_vid:=1:.N]
  setkey(evi_unique,vid)
  evi <- evi_unique[evi]
  
  #Track total vid count for the unique vid
  vid_count <- evi_unique[,.N]
  
  # Loop through and rbind 2:N files
  for(i in 2:length(file_list)){
    filename <- file_list[i]
    evi_new <- fread(paste0(target_dir, filename), col.names = c("vid","start_time", "end_time_chg", "end_time_prk", "dest_type", "dest_chg_level", "kwh", "avg_kw"))
    evi_new[,filename := filename]
    
    #Create unique vid for full aggregated EVI data set. Label this "unique_vid"
    setkey(evi_new,vid)
    evi_unique <- evi_new[,.(vid=unique(vid))][,unique_vid:=vid_count + 1:.N]
    setkey(evi_unique,vid)
    evi_new <- evi_unique[evi_new]
    
    #Row bind
    evi <- rbind(evi, evi_new)
    
    #Track total vid count for the unique vid
    vid_count <- vid_count + evi_unique[,.N]
  }
  
  # cleanup and report
  remove(evi_new,evi_unique)
  print(now())
  tables()
  
  # ---- Add additional information to EVIPro data ----
  
  #Create a schedule kWh and schedule kWh bin. This captures the total kWh consumed for each full vid itinerary, and bins it.
  # kwh bin is left edge
  evi[,schedule_kwh:=sum(kwh),by=unique_vid]
  kwh_bin_size <- 1 #kwh
  evi[,schedule_kwh_bin:=as.integer(floor(sum(kwh)/kwh_bin_size)*kwh_bin_size),by=unique_vid] #bin value is lower edge of bin (10 captures 10 to 19.9999)
  
  #estimate eVMT using nominal efficiency of 325 Wh/mile provided by NREL
  # Note that efficiency in EVIPro is varied by travel speed. But we don't have that resolution in this data set, so appling static nominal value.
  # vmt bin is right-edge
  # Note that schedule_Evmt can be larger than schedule_vmt. This is because we use a static vehicle efficiency to estimate Evmt which
  #   can overestimate Evmt. This should be corrected in a future version of this function to align these two vmt data columns.
  evi[,session_Evmt:=kwh/0.325]
  evi[,schedule_Evmt:=sum(session_Evmt),by=unique_vid]
  evi[,schedule_Evmt_bin:=as.integer(ceiling(sum(session_Evmt)/vmt_bin_width)*vmt_bin_width),by=unique_vid] #bin value is right edge of bin (10 captures 0 to 9.9999)
  
  #Load up daily VMT (gas and electric) from the "chts_dvmt.csv"
  #  Note that "schedule_vmt" used to be associated with evmt. It is now associated with dvmt. The old one is now called schedule_Evmt above.
  chts_dvmt <- data.table(read.csv(dvmt_file))
  setnames(chts_dvmt,c('chts_vid','dvmt'),c('vid','schedule_vmt'))
  
  #Combine dvmt data with evi
  setkey(evi,vid)
  setkey(chts_dvmt,vid)
  evi <- merge(evi,chts_dvmt,all.x=TRUE)
  
  #Create schedule vmt bin using daily vmt
  #  Note that the "schedule_vmt_bin" used to be associated with evmt. It is now associated with dvmt.
  evi[,schedule_vmt_bin:=as.integer(ceiling(schedule_vmt/vmt_bin_width)*vmt_bin_width),by=unique_vid] #bin value is right edge of bin (10 captures 0 to 9.9999)
  
  # identify latest possible start time
  evi[,start_time_late := end_time_prk - (kwh/avg_kw)/24 ]
  
  # parse out start and end times into hour and minute format
  evi[,start_hour:=(start_time*24)%/%1]
  evi[,start_min:=(start_time*24)%%1]
  evi[,start_hour_late:=(start_time_late*24)%/%1]
  evi[,start_min_late:=(start_time_late*24)%%1]
  evi[,end_hour:=(end_time_chg*24)%/%1]
  evi[,end_min:=(end_time_chg*24)%%1]
  evi[,end_hour_late:=(end_time_prk*24)%/%1]
  evi[,end_min_late:=(end_time_prk*24)%%1]
  
  # parse file information
  file_info <- data.table(filename = file_list)
  file_info[ , power_home := as.numeric(substr(filename, 1,1))]
  file_info[ , power_work := as.numeric(substr(filename, 3,3))]
  file_info[ , power_public := as.numeric(substr(filename, 5,5))]
  file_info[ , preferred_loc := as.numeric(substr(filename, 7,7))]
  file_info[ , temp_c := as.numeric(substr(filename, 9,9))]
  file_info[ , pev_type := as.numeric(substr(filename, 14,14))]
  file_info[ , day_of_week := as.numeric(substr(filename, 19,19))]
  
  setkey(file_info, filename)
  setkey(evi, filename)
  evi <- file_info[evi]
  
  # Set file information, and destination type and destination charge level to human readable values and factor.
  # The corresponding factor numeric values correspond to the integer values used by NREL's file naming convention.
  # The factor names correspond to the fleet characteristic names in the weights file
  power_home_names <- c("HomeL1","HomeL2","HomeNone")
  power_work_names <- c("WorkL1","WorkL2")
  power_public_names <- c("Public50kW","Public150kW","Public400kW")
  preferred_loc_names <- c("PrefHome","PrefWork")
  temp_c_names <- c("-20C","-10C","0C","+10C","+20C","+25C","+30C","+40C")
  pev_type_names <- c("PHEV20","PHEV50","BEV100","BEV250")
  day_of_week_names <- c("weekday","weekend")
  dest_type_names <- c("Home","Work","Public")
  dest_chg_level_names <- c("L1","L2","L3")
  
  evi[,power_home:=power_home_names[power_home]]
  evi[,power_work:=power_work_names[power_work]]
  evi[,power_public:=power_public_names[power_public]]
  evi[,preferred_loc:=preferred_loc_names[preferred_loc]]
  evi[,temp_c:=temp_c_names[temp_c]]
  evi[,pev_type:=pev_type_names[pev_type]]
  evi[,day_of_week:=day_of_week_names[day_of_week]]
  evi[,dest_type:=dest_type_names[dest_type]]
  evi[,dest_chg_level:=dest_chg_level_names[dest_chg_level]]
  
  evi[, power_home := factor(power_home, levels=power_home_names)]
  evi[, power_work := factor(power_work, levels=power_work_names)]
  evi[, power_public := factor(power_public, levels=power_public_names)]
  evi[, preferred_loc := factor(preferred_loc, levels=preferred_loc_names)]
  evi[, temp_c := factor(temp_c, levels=temp_c_names)]
  evi[, pev_type := factor(pev_type, levels=pev_type_names)]
  evi[, day_of_week := factor(day_of_week, levels=day_of_week_names)]
  evi[, dest_type := factor(dest_type, levels=dest_type_names)]
  evi[, dest_chg_level := factor(dest_chg_level, levels=dest_chg_level_names)]
  
  #Remove keys and return with ensured order
  setkey(evi,NULL)
  setorder(evi,filename,vid,start_time)
  
  return(evi)
}
