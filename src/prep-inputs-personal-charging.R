###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the personal EV charging inputs required by the mobility model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
###############################################################################################

prep.inputs.personal.charging <- function(exper.row,common.inputs,inputs.mobility){
  ###########################################################
  # Setup variables and environment
  ###########################################################
  workingDir <- pp(gem.raw.inputs,"NREL-EVI-Pro-Preprocessed-Profiles")

  #Directory and file containing NREL EVIPro data
  evi_data_dir <- pp(workingDir,"/data/sessions/")    #Directory of files containing raw charging session data
  dvmt_data_file <- pp(workingDir,"/data/chts_dvmt.csv")  #File containing CHTS total daily vmt by vid to append to EVI data

  source("src/eviPro/func_LoadEVIPro.R")            #loads EVIPro data and stores in a single data table
  source("src/eviPro/func_EVIFleetGen.R")           #Generates a fleet of EVIPro vids
  source("src/eviPro/func_calcBaseEVILoad.R")       #Pre-calculates load profile for all unique_vid in evi_raw
  source("src/eviPro/func_measureFleetWeights.R")   #Creates statistics of generated fleet
  source("src/eviPro/func_LoadFleetWeights.R")      #Loads .csv file where fleet characteristic weights are stored
  source("src/eviPro/func_GenVmtWeights.R")         #Generates vmt distribution for fleet generation
  source("src/eviPro/func_CreateFleetWeights.R")    #Creates fleet weights from values hard coded in this function. Use this instead of loading fleet weights from a file if desired.

  if('fractionSAEVs' %in% names(exper.row)){
    fractionSAEVs <- exper.row$fractionSAEVs
  }
  
  ###########################################################
  # Optionally load EVI charging and load profile data
  ###########################################################
  # Desired size of fleet. The 3.37 is the average number of trips per driver based on https://nhts.ornl.gov/assets/2017_nhts_summary_travel_trends.pdf
  scaling.frac <- ifelse(fractionSAEVs==0,1,(1-fractionSAEVs)/fractionSAEVs)
  fleet.sizes <- inputs.mobility$parameters$demandUnscaled[,list(n.vehs=round(sum(value)/length(days)/3.37*scaling.frac,0)),by='rmob']
  load.data.needed <- F
  for(the.region in common.inputs$sets$rmob){
    fleet_size <- fleet.sizes[rmob==the.region]$n.vehs
    # Look for the results in the gem cache, otherwise process and load
    cache.file <- pp(workingDir,'/data/gem-cache/region-',the.region,'-fleet-',fleet_size,'.Rdata')
    if(!file.exists(cache.file))load.data.needed<-T
  }

  if(load.data.needed){
    cat(pp('Personal EV charging profiles need to be created. This will take a while to load the data and process but results will be cached and future runs for this fleet size will be very fast.\n'))
    Sys.sleep(0.2)
    run_evi_load_func <- FALSE #Set to true to re-create evi_raw data table. Set to false to load .RData file
    run_load_calc_func <- FALSE #Set to true if you want to re-calc. This takes about 3.5 hours. Set to false to load .RData file

    #Load raw EVI data
    raw_data_file_name <- paste0(workingDir,"/data/evi_raw_data_2018-11-26.RData") #Source of data, or file name to save to if re-calc
    if(!exists("evi_raw")) {
      if(run_evi_load_func) {
        evi_raw <- load_EVIPro(evi_data_dir,dvmt_data_file,vmt_bin_size)
        #Make sure no NAs exist in the data set
        print(evi_raw[, lapply(.SD, function(x) sum(is.na(x))), .SDcols=1:ncol(evi_raw)])
        save(evi_raw, file=raw_data_file_name)
      } else {
        load(raw_data_file_name,envir=globalenv())
      }
    }

    #Load power profile for all unique_vids
    load_file_name <- paste0(workingDir,"/data/evi_load_profile_data_2019-01-06.RData") #Source of data, or file name to save to if re-calc
    if(!exists("evi_load_profiles")) {
      if(run_load_calc_func) {
        print(Sys.time())
        evi_load_profiles <- calcBaseEVILoad(evi_raw,time_step) 
        # Adjust the start and tend time of the charing session as much as possible
        evi_delayed <- copy(evi_raw)
        evi_delayed[,start_time:=start_time+(end_time_prk-end_time_chg)]
        evi_delayed[,end_time_chg:=end_time_prk]
        evi_delayed_load_profiles <- calcBaseEVILoad(evi_delayed,time_step) 
        evi_load_profiles <- evi_load_profiles[,list(unique_vid,session_id,time_of_day,schedule_vmt,avg_kw,kwh,level=dest_chg_level,plugged_kw)]
        evi_delayed_load_profiles <- evi_delayed_load_profiles[,list(unique_vid,session_id,time_of_day,schedule_vmt,avg_kw,kwh,level=dest_chg_level,plugged_kw)]
        save(evi_load_profiles,evi_delayed_load_profiles,file=load_file_name)
        print(Sys.time())
      } else {
        load(load_file_name,envir=globalenv())
      }
    }
  }

  ###########################################################
  # Configure variables and process profiles for every region
  ###########################################################
  all.all.energy.constraints <- list()
  for(the.region in common.inputs$sets$rmob){

    fleet_size <- fleet.sizes[rmob==the.region]$n.vehs

    # Look for the results in the gem cache, otherwise process and load
    cache.file <- pp(workingDir,'/data/gem-cache/region-',the.region,'-fleet-',fleet_size,'.Rdata')
    if(file.exists(cache.file)){
      load(cache.file)
    }else{
      #TOOL USER INPUT: Load .csv file containing desired weights of fleet characteristics
      weights_file <- paste0(workingDir,"/data/fleet_weights_dev.csv")

      #TOOL USER INPUT: Define average per-vehicle total dvmt from which a dvmt distribution will be created
      # Note that vmt distribution width is hard coded in the vmt_WeightDistGen() function. This has yet to be verified.
      avg_dvmt <- 28.5 #miles

      #INTERNAL TOOL SETTING: Specifiy the vmt bin size to use for creating the fleet vmt distribution
      vmt_bin_size <- 10 #mile

      #INTERNAL TOOL SETTING: Set time series resolution over which kWh and average kW will be calculated
      # DO NOT CHANGE THIS UNLESS YOU PLAN TO RE-RUN PRECALCULATED LOAD PROFILES.
      time_step <- 1 #hours

      ###########################################################
      # Create or load fleet characteristics weights
      ###########################################################

      #Set to true to load fleet weights from .csv file.
      #Set to false to create fleet weights from create_fleet_weights() function in which you can quickly change weights in RStudio
      use_file <- TRUE
      if(use_file) {
        fleet_weights <- load_fleet_weights(weights_file)
      } else {
        fleet_weights <- create_fleet_weights()
      }

      #Take an average per-vehicle dvmt and generate a fleet dvmt distribution
      fleet_weights$vmt_weights <- vmt_WeightDistGen(avg_dvmt,vmt_bin_size)

      ###########################################################
      # Generate fleet and associated load profile
      ###########################################################
      #print(Sys.time())

      #Check desired fleet size. If greater than 50,000 then set fleet size to 50,000. Scale the load profiles accordingly later on.
      if(fleet_size > 50000) {
        fleet_size_scale <- fleet_size / 50000
        fleet_size <- 50000
      } else {
        fleet_size_scale <- 1
      }

      #Create fleet
      evi_fleet <- list(data = evi_fleetGen(evi_raw,
                                            fleet_size,
                                            fleet_weights$pev_weights,
                                            fleet_weights$pref_weights,
                                            fleet_weights$home_weights,
                                            fleet_weights$work_weights,
                                            fleet_weights$public_weights,
                                            fleet_weights$vmt_weights
                                            ))

      #Generate fleet statistics to check with desired characteristics
      #evi_fleet$stats <- measureFleetWeights(evi_fleet$data)

      #-----Generate 48-hour Fleet Load Profile----------

      #Build list of unique_vid from evi_fleet that we'll use to construct the full load profile
      id_key <- evi_fleet$data[,.(unique_vid = unique(unique_vid)),by=fleet_id]
      setkey(id_key,unique_vid)

      #Subset load profiles specific to those unique_vids in the evi_fleet. This is the full load profile of the fleet
      setkey(evi_load_profiles,unique_vid,session_id)
      fleet_load_profiles <- evi_load_profiles[unique_vid %in% evi_fleet$data[,unique(unique_vid)]] #Subset to those unique_vids we are interested in
      fleet_load_profiles <- id_key[fleet_load_profiles,allow.cartesian=TRUE] #Capture duplicate unique_vids by using fleet_id

      #Add day_of_week information
      weekday_ref <- evi_fleet$data[!duplicated(unique_vid),day_of_week,by=unique_vid]
      setkey(weekday_ref,unique_vid)
      setkey(fleet_load_profiles,unique_vid)
      fleet_load_profiles <- merge(weekday_ref,fleet_load_profiles,by="unique_vid",all.y=TRUE)

      #Scale load profile by fleet size scale factor.
      # The approach is a straight multiplier on the kW of those vehicles in fleet_load_profiles. This does not increase the number of fleet
      #   vehicles. For example, if the desired fleet size is 100,000, then there is a fleet of 50,000 and the kW associated with each charge
      #   event is increased by 100,000 / 50,000 = 2.
      # The variables schedule_vmt and kwh are also scaled.
      fleet_load_profiles[,schedule_vmt := schedule_vmt * fleet_size_scale][,avg_kw := avg_kw * fleet_size_scale][,kwh := kwh * fleet_size_scale][,plugged_kw:=plugged_kw*fleet_size_scale]

      #### REPEAT FOR DELAYED LOAD SHAPE 

      setkey(evi_delayed_load_profiles,unique_vid,session_id)
      delayed_fleet_load_profiles <- evi_delayed_load_profiles[unique_vid %in% evi_fleet$data[,unique(unique_vid)]] #Subset to those unique_vids we are interested in
      delayed_fleet_load_profiles <- id_key[delayed_fleet_load_profiles,allow.cartesian=TRUE] #Capture duplicate unique_vids by using fleet_id
      setkey(delayed_fleet_load_profiles,unique_vid)
      delayed_fleet_load_profiles <- merge(weekday_ref,delayed_fleet_load_profiles,by="unique_vid",all.y=TRUE)
      delayed_fleet_load_profiles[,schedule_vmt := schedule_vmt * fleet_size_scale][,avg_kw := avg_kw * fleet_size_scale][,kwh := kwh * fleet_size_scale]

      #remove temp variables
      remove(id_key,weekday_ref)

      # Turn into energy and power constraints

      fleet_load_profiles[,t:=time_of_day]
      delayed_fleet_load_profiles[,t:=time_of_day]
      energy.constraints.unnormalized <- join.on(join.on(data.table(expand.grid(list(t=u(c(fleet_load_profiles$t,delayed_fleet_load_profiles$t)),day_of_week=c('weekday','weekend')))),fleet_load_profiles[,list(max.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week')), delayed_fleet_load_profiles[,list(min.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week'))
      setkey(energy.constraints.unnormalized,day_of_week,t)
      energy.constraints.unnormalized[is.na(min.energy) & t<5,min.energy:=0]
      energy.constraints.unnormalized[,':='(min.energy=cumsum(min.energy),max.energy=cumsum(max.energy)),by='day_of_week']
      max.diff <- energy.constraints.unnormalized[,list(diff=max(max.energy-min.energy,na.rm=T)),by='day_of_week']

      fleet_load_profiles[,t:=time_of_day%%24]
      delayed_fleet_load_profiles[,t:=time_of_day%%24]
      energy.constraints <- join.on(join.on(data.table(expand.grid(list(t=0:23,day_of_week=c('weekday','weekend')))),fleet_load_profiles[,list(max.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week')), delayed_fleet_load_profiles[,list(min.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week'))
      setkey(energy.constraints,day_of_week,t)
      energy.constraints[,':='(min.energy=cumsum(min.energy),max.energy=cumsum(max.energy)),by='day_of_week']
      energy.constraints <- join.on(energy.constraints,max.diff,'day_of_week','day_of_week')
      energy.constraints[,delta:= diff - max(max.energy-min.energy),by='day_of_week']
      energy.constraints[,min.energy:=min.energy - delta]
      energy.constraints[,':='(delta=NULL,diff=NULL)]

      power.constraints <- fleet_load_profiles[,.(max.power=sum(plugged_kw),min.power=0),by='t']
      setkey(power.constraints)

      wdays <- weekdays(to.posix(pp(year,'-01-01 00:00:00+00'))+24*3600*(days-1))
      setkey(energy.constraints,t)
      all.energy.constraints <- list()
      for(i in 1:length(days)){
        the.day <- days[i]
        the.wday <- wdays[i]
        if(the.wday=='Sunday' || the.wday=='Saturday'){
          the.day.type <- "weekend"
        }else{
          the.day.type <- "weekday"
        }
        df <- copy(energy.constraints)[day_of_week==the.day.type]
        df[,t:=t+(i-1)*24]
        if(i>1){
          maxes <- all.energy.constraints[[length(all.energy.constraints)]][,list(max.max=max(max.energy),max.min=max(min.energy))]
          mins <- df[,list(the.min=min(min.energy))]
          df[,max.energy:=max.energy+maxes$max.max]
          df[,min.energy:=min.energy+maxes$max.min-mins$the.min]
        }
        df[,max.power:=power.constraints$max.power]
        df[,min.power:=power.constraints$min.power]
        #ggplot(df,aes(x=t,y=min.energy,colour=day_of_week))+geom_line()+geom_line(aes(y=max.energy))
        all.energy.constraints[[length(all.energy.constraints)+1]] <- df
      }
      all.energy.constraints <- rbindlist(all.energy.constraints)

      ggplot(all.energy.constraints,aes(x=t,y=min.energy,colour=day_of_week))+geom_line()+geom_line(aes(y=max.energy))

      all.energy.constraints[,t:=pp('t',sprintf('%04d',t+1))]
      all.energy.constraints[,rmob:=the.region]

      save(all.energy.constraints,file=cache.file)
    }
    all.all.energy.constraints[[length(all.all.energy.constraints)+1]] <- copy(all.energy.constraints)
  }
  all.all.energy.constraints <- rbindlist(all.all.energy.constraints)
  inputs <- list()
  inputs$sets <- list()
  inputs$parameters <- list()
  inputs$parameters$personalEVChargeEnergyUB <- all.all.energy.constraints[,list(t,rmob,value=max.energy)]
  inputs$parameters$personalEVChargeEnergyLB <- all.all.energy.constraints[,list(t,rmob,value=min.energy)]
  inputs$parameters$personalEVChargePowerUB <- all.all.energy.constraints[,list(t,rmob,value=max.power)]
  inputs$parameters$personalEVChargePowerLB <- all.all.energy.constraints[,list(t,rmob,value=min.power)]

    #print(Sys.time())

    ##Plot aggregate data-------

    ##Aggregate data
    #agg_plot_data <- copy(fleet_load_profiles)
    #agg_plot_data[time_of_day > 24,
                         #time_of_day := time_of_day - 24]
    #agg_plot_data <- agg_plot_data[,.(avg_kw = sum(avg_kw)),
                                   #by=c("time_of_day")]

    ##Plot
    #agg_plot <- ggplot(data=agg_plot_data, aes(x=time_of_day)) +
      #geom_area(aes(y=avg_kw))

    #agg_plot_2 <- ggplot(data=agg_plot_data, aes(x=time_of_day, y=avg_kw)) +
      #geom_bar(aes(fill=dest_type), stat="identity", position="stack") +
      #facet_wrap(~day_of_week)


    ##Plot weekday disaggregated by pev_type-----------

    ##Aggregate data
    #plot_data_1 <- copy(fleet_load_profiles)
    #plot_data_1[time_of_day > 24,
                         #time_of_day := time_of_day - 24]
    #plot_data_1 <- plot_data_1[,.(avg_kw = sum(avg_kw)),
                               #by=c("day_of_week","time_of_day","dest_type","pev_type")]

    ##Plot
    #plot1 <- ggplot(data=plot_data_1[day_of_week=="weekday"], aes(x=time_of_day)) +
      #geom_area(aes(y=avg_kw, fill=dest_type)) +
      #facet_wrap(~pev_type,scales="free")

    #plot2 <- ggplot(data=plot_data_1[day_of_week=="weekday"], aes(x=time_of_day, y=avg_kw)) +
      #geom_bar(aes(fill=dest_type), stat="identity", position="stack") +
      #facet_wrap(~pev_type, scales="free")

    inputs
  }
