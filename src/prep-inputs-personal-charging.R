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
  if('fractionSmartCharging' %in% names(exper.row)){
    fractionSmartCharging <- exper.row$fractionSmartCharging
  }
  if('privateFleetWeights' %in% names(exper.row)){
    privateFleetWeights <- exper.row$privateFleetWeights
  }
  
  if(fractionSAEVs < 1){
    ###########################################################
    # Optionally load EVI charging and load profile data
    ###########################################################
    # Desired size of fleet. The 3.37 is the average number of trips per driver based on https://nhts.ornl.gov/assets/2017_nhts_summary_travel_trends.pdf
    load.data.needed <- F
    for(the.region in common.inputs$sets$rmob){
      # Look for the results in the gem cache, otherwise process and load
      cache.file <- pp(workingDir,'/data/gem-cache/region-',the.region,'-privateFleetWeights-',privateFleetWeights,'-smart-',fractionSmartCharging,'.Rdata')
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
      
      load_file_name <- pp(gem.raw.inputs,'nhts/daily_vmt_per_person.Rdata')
      if(!file.exists(load_file_name)){
        dfs <- list()
        to.load <- c('avg_daily_vmt_perperson_region_season_day_no_transit.csv','avg_daily_vmt_perperson_region_season_day_with_transit.csv')
        for(file in to.load){
          df <- data.table(read.csv(pp(gem.raw.inputs,'nhts/',file),stringsAsFactors=F))
          df[,transit:=pp(str_split(file,"_")[[1]][8:9],collapse='_')]
          df[,transit:=substr(transit,1,nchar(transit)-4)]
          dfs[[length(dfs)+1]] <- df
        }
        dvmt <- rbindlist(dfs)
        dvmt[,rmob:=pp(CDIVLS,'-',URBRURS)]
        save(dvmt,file=load_file_name)
        rm('dfs')
      }
      load(load_file_name)
    }
  
    ###########################################################
    # Configure variables and process profiles for every region
    ###########################################################
    #INTERNAL TOOL SETTING: Specifiy the vmt bin size to use for creating the fleet vmt distribution
    vmt_bin_size <- 10 #mile
    #INTERNAL TOOL SETTING: Set time series resolution over which kWh and average kW will be calculated
    # DO NOT CHANGE THIS UNLESS YOU PLAN TO RE-RUN PRECALCULATED LOAD PROFILES.
    time_step <- 1 #hours
    #TOOL USER INPUT: Load .csv file containing desired weights of fleet characteristics
    weights_file <- paste0(workingDir,"/data/",privateFleetWeights,".csv")
    if(file.exists(weights_file)) {
      fleet_weights <- load_fleet_weights(weights_file)
    } else {
      cat("WARNING: no fleet weights file found, creating defaults with 'create_fleet_weights'....")
      fleet_weights <- create_fleet_weights()
    }
    transit.type <- ifelse(includeTransitDemand,'with_transit','no_transit')
    # We assume 4.17 trips per person, this is based on NHTS overall trips per person 3.46 but then rescaled by 1/0.83 to account for the people who take no trips (which we effectively remove)
    fleet.sizes <- inputs.mobility$parameters$demandUnscaled[,list(n.vehs=round(sum(value)/length(days)/4.17*(1-fractionSAEVs),0)),by='rmob']
    all.all.energy.constraints <- list()
    all.fleets <- list()
    all.chargers <- list()
    the.region <- common.inputs$sets$rmob[1]
    for(the.region in common.inputs$sets$rmob){
      fleet_size <- fleet.sizes[rmob==the.region]$n.vehs
      
      ##############################################################################
      # Create or load fleet characteristics weights for each DVMT value of interest
      ##############################################################################
      energy.and.power.constraints <- list()
      dates <- date.info(days,year)
      for(season in u(toupper(dates$seasons))){
        energy.and.power.constraints[[season]] <- list()
        for(weekday.type in u(dates$day.types)){
          energy.and.power.constraints[[season]][[weekday.type]] <- list()
  
          # Look for the results in the gem cache, otherwise process and load
          cache.dir <- pp(workingDir,'/data/gem-cache/',the.region,'/')
          make.dir(cache.dir)
          cache.dir <- pp(cache.dir,privateFleetWeights,'/')
          make.dir(cache.dir)
          cache.file <- pp(cache.dir,'smart-',fractionSmartCharging,'-season-',season,'-day-',str_replace_all(weekday.type,"/",""),'-transit-',transit.type,'.Rdata')
          if(file.exists(cache.file)){
            load(cache.file)
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]] <- energy.and.power.constraints.cache[[season]][[weekday.type]][[transit.type]]
          }else{
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]] <- list()
            
            #TOOL USER INPUT: Define average per-vehicle total dvmt from which a dvmt distribution will be created
            # Note that vmt distribution width is hard coded in the vmt_WeightDistGen() function. This has yet to be verified.
            # avg_dvmt <- 28.5 #miles
            avg_dvmt <- dvmt[rmob==the.region & SEASON==season & WKTIME == weekday.type & transit == transit.type]$TRPMILES
            if(length(avg_dvmt)==0)avg_dvmt <- mean(dvmt[rmob==the.region & transit == transit.type]$TRPMILES) # handle missing data
            #cat(pp(the.region,",",season,",",weekday.type,",",transit.type,"\n"))
            #cat(pp("Avg DVMT:",avg_dvmt,"\n"))
      
            #Take an average per-vehicle dvmt and generate a fleet dvmt distribution
            fleet_weights$vmt_weights <- vmt_WeightDistGen(avg_dvmt,vmt_bin_size)
      
            ###########################################################
            # Generate fleet and associated load profile
            ###########################################################
            #Create fleet
            evi_fleet <- evi_fleetGen(evi_raw,
                                        50000,
                                        fleet_weights$pev_weights,
                                        fleet_weights$pref_weights,
                                        fleet_weights$home_weights,
                                        fleet_weights$work_weights,
                                        fleet_weights$public_weights,
                                        fleet_weights$vmt_weights
                                        )
            
            # Collect data on charging infrastructure requirements
            n.home <- evi_fleet[dest_type=='Home',.(req=length(u(unique_vid))),by=c('dest_type','dest_chg_level')]
            evi_fleet[,row:=1:nrow(evi_fleet)]
            occupied <- evi_fleet[dest_type!='Home',.(t=seq(start_time,end_time_prk,by=.25)),by=c('row','dest_type','dest_chg_level')]
            occupied[,t15:=round(t*4,0)/4]
            n.non.home <- occupied[,.(n=.N * 1.2 ),by=c('dest_type','t15','dest_chg_level')][,.(req=max(n)),by=c('dest_type','dest_chg_level')]
            unscaled.ch.requirements <- rbindlist(list(n.non.home,n.home))
            the.weights <- measureFleetWeights(evi_fleet)
            private.fleet <- rbindlist(list(the.weights$weekend$pev_weights[,days:=2],the.weights$weekday$pev_weights[,days:=5]))[,.(share=weighted.mean(weight,days)),by='name']
      
            #-----Generate 48-hour Fleet Load Profile----------
            
            #Handle missing data
            #Build list of unique_vid from evi_fleet that we'll use to construct the full load profile
            id_key <- evi_fleet[,.(unique_vid = unique(unique_vid)),by=fleet_id]
            setkey(id_key,unique_vid)
            #Subset load profiles specific to those unique_vids in the evi_fleet. This is the full load profile of the fleet
            setkey(evi_load_profiles,unique_vid,session_id)
            fleet_load_profiles <- evi_load_profiles[unique_vid %in% evi_fleet[,unique(unique_vid)]] #Subset to those unique_vids we are interested in
            fleet_load_profiles <- id_key[fleet_load_profiles,allow.cartesian=TRUE] #Capture duplicate unique_vids by using fleet_id
            fleet_load_profiles[is.na(avg_kw),avg_kw:=0]
            #Add day_of_week information
            weekday_ref <- evi_fleet[!duplicated(unique_vid),day_of_week,by=unique_vid]
            setkey(weekday_ref,unique_vid)
            setkey(fleet_load_profiles,unique_vid)
            fleet_load_profiles <- merge(weekday_ref,fleet_load_profiles,by="unique_vid",all.y=TRUE)
            #### REPEAT FOR DELAYED LOAD SHAPE 
            setkey(evi_delayed_load_profiles,unique_vid,session_id)
            delayed_fleet_load_profiles <- evi_delayed_load_profiles[unique_vid %in% evi_fleet[,unique(unique_vid)]] #Subset to those unique_vids we are interested in
            delayed_fleet_load_profiles <- id_key[delayed_fleet_load_profiles,allow.cartesian=TRUE] #Capture duplicate unique_vids by using fleet_id
            setkey(delayed_fleet_load_profiles,unique_vid)
            delayed_fleet_load_profiles <- merge(weekday_ref,delayed_fleet_load_profiles,by="unique_vid",all.y=TRUE)
            # Finally dependingon fracSmartCharge, replace some of the delayed with their original load
            n.veh <- length(u(fleet_load_profiles$unique_vid))
            unmanaged.ids <- sample(u(fleet_load_profiles$unique_vid),n.veh*(1-fractionSmartCharging))
            delayed_fleet_load_profiles <- rbindlist(list(delayed_fleet_load_profiles[!unique_vid %in% unmanaged.ids],fleet_load_profiles[unique_vid%in%unmanaged.ids]))
            #remove temp variables
            remove(id_key,weekday_ref)
            # Turn into energy and power constraints
            fleet_load_profiles[,t:=time_of_day]
            delayed_fleet_load_profiles[,t:=time_of_day]
            energy.constraints.unnormalized <- join.on(join.on(data.table(expand.grid(list(t=u(c(fleet_load_profiles$t,delayed_fleet_load_profiles$t)),day_of_week=c('weekday','weekend')))),fleet_load_profiles[,list(max.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week')), delayed_fleet_load_profiles[,list(min.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week'))
            setkey(energy.constraints.unnormalized,day_of_week,t)
            energy.constraints.unnormalized[is.na(min.energy),min.energy:=0]
            energy.constraints.unnormalized[is.na(max.energy),max.energy:=0]
            energy.constraints.unnormalized[,':='(min.energy=cumsum(min.energy),max.energy=cumsum(max.energy)),by='day_of_week']
            max.diff <- energy.constraints.unnormalized[,list(diff=max(max.energy-min.energy,na.rm=T)),by='day_of_week']
      
            fleet_load_profiles[,t:=time_of_day%%24]
            delayed_fleet_load_profiles[,t:=time_of_day%%24]
            energy.constraints <- join.on(join.on(data.table(expand.grid(list(t=0:23,day_of_week=c('weekday','weekend')))),fleet_load_profiles[,list(max.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week')), delayed_fleet_load_profiles[,list(min.energy=sum(avg_kw)),by=c('t','day_of_week')],c('t','day_of_week'),c('t','day_of_week'))
            setkey(energy.constraints,day_of_week,t)
            energy.constraints[is.na(min.energy),min.energy:=0]
            energy.constraints[is.na(max.energy),max.energy:=0]
            energy.constraints[,':='(min.energy=cumsum(min.energy),max.energy=cumsum(max.energy)),by='day_of_week']
            energy.constraints <- join.on(energy.constraints,max.diff,'day_of_week','day_of_week')
            energy.constraints[,delta:= diff - max(max.energy-min.energy),by='day_of_week']
            min.energy.initial.offsets <- energy.constraints[,.(min.offset=min(min.energy)),by='day_of_week']
            energy.constraints[,min.energy:=min.energy - delta]
            energy.constraints[,':='(delta=NULL,diff=NULL)]
            #cat(pp("First max energy UB: ",energy.constraints$max.energy[1],"\n"))
            power.constraints <- fleet_load_profiles[,.(max.power=sum(plugged_kw),min.power=0),by='t']
            setkey(power.constraints)
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['energy']] <- energy.constraints
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['energy.min.offsets']] <- min.energy.initial.offsets
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['power']] <- power.constraints
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['unscaled.charger.reqs']] <- unscaled.ch.requirements
            energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['private.fleet.share']] <- private.fleet
            energy.and.power.constraints.cache <- copy(energy.and.power.constraints)
            save(energy.and.power.constraints.cache,unscaled.ch.requirements,file=cache.file)
          }
        }
      }
  
      fleets <- list()
      chargers <- list()
      cumul.energy.constraints <- list()
      for(i in 1:length(days)){
        the.day <- days[i]
        the.season <- toupper(dates$seasons[i])
        the.day.type <- dates$day.types[i]
        if(the.day.type == 'SA/SU'){
          the.evipro.day.type <- "weekend"
        }else{
          the.evipro.day.type <- "weekday"
        }
        the.transit.type <- ifelse(includeTransitDemand,'with_transit','no_transit')
        scaling.factor <- fleet_size / 50000
        energy.constraints <- copy(energy.and.power.constraints[[the.season]][[the.day.type]][[the.transit.type]][['energy']])
        energy.constraints[,':='(max.energy=max.energy*scaling.factor,min.energy=min.energy*scaling.factor)]
        if(any(is.na(energy.constraints$min.energy)))stop('here')
        min.energy.initial.offsets <- copy(energy.and.power.constraints[[the.season]][[the.day.type]][[the.transit.type]][['energy.min.offsets']])
        min.energy.initial.offsets[,':='(min.offset=min.offset*scaling.factor)]
        power.constraints <- copy(energy.and.power.constraints[[the.season]][[the.day.type]][[the.transit.type]][['power']])
        power.constraints[,':='(max.power=max.power*scaling.factor,min.power=min.power*scaling.factor)]
        df <- energy.constraints[day_of_week==the.evipro.day.type]
        setkey(df,t)
        df[,t:=t+(i-1)*24]
        if(i>1){
          maxes <- cumul.energy.constraints[[length(cumul.energy.constraints)]][,list(max.max=max(max.energy),max.min=max(min.energy))]
          mins <- df[,list(the.min=min(min.energy))]
          df[,max.energy:=max.energy+maxes$max.max]
          df[,min.energy:=min.energy+maxes$max.min - mins$the.min + min.energy.initial.offsets[day_of_week==the.evipro.day.type]$min.offset]
        }
        #cat(pp("First max energy for day ",the.day,": ",df$max.energy[1],"\n"))
        df[,max.power:=power.constraints$max.power]
        df[,min.power:=power.constraints$min.power]
        #ggplot(df,aes(x=t,y=min.energy,colour=day_of_week))+geom_line()+geom_line(aes(y=max.energy))
        cumul.energy.constraints[[length(cumul.energy.constraints)+1]] <- df
        chargers[[length(chargers)+1]] <- energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['unscaled.charger.reqs']][,.(dest_type,dest_chg_level,n.ch=req*scaling.factor,day=i)]
        fleets[[length(fleets)+1]] <- energy.and.power.constraints[[season]][[weekday.type]][[transit.type]][['private.fleet.share']][,.(name,n.veh=share*fleet_size,day=i)]
      }
      cumul.energy.constraints <- rbindlist(cumul.energy.constraints)
      chargers <- rbindlist(chargers)[,.(n.ch=max(n.ch)),by=c('dest_type','dest_chg_level')]
      fleets <- rbindlist(fleets)[,.(n.veh=mean(n.veh)),by=c('name')]

      #p <- ggplot(cumul.energy.constraints,aes(x=t,y=min.energy,colour=day_of_week))+geom_line()+geom_line(aes(y=max.energy))
      #ggsave(pp(substr(cache.file,1,nchar(cache.file)-6),'.pdf'),p,width=10,height=8,units='in')

      cumul.energy.constraints[,t:=pp('t',sprintf('%04d',t+1))]
      cumul.energy.constraints[,rmob:=the.region]
      chargers[,rmob:=the.region]
      fleets[,rmob:=the.region]
      all.chargers[[length(all.chargers)+1]] <- chargers
      all.fleets[[length(all.fleets)+1]] <- fleets
      all.all.energy.constraints[[length(all.all.energy.constraints)+1]] <- copy(cumul.energy.constraints)
    }
    all.all.energy.constraints <- rbindlist(all.all.energy.constraints)
    all.chargers <- rbindlist(all.chargers)
    all.fleets <- rbindlist(all.fleets)
    inputs <- list()
    inputs$sets <- list()
    inputs$parameters <- list()
    inputs$parameters$personalEVChargeEnergyUB <- all.all.energy.constraints[,list(t,rmob,value=ifelse(max.energy>=min.energy,max.energy,min.energy))]
    inputs$parameters$personalEVChargeEnergyLB <- all.all.energy.constraints[,list(t,rmob,value=min.energy)]
    inputs$parameters$personalEVChargePowerUB <- all.all.energy.constraints[,list(t,rmob,value=max.power)]
    inputs$parameters$personalEVChargePowerLB <- all.all.energy.constraints[,list(t,rmob,value=min.power)]
    inputs$parameters$personalEVFleetSize <- all.fleets[,.(rmob,value=n.veh,type=name)]
    inputs$parameters$personalEVChargers <- all.chargers[,.(rmob,value=n.ch,type=dest_type,level=dest_chg_level)]
  }else{
    zero <- data.table(expand.grid(t=common.inputs$sets$t,rmob=common.inputs$sets$rmob,value=0)) 
    inputs$parameters$personalEVChargeEnergyUB <- zero
    inputs$parameters$personalEVChargeEnergyLB <- zero
    inputs$parameters$personalEVChargePowerUB <- zero
    inputs$parameters$personalEVChargePowerLB <- zero
    inputs$parameters$personalEVFleetSize <- data.table(expand.grid(rmob=common.inputs$sets$rmob,value=0,type=c('PHEV20','PHEV50','BEV100','BEV250'))) 
    inputs$parameters$personalEVChargers <- data.table(expand.grid(t=common.inputs$sets$t,rmob=common.inputs$sets$rmob,value=0,type=c(type=c('Public','Home','Work'),level=c('L1','L2','L3')))) 
  }
  inputs
}
