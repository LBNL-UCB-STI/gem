###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This has functions needed to prepare the inputs required by the mobility model.
#
# Argument: one row of the exper.runs table as produced by load.experiment
# Returns: list containing all data tables needed to run the model (as named data.tables)
###############################################################################################

prep.inputs.mobility <- function(exper.row,common.inputs){
  param.names <- names(exper.row)

  inputs <- list()

  ##### GENERIC PROCESSING OF SIMPLE PARAMS #####
  generic.params <- c('battery.cost','dist.bin.labels','discount.rate')

  for(generic.param in generic.params){
    if(generic.param %in% param.names){
      streval(pp('inputs$',generic.param,' <- exper.row$',generic.param))
    }else{
      streval(pp('inputs$',generic.param,' <- ',generic.param))
    }
  }


  ##### SHARING #####
  if('sharing.factor'%in%param.names){
    inputs$sharing.factor <- data.table(d=common.inputs$d,sharingFactor=exper.row$sharing)
  }else{
    inputs$sharing.factor <- sharing.factor
  }

  ##### URBAN FORM - STRAIGHT SCALING ALL REGIONS #####
  if('scale.urban.form.factor' %in% param.names){
    inputs$urban.form.factor <- data.table(r=common.inputs$r,urban.form.factor=exper.row$scale.urban.form.factor * 1.3)
  }else{
    inputs$urban.form.factor <- data.table(r=common.inputs$r,urban.form.factor=1.3)
  }

  #### DEMAND ####
  if(F){
    # The following is only needed to reshape the data from Brian Gerke's python script into a single data frame, so only used when that source data is updated
    dfs <- list()
    for(file in grep('Rdata',grep('dist_hour_hists',list.files(gem.raw.inputs),value=T),invert=T,value=T)){
      df <- data.table(read.csv(pp(gem.raw.inputs,file),stringsAsFactors=F))
      df[,season:=str_split(file,"_")[[1]][4]]
      df[,transit:=str_split(file,"_")[[1]][5]]
      dfs[[length(dfs)+1]] <- df
    }
    dem <- rbindlist(dfs)
    save(dem,file=pp(gem.raw.inputs,'dist_hour_hists.Rdata'))
  }
  load(pp(gem.raw.inputs,'dist_hour_hists.Rdata'))
  dem[,dist:=AVGDIST]
  dem[,d:=MILEBIN]
  dem[,r:=pp(CDIVLS,'-',URBRURS)]
  dem[,':='(MILEBIN=NULL,CDIVLS=NULL,URBRURS=NULL,AVGDIST=NULL,NRAW=NULL,NWTD=NULL)]
  dem[,':='(day.type=WKTIME,WKTIME=NULL)]
  dem <- melt(dem,id.vars=c('d','dist','season','transit','r','day.type'),variable.name='t',value.name='trips')
  dem[,t:=as.numeric(unlist(lapply(str_split(t,"X"),function(x){x[2]})))]
  dem[,use.transit:=transit=='with']
  #ggplot(dem[,.(trips=sum(trips)),by=c('t','d','season','transit')],aes(x=t,y=trips,colour=d))+geom_line()+facet_grid(season~transit)


  #for a truly weighted average distance by bin, weight the day.type
  dem[,weighted.trips:=trips*ifelse(day.type=="TU/WE/TH",3,2)]

  # Day of the week for the days in the simulated year (1 == Sunday, 7 == Saturday)
  wdays <- wday(to.posix(pp(year,'-01-01 00:00:00+00'))+24*3600*(days-1))
  months <- month(to.posix(pp(year,'-01-01 00:00:00+00'))+24*3600*(days-1))
  all.dem <- list()
  for(i in 1:length(days)){
    the.day <- days[i]
    the.month <- months[i]
    the.wday <- wdays[i]
    if(the.wday==1 || the.wday==7){
      the.day.type <- "SA/SU"
    }else if(the.wday==2 || the.wday==6){
      the.day.type <- "MO/FR"
    }else{
      the.day.type <- "TU/WE/TH"
    }
    if(the.month <=2 || the.month==12){
      the.season <- 'dec-feb'
    }else if(the.month>=3 && the.month <=5){
      the.season <- 'mar-may'
    }else if(the.month>=6 && the.month <=8){
      the.season <- 'jun-aug'
    }else if(the.month>=9 && the.month <=11){
      the.season <- 'sep-nov'
    }
    the.dem <- copy(dem[day.type==the.day.type & use.transit == include.transit.demand & season == the.season])
    the.dem[,t:=pp('t',1 + t + 24*(i-1))]
    all.dem[[length(all.dem)+1]] <- the.dem[,.(r,t,d,trips)]
  }
  all.dem <- rbindlist(all.dem)
  inputs$demand <- all.dem

  ##### DISTANCE BINS #####
  inputs$d <- pp('d',sort(u(dem$d)))
  inputs$travelDistance <- dem[,.(d=pp('d',d),travelDistance=weighted.mean(dist,weighted.trips)),by=c('d','r')]
  inputs$travelDistance[,d:=NULL]

  #### SPEED ####
  speed.by.dist <- data.table(d=c("d0-2","d2-5","d5-10","d10-20","d20-30","d30-50","d50-100","d100-300"),speed=c(18,22,32,38,40,45,48,48))
  speeds <- data.table(expand.grid(list(common.inputs$r,common.inputs$t,inputs$d)))
  names(speeds) <- c('r','t','d')
  speeds <- join.on(speeds,speed.by.dist,'d','d')
  setkey(speeds,r,d,t)
  inputs$speed <- speeds[,.(t=t,d,r,speed)]

  ##### CHARGING INFRASTRUCTURE #####
  charger.levels.str <- pp('L',str_pad(charger.levels,3,'left','0'))
  inputs$l <- charger.levels.str
  #### CHARGER CAPITAL ####
  if('l10.charger.cost'%in%param.names){
    l10.charger.cost <- exp.pars$l10.charger.cost[exp.i]
  }else{
    l10.charger.cost <- 500
  }
  if('charger.cap.superlinear'%in%param.names){
    charger.cap.superlinear <- exp.pars$charger.cap.superlinear[exp.i]
  }else{
    charger.cap.superlinear <- 5
  }
  charger.cap.costs <- l10.charger.cost + c(charger.levels-min(charger.levels))*charger.cap.superlinear
  inputs$charger.cap.costs <- data.table(l=charger.levels.str,cap.costs=charger.cap.costs)
  #### CHARGER POWER ####
  inputs$charger.power <- data.table(l=charger.levels.str,charger.power=charger.levels)
  #### CHARGER DISTRBITUION FACTOR ####
  inputs$charger.distrib.factor <- data.table(l=charger.levels.str,charger.distrib.factor=1)
  
  inputs
}

