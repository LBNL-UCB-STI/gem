###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This makes plots related to mobility.
#
# Argument: the results list containing GAMS outputs and the plotting directory
###############################################################################################

plots.mobility <- function(exper,all.inputs,res,plots.dir){
  inputs <- all.inputs[[1]]
  source("src/colors.R") 
  
  # Day-to-Year ratio based on travel demand
  n.days.in.run <- length(inputs$set$t)/24
  
  # Vehicle Distribution
  veh.mv <- data.table(cast(melt(res[['b-d-rmob-t']],id.vars=c('t','b','d','rmob','run'),measure.vars=c('vehiclesMoving')),b + rmob + t + run ~ d))
  d.dot <- str_replace(inputs$sets$d,"-",".")
  streval(pp('veh.mv[,":="(',pp(pp('vehiclesMoving.',d.dot,'=`',inputs$sets$d,'`,`',inputs$sets$d,'`=NULL'),collapse=','),')]'))
  l.dot <- str_replace(inputs$sets$l,"-",".")
  veh.ch <- data.table(cast(melt(res[['b-l-rmob-t']],id.vars=c('t','b','l','rmob','run'),measure.vars=c('vehiclesCharging')),b + rmob + t + run ~ l))
  streval(pp('veh.ch[,":="(',pp(pp('vehiclesCharging.',l.dot,'=`',inputs$sets$l,'`,`',inputs$sets$l,'`=NULL'),collapse=','),')]'))
  vehs <- join.on(res[['b-rmob-t']],join.on(veh.mv,veh.ch,c('b','rmob','t','run')),c('b','rmob','t','run'))
  veh.ch <- melt(res[['b-l-rmob-t']],id.vars=c('t','b','l','rmob','run'),measure.vars=c('vehiclesCharging'))
  veh.ch[,kw:=unlist(lapply(str_split(l,'L'),function(ll){ as.numeric(ll[2])}))]
  veh.ch[,gw.charging:=kw*value/1e6]
  setkey(veh.ch,run,l,rmob,t)
  veh.ch[,charger.level:=gsub('L|L0','',l)]
  veh.ch[,charger.level:=paste(charger.level,'kW')]
  veh.ch$charger.level <- factor(veh.ch$charger.level,levels=unique(veh.ch$charger.level)[mixedorder(unique(veh.ch$charger.level))])
  
  # Private Vehicle Charging
  personal.ev.ch <- res[['rmob-t']][,.(t,rmob,gw.charging=personalEVPower/1e6,run)]
  personal.ev.ch[,l:='Private EVs']
  personal.ev.ch[,charger.level:='Private EVs']
  
  # Energy balance
  en <- join.on(join.on(res[['b-l-rmob-t']],res[['b-l-rmob']],c('l','rmob','b','run'),c('l','rmob','b','run'))[,.(en.ch=sum(energyCharged)),by=c('t','rmob','b','run')],res[['b-d-rmob-t']][,.(en.mob=sum(energyConsumed)),by=c('t','rmob','b','run')],c('b','rmob','t','run'),c('b','rmob','t','run'))
  batt <- join.on(res[['b-rmob']],res[['b']],c('b','run'),c('b','run'))
  batt[,soc:=fleetSize*batteryCapacity]
  en <- join.on(en,batt,c('b','rmob','run'),c('b','rmob','run'),'soc')
  #en[t>0,soc:=0]
  setkey(en,b,rmob,t)
  en[,soc:=soc+cumsum(en.ch-en.mob),by=c('b','rmob','run')]
  en[,battery.level:=gsub('b|b0','',b)]
  en[,battery.level:=paste(battery.level,'mi')]
  en$battery.level <- factor(en$battery.level,levels=unique(en$battery.level)[mixedorder(unique(en$battery.level))])
  
  # Fleet size and num chargers
  by.r <- rbindlist(list(res[['l-rmob']][,':='(run=run,variable=l,value=numChargers,group='Chargers')],res[['b-rmob']][,':='(run=run,variable=b,value=fleetSize,group='Fleet')]),fill=T)
  setkey(by.r,run,variable)
  by.r[,var.clean:=ifelse(grepl('L',variable),paste('Chgr:',variable,'kW'),paste('Bat:',variable,'mi'))]
  by.r[,var.clean:=gsub(' L| L0| b| b0',' ',var.clean)]
  by.r$var.clean <- factor(by.r$var.clean,levels=unique(by.r$var.clean)[mixedorder(unique(by.r$var.clean))])

  # Merit order
  geners <- copy(generators)
  geners$g <- as.character(geners$g)
  geners$FuelType <- as.character(geners$FuelType)
  geners <- merge(x=geners,y=fuels,by='FuelType',all.x=TRUE)
  geners$Simplified <- factor(geners$Simplified,levels=meritOrder)

  # Key for matching regions to shapefile region names
  region.key <- data.table(r=c('ENC','MAT-NL','MAT-NY','MTN','NENG','PAC-CA','PAC-NL','SAT-FL','SAT-NL','WNC','WSC-NL','WSC-TX','ESC'),NAME=c('East North Central','Middle Atlantic','New York','Mountain','New England','California','Pacific','Florida','South Atlantic','West North Central','West South Central','Texas','East South Central'))

  # Map data
  eia.regions <- st_read(file.path(pp(gem.raw.inputs,'census-division-plus-big-4/','census-division-plus-big-four.shp')))
  centroids <- matrix(unlist(do.call(rbind,st_centroid(eia.regions))['geometry',]),nrow=2)
  centroids <- data.table(NAME=eia.regions$NAME,long=centroids[1,],lat=centroids[2,])
  centroids <- merge(x=centroids,y=region.key,by='NAME')

  states <- map_data('state')

  disag.the.private.load <- function(df,personalEVUnmanagedLoads){
    df[l!='Private EVs',charger.level:=pp('SAEV: ',charger.level)]
    # Split private EV out by charger level
    proportional.split <- personalEVUnmanagedLoads[,.(frac=sum(value)/sum(personalEVUnmanagedLoads$value)),by='l']
    private.disag <- join.on(rbindlist(list(df[l=='Private EVs'][,ll:='L1'],df[l=='Private EVs'][,ll:='L2'],df[l=='Private EVs'][,ll:='L3'])),proportional.split,'ll','l')
    private.disag[,gwh:=frac*gwh]
    private.disag[,charger.level:=ifelse(ll=='L1','Private: 1.5kW',ifelse(ll=='L2','Private: 7kW','Private: 50kW'))]
    private.disag[,l:=ifelse(ll=='L1','Private: 1.5kW',ifelse(ll=='L2','Private: 7kW','Private: 50kW'))]
    rbindlist(list(df[l!='Private EVs'],private.disag),fill=T)
  }

  geners[,g:=as.numeric(g)]
  res[['g-t']][,g:=as.numeric(g)]
  generation <- merge(x=res[['g-t']],y=geners,by='g',all.x=TRUE)
  generation <- generation[,list(generation=sum(generation),base.generation=sum(base.generation)),by=list(run,r,Simplified,t)]
  generation[,consq.generation:=generation-base.generation]
  generation <- generation[complete.cases(generation),]
  
  gen.cost <- merge(x=res[['g-t']],y=geners,by='g',all.x=TRUE)
  gen.cost <- gen.cost[,list(energyCost=sum(generationCosts*(generation-base.generation))),by=list(r,run)]

  # Run by Run Plots
  run.i <- u(vehs$run)[1]
  for(run.i in u(vehs$run)){
    if(F){
    day.axis.breaks <- seq(0,max(veh.ch$t),by=24)
    setkey(vehs,run,b,rmob,t)
    # to.plot <- melt(vehs[run==run.i],id.vars=c('b','rmob','t'))
    # to.plot[,vehicle.activity:=gsub('\\.L0|\\.L','(',gsub('vehiclesCharging','Charging ',variable))]
    # to.plot[,vehicle.activity:=ifelse(grepl('Charging',vehicle.activity),paste(vehicle.activity,'kW)'),vehicle.activity)]
    # to.plot[,vehicle.activity:=gsub('\\.','-',gsub('\\.d','(',gsub('vehiclesMoving','Moving ',vehicle.activity)))]
    # to.plot[,vehicle.activity:=ifelse(grepl('Moving',vehicle.activity),paste(vehicle.activity,' mi)'),vehicle.activity)]
    # to.plot[,vehicle.activity:=ifelse(grepl('run',vehicle.activity),'Moving (private)',ifelse(grepl('vehiclesIdle',vehicle.activity),'Idling',vehicle.activity))]
    # to.plot$vehicle.activity <- factor(to.plot$vehicle.activity,levels=unique(to.plot$vehicle.activity)[unique(mixedorder(to.plot$vehicle.activity))])
    # p <- ggplot(to.plot,aes(x=t,y=value/1000,fill=vehicle.activity))+
    #   geom_bar(stat='identity',position='stack')+
    #   xlab('Hour')+
    #   ylab('Number of vehicles (thousands)')+
    #   facet_wrap(~rmob,scales='free_y')+
    #   theme_bw()+
    #   scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$vehicle.activity))
    # pdf.scale <- 1
    # ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    to.plot <- melt(vehs[run==run.i],id.vars=c('b','t','rmob','run'))
    to.plot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),pp('Charging: SAEV ',substr(b,2,5),'mi'),ifelse(variable=='vehiclesIdle',pp('Idle: SAEV ',substr(b,2,5),'mi'),pp('Moving: SAEV ',substr(b,2,5),'mi')))]
    to.remove <- to.plot[,sum(value),by='variable.simp'][V1<1e-4]$variable.simp
    to.plot <- to.plot[!variable.simp%in%to.remove]
    to.plot[,variable.simp:=factor(variable.simp,c(rev(u(to.plot$variable.simp))))]
    if(nrow(to.plot)>0){
      setkey(to.plot,variable.simp)
      p<-ggplot(to.plot[,.(value=sum(value)),by=c('t','variable.simp')],aes(x=t,y=value/1000,fill=variable.simp))+
        geom_area()+
        xlab('Hour')+
        ylab('Number of vehicles (thousands)')+
        scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$variable.simp),guide=guide_legend(reverse=F))+
        scale_x_continuous(breaks=day.axis.breaks)+
        theme_bw()
      pdf.scale <- 1
      ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs-simple-2.pdf'),p,width=10*pdf.scale,height=5*pdf.scale,units='in')
      
      to.plot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),'Charging',ifelse(variable=='vehiclesIdle','Idle','Moving'))]
      to.plot[,variable.simp:=factor(variable.simp,c('Idle','Moving','Charging'))]
      setkey(to.plot,variable.simp)
      p <- ggplot(to.plot,aes(x=t,y=value/1000,fill=variable.simp))+
        geom_area()+
        xlab('Hour')+
        ylab('Number of vehicles (thousands)')+
        facet_wrap(~rmob,scales='free_y')+
        scale_x_continuous(breaks=day.axis.breaks)+
        scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$variable.simp),guide=guide_legend(reverse=F))+
        theme_bw()
      ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs-simple.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    }
  

    # Charging load
    p <- ggplot(veh.ch[run==run.i],aes(x=t,y=gw.charging,fill=fct_rev(charger.level)))+
      geom_area()+
      facet_wrap(~rmob,scales='free_y')+
      scale_fill_manual(values = getPalette(veh.ch$charger.level),guide=guide_legend(reverse=F))+
      labs(x='Hour',y='Load (GW)',fill='Charger Level')+
      theme_bw()
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_charging-saevs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    to.plot <- rbindlist(list(veh.ch,personal.ev.ch),fill=T,use.names=T)[run==run.i]
    to.plot[,gwh:=gw.charging]
    to.plot[,ll:='']
    if(exper$runs[run.i]$fractionSAEVs<1.0)to.plot <- disag.the.private.load(to.plot,all.inputs[[run.i]]$parameters$personalEVUnmanagedLoads)
    to.plot[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),l)]
    to.plot[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]
    the.ch.cols <- to.plot$col
    names(the.ch.cols) <- to.plot$charger.level
    p <- ggplot(to.plot,aes(x=t,y=gwh,fill=charger.level))+
      geom_area()+
      facet_wrap(~rmob,scales='free_y')+
      scale_fill_manual(values = the.ch.cols)+
      labs(x='Hour',y='Load (GW)',fill='Charger Level')+
      theme_bw()
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_charging-all.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    p <- ggplot(to.plot[,.(gwh=sum(gwh)),by=c('t','charger.level')],aes(x=t,y=gwh,fill=charger.level))+
      scale_x_continuous(breaks=day.axis.breaks)+
      geom_area()+
      scale_fill_manual(values = the.ch.cols)+
      labs(x='Hour',y='Load (GW)',fill='Charger Level')+
      theme_bw()
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_charging-all-agg.pdf'),p,width=10*pdf.scale,height=5*pdf.scale,units='in')

    personal.evs.total <- all.inputs[[run.i]]$parameters$personalEVFleetSize
    personal.evs.total <- personal.evs.total[rep(1:nrow(personal.evs.total),length(u(personal.ev.ch$t)))]
    personal.evs.total[,t:=rep(1:length(u(personal.ev.ch$t)),each=nrow(personal.evs.total)/length(u(personal.ev.ch$t)))]
    personal.evs.total <- personal.evs.total[,.(Total=sum(value)),by=.(t,rmob)]

    chts.trips$n <- 1:nrow(chts.trips)
    chts.hours <- unlist(sapply(chts.trips$n,function(x) seq(chts.trips$start_hour[x],chts.trips$end_hour[x])))
    chts.hours <- data.table(table(chts.hours))
    names(chts.hours) <- c('Hour','Count')
    chts.hours[,Probability:=Count/sum(Count)]
    chts.hours[,Hour:=as.numeric(Hour)+1]
    chts.hours <- chts.hours[rep(1:nrow(chts.hours),length(u(personal.ev.ch$t))/24),]
    chts.hours[,t:=(1:length(u(personal.ev.ch$t)))]
    chts.hours <- chts.hours[,.(t,Probability)]

    personal.evs.total <- merge(x=personal.evs.total,y=chts.hours,by='t',all.x=TRUE)
    personal.evs.total[,Moving:=Total*Probability]

    personal.evs.ch.total <- to.plot[ll%in%c('L1','L2','L3'),.(gw.charging=sum(gw.charging)),by=.(ll,t,rmob)]
    personal.evs.ch.total[,Charging:=ifelse(ll=='L1',gw.charging*10^6/1.5,ifelse(ll=='L2',gw.charging*10^6/6.7,gw.charging*10^6/50))]
    personal.evs.ch.total <- personal.evs.ch.total[,.(Charging=sum(Charging)),by=.(t,rmob)]

    personal.evs.total <- merge(x=personal.evs.total,y=personal.evs.ch.total,by=c('t','rmob'))
    personal.evs.total[,Charging:=ifelse(Charging>Total,Total,Charging)]
    personal.evs.total[,Moving:=ifelse(Moving+Charging>Total,Total-Charging,Moving)]
    personal.evs.total[,Idle:=Total-Moving-Charging]
    personal.evs.total <- personal.evs.total[,.(Moving=sum(Moving),Charging=sum(Charging),Idle=sum(Idle)),by=.(t)]
    personal.evs.total[Idle<0,Idle:=0]
    personal.evs.total <- melt(personal.evs.total,id='t',variable.name='Activity',value.name='Cars')
    personal.evs.total$Activity <- factor(personal.evs.total$Activity,levels=c('Idle','Moving','Charging'))

    p<-ggplot(personal.evs.total,aes(x=t,y=Cars/1000,fill=Activity))+
      geom_area()+
      xlab('Hour')+
      ylab('Number of vehicles (thousands)')+
      scale_fill_manual(name='Personal Vehicle activity',values=c('Idle'='#bdbdbd','Moving'='#3182bd','Charging'='#ee3a3a'))+
      #scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$variable.simp),guide=guide_legend(reverse=F))+
      scale_x_continuous(breaks=day.axis.breaks)+
      theme_bw()
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs-simple-personal.pdf'),p,width=10*pdf.scale,height=5*pdf.scale,units='in')

    # Energy balance
    p <- ggplot(en[run==run.i],aes(x=t,y=soc/10^6,colour=fct_rev(battery.level)))+
      geom_line()+
      xlab('Fleet Energy State (GWh)')+
      ylab('Hour')+
      facet_wrap(~rmob,scales='free_y')+
      scale_colour_manual(name='Vehicle battery size',values = rev(getPalette(en$battery.level)),guide=guide_legend(reverse=F))+
      theme_bw() 
    ggsave(pp(plots.dir,'/run-',run.i,'/_soc.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    p <- ggplot(en,aes(x=t,y=soc/10^6,fill=fct_rev(battery.level)))+
      geom_area()+
      xlab('Hour')+
      ylab('Fleet Energy State (GWh)')+
      facet_wrap(~rmob,scales='free_y')+
      scale_fill_manual(name='Vehicle battery size',values=rev(getPalette(en$battery.level)),guide=guide_legend(reverse=F))+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_soc-bar.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Fleet size & type
    p <- ggplot(by.r[run==run.i],aes(x=rmob,y=value/1000,fill=fct_rev(var.clean)))+
      geom_bar(stat='identity')+
      xlab('Region')+
      ylab('Count (thousands)')+
      facet_wrap(~group,scales='free_y')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Charger/Battery Level',values = rev(getPalette(by.r$var.clean)),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_fleet-size-and-type.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
    
    to.plot <- by.r[run==run.i,.(value=value,percent=value/sum(value)*100,variable=var.clean),by=c('rmob','group')]
    to.plot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
    geo.ordered <- cbind(c(sapply(c('-RUR','-URB'),function(x){ pp(c('PAC-NL','PAC-CA','MTN','WNC','WSC-NL','WSC-TX','ENC','ESC','NENG','MAT-NL','MAT-NY','SAT-NL','SAT-FL'),x) })))[,1]
    to.plot[,rmob:=factor(rmob,geo.ordered)]
    setkey(to.plot,rmob,variable)
    #to.plot[,group:=factor(group,c('Fleet','Chargers'))]
    p <- ggplot(to.plot,aes(x=factor(rmob),y=percent,fill=fct_rev(variable)))+
      geom_bar(stat='identity')+
      xlab('Region')+
      ylab('Percent')+
      facet_grid(group~urb,scales='free_x', space ='free_x')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Charger/Battery Level',values = rev(getPalette(to.plot$variable)),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_fleet-size-and-type-percent.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
    to.remove <- to.plot[,sum(value),by='variable'][V1==0]$variable
    to.plot <- to.plot[!variable%in%to.remove,.(value=sum(value)/1e6,percent=value/sum(value)*100),by=c('group','variable','urb')]
    if(nrow(to.plot)>0){
      p <- ggplot(to.plot,aes(x=urb,y=value,fill=variable))+
        geom_bar(stat='identity')+
        xlab('Regional Type')+
        ylab('Count in Millions')+
        facet_wrap(~group,scales='free_y')+ 
        theme_bw()+
        scale_fill_manual(name='Charger/Battery Level',values = getPalette(to.plot$variable),guide=guide_legend(reverse=F))
      ggsave(pp(plots.dir,'/run-',run.i,'/_fleet-size-and-type-agg.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
    }
    
    # to.plot <- res[['rmob']][,.(rmob,urbanFormFactor)]
    # to.plot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
    # to.plot[,Region:=factor(rmob,geo.ordered)]
    # p <- ggplot(to.plot,aes(x=Region,y=urbanFormFactor))+geom_bar(stat='identity')+facet_wrap(~urb,scales='free_x')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+coord_cartesian(ylim=c(1,max(to.plot$urbanFormFactor)*1.01))
    # ggsave(pp(plots.dir,'/run-',run.i,'/_urban-form-factor-by-region.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    # Generation
    to.plot <- merge(x=res[['g-t']][run==run.i],y=geners,by='g',all.x=TRUE)
    to.plot <- to.plot[,list(generation=sum(generation),base.generation=sum(base.generation)),by=list(r,Simplified,t)]
    to.plot[,consq.generation:=generation-base.generation]
    to.plot[,consq.generation.pos:=ifelse(consq.generation>=0,consq.generation,0)]
    to.plot[,consq.generation.neg:=ifelse(consq.generation<0,consq.generation,-1)]
    to.plot <- to.plot[complete.cases(to.plot),]
    cols <- c('Other'='#7fc97f','Coal'='gray27','Hydro'='#386cb0','Natural Gas'='gray73','Nuclear'='#fb9a99','Solar'='#ffff99','Wind'='lightskyblue')
    p <- ggplot(to.plot,aes(x=t,y=generation/1000,fill=Simplified))+
      geom_area()+
      xlab('Hour')+
      ylab('Generation (GW)')+
      scale_fill_manual(name='Fuel Type',values=cols)+
      facet_wrap(~r,scale='free_y')+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_generation-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    p <- ggplot(to.plot,aes(x=t,fill=Simplified))+
      geom_area(aes(y=consq.generation.pos/1000))+
      geom_area(aes(y=consq.generation.neg/1000))+
      xlab('Hour')+
      ylab('Generation (GW)')+
      scale_fill_manual(name='Fuel Type',values=cols)+
      facet_wrap(~r,scale='free_y')+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_generation-cnsq-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    to.plot <- merge(x=res[['g-t']][run==run.i],y=geners,by='g',all.x=TRUE)
    to.plot <- to.plot[,list(emissions=sum(generationCO2*generation),base.emissions=sum(generationCO2*base.generation)),by=list(r,Simplified,t)]
    to.plot[,consq.emissions:=emissions-base.emissions]
    to.plot <- to.plot[complete.cases(to.plot),]
    p <- ggplot(to.plot,aes(x=t,y=emissions/1000,fill=Simplified))+
      geom_area()+
      xlab('Hour')+
      ylab('Hourly CO2 Emissions (tons)')+
      scale_fill_discrete(name='Fuel Type')+
      facet_wrap(~r,scale='free_y')+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_emissions-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    p <- ggplot(to.plot,aes(x=t,y=consq.emissions/1000,fill=Simplified))+
      geom_area()+
      xlab('Hour')+
      ylab('Hourly CO2 Emissions (tons)')+
      scale_fill_discrete(name='Fuel Type')+
      facet_wrap(~r,scale='free_y')+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_emissions-cnsq-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    p <- ggplot(to.plot,aes(x=t,y=consq.emissions/1000,colour=Simplified))+
      geom_line()+
      xlab('Hour')+
      ylab('Hourly CO2 Emissions (tons)')+
      scale_colour_discrete(name='Fuel Type')+
      facet_wrap(~r,scale='free_y')+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_emissions-cnsq-total-by-region-fuel-line.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    make.1d.map <- function() {
      pdf.scale <- 1
      toPlot <- merge(x=res[['g-t']],y=geners,by='g',all.x=TRUE)
      toPlot.map <- merge(x=toPlot,y=region.key,by='r',all.x=TRUE)
      toPlot.map <- toPlot.map[,list(Emissions.Total=sum(generationCO2*generation),Emissions.Conseq=sum(generationCO2*(generation-base.generation))),by=list(run,NAME)]
      toPlot.map$NAME <- factor(toPlot.map$NAME)
      toPlot.map <- merge(x=eia.regions,y=toPlot.map[run==run.i,.(NAME,Emissions.Conseq)],by='NAME',all.x=TRUE)

      toPlot.pie <- generation[run==run.i,.(consq.generation=sum(consq.generation,na.rm=TRUE)),by=.(Simplified,r)]
      toPlot.pie <- toPlot.pie[complete.cases(toPlot.pie),]
      toPlot.pie <- dcast(toPlot.pie,r~Simplified,value.var='consq.generation')
      toPlot.pie[is.na(toPlot.pie)] <- 0
      toPlot.pie <- melt(toPlot.pie,id='r',variable.name='Simplified',value.name='consq.generation')
      toPlot.pie <- merge(x=toPlot.pie,y=region.key,by='r')
      toPlot.pie[,NAME:=gsub(' ','\n',NAME)]

      make.pie <- function(pie,title=NA,legend.position=0) {
        if(is.na(title)) {
          title <- unique(pie$NAME)
        }
        cols <- c('Other'='#7fc97f','Coal'='gray27','Hydro'='#386cb0','Natural Gas'='gray73','Nuclear'='#fb9a99','Solar'='#ffff99','Wind'='lightskyblue')
        ggplot()+
          geom_bar(data=pie,aes(x='',y=consq.generation,fill=Simplified),color='black',stat='identity',width=1)+
          coord_polar('y')+
          ggtitle(title)+
          scale_fill_manual(name='Fuel Type',values=cols)+
          theme_void()+
          theme(legend.position=legend.position,plot.title=element_text(hjust=0.5))
      }

      ENC <- make.pie(toPlot.pie[r=='ENC'])
      ESC <- make.pie(toPlot.pie[r=='ESC'])
      MATNL <- make.pie(toPlot.pie[r=='MAT-NL'])
      MATNY <- make.pie(toPlot.pie[r=='MAT-NY'])
      MTN <- make.pie(toPlot.pie[r=='MTN'])
      NENG <- make.pie(toPlot.pie[r=='NENG'])
      PACCA <- make.pie(toPlot.pie[r=='PAC-CA'])
      PACNL <- make.pie(toPlot.pie[r=='PAC-NL'])
      SATFL <- make.pie(toPlot.pie[r=='SAT-FL'])
      SATNL <- make.pie(toPlot.pie[r=='SAT-NL'])
      WNC <- make.pie(toPlot.pie[r=='WNC'])
      WSCNL <- make.pie(toPlot.pie[r=='WSC-NL'])
      WSCTX <- make.pie(toPlot.pie[r=='WSC-TX'])

      leg <- get_legend(make.pie(toPlot.pie,'',legend.position='right'))

      map.plot <- ggplot()+
        geom_sf(data=toPlot.map,aes(fill=Emissions.Conseq/1000),color='black',lwd=0.2)+
        coord_sf()+
        xlim(-125,-68)+
        ylim(25,50)+
        scale_fill_gradient(name='Consequential\nCO2 Emissions (tons)',low='white',high='darkred')+
        theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())

      all <- ggdraw(map.plot)+
        draw_plot(ENC,x=.06,y=.65,height=.23)+
        draw_plot(ESC,x=.05,y=.33,height=.23)+
        draw_plot(MATNL,x=.15,y=.54,height=.2)+
        draw_plot(MATNY,x=.21,y=.64,height=.2)+
        draw_plot(MTN,x=-.25,y=.49,height=.17)+
        draw_plot(NENG,x=.3,y=.68,height=.2)+
        draw_plot(PACCA,x=-.42,y=.43,height=.17)+
        draw_plot(PACNL,x=-.39,y=.64,height=.17)+
        draw_plot(SATFL,x=.14,y=.13,height=.17)+
        draw_plot(SATNL,x=.16,y=.34,height=.2)+
        draw_plot(WNC,x=-.08,y=.58,height=.23)+
        draw_plot(WSCNL,x=-.05,y=.38,height=.23)+
        draw_plot(WSCTX,x=-.11,y=.24,height=.17)

      final.map <- cowplot::plot_grid(leg,all,rel_widths=c(0.1,1.1))
      ggsave(final.map,file=pp(plots.dir,'/run-',run.i,'/_map_conseqEmissions-pie-region.pdf'),width=12.5*pdf.scale,height=7*pdf.scale)
    }
  make.1d.map()
  }
  }
  # tr <- rbindlist(list(melt(res[['rmob-t']],id.vars=c('t','rmob','run')),melt(en[,.(en.mob=sum(en.mob),en.ch=sum(en.ch)),by=c('rmob','t','run')],id.vars=c('t','rmob','run'))))
  # tr[,group:=ifelse(variable=='price','Price',ifelse(str_sub(variable,1,3)=='en.','Energy','Cost'))]
  # tr <- tr[group!="Cost"]
  # prices <- tr[variable=='price']
  # p <- ggplot(tr,aes(x=t,y=value,colour=variable))+geom_line()+facet_grid(group~rmob,scales='free_y') # geom_bar(stat='identity',position='dodge')
  # ggsave(pp(plots.dir,'_energy-vs-price.pdf'),p,width=12*pdf.scale,height=8*pdf.scale,units='in')
  
  #gen.cost <- join.on(join.on(res[['g-t']],res[['g']],c('g','run'),c('g','run'))[,g:=as.numeric(g)],inputs$sets$gtor,'g','g')
  #gen.cost <- gen.cost[,.(energyCost=sum(genCost*generation)),by=c('run','r')]
  
  costs <- join.on(join.on(res[['rmob']],res[['rmob-t']][,.(demandChargeCost=sum(demandChargeCost),vehicleMaintCost=sum(vehicleMaintCost)),by=c('run','rmob')],c('run','rmob'),c('run','rmob'),c('demandChargeCost','vehicleMaintCost')),inputs$sets$rmobtor,'rmob','rmob')
  costs <- merge(x=costs,y=gen.cost,by=c('r','run'),all.x=TRUE)
  
  # JANKY FIX JUST TO MAKE PLOT (since the merge duplicates the values)
  costs$energyCost <- costs$energyCost/2
  
  # TODO join gen cost with costs but make gen cost be divided proportionately by energy consumed including personal vehicles
  
  vmt <- join.on(res[['b-d-rmob-t']],res[['d-rmob']],c('d','rmob','run'),c('d','rmob','run'),'travelDistance')
  vmt <- join.on(vmt,res[['d-rmob-t']],c('d','t','rmob','run'),c('d','t','rmob','run'),'speed')
  vmt[,vmt:=vehiclesMoving*travelDistance]
  vmt[,pmt:=demandAllocated*travelDistance]
  fleet <- res[['b-rmob']]
  vmt.by.region <- join.on(join.on(fleet,vmt[,.(vmt=sum(vmt)),by=c('rmob','b','run')],c('rmob','b','run'),c('rmob','b','run')),inputs$parameters$vehicleLifetime[,.(value=mean(value)),by=c('b','rmob')],c('rmob','b'),c('rmob','b'),'value','assumed.lifetime.')
  n.days <- max(vmt$t)/24
  vmt.by.region[,daily.vmt.per.vehicle:=vmt/fleetSize/n.days]
  vmt.by.region[,battery.level:=gsub('b|b0','',b)]
  vmt.by.region[,battery.level:=paste(battery.level,'mi')]
  vmt.by.region$battery.level <- factor(vmt.by.region$battery.level,levels=unique(vmt.by.region$battery.level)[mixedorder(unique(vmt.by.region$battery.level))])
  
  data.to.save <- c('vehs','en','by.r','costs','veh.ch','vmt','fleet','personal.ev.ch','vmt.by.region','generation')
  run.params <- copy(exper$runs)[,run:=1:.N]
  for(dat.to.save in data.to.save){
    streval(pp(dat.to.save,' <- join.on(',dat.to.save,',run.params,\'run\',\'run\')'))
  }
  streval(pp('save(',pp(data.to.save,collapse=','),',file="',plots.dir,'/results-for-plotting.Rdata")'))
  #load(file=pp(plots.dir,'/resuls-for-plotting.Rdata'))
  
  ###################################
  # Across factor plots
  ###################################
  param.names <- exper$param.names
  
  # Overall Metrics: # vehicles, peak demand, # chargers, emissions, costs
  # (We also need a counterfactual case (all gasoline vehicles) to provide electricity demand, CO2 emissions and costs to compare with. )
  # Add a plot for utilization of chargers
  personal.ev.conversion.eff <- 0.325
  personal.evs <- rbindlist(lapply(1:length(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVFleetSize[,.(value=sum(value),run=i),by='type'] }))[,.(run,b=pp("Private_",type),value)]
  personal.evs <- join.on(personal.evs,run.params,'run','run')
  personal.evs[,batt.kwh:=unlist(lapply(str_split(b,"EV"),function(ll){ as.numeric(ll[2])}))*personal.ev.conversion.eff]
  
  to.plot.fleet <- rbindlist(list(by.r[group=='Fleet',.(value=sum(fleetSize),bplus=pp('SAEV_BEV',substr(b,2,nchar(b)))),by=c('run',param.names,'b')],personal.evs),fill=T)
  not.needed <- to.plot.fleet[,sum(value),by='b'][V1<=1]$b
  to.plot.fleet <- to.plot.fleet[!b%in%not.needed]
  to.plot.fleet[,variable:=ifelse(is.na(bplus),b,bplus)]
  to.plot.fleet[,metric:='Fleet Size']
  to.plot.fleet[,col:=getPalette(b)[match(to.plot.fleet$b,u(to.plot.fleet$b))]]
  to.plot.fleet <- join.on(to.plot.fleet,res[['b']],c('run','b'),c('run','b'))
  to.plot.fleet[is.na(batteryCapacity),':='(batteryCapacity=batt.kwh,conversionEfficiency=personal.ev.conversion.eff)]
  
  personal.chargers <- rbindlist(lapply(1:length(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVChargers[,.(value=sum(value),run=i),by=c('type','level')] }))[,.(run,type,level,value)]
  personal.chargers[,variable:=pp(type,'_Chgr')]
  personal.chargers[,level.kw:=ifelse(level=='L1',1.5,ifelse(level=='L2',6.7,50))]
  personal.chargers <- join.on(personal.chargers,run.params,'run','run')
  to.plot.chargers <- by.r[group=='Chargers',.(value=sum(numChargers)),by=c('run',param.names,'l')]
  to.plot.chargers[,level.kw:=as.numeric(substr(l,2,nchar(l)))]
  to.plot.chargers[,variable:=pp('SAEV_Chgr_',ifelse(level.kw<=20,'AC','DC'))]
  to.plot.chargers <- rbindlist(list(to.plot.chargers,personal.chargers),fill=T)
  to.plot.chargers <- to.plot.chargers[,.(value=sum(value),l=l[1]),by=c('run','variable',param.names)]
  to.plot.chargers[,metric:='# Chargers']
  to.plot.chargers[is.na(l),l:=variable]
  to.plot.chargers[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),l)]
  to.plot.chargers[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]
  
  to.plot.ch <- rbindlist(list(veh.ch,personal.ev.ch),fill=T,use.names=T)
  to.plot.ch <- join.on(to.plot.ch,to.plot.ch[,.(gw=sum(gw.charging,na.rm=T)),by=c('run','t')][,.(peak.t=t[which.max(gw)]),by='run'],'run','run')
  to.plot.peak.ch <- to.plot.ch[t==peak.t,.(value=sum(gw.charging,na.rm=T)),by=c('run','l',param.names)]
  to.plot.peak.ch[,level.kw:=as.numeric(substr(l,2,nchar(l)))]
  to.plot.peak.ch[,variable:=ifelse(l=='Private EVs','Private_EV_Load',pp('SAEV_Load_',ifelse(level.kw<=20,'AC','DC')))]
  to.plot.peak.ch <- to.plot.peak.ch[,.(value=sum(value),l=l[1],level.kw=level.kw[1]),by=c('run',param.names,'variable')]
  to.plot.peak.ch[,metric:='Peak Load']
  to.plot.peak.ch[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),l)]
  to.plot.peak.ch[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]
  
  #private ev costs
  privateChargerLifetime <- inputs$parameters$chargerLifetime$value[1]*365
  privateVehicleLifetime <- 11*365
  privateBatteryLifetime <- privateVehicleLifetime
  dailyDiscount <- discountRate/365
  vehCapCost <- 30e3
  vehPerDayCost <- 600 / 365
  vehPerMileCosts <- 0.09
  private.charger.levels <- u(personal.chargers$level.kw)
  infra.cost <- join.on(personal.chargers,data.table(level.kw=private.charger.levels,cost.per.kw=l10ChargerCost + c(private.charger.levels-10)*chargerCostSuperlinear),'level.kw','level.kw')
  infra.cost[level=='L1',cost.per.kw:=50]
  infra.cost[,cost:=cost.per.kw*level.kw*value]
  infra.cost <- infra.cost[,.(privateInfrastructureCost=(sum(cost)*dailyDiscount*(1+dailyDiscount)^privateChargerLifetime)/((1+dailyDiscount)^privateChargerLifetime-1)),by='run']
  veh.amort.ratio <- dailyDiscount*(1+dailyDiscount)^privateVehicleLifetime / ((1+dailyDiscount)^privateVehicleLifetime - 1)
  batt.amort.ratio <- dailyDiscount*(1+dailyDiscount)^privateBatteryLifetime / ((1+dailyDiscount)^privateBatteryLifetime - 1)
  veh.cost <- personal.evs[,.(n=sum(value)),by=c('run','batt.kwh')][,.(privateFleetCost=sum(n*(vehPerDayCost + vehCapCost*veh.amort.ratio + batt.kwh*inputs$parameters$batteryCapitalCost$value*batt.amort.ratio + vehPerMileCosts*12e3/365))),by='run']
  # ICE Cost comparison
  iceCapCost <- 27.66e3
  iceNumVehicles <- 255e6
  iceAnnualFleetCost <- iceCapCost * iceNumVehicles * discountRate * (1 + discountRate)^11 / ((1+discountRate)^11 - 1)
  icePerMileCost <- 0.12
  iceAnnualVMT <- 2883e9
  iceTotalFuelCost <- 370.5e9
  iceAnnualCost <- iceAnnualFleetCost + iceTotalFuelCost + icePerMileCost*iceAnnualVMT
  iceTCOPerMile <- iceAnnualCost/iceAnnualVMT
  
  costs[,totalFleetCost:=vehicleMaintCost/n.days.in.run+fleetCost]
  costs[,totalEnergyCost:=demandChargeCost/n.days.in.run+energyCost/n.days.in.run]
  to.plot.cost <- melt(costs,measure.vars=c('totalEnergyCost','totalFleetCost','infrastructureCost'),id.vars=c('run',param.names))[,.(value=sum(value)),by=c('variable','run',param.names)]
  to.plot.cost <- rbindlist(list(to.plot.cost,melt(join.on(join.on(infra.cost,veh.cost,'run','run'),run.params,'run','run'),measure.vars=c('privateInfrastructureCost','privateFleetCost'),id.vars=c('run',param.names))),fill=T)
  to.plot.cost[,max.value:=max(to.plot.cost[,.(val=sum(value)),by='run']$val)]
  to.plot.cost[,metric:='Cost']
  to.plot.cost[,variable.short:=variable]
  to.plot.cost[,value:=value*weekday.to.year.factor]
  cost.key <- data.table(variable=c('infrastructureCost','totalFleetCost','totalEnergyCost','privateInfrastructureCost','privateFleetCost'),cost.key=c('SAEV Infrastructure','SAEV Fleet','Energy','Private Infrastructure','Private Fleet'))
  to.plot.cost <- join.on(to.plot.cost,cost.key,'variable.short','variable')
  to.plot.cost[,variable:=pp('Cost: ',cost.key)]
  to.plot.cost[,col:=getPalette(variable.short)[match(variable.short,u(variable.short))]]
  
  # Emissions
  saev.lifetimes <- vmt.by.region[,.(vehicleLifetime=vehicleLifetime[1],batteryLifetime=batteryLifetime[1]),by=c('run','b')]
  saev.lifetimes[,veh.amort.ratio:=dailyDiscount*(1+dailyDiscount)^(vehicleLifetime*365) / ((1+dailyDiscount)^(vehicleLifetime*365) - 1)]
  saev.lifetimes[,batt.amort.ratio:=dailyDiscount*(1+dailyDiscount)^(batteryLifetime*365) / ((1+dailyDiscount)^(batteryLifetime*365) - 1)]
  if(is.character(res[['g-t']]$g[1]))res[['g-t']][,g:=as.numeric(g)]
  if(is.character(geners$g[1]))geners[,g:=as.numeric(g)]
  to.plot.em <- join.on(res[['g-t']],geners,'g','g')[!Simplified%in%c('Solar','Wind','Hydro','Pumps','Nuclear','Geothermal')]
  to.plot.em[is.na(Simplified) | Simplified=='Biomass',Simplified:='Other']
  to.plot.em <- to.plot.em[,list(emissions=sum(generationCO2*generation),base.emissions=sum(generationCO2*base.generation)),by=c('run','Simplified')]
  to.plot.em[,value:=(emissions-base.emissions)/n.days.in.run]
  veh.manuf.em <- to.plot.fleet[,.(n=sum(value),batteryCapacity=batteryCapacity[1]),by=c('run','b')]
  veh.manuf.em <- join.on(veh.manuf.em,saev.lifetimes,c('run','b'),c('run','b'))
  veh.manuf.em[is.na(vehicleLifetime),':='(vehicleLifetime=privateVehicleLifetime/365,batteryLifetime=privateBatteryLifetime/365,veh.amort.ratio=dailyDiscount*(1+dailyDiscount)^(privateVehicleLifetime) / ((1+dailyDiscount)^(privateVehicleLifetime) - 1),batt.amort.ratio=dailyDiscount*(1+dailyDiscount)^(privateBatteryLifetime) / ((1+dailyDiscount)^(privateBatteryLifetime) - 1))]
  veh.manuf.em[,value:=n*7.1*veh.amort.ratio+n*0.11*batteryCapacity*batt.amort.ratio] # 7.1 and 0.11 are tons per vehicle
  veh.manuf.em[,Simplified:='Vehicle Manufacture']
  to.plot.em <- rbindlist(list(to.plot.em,veh.manuf.em[,.(value=sum(value)),by=c('Simplified','run')]),use.names=T,fill=T)
  to.plot.em[,value:=value*weekday.to.year.factor]
  #to.plot.em<- to.plot.em[complete.cases(to.plot.em),]
  to.plot.em[,metric:='Emissions']
  to.plot.em[,variable:=factor(as.character(Simplified))]
  to.plot.em <- join.on(to.plot.em,run.params,'run','run')
  to.plot.em[,col:=getPalette(variable)[match(variable,u(variable))]]
  
  metric.units <- data.table(metric=c('Fleet Size','# Chargers','Peak Load','Emissions','Cost'),
                             scale.factor=c(1e6,1e6,1,1e6,1e9),
                             label=c('Millions of Vehicles','Millions of Chargers','Peak Load (GW)','Emissions (Million Tonnes CO2eq)','Cost (Billions of $)'))
  factor.labels <- c('fractionSAEVs'='Fraction SAEVs in Fleet','fractionSmartCharging'='Fraction Smart Charging in Private Fleet','renewableScalingFactor'='Solar & Wind Capacity Scaling Factor')
  for(param.name in param.names){
    if(!param.name %in% names(factor.labels)){
      factor.labels <- c(factor.labels,streval(pp("c('",param.name,"'='",param.name,"')")))
    }
  }
  
  all <- rbindlist(list(to.plot.fleet,to.plot.chargers,to.plot.peak.ch,to.plot.em,to.plot.cost),fill=T)
  all[,metric:=factor(metric,levels = c('Fleet Size','# Chargers','Peak Load','Cost','Emissions'))]
  the.cols <- all$col
  names(the.cols) <- all$variable
  
  # 1d # Fleet and Chargers
  make.1d.fleet.and.chargers.plot <- function(sub,code,freeCol,sub.dir,energy.by.r.l){
    # sub <- by.r.sub
    # freeCol <- the.free.col
    # sub.dir <- pp(plots.dir,'/_metrics_1d/',code)
    to.plot <- sub[,.(value=value,percent=value/sum(value)*100,l=l,variable=var.clean,trips=sum(totalSAEVTrips)/n.days.in.run),by=c('run','rmob','group',freeCol)]
    to.plot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
    to.remove <- to.plot[,sum(value),by=c('variable')][V1==0]$variable
    to.plot <- to.plot[!variable%in%to.remove,.(value=sum(value)/1e6,trips=trips[1]/1e6,l=l[1]),by=c('run','group','variable','urb',freeCol)]
    energy.by.r.l[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
    energy.by.r.l.agg <- energy.by.r.l[,.(energyCharged=sum(energyCharged)),by=c('urb','l','run')]
    to.plot <- join.on(to.plot,energy.by.r.l.agg,c('urb','l','run'),c('urb','l','run'))
    fleet.tot <- to.plot[group=='Fleet',.(fleet.tot=sum(value)),by=c('urb',freeCol)]
    trips.tot <- to.plot[group=='Chargers',.(trips.tot=sum(trips)),by=c('urb',freeCol)]
    to.plot <- join.on(join.on(to.plot,fleet.tot,c('urb',freeCol),c('urb',freeCol)),trips.tot,c('urb',freeCol),c('urb',freeCol))
    to.plot[group=='Chargers',value.per:=ifelse(fleet.tot==0,0,value/fleet.tot)]
    to.plot[group=='Fleet',value.per:=ifelse(trips.tot==0,0,value/trips.tot)]
    p <- ggplot(to.plot,aes(x=urb,y=value.per,fill=variable))+
      geom_bar(stat='identity')+
      xlab('Regional Type')+
      ylab('Value (Chargers per Vehicle or Vehicles per Trip)')+
      facet_grid(group~streval(freeCol),scales='free_y')+ 
      theme_bw()+
      scale_fill_manual(name='Charger/Battery Level',values = getPalette(to.plot$variable),guide=guide_legend(reverse=F))
    ggsave(pp(sub.dir,'/fleet_and_chargers_',code,'.pdf'),p,width=8,height=4,units='in')
    write.csv(to.plot,pp(sub.dir,'/fleet_and_chargers_',code,'.csv'))
  }
  # Charging profiles
  to.plot.ch.agg <- to.plot.ch[,.(gwh=sum(gw.charging,na.rm=T)),by=c('run',param.names,'charger.level','l','t')]
  to.plot.ch.agg <- disag.the.private.load(to.plot.ch.agg,inputs$parameters$personalEVUnmanagedLoads)
  to.plot.ch.agg[,l.ordered:=ifelse(substr(l,1,1)=='L',pp('a',l),l)]
  to.plot.ch.agg[,col:=getPalette(l)[match(l.ordered,u(l.ordered))]]
  the.ch.cols <- to.plot.ch.agg$col
  names(the.ch.cols) <- to.plot.ch.agg$charger.level
  day.axis.breaks <- seq(0,72,by=12)
  make.1d.charging.plot <- function(ch.sub,code,freeCol,sub.dir){
    p <- ggplot(ch.sub[t>min(day.axis.breaks)&t<=max(day.axis.breaks)],aes(x=t,y=gwh,fill=charger.level))+geom_area(stat='identity')+facet_grid(.~streval(freeCol))+scale_x_continuous(breaks=day.axis.breaks)+scale_fill_manual(values = the.ch.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x='Hour',y='Charging Power (GW)',title=pp('Charging Power by ',factor.labels[freeCol],ifelse(code=='','',' when '),code),fill='')+theme_bw()
    pdf.scale <- 1
    ggsave(pp(sub.dir,'/charging_',code,'.pdf'),p,width=8*pdf.scale,height=4*pdf.scale,units='in')
    write.csv(ch.sub[t>min(day.axis.breaks)&t<=max(day.axis.breaks)],file=pp(sub.dir,'/charging_',code,'.csv'))
    p <- ggplot(ch.sub[t>min(day.axis.breaks)&t<=max(day.axis.breaks)],aes(x=t,y=gwh,fill=charger.level))+geom_area(stat='identity')+facet_grid(streval(freeCol)~.)+scale_x_continuous(breaks=day.axis.breaks)+scale_fill_manual(values = the.ch.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x='Hour',y='Charging Power (GW)',title=pp('Charging Power by ',factor.labels[freeCol],ifelse(code=='','',' when '),code),fill='')+theme_bw()
    ggsave(pp(sub.dir,'/charging_',code,'_transpose.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
  }
  make.1d.generation.plot <- function(param,param.ind,code,the.free.col,sub.dir) {
    cols <- c('Other'='#7fc97f','Coal'='gray27','Hydro'='#386cb0','Natural Gas'='gray73','Nuclear'='#fb9a99','Solar'='#ffff99','Wind'='lightskyblue')
    regions <- c('NENG','SAT-FL','ESC')
    pdf.scale <- 1
    forPlot <- merge(x=generation[get(param)==param.ind,],y=region.key,by='r')
    p <- ggplot(data=forPlot[r%in%regions],aes(x=t,y=generation/1000,fill=Simplified))+
      geom_area()+
      xlab('Hour')+
      ylab('Generation (GW)')+
      xlim(0,144)+
      scale_fill_manual(name='Fuel Type',values=cols)+
      theme_bw()+
      facet_grid(NAME~streval(the.free.col),scales='free_y')
    ggsave(pp(sub.dir,'/generation_',code,'.pdf'),p,width=8*pdf.scale,height=6*pdf.scale,units='in')
    write.csv(forPlot[r%in%regions],file=pp(sub.dir,'/generation_',code,'.csv'))
  }
  make.2d.charging.plot <- function(ch.sub,code,freeCols){
    p <- ggplot(ch.sub[t-24>min(day.axis.breaks)&t-24<=max(day.axis.breaks)],aes(x=t-24,y=gwh,fill=charger.level))+geom_area(stat='identity')+scale_x_continuous(breaks=day.axis.breaks)+scale_fill_manual(values = the.ch.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x='Hour',y='Charging Power (GW)',title=pp('Charging Power',ifelse(code=='','',' when '),code),fill='')+theme_bw()
    p <- p + streval(pp('facet_grid(',freeCols[1],'~',freeCols[2],')'))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'_charging_2d',ifelse(code=='','',pp('_',code)),'.pdf'),p,width=14*pdf.scale,height=8*pdf.scale,units='in')
    write.csv(ch.sub[t-24>min(day.axis.breaks)&t-24<=max(day.axis.breaks)],file=pp(plots.dir,'_charging_2d',ifelse(code=='','',pp('_',code)),'.csv'))
  }
  make.2d.metric.plot <- function(all.sub,code,freeCols){
    all.sub <- join.on(all.sub,all.sub[,.(val=sum(value,na.rm=T)),by=c('run','metric')][,.(max.value=max(val,na.rm=T)),by=c('metric')],'metric','metric')
    all.sub[,scaled:=value/max.value]
    p <- ggplot(all.sub,aes(x=metric,y=scaled*100,fill=variable))+geom_bar(stat='identity')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x="",y="% of Reference Scenario",fill='')+theme_bw()
    p <- p + streval(pp('facet_grid(',freeCols[1],'~',freeCols[2],')'))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'_metrics_2d',ifelse(code=='','',pp('_',code)),'.pdf'),p,width=14*pdf.scale,height=8*pdf.scale,units='in')
    write.csv(all.sub,file=pp(plots.dir,'/metric_2d',ifelse(code=='','',pp('_',code)),'.csv'))
  }
  if(length(param.names)==2){
    make.2d.metric.plot(all,'',param.names)
    make.2d.charging.plot(to.plot.ch.agg,'',param.names)
  }else if(length(param.names)>2){
    param.inds <- data.table(t(combn(length(param.names),length(param.names)-2)))
    for(param.ind.i in 1:nrow(param.inds)){
      the.param.inds <- unlist(param.inds[param.ind.i])
      param.vals <- sapply(the.param.inds,function(i){ exper$yaml$Factors[[i]]$Levels })
      param.combs <- data.table(expand.grid(param.vals))
      names(param.combs) <- param.names[the.param.inds]
      the.free.cols <- param.names[(1:length(param.names))[!(1:length(param.names)%in%the.param.inds)]]
      for(comb.i in 1:nrow(param.combs)){
        code <- pp(param.names[the.param.inds],unlist(param.combs[comb.i]),collapse='_')
        my.cat(pp('Fixing levels to: ',code))
        all.sub <- streval(pp('all[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
        make.2d.metric.plot(all.sub,code,the.free.cols)
        ch.sub <- streval(pp('to.plot.ch.agg[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
        make.2d.charging.plot(ch.sub,code,the.free.cols)
      }
    }
  }
  make.1d.metric.plot <- function(all.sub,code,freeCol,sub.dir){
    the.levels <- streval(pp('exper$runs[,.(col=',freeCol,')]$col'))
    streval(pp('all.sub[,',freeCol,':=factor(',freeCol,',levels=the.levels)]'))
    make.dir(sub.dir)
    for(the.metric in u(all.sub$metric)){
        p <- ggplot(all.sub[metric==the.metric],aes(x=streval(freeCol),y=value/metric.units[metric==the.metric]$scale.factor,fill=variable))+geom_bar(stat='identity',colour='black')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x=factor.labels[freeCol],y=metric.units[metric==the.metric]$label,title=pp(the.metric,ifelse(code=='','',' when '),code),fill='')+theme_bw()
        pdf.scale <- 1
        ggsave(pp(sub.dir,'/metric_',the.metric,'_',code,'.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
        write.csv(streval(pp('all.sub[metric==the.metric,.(x=',freeCol,',y=value/metric.units[metric==the.metric]$scale.factor,fill=variable)]')),file=pp(sub.dir,'/metric_',the.metric,'_',code,'.csv'))
        if(the.metric=='Emissions'){
          p <- ggplot(all.sub[metric==the.metric],aes(x=streval(freeCol),y=value/metric.units[metric==the.metric]$scale.factor,fill=variable))+geom_bar(stat='identity',colour='black')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x=factor.labels[freeCol],y=metric.units[metric==the.metric]$label,title=pp(the.metric,ifelse(code=='','',' when '),code),fill='')+theme_bw()+scale_y_continuous(limits = c(0,11.5))
          pdf.scale <- 1
          ggsave(pp(sub.dir,'/metric_',the.metric,'_',code,'_scaledTo11.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
        }else if(the.metric=='Cost'){
          p <- ggplot(all.sub[metric==the.metric & variable=='Cost: Energy'],aes(x=streval(freeCol),y=value/metric.units[metric==the.metric]$scale.factor,fill=variable))+geom_bar(stat='identity',colour='black')+scale_fill_manual(values = the.cols)+theme(axis.text.x = element_text(angle = 30, hjust = 1))+labs(x=factor.labels[freeCol],y=metric.units[metric==the.metric]$label,title=pp(the.metric,ifelse(code=='','',' when '),code),fill='')+theme_bw()
          pdf.scale <- 1
          ggsave(pp(sub.dir,'/metric_',the.metric,'_',code,'_energyInset.pdf'),p,width=6*pdf.scale,height=4*pdf.scale,units='in')
        }
    }
  }
  make.dir(pp(plots.dir,'/_metrics_1d'))
  if(length(param.names)==1){
    make.1d.metric.plot(all,'',param.names[1],pp(plots.dir,'/_metrics_1d'))
  }else{
    param.inds <- data.table(t(combn(length(param.names),length(param.names)-1)))
    for(param.ind.i in 1:nrow(param.inds)){
      the.param.inds <- unlist(param.inds[param.ind.i])
      param.vals <- sapply(the.param.inds,function(i){ exper$yaml$Factors[[i]]$Levels })
      param.combs <- data.table(expand.grid(param.vals))
      names(param.combs) <- param.names[the.param.inds]
      the.free.col <- param.names[(1:length(param.names))[!(1:length(param.names)%in%the.param.inds)]]
      for(comb.i in 1:nrow(param.combs)){
        code <- pp(param.names[the.param.inds],unlist(param.combs[comb.i]),collapse='_')
        my.cat(pp('Fixing levels to: ',code))
        all.sub <- streval(pp('all[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
        make.1d.metric.plot(all.sub,code,the.free.col,pp(plots.dir,'/_metrics_1d/',code))
        ch.sub <- streval(pp('to.plot.ch.agg[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
        make.1d.charging.plot(ch.sub,code,the.free.col,pp(plots.dir,'/_metrics_1d/',code))
        make.1d.generation.plot(param.names[the.param.inds],param.combs[comb.i,get(param.names[the.param.inds])],code,the.free.col,pp(plots.dir,'/_metrics_1d/',code))
        by.r.sub <- streval(pp('by.r[',pp(param.names[the.param.inds],'==',unlist(param.combs[comb.i]),collapse=' & '),']'))
        by.r.sub <- join.on(by.r.sub,res[['d-rmob-t']][,.(totalSAEVTrips=sum(demand,na.rm=T)),by=c('run','rmob')],c('run','rmob'),c('run','rmob'))
        energy.by.r.l <- res[['b-l-rmob-t']][run%in%u(by.r.sub$run),.(energyCharged=sum(energyCharged)),by=c('run','l','rmob')]
        if(sum(by.r.sub$value)>0)make.1d.fleet.and.chargers.plot(by.r.sub,code,the.free.col,pp(plots.dir,'/_metrics_1d/',code),energy.by.r.l)
      }
    }
  }
  
  if(length(param.names)==1){
    # Vehicle allocations
    to.plot <- melt(vehs,id.vars=c('b','rmob','t',names(exper$runs)))
    to.plot <- to.plot[,.(value=sum(value)),by=c('b','t',names(exper$runs),'variable')]

    to.plot[,vehicle.activity:=gsub('\\.L0|\\.L','(',gsub('vehiclesCharging','Charging ',variable))]
    to.plot[,vehicle.activity:=ifelse(grepl('Charging',vehicle.activity),paste(vehicle.activity,'kW)'),vehicle.activity)]
    to.plot[,vehicle.activity:=gsub('\\.','-',gsub('\\.d','(',gsub('vehiclesMoving','Moving ',vehicle.activity)))]
    to.plot[,vehicle.activity:=ifelse(grepl('Moving',vehicle.activity),paste(vehicle.activity,' mi)'),vehicle.activity)]
    to.plot[,vehicle.activity:=ifelse(grepl('run',vehicle.activity),'Moving (private)',ifelse(grepl('vehiclesIdle',vehicle.activity),'Idling',vehicle.activity))]
    to.plot$vehicle.activity <- factor(to.plot$vehicle.activity,levels=unique(to.plot$vehicle.activity)[unique(mixedorder(to.plot$vehicle.activity))])

    streval(pp('setkey(to.plot,variable,vehicle.activity,t,b,',param.names,')'))
    p <- ggplot(to.plot,aes(x=t,y=value/10^6,fill=fct_rev(vehicle.activity)))+
      geom_bar(stat='identity',position='stack')+
      xlab('Hour')+
      ylab('Number of shared vehicles (millions)')+
      scale_fill_manual(name='Vehicle activity',values = rev(getPalette(to.plot$vehicle.activity)))+
      theme_bw()
    streval(pp('p <- p + facet_wrap(~',param.names,',scales="free_y")'))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'_num-vehs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    to.plot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),'Charging',ifelse(variable=='vehiclesIdle','Idle','Moving'))]
    to.plot[,variable.simp:=factor(variable.simp,c('Charging','Moving','Idle'))]
    to.plot <- to.plot[,.(value=sum(value)),by=c('t',param.names,'variable.simp')]
    streval(pp('setkey(to.plot,variable.simp,t,',param.names,')'))
    p <- ggplot(to.plot,aes(x=t,y=value/10^6,fill=fct_rev(variable.simp)))+
      geom_bar(stat='identity',position='stack')+
      xlab('Hour')+
      ylab('Number of shared vehicles (millions)')+
      scale_fill_manual(name='Vehicle activity',values = rev(getPalette(to.plot$variable.simp)))+
      theme_bw()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_num-vehs-simple.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Charging load
    streval(pp('setkey(veh.ch,l,rmob,t,',param.names,')'))
    to.plot <- veh.ch[,.(gw.charging=sum(gw.charging)),by=c('l','t',param.names)]
    to.plot[,charger.level:=gsub('L|L0','',l)]
    to.plot[,charger.level:=paste(charger.level,'kW')]
    to.plot$charger.level <- factor(to.plot$charger.level,levels=unique(to.plot$charger.level)[mixedorder(unique(to.plot$charger.level))])

    p <- ggplot(to.plot,aes(x=t,y=gw.charging,fill=fct_rev(charger.level)))+
      geom_bar(stat='identity',position='stack')+
      scale_fill_manual(name='Charger Level',values = rev(getPalette(to.plot$charger.level)))+
      labs(x='Hour',y='Load (GW)',fill='Charger Level')+
      theme_bw()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'_charging-saevs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    streval(pp('setkey(personal.ev.ch,l,rmob,t,',param.names,')'))
    to.plot.personal <- personal.ev.ch[,.(gw.charging=sum(gw.charging)),by=c('l','charger.level','t',param.names)]
    to.plot <- rbindlist(list(to.plot,to.plot.personal),fill=T,use.names=T)
    p <- ggplot(to.plot)+
      geom_bar(aes(x=t,y=gw.charging,fill=fct_rev(charger.level)),stat='identity',position='stack')+
    #geom_text(aes(x=Inf,y=Inf,hjust=1,vjust=1,label=sum(gw.charging)))+
      scale_fill_manual(values = rev(getPalette(to.plot$charger.level)))+labs(x='Hour',y='Load (GW)',fill='Charger Level')+
      theme_bw()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'_charging-all.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Energy balance
    streval(pp('setkey(en,b,rmob,t,',param.names,')'))
    to.plot <- en[,.(soc=sum(soc)),by=c('b','battery.level','t',param.names)]
    p <- ggplot(to.plot,aes(x=t,y=soc/10^6,colour=fct_rev(battery.level)))+
      geom_line()+
      xlab('Hour')+
      ylab('Fleet Energy State (GWh)')+
      scale_colour_manual(name='Vehicle battery size',values = rev(getPalette(to.plot$battery.level)))+
      theme_bw()
    streval(pp('p <- p + facet_wrap(~',param.names,',scales="free_y")'))
    ggsave(pp(plots.dir,'_soc.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    p <- ggplot(to.plot,aes(x=t,y=soc/10^6,fill=fct_rev(battery.level)))+
      geom_bar(stat='identity',position='stack')+
      xlab('Hour')+
      ylab('Fleet Energy State (GWh)')+
      scale_fill_manual(name='Vehicle battery size',values = rev(getPalette(to.plot$battery.level)))+
      theme_bw()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_soc-bar.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Fleet and Chargers
    to.plot <- by.r[,.(value=sum(value)),by=c('group','variable',param.names)]
    to.plot[,var.clean:=ifelse(grepl('L',variable),paste('Chgr',variable,'kW'),paste('Bat',variable,'mi'))]
    to.plot[,var.clean:=gsub(' L| L0| b| b0',' ',var.clean)]
    to.plot$var.clean <- factor(to.plot$var.clean,levels=unique(to.plot$var.clean)[mixedorder(unique(to.plot$var.clean))])

    p <- streval(pp('ggplot(to.plot,aes(x=factor(',param.names,'),y=value/10^6,fill=fct_rev(var.clean)))'))+
      geom_bar(stat='identity')+
      xlab(param.names)+
      ylab('Count (millions)')+
      facet_wrap(~group,scales='free_y')+
      scale_fill_manual(name='Charger/Battery Level',values = rev(getPalette(to.plot$var.clean)))+
      theme_bw()
    ggsave(pp(plots.dir,'_fleet-size-and-type.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    
    # Generation 
    p <- ggplot(generation[,.(generation=sum(generation)),by=c('t','Simplified',param.names)],aes(x=t,y=generation/1000,fill=Simplified))+
      geom_area()+
      xlab('Hour')+
      ylab('Generation (GW)')+
      scale_fill_discrete(name='Fuel Type')+
      theme_bw()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'/_generation-total-by-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    p <- ggplot(generation[,.(consq.generation=sum(consq.generation)),by=c('t','Simplified',param.names)],aes(x=t,y=consq.generation/1000,fill=Simplified))+
      geom_area()+
      xlab('Hour')+
      ylab('Generation (GW)')+
      scale_fill_discrete(name='Fuel Type')+
      theme_bw()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'/_generation-cnsq-total-by-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
    
    # Costs
    to.plot <- melt(costs,measure.vars=c('demandChargeCost','vehicleMaintCost','infrastructureCost','fleetCost','energyCost'),id.vars=c('r',param.names))[,.(value=sum(value)),by=c('variable',param.names)]
    cost.key <- data.table(variable=c('demandChargeCost','vehicleMaintCost','infrastructureCost','fleetCost','energyCost'),cost=c('Demand Charges','Maintenance','Infrastructure','Fleet Capital','Energy'))
    cost.key$cost <- factor(cost.key$cost,levels=c('Demand Charges','Maintenance','Infrastructure','Fleet Capital','Energy'))
    to.plot <- merge(x=to.plot,y=cost.key,by='variable',all.x=TRUE)
    p <- streval(pp('ggplot(to.plot,aes(x=factor(',param.names,'),y=value/10^9,fill=fct_rev(cost)))'))+
      geom_bar(stat='identity')+
      xlab(param.names)+
      ylab('Total cost (bns of $)')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Cost Source',values = rev(getPalette(to.plot$cost)))
    ggsave(pp(plots.dir,'_costs.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    

    ## Flagged, modified but I don't understand this graph? ##
    speed <- copy(inputs$parameters$speed)[,t:=as.numeric(substr(t,2,nchar(as.character(t))))]
    vmt2 <- join.on(vmt,speed,c('t','d','rmob'),c('t','d','rmob'),'value')
    vmt2[,tt:=travelDistance*value]
    to.plot <- join.on(to.plot,vmt2[,.(vmt=sum(vmt),t=sum(tt)),by=param.names],param.names,param.names,c('vmt','t'))
    to.plot <- join.on(to.plot,fleet[,.(fleetSize=sum(fleetSize)),by=param.names],param.names,param.names,'fleetSize')
    to.plot[,cost.per.mile:=value/vmt]
    costs.per.mile <- to.plot 
    p <- streval(pp('ggplot(to.plot,aes(x=factor(',param.names,'),y=cost.per.mile,fill=fct_rev(cost)))'))+
      geom_bar(stat='identity')+
      xlab(param.names)+
      ylab('Cost per mile ($)')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Cost Source',values = rev(getPalette(to.plot$cost)))
    ggsave(pp(plots.dir,'_costs-per-mile.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    ## TO BEAUTIFY ##
    if(length(u(streval(pp('to.plot$',param.names))))==1){
      p <- ggplot(to.plot[,scen:=param.names],aes(x=scen,y=cost.per.mile,fill=fct_rev(variable)))+
        geom_bar(stat='identity')+
        theme(axis.text.x = element_text(angle = 50, hjust = 1))+
        scale_fill_manual(values = rev(getPalette(to.plot$variable)))+
        coord_polar('y',start=0)+theme(axis.text.x=element_blank())+
        geom_text(aes(y = cost.per.mile/3 + c(0, cumsum(cost.per.mile)[-length(cost.per.mile)]), label = roundC(cost.per.mile,3)), size=5)
      ggsave(pp(plots.dir,'_costs-per-mile-pie.pdf'),p,width=6*pdf.scale,height=6*pdf.scale,units='in')  
    }

    ## Flagged, modified but maybe we can visualize differently? Can we also do PMT? ##
    p <- ggplot(melt(vmt.by.region,id.vars=c('rmob','battery.level',param.names),measure.vars='daily.vmt.per.vehicle'),aes(x=rmob,y=value,fill=battery.level))+
      geom_bar(stat='identity',position='dodge')+
      xlab('Region')+
      ylab('Total vehicle miles traveled (mi)')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Vehicle battery size',values = (getPalette(vmt.by.region$battery.level)),guide=guide_legend(reverse=F))
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_daily-vmt.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  

    ## Not sure exactly what the "value" represents ##
    vmt.by.region[,lifetime.target:=200e3/(daily.vmt.per.vehicle*365)]
    vmt.by.region[,lifetime.error:=lifetime.target-assumed.lifetime.value]
    p <- ggplot(melt(vmt.by.region,id.vars=c('rmob','battery.level',param.names),measure.vars='lifetime.target'),aes(x=rmob,y=value,fill=battery.level))+
      geom_bar(stat='identity',position='dodge')+
      xlab('Region')+
      #ylab('???')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Vehicle battery level',values = (getPalette(vmt.by.region$battery.level)),guide=guide_legend(reverse=F))
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_vehicle-lifetime-targets.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    if(F){
      towrite <- melt(vmt.by.region,id.vars=c('rmob','b',param.names),measure.vars='lifetime.target')
      towrite[is.na(value),value:=mean(towrite$value,na.rm=T)]
      towrite[value<=0.5,value:=0.5]
      towrite.prev <- data.table(read.csv(pp(gem.raw.inputs,'/rise-scaling-factors/vehicleLifetimes/vehicleLifetime.csv')))
      towrite.new <- join.on(towrite,towrite.prev,c('b','rmob'),c('b','rmob'))
      towrite.new[,value:=(value*7+i.value)/8] # weight toward previous 7 to 1
      write.csv(towrite.new[,.(b,rmob,value)],file=pp(gem.raw.inputs,'/rise-scaling-factors/vehicleLifetimes/vehicleLifetime.csv'),row.names=F)
      write.csv(towrite.new[,.(b,rmob,value)],file=pp(gem.raw.inputs,'/rise-scaling-factors/vehicleLifetimes/batteryLifetime.csv'),row.names=F)
      write.csv(towrite.new[,.(b,rmob,value)],file=pp(gem.raw.inputs,'/rise-scaling-factors/vehicleLifetime.csv'),row.names=F)
      write.csv(towrite.new[,.(b,rmob,value)],file=pp(gem.raw.inputs,'/rise-scaling-factors/batteryLifetime.csv'),row.names=F)
      the.dir <- '/Users/critter/odrive/GoogleDriveLBL/VGI4NewMobility/gem-raw-inputs/rise-scaling-factors/vehicleLifetimes/'
      last.iter <- tail(sort(as.numeric(unlist(lapply(str_split(grep('-',list.files(the.dir),value=T),"-"),function(ll){ str_split(ll[2],".csv")[[1]][1] })))),1)
      write.csv(towrite.new[,.(b,rmob,value)],file=pp(gem.raw.inputs,'/rise-scaling-factors/vehicleLifetimes/vehicleLifetime-',last.iter+1,'.csv'),row.names=F)
      
      all <- list()
      for(file in grep('-',list.files(the.dir),value=T)){ 
        all[[length(all)+1]] <- data.table(read.csv(pp(the.dir,file)))
        the.iter <- as.numeric(str_split(str_split(tail(str_split(file,"/")[[1]],1),"-")[[1]][2],'.csv')[[1]][1])
        all[[length(all)]][,iter:=the.iter]
      }
      all <- rbindlist(all)
      p <- ggplot(all[iter>=11],aes(x=factor(iter),y=value,fill=b))+geom_bar(position='dodge',stat='identity')+facet_wrap(~rmob)
      ggsave(pp(gem.raw.inputs,'/rise-scaling-factors/vehicleLifetimes/vehicleLifetimeTargets.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
      ## Not sure exactly what the "value" represents ##
      p <- ggplot(melt(vmt.by.region,id.vars=c('rmob','battery.level'),measure.vars='lifetime.error'),aes(x=rmob,y=value,fill=battery.level))+
        geom_bar(stat='identity',position='dodge')+
        xlab('Region')+
        #ylab('???')+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 50, hjust = 1))+
        scale_fill_manual(name='Vehicle battery level',values = (getPalette(vmt.by.region$battery.level)),guide=guide_legend(reverse=F))
      ggsave(pp(plots.dir,'_vehicle-lifetime-errors.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    
        
    }
    lbs <- rbindlist(lapply(seq_along(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVChargeEnergyLB[,run:=i] }),fill=T)
    ubs <- rbindlist(lapply(seq_along(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVChargeEnergyUB[,run:=i] }),fill=T)
    lbs[,lb:=value]
    ubs[,ub:=value]
    lbs <- join.on(lbs,run.params,'run','run')
    ubs <- join.on(ubs,run.params,'run','run')
    to.plot <- melt(join.on(lbs,ubs,c('t','rmob','run'),c('t','rmob','run'),'ub'),id.vars=c('t','rmob','run',names(exper$runs)))[variable!='value']
    to.plot[,t:=to.number(t)]
    to.plot[,value:=value/1e6]
    p <- ggplot(to.plot[,.(value=sum(value)),by=c('t','variable',param.names)],aes(x=t,y=value,colour=variable))+geom_line()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_private-ev-energy-bounds.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    setkey(to.plot,variable,run,rmob,t)
    unmanaged.load <- to.plot[variable=='ub',.(t=t,load=diff(c(0,value))),by=c(param.names,'rmob')]
    p <- ggplot(unmanaged.load[,.(load=sum(load)),by=c('t',param.names)],aes(x=t,y=load))+geom_line()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_private-ev-load-from-bounds.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    
    lbs <- rbindlist(lapply(seq_along(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVChargePowerLB[,run:=i] }),fill=T)
    ubs <- rbindlist(lapply(seq_along(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVChargePowerUB[,run:=i] }),fill=T)
    lbs[,lb:=value]
    ubs[,ub:=value]
    lbs <- join.on(lbs,run.params,'run','run')
    ubs <- join.on(ubs,run.params,'run','run')
    to.plot <- melt(join.on(lbs,ubs,c('t','rmob','run'),c('t','rmob','run'),'ub'),id.vars=c('t','rmob','run',names(exper$runs)))[variable!='value']
    to.plot[,t:=to.number(t)]
    to.plot[,value:=value/1e6]
    p <- ggplot(to.plot[,.(value=sum(value)),by=c('t','variable',param.names)],aes(x=t,y=value,colour=variable))+geom_line()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_private-ev-power-bounds.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    
    p <- ggplot(personal.ev.ch[,.(value=sum(gw.charging)),by=c('t','l',param.names)],aes(x=t,y=value,colour=l))+geom_line()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_private-ev-charging.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    streval(pp('setkey(personal.ev.ch,l,rmob,t,',param.names,')'))
    
    to.plot <- veh.ch[,.(gw.charging=sum(gw.charging)),by=c('l','t',param.names)]
    to.plot <- personal.ev.ch[,.(gw.charging=sum(gw.charging)),by=c('l','t',param.names)]
    to.plot <- rbindlist(list(to.plot,to.plot),fill=T,use.names=T)
    p <- ggplot(to.plot[,.(value=sum(gw.charging)),by=c('t','l',param.names)],aes(x=t,y=value,colour=l))+geom_line()
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_private-ev-charging2.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
  }
}

