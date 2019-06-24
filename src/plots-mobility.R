###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This makes plots related to mobility.
#
# Argument: the results list containing GAMS outputs and the plotting directory
###############################################################################################

plots.mobility <- function(exper,all.inputs,res,plots.dir){
  inputs <- all.inputs[[1]]
  getPalette = function(vals){ 
    ncol <- length(u(vals))
    if(ncol>9){
      colorRampPalette(brewer.pal(9, "Set1"))(ncol)
    }else if(any(u(vals)=='b075',na.rm=TRUE)|any(u(vals)=='75 kWh',na.rm=TRUE)){
      head(colorRampPalette(brewer.pal(9, "Set1"))(10),5)
    }else if(any(u(vals)=='L010',na.rm=TRUE)|any(u(vals)=='10 kW',na.rm=TRUE)){
      if(ncol==5){
        tail(colorRampPalette(brewer.pal(9, "Set1"))(10),5)
      }else{
        c(tail(colorRampPalette(brewer.pal(9, "Set1"))(10),5),'#e41a1c')
      }
    }else{
      colorRampPalette(brewer.pal(ncol, "Set1"))(ncol)
    }
  }
  
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

  # Merit order
  generators$g <- as.character(generators$g)
  generators <- merge(x=generators,y=fuels,by='FuelType',all.x=TRUE)
  generators$Simplified <- factor(generators$Simplified,levels=meritOrder)

  # Run by Run Plots
  run.i <- u(vehs$run)[1]
  for(run.i in u(vehs$run)){
    setkey(vehs,run,b,rmob,t)
    to.plot <- melt(vehs[run==run.i],id.vars=c('b','rmob','t'))
    to.plot[,vehicle.activity:=gsub('\\.L0|\\.L','(',gsub('vehiclesCharging','Charging ',variable))]
    to.plot[,vehicle.activity:=ifelse(grepl('Charging',vehicle.activity),paste(vehicle.activity,'kW)'),vehicle.activity)]
    to.plot[,vehicle.activity:=gsub('\\.','-',gsub('\\.d','(',gsub('vehiclesMoving','Driving ',vehicle.activity)))]
    to.plot[,vehicle.activity:=ifelse(grepl('Driving',vehicle.activity),paste(vehicle.activity,' mi)'),vehicle.activity)]
    to.plot[,vehicle.activity:=ifelse(grepl('run',vehicle.activity),'Driving (private)',ifelse(grepl('vehiclesIdle',vehicle.activity),'Idling',vehicle.activity))]
    to.plot$vehicle.activity <- factor(to.plot$vehicle.activity,levels=unique(to.plot$vehicle.activity)[unique(mixedorder(to.plot$vehicle.activity))])
    p <- ggplot(to.plot,aes(x=t,y=value/1000,fill=vehicle.activity))+
      geom_bar(stat='identity',position='stack')+
      xlab('Hour')+
      ylab('Number of vehicles (thousands)')+
      facet_wrap(~rmob,scales='free_y')+
      theme_bw()+
      scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$vehicle.activity))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    to.plot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),'Charging',ifelse(variable=='vehiclesIdle','Idle','Moving'))]
    to.plot[,variable.simp:=factor(variable.simp,c('Idle','Moving','Charging'))]
    setkey(to.plot,variable.simp)
    p <- ggplot(to.plot,aes(x=t,y=value/1000,fill=variable.simp))+
      geom_bar(stat='identity',position='stack')+
      xlab('Hour')+
      ylab('Number of vehicles (thousands)')+
      facet_wrap(~rmob,scales='free_y')+
      scale_fill_manual(name = 'Vehicle activity',values = getPalette(to.plot$variable.simp),guide=guide_legend(reverse=F))+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs-simple.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
  
    # Charging load
    p <- ggplot(veh.ch[run==run.i],aes(x=t,y=gw.charging,fill=fct_rev(charger.level)))+
      geom_bar(stat='identity',position='stack')+
      facet_wrap(~rmob,scales='free_y')+
      scale_fill_manual(values = rev(getPalette(veh.ch$charger.level)),guide=guide_legend(reverse=F))+
      labs(x='Hour',y='Load (GW)',fill='Charger Level')+
      theme_bw()
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_charging-saevs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    to.plot <- rbindlist(list(veh.ch,personal.ev.ch),fill=T,use.names=T)[run==run.i]
    p <- ggplot(to.plot,aes(x=t,y=gw.charging,fill=fct_rev(charger.level)))+
      geom_bar(stat='identity',position='stack')+
      facet_wrap(~rmob,scales='free_y')+
      scale_fill_manual(values = rev(getPalette(to.plot$charger.level)),guide=guide_legend(reverse=F))+
      labs(x='Hour',y='Load (GW)',fill='Charger Level')+
      theme_bw()
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_charging-all.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
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
      geom_bar(stat='identity',position='stack')+
      xlab('Hour')+
      ylab('Fleet Energy State (GWh)')+
      facet_wrap(~rmob,scales='free_y')+
      scale_fill_manual(name='Vehicle battery size',values=rev(getPalette(en$battery.level)),guide=guide_legend(reverse=F))+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_soc-bar.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Fleet size & type
    by.r <- rbindlist(list(res[['l-rmob']][,':='(run=run,variable=l,value=numChargers,group='Chargers')],res[['b-rmob']][,':='(run=run,variable=b,value=fleetSize,group='Fleet')]),fill=T)
    setkey(by.r,run,variable)
    by.r[,var.clean:=ifelse(grepl('L',variable),paste('Chgr:',variable,'kW'),paste('Bat:',variable,'mi'))]
    by.r[,var.clean:=gsub(' L| L0| b| b0',' ',var.clean)]
    by.r$var.clean <- factor(by.r$var.clean,levels=unique(by.r$var.clean)[mixedorder(unique(by.r$var.clean))])
    p <- ggplot(by.r[run==run.i],aes(x=rmob,y=value/1000,fill=fct_rev(var.clean)))+
      geom_bar(stat='identity')+
      xlab('Region')+
      ylab('Count (thousands)')+
      facet_wrap(~group,scales='free_y')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Charger/Battery Level',values = rev(getPalette(by.r$var.clean)),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_fleet-size-and-type.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
    
    to.plot <- by.r[run==run.i,.(percent=value/sum(value)*100,variable=var.clean),by=c('rmob','group')]
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
    
    # to.plot <- res[['rmob']][,.(rmob,urbanFormFactor)]
    # to.plot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
    # to.plot[,Region:=factor(rmob,geo.ordered)]
    # p <- ggplot(to.plot,aes(x=Region,y=urbanFormFactor))+geom_bar(stat='identity')+facet_wrap(~urb,scales='free_x')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+coord_cartesian(ylim=c(1,max(to.plot$urbanFormFactor)*1.01))
    # ggsave(pp(plots.dir,'/run-',run.i,'/_urban-form-factor-by-region.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    # Generation
    to.plot <- merge(x=res[['g-t']][run==run.i],y=generators,by='g',all.x=TRUE)
    to.plot <- to.plot[,list(generation=sum(generation),base.generation=sum(base.generation)),by=list(r,Simplified,t)]
    to.plot[,consq.generation:=generation-base.generation]
    to.plot <- to.plot[complete.cases(to.plot),]
    p <- ggplot(to.plot,aes(x=t,y=generation/1000,fill=Simplified))+
      geom_area()+
      xlab('Hour')+
      ylab('Generation (GW)')+
      scale_fill_discrete(name='Fuel Type')+
      facet_wrap(~r,scale='free_y')+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_generation-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    p <- ggplot(to.plot,aes(x=t,y=consq.generation/1000,fill=Simplified))+
      geom_area()+
      xlab('Hour')+
      ylab('Generation (GW)')+
      scale_fill_discrete(name='Fuel Type')+
      facet_wrap(~r,scale='free_y')+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_generation-cnsq-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    to.plot <- merge(x=res[['g-t']][run==run.i],y=generators,by='g',all.x=TRUE)
    to.plot <- to.plot[,list(emissions=sum(generationCO2*generation),base.emissions=sum(generationCO2*base.generation)),by=list(r,Simplified,t)]
    to.plot[,consq.emissions:=emissions-base.emissions]
    to.plot <- to.plot[complete.cases(to.plot),]
    p <- ggplot(to.plot,aes(x=t,y=emissions/1000,fill=Simplified))+
      geom_bar(stat='identity')+
      xlab('Hour')+
      ylab('Hourly CO2 Emissions (tons)')+
      scale_fill_discrete(name='Fuel Type')+
      facet_wrap(~r,scale='free_y')+
      theme_bw()
    ggsave(pp(plots.dir,'/run-',run.i,'/_emissions-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    p <- ggplot(to.plot,aes(x=t,y=consq.emissions/1000,fill=Simplified))+
      geom_bar(stat='identity')+
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
  }
  
  # tr <- rbindlist(list(melt(res[['rmob-t']],id.vars=c('t','rmob','run')),melt(en[,.(en.mob=sum(en.mob),en.ch=sum(en.ch)),by=c('rmob','t','run')],id.vars=c('t','rmob','run'))))
  # tr[,group:=ifelse(variable=='price','Price',ifelse(str_sub(variable,1,3)=='en.','Energy','Cost'))]
  # tr <- tr[group!="Cost"]
  # prices <- tr[variable=='price']
  # p <- ggplot(tr,aes(x=t,y=value,colour=variable))+geom_line()+facet_grid(group~rmob,scales='free_y') # geom_bar(stat='identity',position='dodge')
  # ggsave(pp(plots.dir,'_energy-vs-price.pdf'),p,width=12*pdf.scale,height=8*pdf.scale,units='in')
  
  generation <- merge(x=res[['g-t']],y=generators,by='g',all.x=TRUE)
  generation <- generation[,list(generation=sum(generation),base.generation=sum(base.generation)),by=list(run,r,Simplified,t)]
  generation[,consq.generation:=generation-base.generation]
  generation <- generation[complete.cases(generation),]
  
  gen.cost <- merge(x=res[['g-t']],y=generators,by='g',all.x=TRUE)
  gen.cost <- gen.cost[,list(energyCost=sum(generationCosts*(generation-base.generation))),by=list(r,run)]
  
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
  vmt.by.region <- join.on(join.on(fleet,vmt[,.(vmt=sum(vmt)),by=c('rmob','b')],c('rmob','b'),c('rmob','b')),inputs$parameters$vehicleLifetime,c('rmob','b'),c('rmob','b'),'value','assumed.lifetime.')
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
  streval(pp('save(',pp(data.to.save,collapse=','),',file="',plots.dir,'/resuls-for-plotting.Rdata")'))
  load(file=pp(plots.dir,'/resuls-for-plotting.Rdata'))
  
  ###################################
  # Across factor plots
  ###################################
  param.names <- names(exper$runs)
  if(length(param.names)>1){
    cat('Post-process script not yet capable of plotting mulit-factorial results, skipping')
  }else{
    
    # Vehicle allocations
    to.plot <- melt(vehs,id.vars=c('b','rmob','t',param.names))
    to.plot <- to.plot[,.(value=sum(value)),by=c('b','t',param.names,'variable')]

    to.plot[,vehicle.activity:=gsub('\\.L0|\\.L','(',gsub('vehiclesCharging','Charging ',variable))]
    to.plot[,vehicle.activity:=ifelse(grepl('Charging',vehicle.activity),paste(vehicle.activity,'kW)'),vehicle.activity)]
    to.plot[,vehicle.activity:=gsub('\\.','-',gsub('\\.d','(',gsub('vehiclesMoving','Driving ',vehicle.activity)))]
    to.plot[,vehicle.activity:=ifelse(grepl('Driving',vehicle.activity),paste(vehicle.activity,' mi)'),vehicle.activity)]
    to.plot[,vehicle.activity:=ifelse(grepl('run',vehicle.activity),'Driving (private)',ifelse(grepl('vehiclesIdle',vehicle.activity),'Idling',vehicle.activity))]
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
      xlab('Fraction of SAEVs')+
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
      xlab('Fraction of SAEVs')+
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
      xlab('Fraction of SAEVs')+
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
    vmt.by.region[,lifetime.target:=200e3/(daily.vmt.per.vehicle*365)]
    vmt.by.region[,lifetime.error:=lifetime.target-assumed.lifetime.value]

    ## Not sure exactly what the "value" represents ##
    p <- ggplot(melt(vmt.by.region,id.vars=c('rmob','battery.level',param.names),measure.vars='lifetime.target'),aes(x=rmob,y=value,fill=battery.level))+
      geom_bar(stat='identity',position='dodge')+
      xlab('Region')+
      #ylab('???')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Vehicle battery level',values = (getPalette(vmt.by.region$battery.level)),guide=guide_legend(reverse=F))
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_vehicle-lifetime-targets.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    write.csv(melt(vmt.by.region,id.vars=c('rmob','b',param.names),measure.vars='lifetime.target'),file=pp(plots.dir,'_vehicle-lifetime-targets.csv'))

    ## Not sure exactly what the "value" represents ##
    p <- ggplot(melt(vmt.by.region,id.vars=c('rmob','battery.level'),measure.vars='lifetime.error'),aes(x=rmob,y=value,fill=battery.level))+
      geom_bar(stat='identity',position='dodge')+
      xlab('Region')+
      #ylab('???')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 50, hjust = 1))+
      scale_fill_manual(name='Vehicle battery level',values = (getPalette(vmt.by.region$battery.level)),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'_vehicle-lifetime-errors.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    
    
    lbs <- rbindlist(lapply(seq_along(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVChargeEnergyLB[,run:=i] }),fill=T)
    ubs <- rbindlist(lapply(seq_along(all.inputs),function(i){ all.inputs[[i]]$parameters$personalEVChargeEnergyUB[,run:=i] }),fill=T)
    lbs[,lb:=value]
    ubs[,ub:=value]
    lbs <- join.on(lbs,run.params,'run','run')
    ubs <- join.on(ubs,run.params,'run','run')
    to.plot <- melt(join.on(lbs,ubs,c('t','rmob','run'),c('t','rmob','run'),'ub'),id.vars=c('t','rmob','run',param.names))[variable!='value']
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
    to.plot <- melt(join.on(lbs,ubs,c('t','rmob','run'),c('t','rmob','run'),'ub'),id.vars=c('t','rmob','run',param.names))[variable!='value']
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

