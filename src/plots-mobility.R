###############################################################################################
# Grid-Integrated Electric Mobility Model (GEM)
# 
# This makes plots related to mobility.
#
# Argument: the results list containing GAMS outputs and the plotting directory
###############################################################################################

plots.mobility <- function(exper,inputs,res,plots.dir){
  getPalette = function(vals){ 
    ncol <- length(u(vals))
    if(ncol>9){
      colorRampPalette(brewer.pal(9, "Set1"))(ncol)
    }else if(any(u(vals)=='b075')){
      head(colorRampPalette(brewer.pal(9, "Set1"))(10),5)
    }else if(any(u(vals)=='L010')){
      tail(colorRampPalette(brewer.pal(9, "Set1"))(10),5)
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
  
  # Personal Vehicle Charging
  personal.ev.ch <- res[['rmob-t']][,.(t,rmob,kw=personalEVPower)]
  
  # Energy balance
  en <- join.on(join.on(res[['b-l-rmob-t']],res[['b-l-rmob']],c('l','rmob','b','run'),c('l','rmob','b','run'))[,.(en.ch=sum(energyCharged/chargeEff)),by=c('t','rmob','b','run')],res[['b-d-rmob-t']][,.(en.mob=sum(energyConsumed)),by=c('t','rmob','b','run')],c('b','rmob','t','run'),c('b','rmob','t','run'))
  batt <- join.on(res[['b-rmob']],res[['b']],c('b','run'),c('b','run'))
  batt[,soc:=fleetSize*batteryCapacity]
  en <- join.on(en,batt,c('b','rmob','run'),c('b','rmob','run'),'soc')
  #en[t>0,soc:=0]
  setkey(en,b,rmob,t)
  en[,soc:=soc+cumsum(en.ch-en.mob),by=c('b','rmob','run')]

  # Run by Run Plots
  for(run.i in u(vehs$run)){
    setkey(vehs,run,b,rmob,t)
    toplot <- melt(vehs[run==run.i],id.vars=c('b','rmob','t'))
    p <- ggplot(toplot,aes(x=t,y=value,fill=variable))+geom_bar(stat='identity',position='stack')+facet_wrap(~rmob)+scale_fill_manual(values = getPalette(toplot$variable))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    toplot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),'Charging',ifelse(variable=='vehiclesIdle','Idle','Moving'))]
    toplot[,variable.simp:=factor(variable.simp,c('Idle','Moving','Charging'))]
    setkey(toplot,variable.simp)
    p <- ggplot(toplot,aes(x=t,y=value,fill=variable.simp))+geom_bar(stat='identity',position='stack')+facet_wrap(~rmob)+
      scale_fill_manual(values = getPalette(toplot$variable.simp),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_num-vehs-simple.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
  
    # Charging load
    p <- ggplot(veh.ch[run==run.i],aes(x=t,y=gw.charging,fill=fct_rev(l)))+geom_bar(stat='identity',position='stack')+facet_wrap(~rmob)+scale_fill_manual(values = rev(getPalette(veh.ch$l)),guide=guide_legend(reverse=F))+labs(x='Hour',y='Load (GW)',fill='Charger Level')
    pdf.scale <- 1
    ggsave(pp(plots.dir,'/run-',run.i,'/_charging.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Energy balance
    p <- ggplot(en[run==run.i],aes(x=t,y=soc,colour=fct_rev(b)))+geom_line()+facet_wrap(~rmob,scales='free_y')+scale_colour_manual(values = rev(getPalette(en$b)),guide=guide_legend(reverse=F)) 
    ggsave(pp(plots.dir,'/run-',run.i,'/_soc.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    p <- ggplot(en,aes(x=t,y=soc,fill=fct_rev(b)))+geom_bar(stat='identity',position='stack')+facet_wrap(~rmob)+scale_fill_manual(values = rev(getPalette(en$b)),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_soc-bar.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Fleet size & type
    by.r <- rbindlist(list(res[['l-rmob']][,':='(run=run,variable=l,value=numChargers,group='Chargers')],res[['b-rmob']][,':='(run=run,variable=b,value=fleetSize,group='Fleet')]),fill=T)
    setkey(by.r,run,variable)
    p <- ggplot(by.r[run==run.i],aes(x=rmob,y=value,fill=fct_rev(variable)))+geom_bar(stat='identity')+facet_wrap(~group,scales='free_y')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+scale_fill_manual(values = rev(getPalette(by.r$variable)),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_fleet-size-and-type.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
    
    toplot <- by.r[run==run.i,.(percent=value/sum(value)*100,variable=variable),by=c('rmob','group')]
    toplot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
    geo.ordered <- cbind(c(sapply(c('-RUR','-URB'),function(x){ pp(c('PAC-NL','PAC-CA','MTN','WNC','WSC-NL','WSC-TX','ENC','ESC','NENG','MAT-NL','MAT-NY','SAT-NL','SAT-FL'),x) })))[,1]
    toplot[,rmob:=factor(rmob,geo.ordered)]
    setkey(toplot,rmob,variable)
    #toplot[,group:=factor(group,c('Fleet','Chargers'))]
    p <- ggplot(toplot,aes(x=factor(rmob),y=percent,fill=fct_rev(variable)))+geom_bar(stat='identity')+facet_grid(group~urb,scales='free_x', space ='free_x')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+scale_fill_manual(values = rev(getPalette(toplot$variable)),guide=guide_legend(reverse=F))
    ggsave(pp(plots.dir,'/run-',run.i,'/_fleet-size-and-type-percent.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')
    
    toplot <- res[['rmob']][,.(rmob,urbanFormFactor)]
    toplot[,urb:=ifelse(grepl('RUR$',rmob),'Rural','Urban')]
    toplot[,Region:=factor(rmob,geo.ordered)]
    p <- ggplot(toplot,aes(x=Region,y=urbanFormFactor))+geom_bar(stat='identity')+facet_wrap(~urb,scales='free_x')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+coord_cartesian(ylim=c(1,max(toplot$urbanFormFactor)*1.01))
    ggsave(pp(plots.dir,'/run-',run.i,'/_urban-form-factor-by-region.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    # Generation
    generators$g <- as.character(generators$g)
    generators <- merge(x=generators,y=fuels,by='FuelType',all.x=TRUE)
    generators$Simplified <- factor(generators$Simplified,levels=meritOrder)
    toplot <- merge(x=res[['g-t']][run==run.i],y=generators,by='g',all.x=TRUE)
    toplot <- toplot[,list(generation=sum(generation),base.generation=sum(base.generation)),by=list(r,Simplified,t)]
    toplot[,consq.generation:=generation-base.generation]
    p <- ggplot(toplot,aes(x=t,y=generation,fill=Simplified))+geom_area()+scale_fill_discrete(name='Fuel Type')+facet_wrap(~r,scale='free_y')
    ggsave(pp(plots.dir,'/run-',run.i,'/_generation-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    p <- ggplot(toplot,aes(x=t,y=consq.generation,fill=Simplified))+geom_area()+scale_fill_discrete(name='Fuel Type')+facet_wrap(~r,scale='free_y')
    ggsave(pp(plots.dir,'/run-',run.i,'/_generation-cnsq-total-by-region-fuel.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')

    toplot <- merge(x=res[['g-t']][run==run.i],y=generators,by='g',all.x=TRUE)
    
  }
  
  # tr <- rbindlist(list(melt(res[['rmob-t']],id.vars=c('t','rmob','run')),melt(en[,.(en.mob=sum(en.mob),en.ch=sum(en.ch)),by=c('rmob','t','run')],id.vars=c('t','rmob','run'))))
  # tr[,group:=ifelse(variable=='price','Price',ifelse(str_sub(variable,1,3)=='en.','Energy','Cost'))]
  # tr <- tr[group!="Cost"]
  # prices <- tr[variable=='price']
  # p <- ggplot(tr,aes(x=t,y=value,colour=variable))+geom_line()+facet_grid(group~rmob,scales='free_y') # geom_bar(stat='identity',position='dodge')
  # ggsave(pp(plots.dir,'_energy-vs-price.pdf'),p,width=12*pdf.scale,height=8*pdf.scale,units='in')
  
  gen.cost <- join.on(join.on(res[['g-t']],res[['g']],c('g','run'),c('g','run'))[,g:=as.numeric(g)],inputs$sets$gtor,'g','g')
  gen.cost <- gen.cost[,.(energyCost=sum(genCost*generation)),by=c('run','r')]
  costs <- join.on(join.on(res[['rmob']],res[['rmob-t']][,.(demandChargeCost=sum(demandChargeCost),vehicleMaintCost=sum(vehicleMaintCost)),by=c('run','rmob')],c('run','rmob'),c('run','rmob'),c('demandChargeCost','vehicleMaintCost')),inputs$sets$rmobtor,'rmob','rmob')
  # TODO join gen cost with costs but make gen cost be divided proportionately by energy consumed including personal vehicles
  
  vmt <- join.on(res[['b-d-rmob-t']],res[['d-rmob']],c('d','rmob','run'),c('d','rmob','run'),'travelDistance')
  vmt <- join.on(vmt,res[['d-rmob-t']],c('d','t','rmob','run'),c('d','t','rmob','run'),'speed')
  vmt[,vmt:=vehiclesMoving*travelDistance]
  vmt[,pmt:=demandAllocated*travelDistance]
  fleet <- res[['b-rmob']]
  
  data.to.save <- c('vehs','en','by.r','costs','veh.ch','vmt','fleet')
  run.params <- copy(exper$runs)[,run:=1:.N]
  for(dat.to.save in data.to.save){
    streval(pp(dat.to.save,' <- join.on(',dat.to.save,',run.params,\'run\',\'run\')'))
  }
  save(vehs,en,by.r,costs,veh.ch,vmt,fleet,file=pp(plots.dir,'/resuls-for-plotting.Rdata'))
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
    streval(pp('setkey(to.plot,variable,t,b,',param.names,')'))
    p <- ggplot(to.plot,aes(x=t,y=value,fill=fct_rev(variable)))+geom_bar(stat='identity',position='stack')+scale_fill_manual(values = rev(getPalette(to.plot$variable)))
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'_num-vehs.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    to.plot[,variable.simp:=ifelse(grepl('vehiclesCharging',variable),'Charging',ifelse(variable=='vehiclesIdle','Idle','Moving'))]
    to.plot[,variable.simp:=factor(variable.simp,c('Charging','Moving','Idle'))]
    to.plot <- to.plot[,.(value=sum(value)),by=c('t',param.names,'variable.simp')]
    streval(pp('setkey(to.plot,variable.simp,t,',param.names,')'))
    p <- ggplot(to.plot,aes(x=t,y=value,fill=fct_rev(variable.simp)))+geom_bar(stat='identity',position='stack')+scale_fill_manual(values = rev(getPalette(to.plot$variable.simp)))
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_num-vehs-simple.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Charging load
    streval(pp('setkey(veh.ch,l,rmob,t,',param.names,')'))
    to.plot <- veh.ch[,.(gw.charging=sum(gw.charging)),by=c('l','t',param.names)]
    p <- ggplot(to.plot,aes(x=t,y=gw.charging,fill=fct_rev(l)))+geom_bar(stat='identity',position='stack')+scale_fill_manual(values = rev(getPalette(to.plot$l)))+labs(x='Hour',y='Load (GW)',fill='Charger Level')
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    pdf.scale <- 1
    ggsave(pp(plots.dir,'_charging.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Energy balance
    streval(pp('setkey(en,b,rmob,t,',param.names,')'))
    to.plot <- en[,.(soc=sum(soc)),by=c('b','t',param.names)]
    p <- ggplot(to.plot,aes(x=t,y=soc,colour=fct_rev(b)))+geom_line()+scale_colour_manual(values = rev(getPalette(to.plot$b)))
    streval(pp('p <- p + facet_wrap(~',param.names,',scales="free_y")'))
    ggsave(pp(plots.dir,'_soc.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    p <- ggplot(to.plot,aes(x=t,y=soc,fill=fct_rev(b)))+geom_bar(stat='identity',position='stack')+scale_fill_manual(values = rev(getPalette(to.plot$b)))
    streval(pp('p <- p + facet_wrap(~',param.names,')'))
    ggsave(pp(plots.dir,'_soc-bar.pdf'),p,width=10*pdf.scale,height=8*pdf.scale,units='in')
    
    # Fleet and Chargers
    to.plot <- by.r[,.(value=sum(value)),by=c('group','variable',param.names)]
    p <- streval(pp('ggplot(to.plot,aes(x=factor(',param.names,'),y=value,fill=fct_rev(variable)))'))+geom_bar(stat='identity')+facet_wrap(~group,scales='free_y')+scale_fill_manual(values = rev(getPalette(to.plot$variable)))
    ggsave(pp(plots.dir,'_fleet-size-and-type.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    
    # Costs
    to.plot <- melt(costs,measure.vars=c('demandChargeCost','vehicleMaintCost','infrastructureCost','fleetCost'),id.vars=c('r',param.names))[,.(value=sum(value)),by=c('variable',param.names)]
    p <- streval(pp('ggplot(to.plot,aes(x=factor(',param.names,'),y=value,fill=fct_rev(variable)))'))+geom_bar(stat='identity')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+scale_fill_manual(values = rev(getPalette(to.plot$variable)))
    ggsave(pp(plots.dir,'_costs.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    
    speed <- copy(inputs$parameters$speed)[,t:=as.numeric(substr(t,2,nchar(as.character(t))))]
    vmt2 <- join.on(vmt,speed,c('t','d','rmob'),c('t','d','rmob'),'value')
    vmt2[,tt:=travelDistance*value]
    to.plot <- join.on(to.plot,vmt2[,.(vmt=sum(vmt),t=sum(tt)),by=param.names],param.names,param.names,c('vmt','t'))
    to.plot <- join.on(to.plot,fleet[,.(fleetSize=sum(fleetSize)),by=param.names],param.names,param.names,'fleetSize')
    to.plot[,cost.per.mile:=value/vmt]
    costs.per.mile <- to.plot  
    p <- streval(pp('ggplot(to.plot,aes(x=factor(',param.names,'),y=cost.per.mile,fill=fct_rev(variable)))'))+geom_bar(stat='identity')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+scale_fill_manual(values = rev(getPalette(to.plot$variable)))
    ggsave(pp(plots.dir,'_costs-per-mile.pdf'),p,width=10*pdf.scale,height=6*pdf.scale,units='in')  
    if(length(u(streval(pp('to.plot$',param.names))))==1){
      p <- ggplot(to.plot[,scen:=param.names],aes(x=scen,y=cost.per.mile,fill=fct_rev(variable)))+geom_bar(stat='identity')+ theme(axis.text.x = element_text(angle = 50, hjust = 1))+scale_fill_manual(values = rev(getPalette(to.plot$variable)))+coord_polar('y',start=0)+theme(axis.text.x=element_blank())+ geom_text(aes(y = cost.per.mile/3 + c(0, cumsum(cost.per.mile)[-length(cost.per.mile)]), label = roundC(cost.per.mile,3)), size=5)
      ggsave(pp(plots.dir,'_costs-per-mile-pie.pdf'),p,width=6*pdf.scale,height=6*pdf.scale,units='in')  
    }
  }
}

