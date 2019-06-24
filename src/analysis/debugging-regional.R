
load("/Users/critter/Dropbox/ucb/vto/beam-all/gem/experiments/Base/inputs.Rdata",ver=T)
load("/Users/critter/Dropbox/ucb/vto/beam-all/gem/experiments/Base/plots/resuls-for-plotting.Rdata",ver=T)

fleet <- join.on(fleet,all.inputs[[1]]$parameters$urbanFormFactor[,.(urbanFormFactor=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$demand[,.(demand=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$travelDistance[,.(travelDistance=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$speed[,.(speed=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$chargeRelocationRatio[,.(chargeRelocationRatio=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$fleetRatio[,.(fleetRatio=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$batteryRatio[,.(batteryRatio=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$distCorrection[,.(distCorrection=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$timeCorrection[,.(timeCorrection=sum(value)),by='rmob'],'rmob','rmob')
fleet <- join.on(fleet,all.inputs[[1]]$parameters$personalEVChargeEnergyUB[,.(personalEVChargeEnergyUB=sum(value)),by='rmob'],'rmob','rmob')

fleet <- join.on(fleet,all.inputs[[1]]$parameters$chargeRelocationCorrection[,.(chargeRelocationCorrection=sum(value)),by=c('b','rmob')],c('b','rmob'),c('b','rmob'))
#fleet <- join.on(fleet,all.inputs[[1]]$parameters$batteryLifetime[,.(batteryLifetime=sum(value)),by=c('b','rmob')],c('b','rmob'),c('b','rmob'))
#fleet <- join.on(fleet,all.inputs[[1]]$parameters$vehicleLifetime[,.(vehicleLifetime=sum(value)),by=c('b','rmob')],c('b','rmob'),c('b','rmob'))

fleet[,variable:=NULL]
fleet[,b:=as.character(b)]
fleet[,rmob:=as.character(rmob)]

setDF(fleet)

fm <- melt(fleet,id.vars=c('b','rmob','fleetSize'),measure.vars=c('urbanFormFactor','demand','travelDistance','speed','chargeRelocationCorrection',
                                                            'fleetRatio','batteryRatio','distCorrection','timeCorrection','personalEVChargeEnergyUB',
                                                            'batteryLifetime','chargeRelocationCorrection','vehicleLifetime'),variable.factor=F)


ggplot(fm,aes(x=value,y=fleetSize,colour=b))+geom_point()+facet_wrap(~variable,scales='free')
     