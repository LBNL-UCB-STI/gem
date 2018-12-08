library(data.table)
library(reshape)
library(stringr)
library(maps)
library(maptools)
library(ggplot2)
library(grid)
library(gtable)
library(plyr)

fillFuelTypes <- function(data) {
	data$FuelType[data$PlantType=='Coal Steam'&data$FuelType==''] <- 'Coal'
	data$FuelType[data$PlantType=='Combustion Turbine'&data$FuelType==''] <- 'NaturalGas'
	data$FuelType[data$PlantType=='Combined Cycle'&data$FuelType==''] <- 'NaturalGas'
	data$FuelType[data$PlantType=='O/G Steam'&data$FuelType==''] <- 'NaturalGas'
	data$FuelType[data$PlantType=='Nuclear'&data$FuelType==''] <- 'Nuclear'
	data$FuelType[data$PlantType=='O/G steam'&data$FuelType==''] <- 'NaturalGas'
	data$FuelType[data$PlantType=='IGCC'&data$FuelType==''] <- 'Coal'
	data$FuelType[data$PlantType=='Hydro'&data$FuelType==''] <- 'Hydro'
	data$FuelType[data$PlantType=='Fuel Cell'&data$FuelType==''] <- 'NaturalGas' 
	return(data)
}

modifyNERCMatching <- function(data) {
	data$NERC[data$RegionName=='ERC_WEST'] <- 'TRE'
	data$NERC[data$RegionName=='MAP_WAUE'] <- 'MRO'
	data$NERC[data$RegionName=='MIS_IL'] <- 'SERC'
	data$NERC[data$RegionName=='MIS_INKY'] <- 'RFC'
	data$NERC[data$RegionName=='MIS_LMI'] <- 'RFC' 
	data$NERC[data$RegionName=='MIS_MIDA'] <- 'MRO'
	data$NERC[data$RegionName=='MIS_MNWI'] <- 'MRO'
	data$NERC[data$RegionName=='MIS_MO'] <-'SERC'
	data$NERC[data$RegionName=='MIS_WUMS'] <- 'MRO'
	data$NERC[data$RegionName=='NY_Z_J'] <- 'NPCC'
	data$NERC[data$RegionName=='PJM_AP'] <- 'RFC'
	data$NERC[data$RegionName=='PJM_COMD'] <- 'RFC'
	data$NERC[data$RegionName=='PJM_Dom'] <- 'SERC'
	data$NERC[data$RegionName=='PJM_EMAC'] <- 'RFC'
	data$NERC[data$RegionName=='PJM_SMAC'] <- 'RFC'
	data$NERC[data$RegionName=='PJM_West'] <- 'RFC'
	data$NERC[data$RegionName=='S_C_KY'] <- 'SERC'
	data$NERC[data$RegionName=='S_C_TVA'] <- 'SERC'
	data$NERC[data$RegionName=='S_D_AMSO'] <- 'SERC'
	data$NERC[data$RegionName=='S_D_N_AR'] <- 'SERC'
	data$NERC[data$RegionName=='S_D_REST'] <- 'SERC'
	data$NERC[data$RegionName=='S_D_WOTA'] <- 'SERC'
	data$NERC[data$RegionName=='S_SOU'] <- 'SERC'
	data$NERC[data$RegionName=='SPP_N'] <- 'SPP'
	data$NERC[data$RegionName=='SPP_NEBR'] <- 'SPP'
	data$NERC[data$RegionName=='SPP_SPS'] <- 'SPP'
	data$NERC[data$RegionName=='SPP_WEST'] <- 'SPP'
	data$NERC[data$RegionName=='WECC_WY'] <- 'WECC'
	return(data)
}

cleanData <- function(data) {
	data <- fillFuelTypes(data)
	data$RegionName <- gsub('\\-','',data$RegionName)
	data$RegionName <- gsub('\\&','',data$RegionName)
	data <- modifyNERCMatching(data)
	data <- data.table(data)
	data$FuelCostTotal <- round(data$FuelCostTotal,1)
	data$FuelCostTotal[data$FuelCostTotal==0] <- .01
	return(data)
}

genToRegionSet <- function(data,scenarioFile) {
	output <- character()
	for(region in unique(data$DIVISIONCE)) {
		hold <- unique(data$UnitId[data$DIVISIONCE==region])
		temp <- paste('(',paste(hold,collapse=','),').',region,sep='')
		output <- c(output,temp)
	}
	write.table(output,file=paste(scenarioFile,'/inputs/par_gtor.csv',sep=''),row.names=FALSE,col.names=FALSE,sep=',')
}

reformData <- function(data,valueName) {
	output <- data.frame(data)[,c('UnitId',valueName)]
	output[,valueName] <- output[,valueName]+.1
	return(output)
}

exportTable <- function(input,fileName,scenarioFile) {
	write.table(input,file=paste(scenarioFile,'/inputs/',fileName,'.csv',sep=''),row.names=FALSE,col.names=FALSE,sep=',')
}

exportParTable <- function(input,inputDates,inputYear,scenarioFile) {
	write.table(unique(input$DIVISIONCE),paste(scenarioFile,'/inputs/par_r.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	write.table(unique(input$UnitId),paste(scenarioFile,'/inputs/par_g.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	timeColumn <- rep(inputDates,each=length(inputYear),times=1)
	timeColumn <- paste(rep(inputYear,times=length(inputDates)),timeColumn,sep='_')
	timeColumn <- paste(rep(timeColumn,times=24),rep(1:24,each=length(timeColumn)),sep='_')
	timeColumn <- convertPeriodToHour(timeColumn)
	write.table(timeColumn,paste(scenarioFile,'/inputs/par_t.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	write.table(unique(input$UnitId[input$FuelType=='Solar']),paste(scenarioFile,'/inputs/par_solar.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	write.table(unique(input$UnitId[input$FuelType=='Wind']),paste(scenarioFile,'/inputs/par_wind.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	write.table(unique(input$UnitId[input$FuelType=='Hydro']),paste(scenarioFile,'/inputs/par_hydro.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
}

createPairs <- function(data) {
	regions <- unique(data$RegionName)
	regions <- gsub(' Sum','',regions)
	regions <- gsub(' Win','',regions)
	uniqueRegionPairs <- data.frame(t(combn(regions,2)))
	colnames(uniqueRegionPairs) <- c('From','To')
	uniqueRegionPairs <- rbind(uniqueRegionPairs,data.frame('From'=uniqueRegionPairs$To,'To'=uniqueRegionPairs$From))
	uniqueRegionPairs$Capacity <- 1
	return(uniqueRegionPairs)
}

modifyCapacities <- function(data) {
	data$Capacity[data$From=='ERC_FRNT'&data$To=='ERC_REST'] <- 860
	data$Capacity[data$From=='ERC_FRNT'&data$To=='SPP_WEST'] <- 860
	data$Capacity[data$From=='ERC_GWAY'&data$To=='ERC_REST'] <- 845
	data$Capacity[data$From=='ERC_GWAY'&data$To=='SPP_WEST'] <- 845
	data$Capacity[data$From=='ERC_REST'&data$To=='ERC_WEST'] <- 5529
	data$Capacity[data$From=='ERC_REST'&data$To=='SPP_WEST'] <- 600
	data$Capacity[data$From=='ERC_WEST'&data$To=='ERC_REST'] <- 10555
	data$Capacity[data$From=='ERC_WEST'&data$To=='SPP_WEST'] <- 220
	data$Capacity[data$From=='FRCC'&data$To=='S_SOU'] <- 3600
	data$Capacity[data$From=='MAP_WAUE'&data$To=='CN_SK'] <- 100
	data$Capacity[data$From=='MAP_WAUE'&data$To=='MIS_IA'] <- 100
	data$Capacity[data$From=='MAP_WAUE'&data$To=='MIS_MAPP'] <- 1500
	data$Capacity[data$From=='MAP_WAUE'&data$To=='MIS_MIDA'] <- 1000
	data$Capacity[data$From=='MAP_WAUE'&data$To=='MIS_MNWI'] <- 3000
	data$Capacity[data$From=='MAP_WAUE'&data$To=='SPP_NEBR'] <- 1000
	data$Capacity[data$From=='MIS_IA'&data$To=='MAP_WAUE'] <- 100
	data$Capacity[data$From=='MIS_IA'&data$To=='MIS_IL'] <- 100
	data$Capacity[data$From=='MIS_IA'&data$To=='MIS_MIDA'] <- 2000
	data$Capacity[data$From=='MIS_IA'&data$To=='MIS_MNWI'] <- 2000
	data$Capacity[data$From=='MIS_IA'&data$To=='MIS_MO'] <- 711
	data$Capacity[data$From=='MIS_IA'&data$To=='PJM_COMD'] <- 600
	data$Capacity[data$From=='MIS_IA'&data$To=='S_D_N_AR'] <- 100
	data$Capacity[data$From=='MIS_IL'&data$To=='MIS_IA'] <- 100
	data$Capacity[data$From=='MIS_IL'&data$To=='MIS_INKY'] <- 1195
	data$Capacity[data$From=='MIS_IL'&data$To=='MIS_MIDA'] <- 100
	data$Capacity[data$From=='MIS_IL'&data$To=='MIS_MO'] <- 4500
	data$Capacity[data$From=='MIS_IL'&data$To=='PJM_COMD'] <- 3000
	data$Capacity[data$From=='MIS_IL'&data$To=='PJM_West'] <- 1300
	data$Capacity[data$From=='MIS_IL'&data$To=='S_C_TVA'] <- 1500
	data$Capacity[data$From=='MIS_INKY'&data$To=='MIS_IL'] <- 1195
	data$Capacity[data$From=='MIS_INKY'&data$To=='MIS_LMI'] <- 100
	data$Capacity[data$From=='MIS_INKY'&data$To=='PJM_COMD'] <- 3355
	data$Capacity[data$From=='MIS_INKY'&data$To=='PJM_West'] <- 6509
	data$Capacity[data$From=='MIS_INKY'&data$To=='S_C_KY'] <- 3787
	data$Capacity[data$From=='MIS_INKY'&data$To=='S_C_TVA'] <- 500
	data$Capacity[data$From=='MIS_LMI'&data$To=='CN_ON'] <- 1200
	data$Capacity[data$From=='MIS_LMI'&data$To=='MIS_INKY'] <- 100
	data$Capacity[data$From=='MIS_LMI'&data$To=='MIS_WUMS'] <- 100
	data$Capacity[data$From=='MIS_LMI'&data$To=='PJM_ATSI'] <- 2036
	data$Capacity[data$From=='MIS_LMI'&data$To=='PJM_West'] <- 2800
	data$Capacity[data$From=='MIS_MAPP'&data$To=='CN_MB'] <- 500
	data$Capacity[data$From=='MIS_MAPP'&data$To=='MAP_WAUE'] <- 1500
	data$Capacity[data$From=='MIS_MAPP'&data$To=='MIS_MNWI'] <- 5000
	data$Capacity[data$From=='MIS_MIDA'&data$To=='MAP_WAUE'] <- 1000
	data$Capacity[data$From=='MIS_MIDA'&data$To=='MIS_IA'] <- 2000
	data$Capacity[data$From=='MIS_MIDA'&data$To=='MIS_IL'] <- 100
	data$Capacity[data$From=='MIS_MIDA'&data$To=='MIS_MNWI'] <- 1
	data$Capacity[data$From=='MIS_MIDA'&data$To=='MIS_MO'] <- 500
	data$Capacity[data$From=='MIS_MIDA'&data$To=='PJM_COMD'] <- 3000
	data$Capacity[data$From=='MIS_MIDA'&data$To=='S_D_N_AR'] <- 30
	data$Capacity[data$From=='MIS_MIDA'&data$To=='SPP_N'] <- 50
	data$Capacity[data$From=='MIS_MIDA'&data$To=='SPP_NEBR'] <- 2000
	data$Capacity[data$From=='MIS_MNWI'&data$To=='CN_MB'] <- 1700
	data$Capacity[data$From=='MIS_MNWI'&data$To=='CN_ON'] <- 162
	data$Capacity[data$From=='MIS_MNWI'&data$To=='MAP_WAUE'] <- 3000
	data$Capacity[data$From=='MIS_MNWI'&data$To=='MIS_IA'] <- 2000
	data$Capacity[data$From=='MIS_MNWI'&data$To=='MIS_MAPP'] <- 5000
	data$Capacity[data$From=='MIS_MNWI'&data$To=='MIS_MIDA'] <- 1
	data$Capacity[data$From=='MIS_MNWI'&data$To=='MIS_WUMS'] <- 2400
	data$Capacity[data$From=='MIS_MO'&data$To=='MIS_IA'] <- 711
	data$Capacity[data$From=='MIS_MO'&data$To=='MIS_IL'] <- 4500
	data$Capacity[data$From=='MIS_MO'&data$To=='MIS_MIDA'] <- 500
	data$Capacity[data$From=='MIS_MO'&data$To=='S_D_N_AR'] <- 2804
	data$Capacity[data$From=='MIS_MO'&data$To=='SPP_N'] <- 1000
	data$Capacity[data$From=='MIS_WUMS'&data$To=='MIS_LMI'] <- 100
	data$Capacity[data$From=='MIS_WUMS'&data$To=='MIS_MNWI'] <- 2400
	data$Capacity[data$From=='MIS_WUMS'&data$To=='PJM_COMD'] <- 1000
	data$Capacity[data$From=='NENG_CT'&data$To=='NENGREST'] <- 2600
	data$Capacity[data$From=='NENG_CT'&data$To=='NY_Z_GI'] <- 900
	data$Capacity[data$From=='NENG_CT'&data$To=='NY_Z_K'] <- 760
	data$Capacity[data$From=='NENG_ME'&data$To=='CN_NB'] <- 800
	data$Capacity[data$From=='NENG_ME'&data$To=='NENGREST'] <- 1600
	data$Capacity[data$From=='NENGREST'&data$To=='CN_PQ'] <- 1650
	data$Capacity[data$From=='NENGREST'&data$To=='NENG_CT'] <- 2600
	data$Capacity[data$From=='NENGREST'&data$To=='NENG_ME'] <- 1600
	data$Capacity[data$From=='NENGREST'&data$To=='NY_Z_D'] <- 1
	data$Capacity[data$From=='NENGREST'&data$To=='NY_Z_F'] <- 800
	data$Capacity[data$From=='NY_Z_AB'&data$To=='CN_ON'] <- 1200
	data$Capacity[data$From=='NY_Z_AB'&data$To=='NY_Z_CE'] <- 1550
	data$Capacity[data$From=='NY_Z_AB'&data$To=='PJM_PENE'] <- 1000
	data$Capacity[data$From=='NY_Z_CE'&data$To=='NY_Z_AB'] <- 1300
	data$Capacity[data$From=='NY_Z_CE'&data$To=='NY_Z_D'] <- 1600
	data$Capacity[data$From=='NY_Z_CE'&data$To=='NY_Z_F'] <- 3250
	data$Capacity[data$From=='NY_Z_CE'&data$To=='NY_Z_GI'] <- 1700
	data$Capacity[data$From=='NY_Z_CE'&data$To=='PJM_PENE'] <- 1500
	data$Capacity[data$From=='NY_Z_D'&data$To=='CN_PQ'] <- 1200
	data$Capacity[data$From=='NY_Z_D'&data$To=='NENGREST'] <- 150
	data$Capacity[data$From=='NY_Z_D'&data$To=='NY_Z_CE'] <- 2650
	data$Capacity[data$From=='NY_Z_F'&data$To=='NENGREST'] <- 800
	data$Capacity[data$From=='NY_Z_F'&data$To=='NY_Z_CE'] <- 1999
	data$Capacity[data$From=='NY_Z_F'&data$To=='NY_Z_GI'] <- 3450
	data$Capacity[data$From=='NY_Z_GI'&data$To=='NENG_CT'] <- 1130
	data$Capacity[data$From=='NY_Z_GI'&data$To=='PJM_EMAC'] <- 1000
	data$Capacity[data$From=='NY_Z_GI'&data$To=='NY_Z_CE'] <- 1600
	data$Capacity[data$From=='NY_Z_GI'&data$To=='NY_Z_F'] <- 1999
	data$Capacity[data$From=='NY_Z_GI'&data$To=='NY_Z_J'] <- 4350
	data$Capacity[data$From=='NY_Z_GI'&data$To=='NY_Z_K'] <- 1290
	data$Capacity[data$From=='NY_Z_J'&data$To=='NY_Z_GI'] <- 3500
	data$Capacity[data$From=='NY_Z_J'&data$To=='NY_Z_K'] <- 175
	data$Capacity[data$From=='NY_Z_J'&data$To=='PJM_EMAC'] <- 1900
	data$Capacity[data$From=='NY_Z_K'&data$To=='NENG_CT'] <- 760
	data$Capacity[data$From=='NY_Z_K'&data$To=='NY_Z_GI'] <- 530
	data$Capacity[data$From=='NY_Z_K'&data$To=='NY_Z_J'] <- 283
	data$Capacity[data$From=='NY_Z_K'&data$To=='PJM_EMAC'] <- 660
	data$Capacity[data$From=='PJM_AP'&data$To=='PJM_ATSI'] <- 2731
	data$Capacity[data$From=='PJM_AP'&data$To=='PJM_Dom'] <- 8000
	data$Capacity[data$From=='PJM_AP'&data$To=='PJM_PENE'] <- 3200
	data$Capacity[data$From=='PJM_AP'&data$To=='PJM_SMAC'] <- 2200
	data$Capacity[data$From=='PJM_AP'&data$To=='PJM_West'] <- 6300
	data$Capacity[data$From=='PJM_ATSI'&data$To=='MIS_LMI'] <- 2036
	data$Capacity[data$From=='PJM_ATSI'&data$To=='PJM_AP'] <- 2731
	data$Capacity[data$From=='PJM_ATSI'&data$To=='PJM_PENE'] <- 1500
	data$Capacity[data$From=='PJM_ATSI'&data$To=='PJM_West'] <- 9700
	data$Capacity[data$From=='PJM_COMD'&data$To=='MIS_IA'] <- 600
	data$Capacity[data$From=='PJM_COMD'&data$To=='MIS_IL'] <- 3000
	data$Capacity[data$From=='PJM_COMD'&data$To=='MIS_INKY'] <- 5098
	data$Capacity[data$From=='PJM_COMD'&data$To=='MIS_MIDA'] <- 3000
	data$Capacity[data$From=='PJM_COMD'&data$To=='MIS_WUMS'] <- 1000
	data$Capacity[data$From=='PJM_COMD'&data$To=='PJM_West'] <- 4000
	data$Capacity[data$From=='PJM_Dom'&data$To=='PJM_AP'] <- 8000
	data$Capacity[data$From=='PJM_Dom'&data$To=='PJM_SMAC'] <- 2812
	data$Capacity[data$From=='PJM_Dom'&data$To=='PJM_West'] <- 3800
	data$Capacity[data$From=='PJM_Dom'&data$To=='S_VACA'] <- 2598
	data$Capacity[data$From=='PJM_EMAC'&data$To=='NY_Z_J'] <- 1900
	data$Capacity[data$From=='PJM_EMAC'&data$To=='NY_Z_K'] <- 660
	data$Capacity[data$From=='PJM_EMAC'&data$To=='NY_Z_GI'] <- 500
	data$Capacity[data$From=='PJM_EMAC'&data$To=='PJM_SMAC'] <- 1095
	data$Capacity[data$From=='PJM_EMAC'&data$To=='PJM_WMAC'] <- 6900
	data$Capacity[data$From=='PJM_PENE'&data$To=='NY_Z_AB'] <- 1000
	data$Capacity[data$From=='PJM_PENE'&data$To=='NY_Z_CE'] <- 1500
	data$Capacity[data$From=='PJM_PENE'&data$To=='PJM_AP'] <- 3200
	data$Capacity[data$From=='PJM_PENE'&data$To=='PJM_ATSI'] <- 1500
	data$Capacity[data$From=='PJM_PENE'&data$To=='PJM_WMAC'] <- 3565
	data$Capacity[data$From=='PJM_SMAC'&data$To=='PJM_AP'] <- 2200
	data$Capacity[data$From=='PJM_SMAC'&data$To=='PJM_Dom'] <- 2812
	data$Capacity[data$From=='PJM_SMAC'&data$To=='PJM_EMAC'] <- 1095
	data$Capacity[data$From=='PJM_SMAC'&data$To=='PJM_WMAC'] <- 2000
	data$Capacity[data$From=='PJM_West'&data$To=='MIS_IL'] <- 1300
	data$Capacity[data$From=='PJM_West'&data$To=='MIS_INKY'] <- 6415
	data$Capacity[data$From=='PJM_West'&data$To=='MIS_LMI'] <- 2800
	data$Capacity[data$From=='PJM_West'&data$To=='PJM_AP'] <- 6300
	data$Capacity[data$From=='PJM_West'&data$To=='PJM_ATSI'] <- 9700
	data$Capacity[data$From=='PJM_West'&data$To=='PJM_COMD'] <- 4000
	data$Capacity[data$From=='PJM_West'&data$To=='PJM_Dom'] <- 3800
	data$Capacity[data$From=='PJM_West'&data$To=='S_C_KY'] <- 2074
	data$Capacity[data$From=='PJM_West'&data$To=='S_C_TVA'] <- 3118
	data$Capacity[data$From=='PJM_West'&data$To=='S_VACA'] <- 1000
	data$Capacity[data$From=='PJM_WMAC'&data$To=='PJM_EMAC'] <- 6900
	data$Capacity[data$From=='PJM_WMAC'&data$To=='PJM_PENE'] <- 3565
	data$Capacity[data$From=='PJM_WMAC'&data$To=='PJM_SMAC'] <- 2000
	data$Capacity[data$From=='S_C_KY'&data$To=='MIS_INKY'] <- 3787
	data$Capacity[data$From=='S_C_KY'&data$To=='PJM_West'] <- 2074
	data$Capacity[data$From=='S_C_TVA'&data$To=='MIS_IL'] <- 1500
	data$Capacity[data$From=='S_C_TVA'&data$To=='MIS_INKY'] <- 500
	data$Capacity[data$From=='S_C_TVA'&data$To=='PJM_West'] <- 3118
	data$Capacity[data$From=='S_C_TVA'&data$To=='S_D_N_AR'] <- 3019
	data$Capacity[data$From=='S_C_TVA'&data$To=='S_D_REST'] <- 2494
	data$Capacity[data$From=='S_C_TVA'&data$To=='S_SOU'] <- 5098
	data$Capacity[data$From=='S_C_TVA'&data$To=='S_VACA'] <- 276
	data$Capacity[data$From=='S_D_AMSO'&data$To=='S_D_REST'] <- 2450
	data$Capacity[data$From=='S_D_AMSO'&data$To=='S_SOU'] <- 700
	data$Capacity[data$From=='S_D_AMSO'&data$To=='SPP_SE'] <- 500
	data$Capacity[data$From=='S_D_N_AR'&data$To=='MIS_IA'] <- 100
	data$Capacity[data$From=='S_D_N_AR'&data$To=='MIS_MIDA'] <- 30
	data$Capacity[data$From=='S_D_N_AR'&data$To=='MIS_MO'] <- 2804
	data$Capacity[data$From=='S_D_N_AR'&data$To=='S_C_TVA'] <- 3019
	data$Capacity[data$From=='S_D_N_AR'&data$To=='SPP_N'] <- 2955
	data$Capacity[data$From=='S_D_N_AR'&data$To=='SPP_WEST'] <- 3000
	data$Capacity[data$From=='S_D_REST'&data$To=='S_C_TVA'] <- 2494
	data$Capacity[data$From=='S_D_REST'&data$To=='S_D_AMSO'] <- 2450
	data$Capacity[data$From=='S_D_REST'&data$To=='S_D_WOTA'] <- 1050
	data$Capacity[data$From=='S_D_REST'&data$To=='S_SOU'] <- 2000
	data$Capacity[data$From=='S_D_REST'&data$To=='SPP_SE'] <- 3136
	data$Capacity[data$From=='S_D_REST'&data$To=='SPP_WEST'] <- 900
	data$Capacity[data$From=='S_D_WOTA'&data$To=='S_D_REST'] <- 1250
	data$Capacity[data$From=='S_D_WOTA'&data$To=='SPP_SE'] <- 2835
	data$Capacity[data$From=='S_SOU'&data$To=='FRCC'] <- 3600
	data$Capacity[data$From=='S_SOU'&data$To=='S_C_TVA'] <- 5893
	data$Capacity[data$From=='S_SOU'&data$To=='S_D_AMSO'] <- 700
	data$Capacity[data$From=='S_SOU'&data$To=='S_D_REST'] <- 2000
	data$Capacity[data$From=='S_SOU'&data$To=='S_VACA'] <- 3000
	data$Capacity[data$From=='S_VACA'&data$To=='PJM_Dom'] <- 2598
	data$Capacity[data$From=='S_VACA'&data$To=='PJM_West'] <- 1000
	data$Capacity[data$From=='S_VACA'&data$To=='S_C_TVA'] <- 276
	data$Capacity[data$From=='S_VACA'&data$To=='S_SOU'] <- 3000
	data$Capacity[data$From=='SPP_KIAM'&data$To=='ERC_REST'] <- 1178
	data$Capacity[data$From=='SPP_KIAM'&data$To=='SPP_WEST'] <- 1178
	data$Capacity[data$From=='SPP_N'&data$To=='MIS_MIDA'] <- 50
	data$Capacity[data$From=='SPP_N'&data$To=='MIS_MO'] <- 1000
	data$Capacity[data$From=='SPP_N'&data$To=='S_D_N_AR'] <- 2955
	data$Capacity[data$From=='SPP_N'&data$To=='SPP_NEBR'] <- 1666
	data$Capacity[data$From=='SPP_N'&data$To=='SPP_SPS'] <- 900
	data$Capacity[data$From=='SPP_N'&data$To=='SPP_WEST'] <- 3600
	data$Capacity[data$From=='SPP_NEBR'&data$To=='MAP_WAUE'] <- 1000
	data$Capacity[data$From=='SPP_NEBR'&data$To=='MIS_WIDA'] <- 2000
	data$Capacity[data$From=='SPP_NEBR'&data$To=='SPP_N'] <- 1666
	data$Capacity[data$From=='SPP_SE'&data$To=='S_D_AMSO'] <- 500
	data$Capacity[data$From=='SPP_SE'&data$To=='S_D_REST'] <- 3136
	data$Capacity[data$From=='SPP_SE'&data$To=='S_D_WOTA'] <- 2835
	data$Capacity[data$From=='SPP_SE'&data$To=='SPP_WEST'] <- 852
	data$Capacity[data$From=='SPP_SPS'&data$To=='SPP_N'] <- 900
	data$Capacity[data$From=='SPP_SPS'&data$To=='SPP_WEST'] <- 2205
	data$Capacity[data$From=='SPP_SPS'&data$To=='WECC_NM'] <- 610
	data$Capacity[data$From=='SPP_WEST'&data$To=='ERC_REST'] <- 600
	data$Capacity[data$From=='SPP_WEST'&data$To=='ERC_WEST'] <- 220
	data$Capacity[data$From=='SPP_WEST'&data$To=='S_D_N_AR'] <- 3000
	data$Capacity[data$From=='SPP_WEST'&data$To=='S_D_REST'] <- 900
	data$Capacity[data$From=='SPP_WEST'&data$To=='SPP_N'] <- 2700
	data$Capacity[data$From=='SPP_WEST'&data$To=='SPP_SE'] <- 688
	data$Capacity[data$From=='SPP_WEST'&data$To=='SPP_SPS'] <- 2205
	data$Capacity[data$From=='WEC_CALN'&data$To=='WECC_NNV'] <- 100
	data$Capacity[data$From=='WEC_CALN'&data$To=='WECC_PNW'] <- 3675
	data$Capacity[data$From=='WEC_CALN'&data$To=='WECC_SCE'] <- 1275
	data$Capacity[data$From=='WEC_CALN'&data$To=='WECC_SF'] <- 1272
	data$Capacity[data$From=='WEC_LADW'&data$To=='WECC_AZ'] <- 468
	data$Capacity[data$From=='WEC_LADW'&data$To=='WECC_PNW'] <- 2858
	data$Capacity[data$From=='WEC_LADW'&data$To=='WECC_SCE'] <- 3750
	data$Capacity[data$From=='WEC_LADW'&data$To=='WECC_SNV'] <- 3883
	data$Capacity[data$From=='WEC_LADW'&data$To=='WECC_UT'] <- 1400
	data$Capacity[data$From=='WEC_SDGE'&data$To=='WECC_AZ'] <- 1168
	data$Capacity[data$From=='WEC_SDGE'&data$To=='WECC_IID'] <- 150
	data$Capacity[data$From=='WEC_SDGE'&data$To=='WECC_SCE'] <- 2440
	data$Capacity[data$From=='WECC_AZ'&data$To=='WEC_LADW'] <- 362
	data$Capacity[data$From=='WECC_AZ'&data$To=='WEC_SDGE'] <- 1163
	data$Capacity[data$From=='WECC_AZ'&data$To=='WECC_IID'] <- 195
	data$Capacity[data$From=='WECC_AZ'&data$To=='WECC_NM'] <- 5522
	data$Capacity[data$From=='WECC_AZ'&data$To=='WECC_SCE'] <- 1600
	data$Capacity[data$From=='WECC_AZ'&data$To=='WECC_SNV'] <- 4727
	data$Capacity[data$From=='WECC_AZ'&data$To=='WECC_UT'] <- 250
	data$Capacity[data$From=='WECC_CO'&data$To=='WECC_NM'] <- 614
	data$Capacity[data$From=='WECC_CO'&data$To=='WECC_UT'] <- 650
	data$Capacity[data$From=='WECC_CO'&data$To=='WECC_WY'] <- 1400
	data$Capacity[data$From=='WECC_ID'&data$To=='WECC_MT'] <- 200
	data$Capacity[data$From=='WECC_ID'&data$To=='WECC_NNV'] <- 350
	data$Capacity[data$From=='WECC_ID'&data$To=='WECC_PNW'] <- 1800
	data$Capacity[data$From=='WECC_ID'&data$To=='WECC_UT'] <- 680
	data$Capacity[data$From=='WECC_ID'&data$To=='WECC_WY'] <- 1
	data$Capacity[data$From=='WECC_IID'&data$To=='WEC_SDGE'] <- 150
	data$Capacity[data$From=='WECC_IID'&data$To=='WECC_AZ'] <- 163
	data$Capacity[data$From=='WECC_IID'&data$To=='WECC_SCE'] <- 600
	data$Capacity[data$From=='WECC_MT'&data$To=='WECC_ID'] <- 325
	data$Capacity[data$From=='WECC_MT'&data$To=='WECC_PNW'] <- 2000
	data$Capacity[data$From=='WECC_MT'&data$To=='WECC_WY'] <- 400
	data$Capacity[data$From=='WECC_NM'&data$To=='SPP_SPS'] <- 610
	data$Capacity[data$From=='WECC_NM'&data$To=='WECC_AZ'] <- 5582
	data$Capacity[data$From=='WECC_NM'&data$To=='WECC_CO'] <- 664
	data$Capacity[data$From=='WECC_NM'&data$To=='WECC_UT'] <- 530
	data$Capacity[data$From=='WECC_NNV'&data$To=='WEC_CALN'] <- 100
	data$Capacity[data$From=='WECC_NNV'&data$To=='WECC_ID'] <- 185
	data$Capacity[data$From=='WECC_NNV'&data$To=='WECC_PNW'] <- 300
	data$Capacity[data$From=='WECC_NNV'&data$To=='WECC_UT'] <- 235
	data$Capacity[data$From=='WECC_PNW'&data$To=='CN_BC'] <- 1000
	data$Capacity[data$From=='WECC_PNW'&data$To=='WEC_CALN'] <- 4200
	data$Capacity[data$From=='WECC_PNW'&data$To=='WEC_LADW'] <- 2600
	data$Capacity[data$From=='WECC_PNW'&data$To=='WECC_ID'] <- 500
	data$Capacity[data$From=='WECC_PNW'&data$To=='WECC_MT'] <- 1000
	data$Capacity[data$From=='WECC_PNW'&data$To=='WECC_NNV'] <- 300
	data$Capacity[data$From=='WECC_SCE'&data$To=='WEC_CALN'] <- 3000
	data$Capacity[data$From=='WECC_SCE'&data$To=='WEC_LADW'] <- 3750
	data$Capacity[data$From=='WECC_SCE'&data$To=='WEC_SDGE'] <- 2200
	data$Capacity[data$From=='WECC_SCE'&data$To=='WECC_AZ'] <- 1082
	data$Capacity[data$From=='WECC_SCE'&data$To=='WECC_IID'] <- 50
	data$Capacity[data$From=='WECC_SCE'&data$To=='WECC_SNV'] <- 2814
	data$Capacity[data$From=='WECC_SF'&data$To=='WEC_CALN'] <- 1100
	data$Capacity[data$From=='WECC_SNV'&data$To=='WEC_LADW'] <- 2300
	data$Capacity[data$From=='WECC_SNV'&data$To=='WECC_AZ'] <- 4785
	data$Capacity[data$From=='WECC_SNV'&data$To=='WECC_SCE'] <- 1700
	data$Capacity[data$From=='WECC_SNV'&data$To=='WECC_UT'] <- 250
	data$Capacity[data$From=='WECC_UT'&data$To=='WEC_LADW'] <- 1920
	data$Capacity[data$From=='WECC_UT'&data$To=='WECC_AZ'] <- 250
	data$Capacity[data$From=='WECC_UT'&data$To=='WECC_CO'] <- 650
	data$Capacity[data$From=='WECC_UT'&data$To=='WECC_ID'] <- 775
	data$Capacity[data$From=='WECC_UT'&data$To=='WECC_NM'] <- 600
	data$Capacity[data$From=='WECC_UT'&data$To=='WECC_NNV'] <- 360
	data$Capacity[data$From=='WECC_UT'&data$To=='WECC_SNV'] <- 140
	data$Capacity[data$From=='WECC_UT'&data$To=='WECC_WY'] <- 400
	data$Capacity[data$From=='WECC_WY'&data$To=='WECC_CO'] <- 1400
	data$Capacity[data$From=='WECC_WY'&data$To=='WECC_ID'] <- 2200
	data$Capacity[data$From=='WECC_WY'&data$To=='WECC_MT'] <- 200
	data$Capacity[data$From=='WECC_WY'&data$To=='WECC_UT'] <- 400
	return(data)
}

modifyCosts <- function(data) {
	names(data) <- c('From','To','Cost')
	data$Cost <- .01
	data$Cost[data$From=='ERC_FRNT'&data$To=='ERC_REST'] <- .01
	data$Cost[data$From=='ERC_FRNT'&data$To=='SPP_WEST'] <- 6
	data$Cost[data$From=='ERC_GWAY'&data$To=='ERC_REST'] <- .01
	data$Cost[data$From=='ERC_GWAY'&data$To=='SPP_WEST'] <- 6
	data$Cost[data$From=='ERC_REST'&data$To=='ERC_WEST'] <- .01
	data$Cost[data$From=='ERC_REST'&data$To=='SPP_WEST'] <- 6
	data$Cost[data$From=='ERC_WEST'&data$To=='ERC_REST'] <- .01
	data$Cost[data$From=='ERC_WEST'&data$To=='SPP_WEST'] <- 6
	data$Cost[data$From=='FRCC'&data$To=='S_SOU'] <- 8
	data$Cost[data$From=='MAP_WAUE'&data$To=='CN_SK'] <- 8
	data$Cost[data$From=='MAP_WAUE'&data$To=='MIS_IA'] <- 6
	data$Cost[data$From=='MAP_WAUE'&data$To=='MIS_MAPP'] <- .01
	data$Cost[data$From=='MAP_WAUE'&data$To=='MIS_MIDA'] <- 6
	data$Cost[data$From=='MAP_WAUE'&data$To=='MIS_MNWI'] <- 6
	data$Cost[data$From=='MAP_WAUE'&data$To=='SPP_NEBR'] <- 6
	data$Cost[data$From=='MIS_IA'&data$To=='MAP_WAUE'] <- 6
	data$Cost[data$From=='MIS_IA'&data$To=='MIS_IL'] <- .01
	data$Cost[data$From=='MIS_IA'&data$To=='MIS_MIDA'] <- .01
	data$Cost[data$From=='MIS_IA'&data$To=='MIS_MNWI'] <- .01
	data$Cost[data$From=='MIS_IA'&data$To=='MIS_MO'] <- .01
	data$Cost[data$From=='MIS_IA'&data$To=='PJM_COMD'] <- 3
	data$Cost[data$From=='MIS_IA'&data$To=='S_D_N_AR'] <- 1
	data$Cost[data$From=='MIS_IL'&data$To=='MIS_IA'] <- .01
	data$Cost[data$From=='MIS_IL'&data$To=='MIS_INKY'] <- .01
	data$Cost[data$From=='MIS_IL'&data$To=='MIS_MIDA'] <- .01
	data$Cost[data$From=='MIS_IL'&data$To=='MIS_MO'] <- .01
	data$Cost[data$From=='MIS_IL'&data$To=='PJM_COMD'] <- 3
	data$Cost[data$From=='MIS_IL'&data$To=='PJM_West'] <- 3
	data$Cost[data$From=='MIS_IL'&data$To=='S_C_TVA'] <- 6
	data$Cost[data$From=='MIS_INKY'&data$To=='MIS_IL'] <- .01
	data$Cost[data$From=='MIS_INKY'&data$To=='MIS_LMI'] <- .01
	data$Cost[data$From=='MIS_INKY'&data$To=='PJM_COMD'] <- 3
	data$Cost[data$From=='MIS_INKY'&data$To=='PJM_West'] <- 3
	data$Cost[data$From=='MIS_INKY'&data$To=='S_C_KY'] <- 6
	data$Cost[data$From=='MIS_INKY'&data$To=='S_C_TVA'] <- 6
	data$Cost[data$From=='MIS_LMI'&data$To=='CN_ON'] <- 8
	data$Cost[data$From=='MIS_LMI'&data$To=='MIS_INKY'] <- .01
	data$Cost[data$From=='MIS_LMI'&data$To=='MIS_WUMS'] <- .01
	data$Cost[data$From=='MIS_LMI'&data$To=='PJM_ATSI'] <- 3
	data$Cost[data$From=='MIS_LMI'&data$To=='PJM_West'] <- 3
	data$Cost[data$From=='MIS_MAPP'&data$To=='CN_MB'] <- 8
	data$Cost[data$From=='MIS_MAPP'&data$To=='MAP_WAUE'] <- .01
	data$Cost[data$From=='MIS_MAPP'&data$To=='MIS_MNWI'] <- 6
	data$Cost[data$From=='MIS_MIDA'&data$To=='MAP_WAUE'] <- 6
	data$Cost[data$From=='MIS_MIDA'&data$To=='MIS_IA'] <- .01
	data$Cost[data$From=='MIS_MIDA'&data$To=='MIS_IL'] <- .01
	data$Cost[data$From=='MIS_MIDA'&data$To=='MIS_MNWI'] <- .01
	data$Cost[data$From=='MIS_MIDA'&data$To=='MIS_MO'] <- .01
	data$Cost[data$From=='MIS_MIDA'&data$To=='PJM_COMD'] <- 3
	data$Cost[data$From=='MIS_MIDA'&data$To=='S_D_N_AR'] <- 1
	data$Cost[data$From=='MIS_MIDA'&data$To=='SPP_N'] <- 6
	data$Cost[data$From=='MIS_MIDA'&data$To=='SPP_NEBR'] <- 6
	data$Cost[data$From=='MIS_MNWI'&data$To=='CN_MB'] <- 8
	data$Cost[data$From=='MIS_MNWI'&data$To=='CN_ON'] <- 8
	data$Cost[data$From=='MIS_MNWI'&data$To=='MAP_WAUE'] <- 6
	data$Cost[data$From=='MIS_MNWI'&data$To=='MIS_IA'] <- .01
	data$Cost[data$From=='MIS_MNWI'&data$To=='MIS_MAPP'] <- 6
	data$Cost[data$From=='MIS_MNWI'&data$To=='MIS_MIDA'] <- .01
	data$Cost[data$From=='MIS_MNWI'&data$To=='MIS_WUMS'] <- .01
	data$Cost[data$From=='MIS_MO'&data$To=='MIS_IA'] <- .01
	data$Cost[data$From=='MIS_MO'&data$To=='MIS_IL'] <- .01
	data$Cost[data$From=='MIS_MO'&data$To=='MIS_MIDA'] <- .01
	data$Cost[data$From=='MIS_MO'&data$To=='S_D_N_AR'] <- 1
	data$Cost[data$From=='MIS_MO'&data$To=='SPP_N'] <- 6
	data$Cost[data$From=='MIS_WUMS'&data$To=='MIS_LMI'] <- .01
	data$Cost[data$From=='MIS_WUMS'&data$To=='MIS_MNWI'] <- .01
	data$Cost[data$From=='MIS_WUMS'&data$To=='PJM_COMD'] <- 3
	data$Cost[data$From=='NENG_CT'&data$To=='NENGREST'] <- .01
	data$Cost[data$From=='NENG_CT'&data$To=='NY_Z_GI'] <- 3
	data$Cost[data$From=='NENG_CT'&data$To=='NY_Z_K'] <- 3
	data$Cost[data$From=='NENG_ME'&data$To=='CN_NB'] <- 8
	data$Cost[data$From=='NENG_ME'&data$To=='NENGREST'] <- .01
	data$Cost[data$From=='NENGREST'&data$To=='CN_PQ'] <- 8
	data$Cost[data$From=='NENGREST'&data$To=='NENG_CT'] <- .01
	data$Cost[data$From=='NENGREST'&data$To=='NENG_ME'] <- .01
	data$Cost[data$From=='NENGREST'&data$To=='NY_Z_D'] <- 3
	data$Cost[data$From=='NENGREST'&data$To=='NY_Z_F'] <- 3
	data$Cost[data$From=='NY_Z_AB'&data$To=='CN_ON'] <- 8
	data$Cost[data$From=='NY_Z_AB'&data$To=='NY_Z_CE'] <- .01
	data$Cost[data$From=='NY_Z_AB'&data$To=='PJM_PENE'] <- 6
	data$Cost[data$From=='NY_Z_CE'&data$To=='NY_Z_AB'] <- .01
	data$Cost[data$From=='NY_Z_CE'&data$To=='NY_Z_D'] <- .01
	data$Cost[data$From=='NY_Z_CE'&data$To=='NY_Z_F'] <- .01
	data$Cost[data$From=='NY_Z_CE'&data$To=='NY_Z_GI'] <- .01
	data$Cost[data$From=='NY_Z_CE'&data$To=='PJM_PENE'] <- 6
	data$Cost[data$From=='NY_Z_D'&data$To=='CN_PQ'] <- 8
	data$Cost[data$From=='NY_Z_D'&data$To=='NENGREST'] <- 3
	data$Cost[data$From=='NY_Z_D'&data$To=='NY_Z_CE'] <- .01
	data$Cost[data$From=='NY_Z_F'&data$To=='NENGREST'] <- 3
	data$Cost[data$From=='NY_Z_F'&data$To=='NY_Z_CE'] <- .01
	data$Cost[data$From=='NY_Z_F'&data$To=='NY_Z_GI'] <- .01
	data$Cost[data$From=='NY_Z_GI'&data$To=='NENG_CT'] <- 3
	data$Cost[data$From=='NY_Z_GI'&data$To=='PJM_EMAC'] <- .01
	data$Cost[data$From=='NY_Z_GI'&data$To=='NY_Z_CE'] <- .01
	data$Cost[data$From=='NY_Z_GI'&data$To=='NY_Z_F'] <- .01
	data$Cost[data$From=='NY_Z_GI'&data$To=='NY_Z_J'] <- .01
	data$Cost[data$From=='NY_Z_GI'&data$To=='NY_Z_K'] <- .01
	data$Cost[data$From=='NY_Z_J'&data$To=='NY_Z_GI'] <- .01
	data$Cost[data$From=='NY_Z_J'&data$To=='NY_Z_K'] <- .01
	data$Cost[data$From=='NY_Z_J'&data$To=='PJM_EMAC'] <- 6
	data$Cost[data$From=='NY_Z_K'&data$To=='NENG_CT'] <- 3
	data$Cost[data$From=='NY_Z_K'&data$To=='NY_Z_GI'] <- .01
	data$Cost[data$From=='NY_Z_K'&data$To=='NY_Z_J'] <- .01
	data$Cost[data$From=='NY_Z_K'&data$To=='PJM_EMAC'] <- 6
	data$Cost[data$From=='PJM_AP'&data$To=='PJM_ATSI'] <- .01
	data$Cost[data$From=='PJM_AP'&data$To=='PJM_Dom'] <- .01
	data$Cost[data$From=='PJM_AP'&data$To=='PJM_PENE'] <- .01
	data$Cost[data$From=='PJM_AP'&data$To=='PJM_SMAC'] <- .01
	data$Cost[data$From=='PJM_AP'&data$To=='PJM_West'] <- .01
	data$Cost[data$From=='PJM_ATSI'&data$To=='MIS_LMI'] <- 3
	data$Cost[data$From=='PJM_ATSI'&data$To=='PJM_AP'] <- .01
	data$Cost[data$From=='PJM_ATSI'&data$To=='PJM_PENE'] <- .01
	data$Cost[data$From=='PJM_ATSI'&data$To=='PJM_West'] <- .01
	data$Cost[data$From=='PJM_COMD'&data$To=='MIS_IA'] <- 3
	data$Cost[data$From=='PJM_COMD'&data$To=='MIS_IL'] <- 3
	data$Cost[data$From=='PJM_COMD'&data$To=='MIS_INKY'] <- 3
	data$Cost[data$From=='PJM_COMD'&data$To=='MIS_MIDA'] <- 3
	data$Cost[data$From=='PJM_COMD'&data$To=='MIS_WUMS'] <- 3
	data$Cost[data$From=='PJM_COMD'&data$To=='PJM_West'] <- .01
	data$Cost[data$From=='PJM_Dom'&data$To=='PJM_AP'] <- .01
	data$Cost[data$From=='PJM_Dom'&data$To=='PJM_SMAC'] <- .01
	data$Cost[data$From=='PJM_Dom'&data$To=='PJM_West'] <- .01
	data$Cost[data$From=='PJM_Dom'&data$To=='S_VACA'] <- 6
	data$Cost[data$From=='PJM_EMAC'&data$To=='NY_Z_J'] <- 6
	data$Cost[data$From=='PJM_EMAC'&data$To=='NY_Z_K'] <- 6
	data$Cost[data$From=='PJM_EMAC'&data$To=='NY_Z_GI'] <- .01
	data$Cost[data$From=='PJM_EMAC'&data$To=='PJM_SMAC'] <- .01
	data$Cost[data$From=='PJM_EMAC'&data$To=='PJM_WMAC'] <- .01
	data$Cost[data$From=='PJM_PENE'&data$To=='NY_Z_AB'] <- 6
	data$Cost[data$From=='PJM_PENE'&data$To=='NY_Z_CE'] <- 6
	data$Cost[data$From=='PJM_PENE'&data$To=='PJM_AP'] <- .01
	data$Cost[data$From=='PJM_PENE'&data$To=='PJM_ATSI'] <- .01
	data$Cost[data$From=='PJM_PENE'&data$To=='PJM_WMAC'] <- .01
	data$Cost[data$From=='PJM_SMAC'&data$To=='PJM_AP'] <- .01
	data$Cost[data$From=='PJM_SMAC'&data$To=='PJM_Dom'] <- .01
	data$Cost[data$From=='PJM_SMAC'&data$To=='PJM_EMAC'] <- .01
	data$Cost[data$From=='PJM_SMAC'&data$To=='PJM_WMAC'] <- .01
	data$Cost[data$From=='PJM_West'&data$To=='MIS_IL'] <- 3
	data$Cost[data$From=='PJM_West'&data$To=='MIS_INKY'] <- 3
	data$Cost[data$From=='PJM_West'&data$To=='MIS_LMI'] <- 3
	data$Cost[data$From=='PJM_West'&data$To=='PJM_AP'] <- .01
	data$Cost[data$From=='PJM_West'&data$To=='PJM_ATSI'] <- .01
	data$Cost[data$From=='PJM_West'&data$To=='PJM_COMD'] <- .01
	data$Cost[data$From=='PJM_West'&data$To=='PJM_Dom'] <- .01
	data$Cost[data$From=='PJM_West'&data$To=='S_C_KY'] <- 6
	data$Cost[data$From=='PJM_West'&data$To=='S_C_TVA'] <- 6
	data$Cost[data$From=='PJM_West'&data$To=='S_VACA'] <- 6
	data$Cost[data$From=='PJM_WMAC'&data$To=='PJM_EMAC'] <- .01
	data$Cost[data$From=='PJM_WMAC'&data$To=='PJM_PENE'] <- .01
	data$Cost[data$From=='PJM_WMAC'&data$To=='PJM_SMAC'] <- .01
	data$Cost[data$From=='S_C_KY'&data$To=='MIS_INKY'] <- 6
	data$Cost[data$From=='S_C_KY'&data$To=='PJM_West'] <- 6
	data$Cost[data$From=='S_C_TVA'&data$To=='MIS_IL'] <- 6
	data$Cost[data$From=='S_C_TVA'&data$To=='MIS_INKY'] <- 6
	data$Cost[data$From=='S_C_TVA'&data$To=='PJM_West'] <- 6
	data$Cost[data$From=='S_C_TVA'&data$To=='S_D_N_AR'] <- 8
	data$Cost[data$From=='S_C_TVA'&data$To=='S_D_REST'] <- 8
	data$Cost[data$From=='S_C_TVA'&data$To=='S_SOU'] <- 8
	data$Cost[data$From=='S_C_TVA'&data$To=='S_VACA'] <- 8
	data$Cost[data$From=='S_D_AMSO'&data$To=='S_D_REST'] <- .01
	data$Cost[data$From=='S_D_AMSO'&data$To=='S_SOU'] <- 8
	data$Cost[data$From=='S_D_AMSO'&data$To=='SPP_SE'] <- 6
	data$Cost[data$From=='S_D_N_AR'&data$To=='MIS_IA'] <- 1
	data$Cost[data$From=='S_D_N_AR'&data$To=='MIS_MIDA'] <- 1
	data$Cost[data$From=='S_D_N_AR'&data$To=='MIS_MO'] <- 1
	data$Cost[data$From=='S_D_N_AR'&data$To=='S_C_TVA'] <- 8
	data$Cost[data$From=='S_D_N_AR'&data$To=='SPP_N'] <- 6
	data$Cost[data$From=='S_D_N_AR'&data$To=='SPP_WEST'] <- 6
	data$Cost[data$From=='S_D_REST'&data$To=='S_C_TVA'] <- 8
	data$Cost[data$From=='S_D_REST'&data$To=='S_D_AMSO'] <- .01
	data$Cost[data$From=='S_D_REST'&data$To=='S_D_WOTA'] <- .01
	data$Cost[data$From=='S_D_REST'&data$To=='S_SOU'] <- 8
	data$Cost[data$From=='S_D_REST'&data$To=='SPP_SE'] <- 6
	data$Cost[data$From=='S_D_REST'&data$To=='SPP_WEST'] <- 6
	data$Cost[data$From=='S_D_WOTA'&data$To=='S_D_REST'] <- .01
	data$Cost[data$From=='S_D_WOTA'&data$To=='SPP_SE'] <- 6
	data$Cost[data$From=='S_SOU'&data$To=='FRCC'] <- 8
	data$Cost[data$From=='S_SOU'&data$To=='S_C_TVA'] <- 8
	data$Cost[data$From=='S_SOU'&data$To=='S_D_AMSO'] <- 8
	data$Cost[data$From=='S_SOU'&data$To=='S_D_REST'] <- 8
	data$Cost[data$From=='S_SOU'&data$To=='S_VACA'] <- 8
	data$Cost[data$From=='S_VACA'&data$To=='PJM_Dom'] <- 6
	data$Cost[data$From=='S_VACA'&data$To=='PJM_West'] <- 6
	data$Cost[data$From=='S_VACA'&data$To=='S_C_TVA'] <- 8
	data$Cost[data$From=='S_VACA'&data$To=='S_SOU'] <- 8
	data$Cost[data$From=='SPP_KIAM'&data$To=='ERC_REST'] <- 6
	data$Cost[data$From=='SPP_KIAM'&data$To=='SPP_WEST'] <- .01
	data$Cost[data$From=='SPP_N'&data$To=='MIS_MIDA'] <- 6
	data$Cost[data$From=='SPP_N'&data$To=='MIS_MO'] <- 6
	data$Cost[data$From=='SPP_N'&data$To=='S_D_N_AR'] <- 6
	data$Cost[data$From=='SPP_N'&data$To=='SPP_NEBR'] <- .01
	data$Cost[data$From=='SPP_N'&data$To=='SPP_SPS'] <- .01
	data$Cost[data$From=='SPP_N'&data$To=='SPP_WEST'] <- .01
	data$Cost[data$From=='SPP_NEBR'&data$To=='MAP_WAUE'] <- 6
	data$Cost[data$From=='SPP_NEBR'&data$To=='MIS_WIDA'] <- 6
	data$Cost[data$From=='SPP_NEBR'&data$To=='SPP_N'] <- .01
	data$Cost[data$From=='SPP_SE'&data$To=='S_D_AMSO'] <- 6
	data$Cost[data$From=='SPP_SE'&data$To=='S_D_REST'] <- 6
	data$Cost[data$From=='SPP_SE'&data$To=='S_D_WOTA'] <- 6
	data$Cost[data$From=='SPP_SE'&data$To=='SPP_WEST'] <- .01
	data$Cost[data$From=='SPP_SPS'&data$To=='SPP_N'] <- .01
	data$Cost[data$From=='SPP_SPS'&data$To=='SPP_WEST'] <- .01
	data$Cost[data$From=='SPP_SPS'&data$To=='WECC_NM'] <- 6
	data$Cost[data$From=='SPP_WEST'&data$To=='ERC_REST'] <- 6
	data$Cost[data$From=='SPP_WEST'&data$To=='ERC_WEST'] <- 6
	data$Cost[data$From=='SPP_WEST'&data$To=='S_D_N_AR'] <- 6
	data$Cost[data$From=='SPP_WEST'&data$To=='S_D_REST'] <- 6
	data$Cost[data$From=='SPP_WEST'&data$To=='SPP_N'] <- .01
	data$Cost[data$From=='SPP_WEST'&data$To=='SPP_SE'] <- .01
	data$Cost[data$From=='SPP_WEST'&data$To=='SPP_SPS'] <- .01
	data$Cost[data$From=='WEC_CALN'&data$To=='WECC_NNV'] <- 8
	data$Cost[data$From=='WEC_CALN'&data$To=='WECC_PNW'] <- 8
	data$Cost[data$From=='WEC_CALN'&data$To=='WECC_SCE'] <- .01
	data$Cost[data$From=='WEC_CALN'&data$To=='WECC_SF'] <- .01
	data$Cost[data$From=='WEC_LADW'&data$To=='WECC_AZ'] <- 8
	data$Cost[data$From=='WEC_LADW'&data$To=='WECC_PNW'] <- 8
	data$Cost[data$From=='WEC_LADW'&data$To=='WECC_SCE'] <- 8
	data$Cost[data$From=='WEC_LADW'&data$To=='WECC_SNV'] <- 8
	data$Cost[data$From=='WEC_LADW'&data$To=='WECC_UT'] <- 8
	data$Cost[data$From=='WEC_SDGE'&data$To=='WECC_AZ'] <- 8
	data$Cost[data$From=='WEC_SDGE'&data$To=='WECC_IID'] <- 8
	data$Cost[data$From=='WEC_SDGE'&data$To=='WECC_SCE'] <- .01
	data$Cost[data$From=='WECC_AZ'&data$To=='WEC_LADW'] <- 8
	data$Cost[data$From=='WECC_AZ'&data$To=='WEC_SDGE'] <- 8
	data$Cost[data$From=='WECC_AZ'&data$To=='WECC_IID'] <- 8
	data$Cost[data$From=='WECC_AZ'&data$To=='WECC_NM'] <- .01
	data$Cost[data$From=='WECC_AZ'&data$To=='WECC_SCE'] <- 8
	data$Cost[data$From=='WECC_AZ'&data$To=='WECC_SNV'] <- .01
	data$Cost[data$From=='WECC_AZ'&data$To=='WECC_UT'] <- 8
	data$Cost[data$From=='WECC_CO'&data$To=='WECC_NM'] <- 8
	data$Cost[data$From=='WECC_CO'&data$To=='WECC_UT'] <- 8
	data$Cost[data$From=='WECC_CO'&data$To=='WECC_WY'] <- .01
	data$Cost[data$From=='WECC_ID'&data$To=='WECC_MT'] <- 8
	data$Cost[data$From=='WECC_ID'&data$To=='WECC_NNV'] <- .01
	data$Cost[data$From=='WECC_ID'&data$To=='WECC_PNW'] <- 8
	data$Cost[data$From=='WECC_ID'&data$To=='WECC_UT'] <- .01
	data$Cost[data$From=='WECC_ID'&data$To=='WECC_WY'] <- 8
	data$Cost[data$From=='WECC_IID'&data$To=='WEC_SDGE'] <- 8
	data$Cost[data$From=='WECC_IID'&data$To=='WECC_AZ'] <- 8
	data$Cost[data$From=='WECC_IID'&data$To=='WECC_SCE'] <- 8
	data$Cost[data$From=='WECC_MT'&data$To=='WECC_ID'] <- 8
	data$Cost[data$From=='WECC_MT'&data$To=='WECC_PNW'] <- .01
	data$Cost[data$From=='WECC_MT'&data$To=='WECC_WY'] <- 8
	data$Cost[data$From=='WECC_NM'&data$To=='SPP_SPS'] <- 6
	data$Cost[data$From=='WECC_NM'&data$To=='WECC_AZ'] <- .01
	data$Cost[data$From=='WECC_NM'&data$To=='WECC_CO'] <- 8
	data$Cost[data$From=='WECC_NM'&data$To=='WECC_UT'] <- 8
	data$Cost[data$From=='WECC_NNV'&data$To=='WEC_CALN'] <- 8
	data$Cost[data$From=='WECC_NNV'&data$To=='WECC_ID'] <- .01
	data$Cost[data$From=='WECC_NNV'&data$To=='WECC_PNW'] <- 8
	data$Cost[data$From=='WECC_NNV'&data$To=='WECC_UT'] <- .01
	data$Cost[data$From=='WECC_PNW'&data$To=='CN_BC'] <- 8
	data$Cost[data$From=='WECC_PNW'&data$To=='WEC_CALN'] <- 8
	data$Cost[data$From=='WECC_PNW'&data$To=='WEC_LADW'] <- 8
	data$Cost[data$From=='WECC_PNW'&data$To=='WECC_ID'] <- 8
	data$Cost[data$From=='WECC_PNW'&data$To=='WECC_MT'] <- .01
	data$Cost[data$From=='WECC_PNW'&data$To=='WECC_NNV'] <- 8
	data$Cost[data$From=='WECC_SCE'&data$To=='WEC_CALN'] <- .01
	data$Cost[data$From=='WECC_SCE'&data$To=='WEC_LADW'] <- 8
	data$Cost[data$From=='WECC_SCE'&data$To=='WEC_SDGE'] <- .01
	data$Cost[data$From=='WECC_SCE'&data$To=='WECC_AZ'] <- 8
	data$Cost[data$From=='WECC_SCE'&data$To=='WECC_IID'] <- 8
	data$Cost[data$From=='WECC_SCE'&data$To=='WECC_SNV'] <- 8
	data$Cost[data$From=='WECC_SF'&data$To=='WEC_CALN'] <- .01
	data$Cost[data$From=='WECC_SNV'&data$To=='WEC_LADW'] <- 8
	data$Cost[data$From=='WECC_SNV'&data$To=='WECC_AZ'] <- .01
	data$Cost[data$From=='WECC_SNV'&data$To=='WECC_SCE'] <- 8
	data$Cost[data$From=='WECC_SNV'&data$To=='WECC_UT'] <- 8
	data$Cost[data$From=='WECC_UT'&data$To=='WEC_LADW'] <- 8
	data$Cost[data$From=='WECC_UT'&data$To=='WECC_AZ'] <- 8
	data$Cost[data$From=='WECC_UT'&data$To=='WECC_CO'] <- 8
	data$Cost[data$From=='WECC_UT'&data$To=='WECC_ID'] <- .01
	data$Cost[data$From=='WECC_UT'&data$To=='WECC_NM'] <- 8
	data$Cost[data$From=='WECC_UT'&data$To=='WECC_NNV'] <- .01
	data$Cost[data$From=='WECC_UT'&data$To=='WECC_SNV'] <- 8
	data$Cost[data$From=='WECC_UT'&data$To=='WECC_WY'] <- 8
	data$Cost[data$From=='WECC_WY'&data$To=='WECC_CO'] <- .01
	data$Cost[data$From=='WECC_WY'&data$To=='WECC_ID'] <- 8
	data$Cost[data$From=='WECC_WY'&data$To=='WECC_MT'] <- 8
	data$Cost[data$From=='WECC_WY'&data$To=='WECC_UT'] <- 8
	return(data)
}

exportTransmissionTable <- function(data,scenarioFile,key) {
	transCap <- modifyCapacities(createPairs(data))
	transCost <- modifyCosts(createPairs(data))
	trans <- merge(x=transCap,y=transCost,by=c('From','To'))
	names(trans) <- c('From.Region','To.Region','Capacity','Cost')
	trans <- merge(x=trans,y=key,by.x='From.Region',by.y='RegionName')
	trans <- merge(x=trans,y=key,by.x='To.Region',by.y='RegionName')
	names(trans) <- c('From.Region','To.Region','Capacity','Cost','From','To')
	trans <- data.table(trans)
	trans <- trans[,list(Capacity=sum(Capacity),Cost=Cost*Capacity/sum(Capacity)),by=list(From,To)]
	trans <- trans[trans$From!=trans$To,]
	transCap <- trans[,list(From,To,Capacity)]
	transCost <- trans[,list(From,To,Cost)]
	write.table(transCap,paste(scenarioFile,'/inputs/transmissionConstraints.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	write.table(transCost,paste(scenarioFile,'/inputs/transmissionCosts.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
}

loadSolarData <- function(data) {
	solarLimits <- data.table(read.csv('data/solarDay.csv',header=TRUE))
	solarLimits <- solarLimits[,list(Hour01=mean(Hour01),Hour02=mean(Hour02),Hour03=mean(Hour03),Hour04=mean(Hour04),Hour05=mean(Hour05),Hour06=mean(Hour06),Hour07=mean(Hour07),Hour08=mean(Hour08),Hour09=mean(Hour09),Hour10=mean(Hour10),Hour11=mean(Hour11),Hour12=mean(Hour12),Hour13=mean(Hour13),Hour14=mean(Hour14),Hour15=mean(Hour15),Hour16=mean(Hour16),Hour17=mean(Hour17),Hour18=mean(Hour18),Hour19=mean(Hour19),Hour20=mean(Hour20),Hour21=mean(Hour21),Hour22=mean(Hour22),Hour23=mean(Hour23),Hour24=mean(Hour24)),by=list(Region.Name,Season)]
	forSolar <- data[data$FuelType=='Solar',]
	forSolar <- data.frame(forSolar[,list(Capacity=sum(Capacity)),by=list(RegionName,DIVISIONCE)])
	return(list(solarLimits,forSolar))
}

loadWindData <- function(data) {
	windLimits <- data.table(read.csv('data/windDay.csv',header=TRUE))
	windLimits <- windLimits[,list(Hour01=mean(Hour01),Hour02=mean(Hour02),Hour03=mean(Hour03),Hour04=mean(Hour04),Hour05=mean(Hour05),Hour06=mean(Hour06),Hour07=mean(Hour07),Hour08=mean(Hour08),Hour09=mean(Hour09),Hour10=mean(Hour10),Hour11=mean(Hour11),Hour12=mean(Hour12),Hour13=mean(Hour13),Hour14=mean(Hour14),Hour15=mean(Hour15),Hour16=mean(Hour16),Hour17=mean(Hour17),Hour18=mean(Hour18),Hour19=mean(Hour19),Hour20=mean(Hour20),Hour21=mean(Hour21),Hour22=mean(Hour22),Hour23=mean(Hour23),Hour24=mean(Hour24)),by=list(Region.Name,Season)]
	forWind <- data[data$FuelType=='Wind',]
	forWind <- data.frame(forWind[,list(Capacity=sum(Capacity)),by=list(RegionName,DIVISIONCE)])
	return(list(windLimits,forWind))
}

loadHydroData <- function(data,hold,key) {
	hydroLimits <- data.table(read.csv('data/hydroCapacities.csv',header=TRUE))
	hydroLimits <- melt(hydroLimits,id='Region')
	names(hydroLimits) <- c('Region.Name','Season','Hour01')
	hydroLimits <- merge(x=hydroLimits,y=key,by.x='Region.Name',by.y='RegionName')
	hydroLimits <- hydroLimits[,list(Hour01=mean(Hour01)),by=list(DIVISIONCE,Season)]
	hydroHold <- data.table(merge(x=hydroLimits,y=hold[hold$FuelType=='Hydro',],by='DIVISIONCE',all=TRUE,allow.cartesian=TRUE))
	hydroHold$Hour01 <- hydroHold$Hour01*hydroHold$Capacity
	hydroLimits <- hydroHold[,list(Region.Name=DIVISIONCE,Season=Season,Hour01=Hour01,Hour02=Hour01,Hour03=Hour01,Hour04=Hour01,Hour05=Hour01,Hour06=Hour01,Hour07=Hour01,Hour08=Hour01,Hour09=Hour01,Hour10=Hour01,Hour11=Hour01,Hour12=Hour01,Hour13=Hour01,Hour14=Hour01,Hour15=Hour01,Hour16=Hour01,Hour17=Hour01,Hour18=Hour01,Hour19=Hour01,Hour20=Hour01,Hour21=Hour01,Hour22=Hour01,Hour23=Hour01,Hour24=Hour01)]
	forHydro <- data[data$FuelType=='Hydro',]
	forHydro <- data.frame(forHydro[,list(Capacity=sum(Capacity)),by=list(DIVISIONCE)])
	forHydro$RegionName <- forHydro$DIVISIONCE
	return(list(hydroLimits,forHydro))
}

expandRenewTimePeriods <- function(input,inputDates) {
	hold <- input
	hours <- 1:24
	period <- paste(rep(inputDates,each=length(hours)),rep(hours,times=length(inputDates)),sep='_')
	output <- hold[rep(seq_len(nrow(hold)),length(inputDates)*length(hours)),]
	output$Period <- paste(2040,rep(period,each=nrow(hold)),sep='_')
	return(output)
}

expandLimitsTimePeriods <- function(input) {
	years <- c(2040)
	hold <- melt(input,id=c('Region.Name','Season'))
	colnames(hold) <- c('RegionName','Season','Hour','Limit')
	hold$Hour <- as.integer(gsub('Hour','',hold$Hour))
	springHold <- hold[hold$Season=='Winter',]
	summerHold <- hold[hold$Season=='Summer',]
	fallHold <- hold[hold$Season=='Summer',]
	winterHold <- hold[hold$Season=='Winter',]
	springDates <- 1:120
	summerDates <- 121:225
	fallDates <- 226:300
	winterDates <- 301:365
	days <- 1:365
	springHold <- springHold[rep(seq_len(nrow(springHold)),length(springDates)),]
	summerHold <- summerHold[rep(seq_len(nrow(summerHold)),length(summerDates)),]
	fallHold <- fallHold[rep(seq_len(nrow(fallHold)),length(fallDates)),]
	winterHold <- winterHold[rep(seq_len(nrow(winterHold)),length(winterDates)),]
	output <- rbind(springHold,summerHold,fallHold,winterHold)
	output <- output[rep(seq_len(nrow(output)),length(years)),]
	output$Period <- paste(rep(years,each=length(days)*24),rep(days,each=nrow(input)/2*24,times=length(years)),output$Hour,sep='_')
	return(output)
}

combineRenewableLimits <- function(limitInput,capacityInput,type,inputDates,scenarioFile) {
	limitHold <- expandLimitsTimePeriods(limitInput)
	capacityHold <- expandRenewTimePeriods(capacityInput,inputDates)
	hold <- merge(limitHold,capacityHold,by=c('RegionName','Period'))
	hold[is.na(hold)] <- 0
	hold$Max <- hold$Limit*hold$Capacity/1000+.1
	hold <- hold[,list(Max=sum(Max)),by=list(DIVISIONCE,Period)]
	output <- data.frame('Region'=hold$DIVISIONCE,'Period'=hold$Period,'MaxCap'=hold$Max)
	output$Period <- convertPeriodToHour(output$Period)
	if(type=='Solar') {
		write.table(output,paste(scenarioFile,'/inputs/maximumSolar.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	}
	else if(type=='Wind') {
		write.table(output,paste(scenarioFile,'/inputs/maximumWind.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	}
	else if(type=='Hydro') {
		write.table(output,paste(scenarioFile,'/inputs/maximumHydro.csv',sep=''),sep=',',row.names=FALSE,col.names=FALSE)
	}
}

prepLoadData <- function(load,inputDates,data,key) {
	regions <- unique(data.frame(data)[,'DIVISIONCE'])
	load <- load[load$Day%in%inputDates,]
	hold <- melt(load,id=c('Region','Month','Day'))
	colnames(hold) <- c('RegionName','Month','Day','Hour','Load')
	hold$Hour <- as.integer(gsub('Hour.','',hold$Hour))
	hold$Period <- paste(hold$Day,hold$Hour,sep='_')
	hold$Load <- as.numeric(gsub(',','',as.character(hold$Load)))
	hold <- data.table(merge(x=hold,y=key,by='RegionName'))
	hold <- hold[,list(Load=sum(Load)),by=list(DIVISIONCE,Period)]
	colnames(hold) <- c('Region','Period','Load')
	output <- cast(hold,Period~Region,value='Load')
	output[,-1] <- sapply(output[,-1],function(x) {as.numeric(gsub(',','',as.character(x)))})
	for(r in regions[!(regions%in%names(output))]) {
		output <- cbind(output,rep(0,nrow(output)))
		names(output)[ncol(output)] <- r
	}
	return(output)
}

expandLoadTimePeriods <- function(input,inputYear,scenarioFile) {
	output <- input[which(is.na(input$text)),]
	period <- paste(inputYear,input$Period,sep='_')
	load <- input[,-1]*(.99^(inputYear-2016))+.1
	hold <- cbind('Period'=period,load)
	output <- rbind(output,hold)
	h <- as.data.frame(do.call('rbind',strsplit(as.character(output$Period),'_')))
	h$V2 <- as.numeric(as.character(h$V2))
	h$V3 <- as.numeric(as.character(h$V3))
	output$Period <- (h$V2-1)*24+h$V3+3
	write.table(output,paste(scenarioFile,'/inputs/loadProfile.csv',sep=''),sep=',',row.names=FALSE,col.names=TRUE)
}

convertPeriodToHour <- function(input) {
	hold <- as.data.frame(do.call('rbind',strsplit(as.character(input),'_')))
	output <- (as.numeric(as.character(hold$V2))-1)*24+as.numeric(as.character(hold$V3))+3
	return(output)
}

generateAllInputFiles <- function(inputDates,scenarioFile) {
	data <- read.csv('data/parsedGenerators2.csv',header=TRUE)
	data <- cleanData(data)
	data <- data[data$DIVISIONCE!='',]
	divToRegion <- unique(data[,list(RegionName,DIVISIONCE)])
	hold <- data[,list(Capacity=sum(Capacity)),by=list(DIVISIONCE,FuelType,FuelCostTotal,PLCO2RTA)]
	hold <- hold[hold$PLCO2RTA<100000,]
	hold$UnitId <- 1:nrow(hold)
	hold <- hold[complete.cases(hold)]
	solarHold <- loadSolarData(data)
	windHold <- loadWindData(data)
	hydroHold <- loadHydroData(data,hold,divToRegion)
	load <- read.csv('data/loadData.csv',header=TRUE)
	load$Region <- gsub('\\-','',load$Region)
	load$Region <- gsub('\\&','',load$Region)

	genToRegionSet(hold,scenarioFile)
	exportTransmissionTable(data,scenarioFile,divToRegion)
	exportTable(reformData(hold,'Capacity'),'generationCapacities',scenarioFile)
	exportTable(reformData(hold,'FuelCostTotal'),'generationCosts',scenarioFile)
	exportTable(reformData(hold,'PLCO2RTA'),'generationCO2',scenarioFile)
	exportParTable(hold,inputDates,2040,scenarioFile)
	combineRenewableLimits(solarHold[[1]],solarHold[[2]],'Solar',inputDates,scenarioFile)
	combineRenewableLimits(windHold[[1]],windHold[[2]],'Wind',inputDates,scenarioFile)
	combineRenewableLimits(hydroHold[[1]],hydroHold[[2]],'Hydro',inputDates,scenarioFile)
	expandLoadTimePeriods(prepLoadData(load,inputDates,hold,divToRegion),2040,scenarioFile)
}
