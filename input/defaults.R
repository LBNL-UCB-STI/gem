
#### Common ####

# days <- c(1:3) 
#days <- c(74:81,147:154,260:267,351:358) # 8 per week
days <- c(74:77,79,148:152,260:264,351:355) # 5 per week, 4 weekday, 1 weekend
#days <- c(148:152) 
group.days <- 0 # set this to 0 to run all "days" at once, set to non-zero to run "grouped.days" at a time over the full range of "days" overlap occurs for one day on end-points

year <- 2040
discountRate <- 0.05

#### Mobility ####
battery.capital.cost <- 150 # $/kWh
charger.levels <- c(10,20,50,100,250) # kW
chargerLifetime <- 10
sharingFactor <- 1.5
batteryCapitalCost <- 150
scale.urban.form.factor <- 1.0
includeTransitDemand <- 0 # 0 or 1
fractionSAEVs <- 0
fractionSmartCharging <- 0
privateFleetWeights <- "fleet_weights_dev" # name (without extension) of file in {gem.raw.inputs}/NREL-EVI-Pro-Preprocessed-Profiles/data
congestion <- 'Freeflow'
l10ChargerCost <- 500 # $/kW
chargerCostSuperlinear <- 3 # rate of increase beyond linear from low to high power chargers

#### Grid ####
generators <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_generators.csv')))
load <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_load.csv')))
transmission <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_transmission.csv')))
renewableCF <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_renewableCF.csv')))

fuels <- data.frame('FuelType'=c('Wind','Waste Coal','Tire','Solar','Pumps','Pet. Coke','Oil','Nuclear','None','Non-Fossil','NaturalGas','MSW','LF Gas','Hydro','Geothermal','Fwaste','Coal','Biomass'),'Simplified'=c('Wind','Coal','Other','Solar','Pumps','Other','Other','Nuclear','Other','Other','NaturalGas','Biomass','NaturalGas','Hydro','Geothermal','Biomass','Coal','Biomass'))
meritOrder <- c('Solar','Wind','Geothermal','Other','Hydro','NaturalGas','Pumps','Biomass','Coal','Nuclear')
