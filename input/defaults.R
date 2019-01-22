
#### Common ####
days <- c(1)
year <- 2040
discountRate <- 0.05

#### Mobility ####
battery.capital.cost <- 150 # $/kWh
charger.levels <- c(10,20,50,100,250) # kW
chargerLifetime <- 10
sharingFactor <- 1.5
batteryCapitalCost <- 150
scale.urban.form.factor <- 1.0
include.transit.demand <- F
fractionSAEVs <- 1
congestion <- 'Freeflow'
l10ChargerCost <- 500 # $/kW
chargerCostSuperlinear <- 5 # rate of increase beyond linear from low to high power chargers

#### Grid ####
generators <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_generators.csv')))
load <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_load.csv')))
transmission <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_transmission.csv')))
renewableCF <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_renewableCF.csv')))
