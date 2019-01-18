
#### Common ####
days <- c(1:3)
year <- 2040
discountRate <- 0.05

#### Mobility ####
battery.capital.cost <- 150 # $/kWh
charger.levels <- c(10,20,50,100,250) # kW
sharingFactor <- 1.5
batteryCost <- 150
scale.urban.form.factor <- 1.0
include.transit.demand <- F
fraction.mobility.served.by.saevs <- .99

#### Grid ####
generators <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_generators.csv')))
load <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_load.csv')))
transmission <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_transmission.csv')))
renewableCF <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_renewableCF.csv')))
