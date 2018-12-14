
#### Common ####
days <- c(1:9,91:100)
year <- 2040
discount.rate <- 0.05

#### Mobility ####
battery.capital.cost <- 150 # $/kWh
charger.levels <- c(10,20,50,100,250) # kW
sharing.factor <- 1.5
scale.urban.form.factor <- 1.0
include.transit.demand <- F

#### Grid ####
generators <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_generators.csv')))
load <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_load.csv')))
transmission <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_transmission.csv')))
renewableCF <- data.table(read.csv(pp(gem.raw.inputs,'gem_gridInputs_renewableCF.csv')))