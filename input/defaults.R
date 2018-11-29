
#### Common ####
days <- 1:2
discount.rate <- 0.05

#### Mobility ####
battery.capital.cost <- 150 # $/kWh
charger.levels <- c(10,20,50,100,250) # kW
sharing.factor <- 1.5
scale.urban.form.factor <- 1.0
dist.bins <- c(1.052,2.606,6.027,8.529,14.092,24.116,35.535,44.148,73.726,160.384)
dist.bin.labels <- pp('d',roundC(dist.bins,1))
num.dist.bins <- length(dist.bins)

#### Grid ####
