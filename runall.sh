#!/bin/bash

#./src/gem.R input/experiments/carbonTax.yaml
#./src/gem.R input/experiments/congestion.yaml
#./src/gem.R input/experiments/fractionSAEVs.yaml
#./src/gem.R input/experiments/l10ChargerCost.yaml
#./src/gem.R input/experiments/batteryCapitalCost.yaml
#./src/gem.R input/experiments/chargerCostSuperlinear.yaml
#./src/gem.R input/experiments/discountRate.yaml
#./src/gem.R input/experiments/fractionSmartCharging.yaml
#./src/gem.R input/experiments/renewableScalingFactor.yaml

./src/gem.R -d -t --experiment=input/experiments/vehicleCapitalCost.yaml
./src/gem.R -d -t --experiment=input/experiments/batteryLifetime.yaml
./src/gem.R -d -t --experiment=input/experiments/b150ConversionEfficiency.yaml
./src/gem.R -d -t --experiment=input/experiments/sharingFactor.yaml
./src/gem.R -d -t --experiment=input/experiments/batteryCapitalCost.yaml
