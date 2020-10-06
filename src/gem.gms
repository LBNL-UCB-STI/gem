$title gem

$offlisting
options
	limrow = 0,
	limcol = 0,
	solprint = off,
	sysout = off,
	profile = 1
;

set
	t				Time period
	b				Battery capacity /b075, b150, b225, b300, b400/
	d				Travel distance 
	l				Charger level 
	r				Region
	rmob			Region (mobility)
	g				Generators
	solar(g)		Solar generators
	wind(g)			Wind generators
	hydro(g)		Hydro generators
	gtor(g,r) 		Generator to region mapping
	rmobtor(r,rmob) 		Region mobility to region mapping

	tb                                truck battery capacity /tb500,tb800,tb1000,tb1500,tb2000/
	td                                truck travel distance
	tl                                truck charger level
;

alias (t,tp);
alias (r,o,p);

parameters
	demand(t,d,rmob)			Demand by distance type time and region
	chargerPower(l)				kW per charger 
	chargeRelocationRatio(rmob)		increase in energy consumption due to charging relocation
    chargeRelocationCorrection(b,l,rmob)	decrease in charger power due to relocation
	fleetRatio(rmob)			ratio of optimal to actual fleet size
	batteryRatio(rmob)			ratio of optimal to actual battery range
    distCorrection(rmob)			one + distance dead head ratio
	timeCorrection(rmob)			one + time dead head ratio
	sharingFactor				Avg person per vehicle trip 
	chargerDistributionFactor(l)		increased chargers needed to serve vehs 
************* We original varied from 0.262-0.310 per ES&T paper, but changed to center around 0.325 to match EVI-Pro assumptiosn ***************
*	kwh per mile /	b075  b150  b225  b300	b400
*	TRB Paper 2019	0.262 0.274 0.286 0.298	0.310 /									   
*	2nd Paper 2020	0.31  0.324 0.338 0.351 0.353
	conversionEfficiency(b) 		kwh per mile 									   
	travelDistance(d,rmob)				avg miles per passenger 
	speed(t,d,rmob)
	demandCharge(rmob) 				USD per kW month
	personalEVChargeEnergyLB(t,rmob)	  Lower boundary for cumulative energy delivered to personal evs
	personalEVChargeEnergyUB(t,rmob)          Upper boundary for cumulative energy delivered to personal evs
	personalEVChargePowerLB(t,rmob)           Lower boundary for power delivered to personal evs
	personalEVChargePowerUB(t,rmob)           Upper boundary for power delivered to personal evs
	discountRate				  rate
	chargerCapitalCost(l)			  cost per kW 	
	chargerLifetime				  years
	vehicleLifetime(b,rmob)			  years
	batteryLifetime(b,rmob)			  years
	batteryCapitalCost			  USD per kWh 
	batteryCapacity(b)			  avg per veh in kWh /b075 19.65
													b150 41.10
													b225 64.35
													b300 89.40
													b400 124.0 /
	genCost(g)						Marginal cost of power generation
	demandLoad(r,t)					Baseload electricity demand
	maxGen(g)						Maximum generating capacity by generator
	maxSolar(r,t)					Regional solar limits
	maxWind(r,t)					Regional wind limits
*	#maxHydro(r,t)					Regional hydro limits
	transCap(r,o)					Maximum transmission capacity between two regions
	transCost(r,o)					Wheeling costs for transmitting power



	truckdemand(t,td,rmob)			Demand by distance type time and region
	truckchargerPower(tl)				kW per charger 
	truckchargeRelocationRatio(rmob)		increase in energy consumption due to charging relocation
    truckchargeRelocationCorrection(tb,tl,rmob)	decrease in charger power due to relocation
	truckfleetRatio(rmob)			ratio of optimal to actual fleet size
	truckbatteryRatio(rmob)			ratio of optimal to actual battery range
    truckdistCorrection(rmob)			one + distance dead head ratio
	trucktimeCorrection(rmob)			one + time dead head ratio
*	sharingFactor				Avg person per vehicle trip 
	truckchargerDistributionFactor(tl)		increased chargers needed to serve vehs 
************* We original varied from 0.262-0.310 per ES&T paper, but changed to center around 0.325 to match EVI-Pro assumptiosn ***************
*	kwh per mile /	b075  b150  b225  b300	b400
*	TRB Paper 2019	0.262 0.274 0.286 0.298	0.310 /									   
*	2nd Paper 2020	0.31  0.324 0.338 0.351 0.353
	truckconversionEfficiency(tb) 		kwh per mile 									   
	trucktravelDistance(td,rmob)				avg miles per passenger 
	tspeed(t,td,rmob)
	truckdemandCharge(rmob) 				USD per kW month

	truckchargerCapitalCost(tl)			  cost per kW 	
	truckvehicleLifetime(tb,rmob)			  years
	truckbatteryLifetime(tb,rmob)			  years

        truckbatteryCapacity(tb)                    /tb500 154.61
                                                    tb800 251.15
                                                    tb1000 315.51
                                                    tb1500 476.41
                                                    tb2000 637.31/
        truckbatteryCapitalCost                     /150/
;

scalar
	deltaT				time step size in hours /1/
	dailyDiscountRate 		rate /0/
	chargerVariableCost		O&M cost per kW per day /0.04/
	vehicleCapitalCost 		USD 
	vehiclePerYearCosts		USD for insurance /600/
	vehiclePerMileCosts		USD for insurance & maint /0.09/
	dailyVehicleCost 		amortized cap /0/
	dailyBatteryCost 		amortized cap per kWh /0/
	transLoss			Transmission efficiency /.972/

	truckchargerVariableCost		O&M cost per kW per day /0.08/
	truckvehicleCapitalCost 		USD /70000/
	truckvehiclePerYearCosts		USD for insurance /1000/
	truckvehiclePerMileCosts		USD for insurance & maint /0.18/
	truckdailyVehicleCost 		amortized cap /0/
	truckdailyBatteryCost 		amortized cap per kWh /0/
	trucksharingFactor               /1/
	truckchargerLifetime             /10/

;


variable
	systemCost 			System Cost
;

positive variable
	energyCharged(t,b,l,rmob) 		Nominal energy charged kWh (before adjustment by chargeRelocationCorrection)
	energyConsumed(t,b,d,rmob) 		Energy consumed kWh
	demandChargeCost(t,rmob) 		Cost USD
	vehicleMaintCost(t,rmob) 		Cost USD
	maxDemand(rmob) 				Power kW
	fleetSize(b,rmob) 				Total no. in fleet
	vehiclesCharging(t,b,l,rmob) 	No. charging
	vehiclesMoving(t,b,d,rmob) 		No. serving mobility
	vehiclesIdle(t,b,rmob)			No. parked
	numChargers(l,rmob) 			Total no. infrastructure
	infrastructureCost(rmob) 		Total daily infrastructure cost
	fleetCost(rmob) 				Total daily fleet cost
	demandAllocated(t,b,d,rmob) 	Demand as allocated by battery type
	generation(g,t)					Power generation by each generator
	trans(r,t,o)					Power transmission between regions
  	personalEVPower(t,rmob)         Power profile of private vehicles


        truckenergyCharged(t,tb,tl,rmob) 		Nominal energy charged kWh (before adjustment by chargeRelocationCorrection)
	truckenergyConsumed(t,tb,td,rmob) 		Energy consumed kWh
	truckdemandChargeCost(t,rmob) 		Cost USD
	truckvehicleMaintCost(t,rmob) 		Cost USD
	truckfleetSize(tb,rmob) 				Total no. in fleet
	truckvehiclesCharging(t,tb,tl,rmob) 	No. charging
	truckvehiclesMoving(t,tb,td,rmob) 		No. serving mobility
	truckvehiclesIdle(t,tb,rmob)			No. parked
	trucknumChargers(tl,rmob) 			Total no. infrastructure
	truckinfrastructureCost(rmob) 		Total daily infrastructure cost
	truckfleetCost(rmob) 				Total daily fleet cost
	truckdemandAllocated(t,tb,td,rmob) 	Demand as allocated by battery type

;


$gdxin <<gdxName>>
$load d r rmob l t td tl g gtor rmobtor demand speed sharingFactor travelDistance demandCharge chargerPower chargerCapitalCost chargerDistributionFactor conversionEfficiency solar wind hydro genCost demandLoad maxGen maxSolar maxWind transCap transCost personalEVChargeEnergyLB personalEVChargeEnergyUB personalEVChargePowerLB personalEVChargePowerUB distCorrection timeCorrection chargeRelocationRatio chargeRelocationCorrection fleetRatio batteryRatio vehicleLifetime batteryLifetime batteryCapitalCost vehicleCapitalCost discountRate chargerLifetime truckdemand truckchargerPower truckchargeRelocationRatio truckchargeRelocationCorrection truckfleetRatio truckbatteryRatio truckdistCorrection trucktimeCorrection truckchargerDistributionFactor truckconversionEfficiency trucktravelDistance tspeed truckdemandCharge truckchargerCapitalCost truckvehicleLifetime truckbatteryLifetime
$gdxin

display
	chargeRelocationRatio;

dailyDiscountRate = ((1 + discountRate)**(1/365)) - 1;

*Variable limits
	generation.up(g,t) = maxGen(g);
	generation.lo(g,t) = 0;
	trans.up(r,t,o) = transCap(r,o);
	trans.lo(r,t,o) = 0;


equations
	obj 				Objective Function
	cDemandChargeCost 		Cost equality
	cVehicleMaintCost 		Cost equality
	cDemandAllocation 		Our allocated demand must meet the exogenous value
	cEnergyToMeetDemand		Mobility demand function
	cChargingUpperBound		Cannot charge more than has been consumed
	cChargingLowerBound		Cannot consume more than bat cap of fleet must charge to keep up
	cNoChargeAtStart 		First hour no charging
	cTerminalSOC			End the day with same energy in batteries as beginning
	cNumCharging 			Num veh charging proportional to energy delivered
	cMaxCharging 			Charging infrastructure limit
	cNumMoving 			Vehicles to serve demand
	cFleetDispatch 			Fleet dispatch
	cInfrastructureCost		Infrastructure cost
	cFleetCost			Fleet cost
	cMaxDemand 			Inequality to capture max demand for a day
	cGeneration			Generation must equal load
	cMaxSolar			Solar generation cannot exceed sun supply
	cMaxWind			Wind generation cannot exceed wind supply
*	#constraint4			Hydro generation cannot exceed capacity factor
	cPersonalEVChargeEnergyLB	Energy boundaries and power boundaries
	cPersonalEVChargeEnergyUB
	cPersonalEVChargePowerLB
	cPersonalEVChargePowerUB


	cTruckDemandAllocation 		Our allocated demand must meet the exogenous value
	cTruckEnergyToMeetDemand		Mobility demand function
	cTruckChargingUpperBound		Cannot charge more than has been consumed
	cTruckChargingLowerBound		Cannot consume more than bat cap of fleet must charge to keep up
	cTruckNoChargeAtStart 		First hour no charging
	cTruckTerminalSOC			End the day with same energy in batteries as beginning
	cTruckNumCharging 			Num veh charging proportional to energy delivered
	cTruckMaxCharging 			Charging infrastructure limit
	cTruckNumMoving 			Vehicles to serve demand
	cTruckFleetDispatch 			Fleet dispatch	
;


obj..
	systemCost =e= sum(rmob,sum(t,demandChargeCost(t,rmob)+vehicleMaintCost(t,rmob))+card(t)/24*infrastructureCost(rmob)+card(t)/24*fleetCost(rmob))+sum((g,t),generation(g,t)*genCost(g))+sum((r,t,o),trans(r,t,o)*transCost(r,o));

cDemandChargeCost(t,rmob)..
	demandChargeCost(t,rmob) - maxDemand(rmob)*demandCharge(rmob)/30.4/24 =e= 0;

cVehicleMaintCost(t,rmob)..
	vehicleMaintCost(t,rmob) - vehiclePerMileCosts*sum((b,d),vehiclesMoving(t,b,d,rmob)*speed(t,d,rmob)) - truckvehiclePerMileCosts*sum((tb,td),truckvehiclesMoving(t,tb,td,rmob)*tspeed(t,td,rmob)) =e= 0;

cInfrastructureCost(rmob)..
	infrastructureCost(rmob) - sum(l,numChargers(l,rmob)*(chargerCapitalCost(l) * dailyDiscountRate * (1 + dailyDiscountRate)**(chargerLifetime*365) / ((1 +  dailyDiscountRate)**(chargerLifetime*365) - 1))*chargerDistributionFactor(l)*chargerPower(l)) - sum(tl,trucknumChargers(tl,rmob)*(truckchargerCapitalCost(tl) * dailyDiscountRate * (1 + dailyDiscountRate)**(truckchargerLifetime*365) / ((1 +  dailyDiscountRate)**(truckchargerLifetime*365) - 1))*truckchargerDistributionFactor(tl)*truckchargerPower(tl)) =e= 0;

cFleetCost(rmob)..
        fleetCost(rmob) - sum(b,fleetSize(b,rmob) * fleetRatio(rmob) * (vehiclePerYearCosts / 365 + vehicleCapitalCost * dailyDiscountRate * (1 + dailyDiscountRate)**(vehicleLifetime(b,rmob)*365) / ((1 +  dailyDiscountRate)**(vehicleLifetime(b,rmob)*365) - 1) + batteryRatio(rmob) * batteryCapacity(b) * batteryCapitalCost * dailyDiscountRate * (1 + dailyDiscountRate)**(batteryLifetime(b,rmob)*365) / ((1 +  dailyDiscountRate)**(batteryLifetime(b,rmob)*365) - 1)))  - sum(tb,truckfleetSize(tb,rmob) * truckfleetRatio(rmob) * (truckvehiclePerYearCosts / 365 + truckvehicleCapitalCost * dailyDiscountRate * (1 + dailyDiscountRate)**(truckvehicleLifetime(tb,rmob)*365) / ((1 +  dailyDiscountRate)**(truckvehicleLifetime(tb,rmob)*365) - 1) + truckbatteryRatio(rmob) * truckbatteryCapacity(tb) * truckbatteryCapitalCost * dailyDiscountRate * (1 + dailyDiscountRate)**(truckbatteryLifetime(tb,rmob)*365) / ((1 +  dailyDiscountRate)**(truckbatteryLifetime(tb,rmob)*365) - 1))) =e= 0;

cDemandAllocation(t,d,rmob)..
	demand(t,d,rmob) - sum(b,demandAllocated(t,b,d,rmob)) =e= 0;
	
cTruckDemandAllocation(t,td,rmob)..
	truckdemand(t,td,rmob) - sum(tb,truckdemandAllocated(t,tb,td,rmob)) =e= 0;

cEnergyToMeetDemand(t,b,d,rmob)..
	energyConsumed(t,b,d,rmob) / chargeRelocationRatio(rmob) * sharingFactor / (distCorrection(rmob) * conversionEfficiency(b) * travelDistance(d,rmob)) - demandAllocated(t,b,d,rmob) =e= 0;

cTruckEnergyToMeetDemand(t,tb,td,rmob)..
	truckenergyConsumed(t,tb,td,rmob) / truckchargeRelocationRatio(rmob) * trucksharingFactor / (truckdistCorrection(rmob) * truckconversionEfficiency(tb) * trucktravelDistance(td,rmob)) - truckdemandAllocated(t,tb,td,rmob) =e= 0;

cNumMoving(t,b,d,rmob)..git
	demandAllocated(t,b,d,rmob) * travelDistance(d,rmob) * timeCorrection(rmob) - vehiclesMoving(t,b,d,rmob) * sharingFactor * deltaT * speed(t,d,rmob) =e= 0;

cTruckNumMoving(t,tb,td,rmob)..
	truckdemandAllocated(t,tb,td,rmob) * trucktravelDistance(td,rmob) * trucktimeCorrection(rmob) - truckvehiclesMoving(t,tb,td,rmob) * trucksharingFactor * deltaT * tspeed(t,td,rmob) =e= 0;

cNumCharging(t,b,l,rmob)..
	energyCharged(t,b,l,rmob) / (chargerPower(l)*chargeRelocationCorrection(b,l,rmob)) - vehiclesCharging(t,b,l,rmob) =e= 0;

cTruckNumCharging(t,tb,tl,rmob)..
	truckenergyCharged(t,tb,tl,rmob) / (truckchargerPower(tl)*truckchargeRelocationCorrection(tb,tl,rmob)) - truckvehiclesCharging(t,tb,tl,rmob) =e= 0;

cChargingUpperBound(t,b,rmob)..
	sum(tp$(ord(tp) le ord(t)),sum(d,energyConsumed(tp,b,d,rmob)))-sum(tp$(ord(tp) le ord(t)),sum(l,energyCharged(tp,b,l,rmob))) =g= 0;

cChargingLowerBound(t,b,rmob)..
	fleetSize(b,rmob) * batteryCapacity(b) - sum(tp$(ord(tp) le ord(t)),sum(d,energyConsumed(tp,b,d,rmob)))+sum(tp$(ord(tp) lt ord(t)),sum(l,energyCharged(tp,b,l,rmob))) =g= 0;

cTruckChargingUpperBound(t,tb,rmob)..
	sum(tp$(ord(tp) le ord(t)),sum(td,truckenergyConsumed(tp,tb,td,rmob)))-sum(tp$(ord(tp) le ord(t)),sum(tl,truckenergyCharged(tp,tb,tl,rmob))) =g= 0;

cTruckChargingLowerBound(t,tb,rmob)..
	truckfleetSize(tb,rmob) * truckbatteryCapacity(tb) - sum(tp$(ord(tp) le ord(t)),sum(td,truckenergyConsumed(tp,tb,td,rmob)))+sum(tp$(ord(tp) lt ord(t)),sum(tl,truckenergyCharged(tp,tb,tl,rmob))) =g= 0;

cNoChargeAtStart(b,l,rmob)..
	sum(t$(ord(t) eq 1),energyCharged(t,b,l,rmob)) =e= 0;
	
cTruckNoChargeAtStart(tb,tl,rmob)..
	sum(t$(ord(t) eq 1),truckenergyCharged(t,tb,tl,rmob)) =e= 0;

cTerminalSOC(b,rmob)..
	sum(t,sum(d,energyConsumed(t,b,d,rmob)))-sum(t,sum(l,energyCharged(t,b,l,rmob))) =e= 0;

cTruckTerminalSOC(tb,rmob)..
	sum(t,sum(td,truckenergyConsumed(t,tb,td,rmob)))-sum(t,sum(tl,truckenergyCharged(t,tb,tl,rmob))) =e= 0;

cFleetDispatch(t,b,rmob)..
	fleetSize(b,rmob) - sum(l, vehiclesCharging(t,b,l,rmob)) - sum(d,vehiclesMoving(t,b,d,rmob)) - vehiclesIdle(t,b,rmob)  =e= 0;

cTruckFleetDispatch(t,tb,rmob)..
	truckfleetSize(tb,rmob) - sum(tl, truckvehiclesCharging(t,tb,tl,rmob)) - sum(td,truckvehiclesMoving(t,tb,td,rmob)) - truckvehiclesIdle(t,tb,rmob)  =e= 0;

cMaxCharging(t,l,rmob)..
	numChargers(l,rmob) - sum(b,vehiclesCharging(t,b,l,rmob)) =g= 0;

cTruckMaxCharging(t,tl,rmob)..
	trucknumChargers(tl,rmob) - sum(tb,truckvehiclesCharging(t,tb,tl,rmob)) =g= 0;

cMaxDemand(t,rmob)..
	maxDemand(rmob) - sum((b,l),energyCharged(t,b,l,rmob)) / deltaT -  sum((tb,tl),truckenergyCharged(t,tb,tl,rmob)) / deltaT - personalEVPower(t,rmob)/deltaT =g= 0;

cGeneration(t,r)..
	sum(g$gtor(g,r),generation(g,t))+(sum(o,trans(o,t,r))*transLoss-sum(p,trans(r,t,p)))-demandLoad(r,t)-sum(rmob$rmobtor(r,rmob),personalEVPower(t,rmob)/1000)-sum((b,l),sum(rmob$rmobtor(r,rmob),energyCharged(t,b,l,rmob)/1000)) - sum((tb,tl),sum(rmob$rmobtor(r,rmob),truckenergyCharged(t,tb,tl,rmob)/1000)) =g= 0;

cMaxSolar(t,r)..
	maxSolar(r,t)-sum(solar$gtor(solar,r),generation(solar,t)) =g= 0;

cMaxWind(t,r)..
	maxWind(r,t)-sum(wind$gtor(wind,r),generation(wind,t)) =g= 0;

*cMaxHydro(t,r)..
*	maxHydro(r,t)-sum(hydro$gtor(hydro,r),generation(hydro,t)) =g= 0;

cPersonalEVChargePowerLB(t,rmob)..
	personalEVPower(t,rmob) - personalEVChargePowerLB(t,rmob) =g= 0;

cPersonalEVChargePowerUB(t,rmob)..
	personalEVPower(t,rmob) - personalEVChargePowerUB(t,rmob) =l= 0;

cPersonalEVChargeEnergyLB(t,rmob)..
	sum(tp$(ord(tp) le ord(t)), personalEVPower(tp,rmob))  =g= personalEVChargeEnergyLB(t,rmob);

cPersonalEVChargeEnergyUB(t,rmob)..
	sum(tp$(ord(tp) le ord(t)), personalEVPower(tp,rmob))  =l= personalEVChargeEnergyUB(t,rmob);



model
	combinedModel /obj,cDemandAllocation,cDemandChargeCost,cVehicleMaintCost,cEnergyToMeetDemand,cChargingUpperBound,cChargingLowerBound,cNoChargeAtStart,cTerminalSOC,cNumCharging,cMaxCharging,cNumMoving,cFleetDispatch,cInfrastructureCost,cFleetCost,cMaxDemand,cGeneration,cMaxSolar,cMaxWind,cPersonalEVChargeEnergyLB,cPersonalEVChargeEnergyUB,cPersonalEVChargePowerLB,cPersonalEVChargePowerUB,cTruckDemandAllocation,cTruckEnergyToMeetDemand,cTruckChargingUpperBound,cTruckChargingLowerBound,cTruckNoChargeAtStart,cTruckTerminalSOC,cTruckNumCharging,cTruckMaxCharging,cTruckNumMoving,cTruckFleetDispatch /
*	combinedModel /obj,cDemandAllocation,cDemandChargeCost,cVehicleMaintCost,cEnergyToMeetDemand,cChargingUpperBound,cChargingLowerBound,cNoChargeAtStart,cTerminalSOC,cNumCharging,cMaxCharging,cNumMoving,cFleetDispatch,cInfrastructureCost,cFleetCost,cMaxDemand,cGeneration,cMaxSolar,cMaxWind,cPersonalEVChargeEnergyLB,cPersonalEVChargeEnergyUB,cPersonalEVChargePowerLB,cPersonalEVChargePowerUB/

options
	qcp = cplex
	solvelink = 2
	reslim = 500000
;

$onecho > cplex.opt
threads = 16
barepcomp = 1e-002
barqcpepcomp = 1e-001
* baralg = 1
* scaind = -1
$offecho
combinedModel.optFile = 1;
combinedModel.holdfixed = 1;

solve
	combinedModel
	using qcp
	minimizing systemCost
;

Execute_Unload "results.gdx";
