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
;

alias (t,tp);
alias (r,o,p);

parameters
	demand(t,d,rmob)			Demand by distance type, time, and region
	chargerPower(l)				kW per charger 
	chargeReloc(rmob)			increase in energy consumption due to charging relocation
        chargeEff(b,l,rmob)			decrease in charger power due to relocation
	fleetRatio(rmob)			ratio of optimal to actual fleet size
	batteryRatio(rmob)			ratio of optimal to actual battery range
        distCorrection(rmob)			one + distance dead head ratio
	timeCorrection(rmob)			one + time dead head ratio
	sharingFactor				Avg person per vehicle trip 
	urbanFormFactor(rmob)			one + dead head ratio
	chargerDistributionFactor(l)		increased chargers needed to serve vehs 
	conversionEfficiency(b) 		kwh per mile / b075 0.262
												   b150 0.274
												   b225 0.286
												   b300 0.298
												   b400 0.310 /									   
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
;

scalar
	deltaT				time step size in hours /1/
	dailyDiscountRate 		rate /0/
	chargerVariableCost		O&M cost per kW per day /0.04/
	vehicleCapitalCost 		USD /30000/
	vehiclePerYearCosts		USD for insurance /600/
	vehiclePerMileCosts		USD for insurance & maint /0.09/
	dailyVehicleCost 		amortized cap /0/
	dailyBatteryCost 		amortized cap per kWh /0/
	transLoss			Transmission efficiency /.972/
;


variable
	systemCost 			System Cost
;

positive variable
	generation(g,t)					Power generation by each generator
	trans(r,t,o)					Power transmission between regions
;


$gdxin <<gdxName>>
$load d r rmob l t g gtor rmobtor demand speed sharingFactor urbanFormFactor travelDistance demandCharge chargerPower chargerCapitalCost chargerDistributionFactor solar wind hydro genCost demandLoad maxGen maxSolar maxWind transCap transCost personalEVChargeEnergyLB personalEVChargeEnergyUB personalEVChargePowerLB personalEVChargePowerUB distCorrection timeCorrection chargeReloc chargeEff fleetRatio batteryRatio vehicleLifetime batteryLifetime batteryCapitalCost discountRate chargerLifetime
$gdxin

display
	demandLoad
	;

dailyDiscountRate = ((1 + discountRate)**(1/365)) - 1;

*Variable limits
	generation.up(g,t) = maxGen(g);
	generation.lo(g,t) = 0;
	trans.up(r,t,o) = transCap(r,o);
	trans.lo(r,t,o) = 0;


equations
	obj 				Objective Function
	cGeneration			Generation must equal load
	cMaxSolar			Solar generation cannot exceed sun supply
	cMaxWind			Wind generation cannot exceed wind supply
*	#constraint4			Hydro generation cannot exceed capacity factor
;

obj..
	systemCost =e= sum((g,t),generation(g,t)*genCost(g))+sum((r,t,o),trans(r,t,o)*transCost(r,o));

cGeneration(t,r)..
	sum(g$gtor(g,r),generation(g,t))+(sum(o,trans(o,t,r))*transLoss-sum(p,trans(r,t,p)))-demandLoad(r,t) =g= 0;

cMaxSolar(t,r)..
	maxSolar(r,t)-sum(solar$gtor(solar,r),generation(solar,t)) =g= 0;

cMaxWind(t,r)..
	maxWind(r,t)-sum(wind$gtor(wind,r),generation(wind,t)) =g= 0;

*cMaxHydro(t,r)..
*	maxHydro(r,t)-sum(hydro$gtor(hydro,r),generation(hydro,t)) =g= 0;

model
	combinedModel /obj,cGeneration,cMaxSolar,cMaxWind/
*	combinedModel /obj,cDemandAllocation,cDemandChargeCost,cVehicleMaintCost,cEnergyToMeetDemand,cChargingUpperBound,cChargingLowerBound,cNoChargeAtStart,cTerminalSOC,cNumCharging,cMaxCharging,cNumMoving,cFleetDispatch,cInfrastructureCost,cFleetCost,cDemandCharges,cGeneration,cMaxSolar,cMaxWind,cPersonalEVChargeEnergyLB,cPersonalEVChargeEnergyUB,cPersonalEVChargePowerLB,cPersonalEVChargePowerUB/

options
	qcp = cplex
	solvelink = 2
	reslim = 50000
;

$onecho > cplex.opt
$offecho
combinedModel.optFile = 1;
combinedModel.holdfixed = 1;

solve
	combinedModel
	using qcp
	minimizing systemCost
;

Execute_Unload "results-baseGeneration.gdx";
