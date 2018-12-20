$title toyModel

sets
	g			Generators
	t 			Time Period		/t1,t2,t3/
	r			Region			/r1,r2,r3/
	gtor(g,r)
	;

parameters
	genCost(g)
	maxGen(g)
	demandLoad(r,t)
	test
	;

$gdxin test
$load g gtor genCost demandLoad maxGen test
$gdxin

display gtor;

variable
	systemCost
	;

positive variable
	generation(g,t,r)
	;

display
	genCost
	demandLoad
	maxGen
	;

equations
	obj				Objective function minimizing total cost
	constraint1		Generation must equal load
	constraint2		Max generation for each generator
	constraint3		Renewable generation cannot exceed wind or sun supply
	;

obj..
	systemCost =e= sum((g,t,r),generation(g,t,r)*genCost(g))+test;

constraint1(t,r)..
	sum(g,generation(g,t,r))-demandLoad(r,t) =g= 0;

constraint2(g,t,r)..
	maxGen(g)-generation(g,t,r) =g= 0;

model
	toyModel /obj,constraint1,constraint2/
	;

options
	lp = cplex
	solvelink = 2
	;

$onecho > cplex.opt
$offecho
toyModel.optFile = 1;
toyModel.holdfixed = 1;

solve
	toyModel
	using lp
	minimizing systemCost
	;

display
	generation.l
	;

Execute_Unload "results.gdx";
