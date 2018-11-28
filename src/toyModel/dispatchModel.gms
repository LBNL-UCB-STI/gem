$title toyModel

sets
	g			Generators		/g1,g2,g3/
	t 			Time Period		/t1,t2,t3/
	r			Region			/r1,r2,r3/
	;

parameters
	genCost(g)		/g1 10,
					g2 20,
					g3 30/
	maxGen(g)		/g1 100,
					g2 100,
					g3 100/
	;
	
table demandLoad(r,t)
			t1		t2		t3
	r1		5		15		20
	r2		15		10		10
	r3		10		15		10  ;

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
	systemCost =e= sum((g,t,r),generation(g,t,r)*genCost(g));

constraint1(t,r)..
	sum(g,generation(g,t,r))-demandLoad(r,t) =g= 0;

constraint2(g,t,r)..
	maxGen(g)-generation(g,t,r) =g= 0;

model
	toyModel /obj/
	;

options
	lp = cplex
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
