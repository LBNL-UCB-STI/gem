# Grid-integrated Electric Mobility (GEM) Model

The Grid-integrated Electric Mobility (GEM) model is an open-source modeling platform developed by researchers at Lawrence Berkeley National Laboratory, UC Davis, and UC Berkeley.  This modeling system simulates the operation of both the mobility sector and the electricity sector on a national scale in the United States.  The framework of GEM is unique in that it optimizes the development of a fully autonomous, electric, and shared mobility system while dynamically accounting for the relationship with a high resolution grid model.

## Table of Contents

## Installation and Setup

GEM is run on both the R platform (https://www.r-project.org/), which acts as a data parser and plotting device for the model. The optimization backbone of GEM employs the General Algebraic Modeling System, also called GAMS (https://www.gams.com/).  While R is an open-source software and readily available, GAMS is a commercial platform and a license is required to operate it.  Additionally, the optimization routine in GEM requires a quadratic convex program solver; we employ CPLEX which also requires a license (https://www.ibm.com/analytics/cplex-optimizer and https://www.gams.com/latest/docs/S_MAIN.html).  We recommend 

## Platform Description

![plot](./readme_images/forPaper_model-approach-1.png)

## GAMS Optimization Walkthrough

## Data Description

## Parameter Setup

## Running the Model

The GEM model can be fully run in console environment (e.g. cmd.exe in Windows or Terminal in MacOS) using Rscript (https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/Rscript).  A list of commands can be shown with the help command:

```
Rscript ./src.gem.R --help
```

The help option will dispaly the following message:

```
Usage: gem.R [exp-file]

Options:                                                                                                                                                                                                         
        -p, --plots 
        Only run code that produces plots, requires results to be already present in outputs [default FALSE]

        -t, --notimestamp
        Don't add timestamp to outputs directory [default FALSE]

        -d, --trimdays
        Trim a day off beginning and end of simulation results to avoid edge effects [default FALSE] 

        -e EXP, --experiment=EXP
        Path to experiment file [default input/experiments/base.yaml] 

        -r RUNSUBSET, --runsubset=RUNSUBSET
        Comma separate list of runs to execute [default ]
        
        -o, --overwrite
        Overwrite an existing solution from GAMS [default FALSE]
       
        -h, --help
        Show this help message and exit                                                                                                                                                                                                                 
```



## License

See the file LICENSE for the modified BSD license terms.

Copyright notice:

Grid-Integrated Electric Mobility Model (GEM) Copyright (c) 2021, The Regents of the University of California, through Lawrence Berkeley National Laboratory (subject to receipt of any required approvals from the U.S. Dept. of Energy) and University of California, Davis. All rights reserved.

If you have questions about your rights to use or distribute this software, please contact Berkeley Lab's Intellectual Property Office at IPO@lbl.gov.

NOTICE. This Software was developed under funding from the U.S. Department of Energy and the U.S. Government consequently retains certain rights. As such, the U.S. Government has been granted for itself and others acting on its behalf a paid-up, nonexclusive, irrevocable, worldwide license in the Software to reproduce, distribute copies to the public, prepare derivative works, and perform publicly and display publicly, and to permit others to do so.