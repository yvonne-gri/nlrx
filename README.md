
<!-- README.md is generated from README.Rmd. Please edit that file -->
nlrx <img src="man/figures/logo.png" align="right" width="150" />
=================================================================

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/nldoc/nlrx?branch=master&svg=true)](https://ci.appveyor.com/project/nldoc/nlrx) [![Travis build status](https://travis-ci.org/nldoc/nlrx.svg?branch=master)](https://travis-ci.org/nldoc/nlrx) [![Codecov test coverage](https://codecov.io/gh/nldoc/nlrx/branch/master/graph/badge.svg)](https://codecov.io/gh/nldoc/nlrx?branch=master) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![ropensci](https://badges.ropensci.org/262_status.svg)](https://github.com/ropensci/onboarding/issues/262)

The nlrx package provides tools to setup and execute NetLogo simulations from R. NetLogo is a free, open-source and cross-platform modelling environment for simulating natural and social phenomena. NetLogo focusses on implementation of agent-based and spatially explicit simulation models, although system dynamics models are supported as well. NetLogo is developed and maintained at the Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL. More details on NetLogo itself are available online: [NetLogo online documentation](https://ccl.northwestern.edu/netlogo/docs/)

NetLogo comes with the built-in experiment tool [Behavior Space](https://ccl.northwestern.edu/netlogo/docs/behaviorspace.html) that allows to setup and execute model simulations with different settings and parameter variations and to collect model output. This experiment tool can be executed via command line in combination with an XML file that contains the experiment specifications, such as runtime, output measurements, stop conditions, and more. One limitation of Behavior Space is, that it only supports full-factorial parameter designs, which may not be appropriate for complex model analyses. Furthermore, Behavior Space experiment specifications are stored within the NetLogo file and are not easily accessible from R for further reproducible analysis.

The nlrx package utilizes the commandline functionality of Behavior Space to execute NetLogo simulations directly from R. Instead of defining experiments within NetLogo Behavior Space, experiments are defined in R using the class objects of the nlrx package. These class objects hold all the information that is needed to run these experiments remotely from R, such as path to NetLogo installation folder, path to the model file and the experiment specifications itself. nlrx provides useful helper functions to generate parameter input matrices from parameter range definitions that cover a wide range of parameter exploration approaches.

The nlrx package uses class objects to store all relevant information on simulation experiments, including the output of the model simulations. This allows to easily store and share simulation results including all experiment specifications.

In summary, the nlrx package uses a similar structure as NetLogos Behavior Space but offers more flexibility and additional tools for running complex model analyses directly from R.

Prerequirements
---------------

In order to use the nlrx package, NetLogo needs to be installed on the system that is used to execute model simulations (local/remote). For remote execution, NetLogo needs to be installed on remote machines as well. Because NetLogo is executed in a Java virtual machine, Java needs to be installed on the local/remote system as well. We recommend the [Oracle Java SE Development Kit 8](https://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html). While the nlrx package might work without setting the Java system path explicitly, we recommend to make sure that JAVA\_HOME points to the correct Java installation of the system.

Installation
------------

~~You can install the released version of nlrx from~~ ~~[CRAN](https://CRAN.R-project.org) with:~~ ~~install.packages("nlrx")~~

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nldoc/nlrx")
```

Get started
-----------

The main focus of nlrx is to define and execute NetLogo model simulation experiments from R. All information that is needed to run NetLogo simulations remotely is stored within a `nl` class object. The `nl` class object is the main class in nlrx which stores information on the NetLogo version, a path to the NetLogo directory with the defined version, a path to the model file, and the desired memory for the java virtual machine. Nested within this `nl` class are the classes `experiment` and `simdesign`. The `experiment` class stores all experiment specifications, such as runtime, variables with variable ranges, constants, output metrics, and more. A `nl` class object containing a valid `experiment` class object can then be used to automatically generate a `simdesign` class object that is attached to the `nl` class object, by using one of the simdesign helper functions. These helper functions create different parameter input matrices from the experiment variable definitions that can then be executed by the `run_nl_one()` and `run_nl_all()` functions. The nested design allows to store everything related to the experiment within one R object. Additionally, different simdesign helper functions can be applied to the same `nl` object in order to repeat the same experiment with different parameter exploration methods (simdesigns).

Step by step application example
--------------------------------

The "Wolf Sheep Predation" model from the NetLogo models library is used to present a basic example on how to setup and run NetLogo model simulations from R. The following steps guide you trough the process on how to setup, run and analyze the NetLogo model simulations with nlrx.

#### Step 1: Create a nl object:

The nl object holds all information on the NetLogo version, a path to the NetLogo directory with the defined version, a path to the model file, and the desired memory for the java virtual machine. Depending on the operation system, paths to NetLogo and the model need to be adjusted.

``` r
library(nlrx)
# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.0.3")
modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
outpath <- file.path("C:/out")
# Unix default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("/home/NetLogo 6.0.3")
modelpath <- file.path(netlogopath, "app/models/Sample Models/Biology/Wolf Sheep Predation.nlogo")
outpath <- file.path("/home/out")

nl <- nl(nlversion = "6.0.3",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024)
```

#### Step 2: Attach an experiment

The experiment object is organized in a similar fashion as NetLogo Behavior Space experiments. It contains all information that is needed to generate a simulation parameter matrix and to execute the NetLogo simulations:

-   expname: The experiment name is only used for documentation
-   outpath: The outpath can be used to write simulation results to disk after simulations have been executed
-   repetition: Repetition defines how often the same parameterisation should be repeated. However, random-seed and repetitions management in nlrx is usually done via the simdesign helper functions and thus, repetition of the experiment should be set to 1 in most cases (see vignette "Further Notes" for more information on random-seed and repition management of nlrx).
-   tickmetrics: can either be "true" or "false" and defines if output metrics should be measured on each tick or only on the final simulation tick
-   idsetup: A vector of strings defining one or more NetLogo model procedures that are executed at the start of each simulation. Procedures are executed in the order provided.
-   idgo: A vector of strings defining one or more NetLogo model procedures that are executed on each step of each model simulation. Procedures are executed in the order provided.
-   idfinal: A vector of strings defining one or more NetLogo model procedures that are executed at the end of each model simulation. Procedures are executed in the order provided.
-   idrunnum: Can be used to send the current model run information (current row of parameter matrix, random seed) to an already defined NetLogo global variable. This can be useful if the NetLogo model produces self-written output that needs to be referenced to the nlrx output.
-   evalticks: a vector or sequence of tick values for which model output should be stored and reported. Model output from ticks that are not defined in evalticks will be dropped after the simulation.
-   stopcond: A NetLogo reporter or string that stops a running model simulation when it reports TRUE (e.g. "not any? turtles").
-   metrics: A vector with valid NetLogo reporters that are used to measure output from each model simulation.
-   metrics.patches: A vector with valid patches-own variables that are measured on each evaluation tick.
-   metrics.turtles: A vector with valid turtles-own variables that are measured on each evaluation tick.
-   metrics.links: A vector with valid links-own variables that are measured on each evaluation tick.
-   variables: are defined as a list of named lists for each model parameter that should be varied. These sublists may contain distinct values, ranges from min to max, a step increment for full-factorial designs or a q-function for random sampling within the defined range. The type of information needed for each variable depends on the simdesign helper function that is used to setup the parameter matrix later on. A table with requirements for each simdesign helper function can be found in the "Further Notes" vignette or the corresponding help files of each simdesign helper function. In this example, we want to perform a latin hypercube sampling, which needs a parameter range (min, max) and a random sampling function (qfun).
-   constants: are defined as a list of values (etiher string or numeric), that are kept constant for all experiment runs.

A note on variables and constants: It is not allowed to list the same variable in the variables and constants list. NetLogo model parameters that are not listed in any of these two lists will be set with their default value from the NetLogo model interface.

``` r
nl@experiment <- experiment(expname="wolf-sheep",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup",
                            idgo="go",
                            idfinal=NA_character_,
                            idrunnum=NA_character_,
                            runtime=50,
                            evalticks=seq(40,50),
                            metrics=c("count sheep", "count wolves", "count patches with [pcolor = green]"),
                            variables = list('initial-number-sheep' = list(min=50, max=150, qfun="qunif"),
                                             'initial-number-wolves' = list(min=50, max=150, qfun="qunif")),
                            constants = list("model-version" = "\"sheep-wolves-grass\"",
                                             "grass-regrowth-time" = 30,
                                             "sheep-gain-from-food" = 4,
                                             "wolf-gain-from-food" = 20,
                                             "sheep-reproduce" = 4,
                                             "wolf-reproduce" = 5,
                                             "show-energy?" = "false"))
```

#### Step 3: Attach a simulation design

While the experiment defines the variables and specifications of the model, the simulation design creates a parameter input table based on these model specifications and the chosen simulation design method. nlrx provides a bunch of different simulation designs, such as full-factorial, latin-hypercube, sobol, morris and eFast. All simdesign helper functions need a properly defined nl object with a valid experiment design. Each simdesign helper also allows to define a number of random seeds that are randomly generated and can be used to execute repeated simulations of the same parameter matrix with different random-seeds (see "Further Notes" vignette for more information on random-seed and repetition management.) A simulation design is attached to a nl object by using one of the simdesign helper functions:

``` r
nl@simdesign <- simdesign_lhs(nl=nl,
                               samples=100,
                               nseeds=3,
                               precision=3)
```

#### Step 4: Run simulations

All information that is needed to run the simulations is now stored within the nl object. The run\_nl\_one() function allows to run one specific simulation from the siminput parameter table. The run\_nl\_all() function runs a loop over all simseeds and rows of the parameter input table siminput. The loops are created by calling furr::future\_map\_dfr which allows running the function either locally or on remote HPC machines.

``` r
future::plan(multisession)

results %<-% run_nl_all(nl = nl, cleanup = "all")
```

#### Step 5: Attach results to nl and run analysis

nlrx provides method specific analysis functions for each simulation design. Depending on the chosen design, the function reports a tibble with aggregated results or sensitivity indices. In order to run the analyze\_nl function, the simulation output has to be attached to the nl object first. After attaching the simulation results, these can also be written to the defined outpath of the experiment object.

``` r
# Attach results to nl object:
setsim(nl, "simoutput") <- results

# Write output to outpath of experiment within nl
write_simoutput(nl)

# Do further analysis:
analyze_nl(nl)
```

Meta
----

-   Please [report any issues or bugs](https://github.com/nldoc/nlrx/issues/new/).
-   License: GPL3
-   Get citation information for `nlrx` in R doing `citation(package = 'nlrx')`
-   We are very open to contributions - if you are interested check [Contributing](CONTRIBUTING.md).
    -   Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
