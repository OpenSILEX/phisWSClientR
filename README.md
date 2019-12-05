# phisWSClientR

PHIS client R

A set of functions to connect R to the phenomeapi web service in OpenSILEX PHIS. You can retrieve phenotypic data from the greenhouse platforms (GET imagery, environment, watering and weighing) or the field platforms. Public access is allowed with specific login as well as private access if the user has an account on an instance of the PHIS system information.

in progress...

To initialize a client request:

- connectToPHISWS()

For the first web service, the available functions are:

- getEnvironment()
- getExperiments()
- getImageAnalysis()
- getProjects()
- getVariableByCategory()
- getWatering()

For the second web service, the available functions are:

- getAnnotations()
- getData()
- getEnvironmentData()
- getEvents()
- getExperiments2()
- getInfrastructures()
- getMethods2()
- getPhenotypeData()
- getProjects2()
- getScientificObjects()
- getSensors()
- getSpecies()
- getTraits()
- getUnits()
- getVariables2()
- getVariablesByExperiment()
- getVectors()

You can find a shinydashboard app to document the first phenomeapi web service using the phisWSClientR package [here](https://github.com/sanchezi/docAppPhisWSClientR).

# Installation

To install the **phisWSClientR** package, the easiest is to install it directly from Github. Open an R session and run the following commands:

```R
library(remotes)
install_github("OpenSILEX/phisWSClientR", build_vignettes=TRUE, ref="2.2.0")
```

You can also download a tar.gz archive of "[2.2.0](https://github.com/OpenSILEX/phisWSClientR/tree/2.2.0)" version and install it with _install_packages()_.

This package use [Semantic Versioning Specification](https://semver.org/) for versionning tags.

# Usage

Once the package is installed on your computer, it can be loaded into a R session:

```R
library(phisWSClientR)
help(package="phisWSClientR")
```

# Test

You can give a test to the package using the available vignettes (/doc directory) and use the documentation. if you have some difficulties to retrieve the html vignettes, you can use https://rawgit.com on the github file paths:

- https://github.com/OpenSILEX/phisWSClientR/blob/master/doc/requestsWS2.html
- https://github.com/OpenSILEX/phisWSClientR/blob/master/doc/requestsWS1.html

# Citation

You should cite the **phisWSClientR** package:

```R
citation("phisWSClientR")
```

See also citation() for citing R itself.
