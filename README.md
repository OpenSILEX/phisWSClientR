# phisWSClientR

PHIS client R

A set of functions to connect R to the phenomeapi web service in OpenSILEX PHIS. You can retrieve phenotypic data from the greenhouse platforms (GET imagery, environment, watering and weighing) or the field platforms. Public access is allowed with specific login as well as private access if the user has an account on an instance of the PHIS system information.

in progress...

To initialize a client request:

- connectToPHISWS()

For the first web service (<= v1.0.0), the available functions are:

- getEnvironment()
- getExperiments()
- getImageAnalysis()
- getProjects()
- getVariableByCategory()
- getWatering()

For the second web service (> v1.0.0 & <= v3.5.0), the available functions are:

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
install_github("OpenSILEX/phisWSClientR", build_vignettes=TRUE, ref="2.4.1")
```

You can also download a tar.gz archive of "[2.4.1](https://github.com/OpenSILEX/phisWSClientR/tree/2.4.1)" version and install it with _install_packages()_.

This package use [Semantic Versioning Specification](https://semver.org/) for versioning tags.

# Usage

Once the package is installed on your computer, it can be loaded into a R session:

```R
library(phisWSClientR)
help(package="phisWSClientR")
```

# Test

You can give a test to the package using the available vignettes (/doc directory) and use the documentation. if you have some difficulties to retrieve the html vignettes, you can use https://htmlpreview.github.io on the github file paths:

- [https://github.com/OpenSILEX/phisWSClientR/blob/2.4.1/doc/requestsWS2.html](https://htmlpreview.github.io/?https://github.com/OpenSILEX/phisWSClientR/blob/2.4.1/doc/requestsWS2.html)
- [https://github.com/OpenSILEX/phisWSClientR/blob/2.4.1/doc/requestsWS1.html](https://htmlpreview.github.io/?https://github.com/OpenSILEX/phisWSClientR/blob/2.4.1/doc/requestsWS1.html)

# Experimental functions

- [https://github.com/OpenSILEX/phisWSClientR/blob/2.4.1/doc/postRequestsWS2.html](https://htmlpreview.github.io/?https://github.com/OpenSILEX/phisWSClientR/blob/2.4.1/doc/postRequestsWS2.html)

# Citation

You should cite the **phisWSClientR** package:

```R
citation("phisWSClientR")
```

See also citation() for citing R itself.
