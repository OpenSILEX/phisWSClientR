# phisWSClientR

PHIS client R

A set of functions to connect R to the phenomeapi web service in PHIS-SILEX. You can retrieve phenotypic data from the greenhouse platforms (GET imagery, environment, watering and weighing) or the field platforms. Public access is allowed with specific login as well as private access if the user has an account on an instance of the PHIS system information.

in progress...

You can find a shinydashboard app to document this phenomeapi web service using the phisWSClientR package [here](https://github.com/sanchezi/docAppPhisWSClientR).
   
# Installation

To install the **phisWSClientR** package, the easiest is to install it directly from Gitlab. Open an R session and run the following commands:

```R
library(remotes) 
install_gitlab("OpenSILEX/data-analysis-visualisation/phisWSClientR", build_vignettes=TRUE)
```

# Usage

Once the package is installed on your computer, it can be loaded into a R session:

```R
library(phisWSClientR)
help(package="phisWSClientR")
```

# Citation

You should cite the **phisWSClientR** package:

```R
citation("phisWSClientR")
```

See also citation() for citing R itself.
