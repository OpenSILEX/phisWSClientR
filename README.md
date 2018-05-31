# phisWSClientR

PHIS client R

a set of functions to connect R to the phenomeapi web service in phenoarch greenhouse - PHIS-SILEX. You can retrieve phenotypic data from the phenoarch platform (GET imagery, environment, watering and weighing). Public access is allowed with specific login as well as private access if the user has an account on PHIS system information.

# Installation

To install the **phisWSClientR** package, the easiest is to install it directly from GitHub. Open an R session and run the following commands:

```R
library(devtools) 
install_github("sanchezi/phisWSClientR", build_vignettes=TRUE)
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
