#-------------------------------------------------------------------------------
# Program: zzz.R
# Objective:
# Author: I. Sanchez
# Creation: 19/03/2018
# Update: 29/05/2018
#-------------------------------------------------------------------------------

## title defines a public configuration for the web service access
##
## description defines a public configuration for the web service access
## param libname character
## param pkgname character
## examples
## # not run
## keywords internal
# http://147.100.179.156:8080/phenomeapi/
#   configWS <- list(BASE_PATH = "http://147.100.179.156:8080/phenomeapi/resources/",
#                    TOKEN = "token",
#                    EXPERIMENT = "experiments",
#                    VARIABLES = "variables",
#                    ENVIRONMENT = "environment",
#                    PROJECTS = "projects",
#                    PLANTS = "plants",
#                    IMAGESANALYSIS = "imagesAnalysis",
#                    PHENOTYPES = "phenotypes",
#                    WATERING = "watering",
#                    DEFAULT_PAGE = 0,
#                    DEFAULT_PAGESIZE = 100)

# Define an environment for the phenomeapi configuration
configWS<-new.env(emptyenv())

.onLoad <- function(libname, pkgname){

  assign("BASE_PATH","http://147.100.179.156:8080/phenomeapi/resources/", configWS)
  # WS phis1
  assign("TOKEN", "token", configWS)
  assign("EXPERIMENT", "experiments", configWS)
  assign("VARIABLES", "variables", configWS)
  assign("PLANTS", "plants", configWS)
  assign("IMAGESANALYSIS", "imagesAnalysis", configWS)
  assign("PHENOTYPES", "phenotypes", configWS)
  assign("WATERING", "watering", configWS)

  # WS phis2
  assign("BRAPITOKEN", "brapiv1token", configWS)
  assign("AGROOBJECTS", "agronomicalObjects", configWS)
  assign("DATASETS", "datasets", configWS)

  # commun
  assign("VARIABLES", "variables", configWS)
  assign("ENVIRONMENT", "environment", configWS)
  assign("PROJECTS", "projects", configWS)
  assign("DEFAULT_PAGE", 0, configWS)
  assign("DEFAULT_PAGESIZE", 100, configWS)

  invisible()
}
