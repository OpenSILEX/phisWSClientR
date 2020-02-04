## ----option, include = FALSE--------------------------------------------------
knitr::opts_chunk$set( echo = TRUE, message = FALSE, warning = FALSE)

## ----library, echo=TRUE,message=FALSE, warning=FALSE--------------------------
  library(phisWSClientR)

## ----connect------------------------------------------------------------------
  # If you want to access to a private web service, you have to insert the address of the WS and the port
  #connectToPHISWS(apiID="ws_private",username="guest@opensilex.org",password="guest", url = "www.opensilex.org/openSilexAPI/rest/")
  connectToPHISWS(apiID="ws_private",username="guest@opensilex.org",password="guest", url = "http://www.opensilex.org/openSilexSandBoxAPI/rest/")

## ----token, results = "hide"--------------------------------------------------
  aToken <- opensilexWSClientR::getToken("guest@opensilex.org","guest")

## ----get user info------------------------------------------------------------
getUserInformations()

## ----getProject---------------------------------------------------------------
  getProjects2( acronyme = "PHENOME-FPPN")$data

## ----getExperiments-----------------------------------------------------------
  getExperiments2( uri="http://www.opensilex.org/demo/DIA2017-1")

## ----getScientificObjects-----------------------------------------------------
  getScientificObjects( uri="http://www.opensilex.org/demo/2018/o18000076")

## ----getInfrastructures-------------------------------------------------------
  getInfrastructures(uri="https://emphasis.plant-phenotyping.eu")

## ----getVariables-------------------------------------------------------------
  getVariables2( uri="http://www.opensilex.org/demo/id/variables/v010")

## ----getTraits----------------------------------------------------------------
  getTraits( uri="http://www.opensilex.org/demo/id/traits/t010")

## ----getMethods---------------------------------------------------------------
  getMethods2( uri="http://www.opensilex.org/demo/id/methods/m010")

## ----getUnits-----------------------------------------------------------------
  getUnits( uri="http://www.opensilex.org/demo/id/units/u007")

## ----getProvenances-----------------------------------------------------------
getProvenances(label = "ImageAnalysis-ZA17-Maize" )

## ----getSpecies---------------------------------------------------------------
  getSpecies( uri="http://www.opensilex.org/id/species/helianthusannuus", language = "en")

## ----getSensors---------------------------------------------------------------
  getSensors( uri="http://www.opensilex.org/demo/2018/s18001")

## ----getVectors---------------------------------------------------------------
  getVectors( serialNumber = "01BD1DD71500001")

## ----getEvents----------------------------------------------------------------
# Where to find the "type" URL ?
# Where is the ontology access  https://github.com/OpenSILEX/ontology-vocabularies/blob/master/oeev.owl
  getEvents( type = "http://www.opensilex.org/vocabulary/oeev#Fertilization", pageSize=10)

## ----getAnnotations, eval=FALSE-----------------------------------------------
#  
#    getAnnotations( comment = "Ustilago maydis infection" , pageSize=10)

## ----getData------------------------------------------------------------------
getData(variableUri = "http://www.opensilex.org/demo/id/variables/v014")$data

## ----getEnvironmentData-------------------------------------------------------
  getEnvironmentData( variable = "http://www.opensilex.org/demo/id/variables/v010", pageSize=50)$data

## ----getPhenotypeData---------------------------------------------------------
  head(getPhenotypeData( 
                        variable = "http://www.opensilex.org/demo/id/variables/v001", 
                        pageSize=10)$data)

## ----setLog level-------------------------------------------------------------
  setLogLevel("DEBUG")
  getEnvironmentData( variable = "http://www.opensilex.org/demo/id/variables/v050", pageSize=50)
  setLogLevel("INFO")

## ----session,echo=FALSE-------------------------------------------------------
  sessionInfo()

