## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  library(phisWSClientR)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # If you want to access to a private web service, you have to insert the address of the WS and the port
  connectToPHISWS(apiID="ws_private",username="guest@opensilex.org",password="guest", url = "www.opensilex.org/openSilexAPI/rest/")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  aToken <- opensilexWSClientR::getToken("guest@opensilex.org","guest")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getProjects2( uri="http://www.opensilex.org/demo/PHENOME-FPPN")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getExperiments2( uri="http://www.opensilex.org/demo/DIA2017-1")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getScientificObjects( uri="http://www.opensilex.org/demo/2018/o18000076")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getInfrastructures(uri="https://emphasis.plant-phenotyping.eu")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getVariables2( uri="http://www.opensilex.org/demo/id/variables/v010")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getTraits( uri="http://www.opensilex.org/demo/id/traits/t010")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getMethods2( uri="http://www.opensilex.org/demo/id/methods/m010")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getUnits( uri="http://www.opensilex.org/demo/id/units/u007")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getSpecies( uri="http://www.opensilex.org/id/species/helianthusannuus", language = "en")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getSensors( uri="http://www.opensilex.org/demo/2018/s18001")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getVectors( serialNumber = "01BD1DD71500001")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
# Where to find the "type" URL ?
# Where is the ontology access  https://github.com/OpenSILEX/ontology-vocabularies/blob/master/oeev.owl
  getEvents( type = "http://www.opensilex.org/vocabulary/oeev#Fertilization", pageSize=10)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
# Where to find the "type" URL ?
# Where is the ontology access https://github.com/OpenSILEX/ontology-vocabularies/blob/master/oeev.owl
  getAnnotations( comment = "Ustilago maydis infection" , pageSize=10)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getEnvironmentData( variable = "http://www.opensilex.org/demo/id/variables/v010", pageSize=50)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  head(getPhenotypeData( 
                        variable = "http://www.opensilex.org/demo/id/variables/v001", 
                        pageSize=10)$data)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  setLogLevel("DEBUG")
  getEnvironmentData( variable = "http://www.opensilex.org/demo/id/variables/v010", pageSize=50)
  setLogLevel("INFO")

## ----session,echo=FALSE,message=FALSE, warning=FALSE---------------------
  sessionInfo()

