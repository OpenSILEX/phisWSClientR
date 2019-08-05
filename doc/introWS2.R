## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  library(phisWSClientR)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  # If you want to access to a private web service, you have to insert the address of the WS and the port
  connect(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  aToken <- getToken("guestphis@supagro.inra.fr","guestphis")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getProjects2(aToken$data, uri="http://www.opensilex.org/demo/PHENOME-FPPN")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getExperiments2(aToken$data, uri="http://www.opensilex.org/demo/DIA2017-1")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getScientificObjects(aToken$data, uri="http://www.opensilex.org/demo/2018/o18000076")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getInfrastructures(aToken$data, uri="https://emphasis.plant-phenotyping.eu")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getVariables2(aToken$data, uri="http://www.opensilex.org/demo/id/variables/v010")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getTraits(aToken$data, uri="http://www.opensilex.org/demo/id/traits/t010")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getMethods2(aToken$data, uri="http://www.opensilex.org/demo/id/methods/m010")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getUnits(aToken$data, uri="http://www.opensilex.org/demo/id/units/u007")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getSpecies(aToken$data, uri="http://www.opensilex.org/id/species/helianthusannuus", language = "en")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getSensors(aToken$data, uri="http://www.opensilex.org/demo/2018/s18001")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getVectors(aToken$data, serialNumber = "01BD1DD71500001")

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
# Where to find the "type" URL ?
# Where is the ontology access ?
  getEvents(aToken$data, type = "http://www.opensilex.org/vocabulary/oeev#Fertilization", pageSize=10)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
# Where to find the "type" URL ?
# Where is the ontology access ?
  getAnnotations(aToken$data, comment = "Ustilago maydis infection" , pageSize=10)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  getEnvironmentData(aToken$data, variable = "http://www.opensilex.org/demo/id/variables/v010", pageSize=50)

## ----echo=TRUE,message=FALSE, warning=FALSE------------------------------
  head(getPhenotypeData(aToken$data, 
                        variable = "http://www.opensilex.org/demo/id/variables/v001", 
                        pageSize=10)$data)

## ----session,echo=FALSE,message=FALSE, warning=FALSE---------------------
  sessionInfo()

