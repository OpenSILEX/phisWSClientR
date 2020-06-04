## ----option-------------------------------------------------------------------
knitr::opts_chunk$set( echo = TRUE, message = FALSE, warning = FALSE)

## ----library, echo=TRUE,message=FALSE, warning=FALSE--------------------------
  library(phisWSClientR)

## ----connect------------------------------------------------------------------
  # If you want to access to a private web service, you have to insert the address of the WS and the port
  #connectToPHISWS(apiID="ws_private",username="guest@opensilex.org",password="guest", url = "www.opensilex.org/openSilexAPI/rest/")
  connectToPHISWS(apiID="ws_private",username="guest@opensilex.org",password="guest", url = "http://www.opensilex.org/openSilexSandBoxAPI/rest/")

## ----get user info------------------------------------------------------------
getUserInformations()

## ----postAnnotations, eval = FALSE--------------------------------------------
#   newAnnotation <- AnnotationDTO$new(
#      creator = "http://www.opensilex.org/demo/id/agent/marie_dupond",
#      targets = list("http://www.phenome-fppn.fr/test/2019/o19000074"),
#      motivatedBy = "http://www.w3.org/ns/oa#describing",
#      bodyValues =  list("the object has been observed")
#    )
#  
#    postAnnotations(list(newAnnotation))

## ----postData-----------------------------------------------------------------

 data = data.frame(
   provenanceUri = c(
   "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
   "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
   "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
   "http://www.phenome-fppn.fr/test/id/provenance/1569422784579"),
   objectUri = c(
   "http://www.phenome-fppn.fr/test/2019/o19000076",
   "http://www.phenome-fppn.fr/test/2019/o19000076",
   "http://www.phenome-fppn.fr/test/2019/o19000076",
   "http://www.phenome-fppn.fr/test/2019/o19000076"),
   variableUri = c(
   "http://www.phenome-fppn.fr/test/id/variables/v027",
   "http://www.phenome-fppn.fr/test/id/variables/v027",
   "http://www.phenome-fppn.fr/test/id/variables/v027",
   "http://www.phenome-fppn.fr/test/id/variables/v027"),
   date = c(
   "2017-06-15T10:45:00+0200",
   "2017-06-15T10:46:00+0200",
   "2017-06-15T11:47:00+0200",
   "2017-06-15T11:48:00+0200"),
   value = c(1111.3, 1010, 3030, 4040),
   stringsAsFactors = FALSE
 )
 postData(
   data
 )

## ----postEvents---------------------------------------------------------------
  postEvents(    
    rdfType = "http://www.opensilex.org/vocabulary/oeev#MoveFrom",
    concernedItemsUris = list("http://www.opensilex.org/demo/2018/o18000076"),
    date = "2017-09-08T12:00:00+01:00",
    properties = list(
      rdfType = "http://xmlns.com/foaf/0.1/Agent",
      relation = "http://www.opensilex.org/vocabulary/2018#hasContact",
      value= "http://www.opensilex.org/demo/id/agent/guest_phis"
    ),
    description = "The description of your event",
    creator = "http://www.opensilex.org/demo/id/agent/guest_phis"
  )

## ----postProvenances----------------------------------------------------------
  prov = ProvenanceDTO$new(
      label = "insertionProvenance_label",
      comment = "comment my provenance",
      metadata = ObjectDTO$new(list(meta1 = "15", meta2 = "owner1"))
  )
  postProvenances(list(prov))

## ----postScienticObjects, eval = F--------------------------------------------
#    expDesign = data.frame(
#        Alias = c(
#         "1/DZ_PG_67/ZM4394/WW/1/DIA2017-05-19",
#         "2/DZ_PG_30/ZM4361/WW/1/DIA2017-05-19",
#         "3/DZ_PG_49/ZM4389/WW/1/DIA2017-05-19"),
#        Type = c("Plot", "Plot", "Plot"),
#        Experiment = c(
#        "http://www.opensilex.org/demo/DMO2000-1",
#        "http://www.opensilex.org/demo/DMO2000-1",
#        "http://www.opensilex.org/demo/DMO2000-1"),
#        Species = c("Maize", "Maize", "Maize"),
#        Variety = c("iPG310", "iPG152", "iPG228"),
#        Geometry = c(
#        "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))",
#        "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))",
#        "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))"
#        ),
#        stringsAsFactors = FALSE
#    )
#    response <- postScientificObjects(expDesign)

## ----postSensors--------------------------------------------------------------
postSensors(rdfType = "http://www.opensilex.org/vocabulary/oeso#Spectrometer",
            label = "Sensor_label",
            brand = "Sensor_brand",model = "Sensor_model",serialNumber = "",
            inServiceDate = "2017-06-15",dateOfPurchase = "2017-06-15",
            dateOfLastCalibration = "2017-06-15",personInCharge = "admin@opensilex.org"
)

## ----postTraits---------------------------------------------------------------
  postTraits(
   label = "insertionTrait_label",
   comment = "comment my trait",
   ontologiesReferences = list(property = "http://www.w3.org/2004/02/skos/core#closeMatch",
                   object = "http://www.cropontology.org/rdf/CO_715:0000139",
                   seeAlso = "http://www.cropontology.org/ontology/CO_715/")
                   )

## ----postMethods--------------------------------------------------------------
  postMethods(
    uri = "http://www.opensilex.org/demo/id/units/m016",
  label = "insertionmethod_label",
  comment = "comment my method",
  ontologiesReferences = list(
     property = "http://www.w3.org/2004/02/skos/core#closeMatch",
     object = "http://www.cropontology.org/rdf/CO_715:0000139",
     seeAlso = "http://www.cropontology.org/ontology/CO_715/")
  )

## ----postUnits----------------------------------------------------------------
  postUnits(
   label = "insertionUnit_label",
   comment = "comment my unit",
   ontologiesReferences = list(property = "http://www.w3.org/2004/02/skos/core#closeMatch",
                   object = "http://www.cropontology.org/rdf/CO_715:0000139",
                   seeAlso = "http://www.cropontology.org/ontology/CO_715/")
                   )
                   

## ----postVariables------------------------------------------------------------
postVariables(uri="http://www.opensilex.org/demo/id/variables/v0042",
              label = "insertionVariable_label",
              comment = "comment my variable",
              ontologiesReferences = list(
                property = "http://www.w3.org/2004/02/skos/core#closeMatch",
                object = "http://www.cropontology.org/rdf/CO_715:0000139",
                seeAlso = "http://www.cropontology.org/ontology/CO_715/"),
              trait = "http://www.opensilex.org/demo/id/traits/t010", 
              method = "http://www.opensilex.org/demo/id/methods/m010", 
              unit = "http://www.opensilex.org/demo/id/units/u007")
  

## ----postvectors--------------------------------------------------------------
postVectors(
  uri="http://www.opensilex.org/demo/2019/v1909",
  rdfType = "http://www.opensilex.org/vocabulary/oeso#UAV",
  label = "Vector_label",
  brand = "Vector_brand",
  serialNumber = "1477",
  inServiceDate = "2017-06-15",
  dateOfPurchase = "2017-06-15",
  personInCharge = "admin@opensilex.org"
)

## ----setLog level-------------------------------------------------------------
  setLogLevel("DEBUG")
  getEnvironmentData( variable = "http://www.opensilex.org/demo/id/variables/v050", pageSize=50)
  setLogLevel("INFO")

## ----session,echo=FALSE-------------------------------------------------------
  sessionInfo()

