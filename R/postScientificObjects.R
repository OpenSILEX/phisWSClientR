#-------------------------------------------------------------------------------
# Program: postScientificObjects.R
# Objective: functions to post a new scientific object to the WS2
#            * postSensors
# Authors: Hollebecq Jean-Eudes
# Creation: 24/09/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postVectors
##'
##' @description send a scientific object to the web service
##' @param rdfType character, the rdfType of the scientific object ex: http://www.opensilex.org/vocabulary/oeso#Plot
##' @param alias character, give an alias to the scientific object
##' @param geometry character, give the geometry of this scientific object. For example a plot can be : "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"
##' @param experiment character, uri of the experiment of the scientific object
##' @param isPartOf character, a scientific object the scientific object is part of
##' @param year character, the year of the scientific object
##' @param properties list, a list for example: list(rdfType = "http://xmlns.com/foaf/0.1/Agent", 
##'                                                   relation = "http://www.opensilex.org/vocabulary/2018#hasContact", 
##'                                                   value = "http://www.opensilex.org/demo/id/agent/marie_dupond")
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToPHISWS(apiID="ws_private",
##'                url = "http://www.opensilex.org/openSilexAPI/rest/",
##'                username="guest@opensilex.org",
##'                password="guest")
##'   postSensors(
##'    rdfType = "http://www.opensilex.org/vocabulary/oeso#Spectrometer",
##'    label = "aligot",
##'    brand = "Fait maison",
##'    model = "avec du  cantal",
##'    serialNumber = "",
##'    inServiceDate = "2017-06-15",
##'    dateOfPurchase = "2017-06-15",
##'    dateOfLastCalibration = "2017-06-15",
##'    personInCharge = "admin@opensilex.org"
##'    )
##'    }
##' @export
postSensors <- function(rdfType, alias, geometry, brand, serialNumber, inServiceDate, dateOfPurchase, personInCharge){
  attributes <- list()
  if (rdfType!="")    attributes <- c(attributes, rdfType = rdfType)       else stop("You must provide a type of scientific object")
  if (alias!="")      attributes <- c(attributes, alias = alias)           else stop("You must provide an alias to the scientific object")
  if (geometry!="")   attributes <- c(attributes, geometry = geometry)     else stop("You must provide a geometry")
  if (experiment!="") attributes <- c(attributes, experiment = experiment) else stop("You must provide a experiment")
  if (isPartOf!="")   attributes <- c(attributes, isPartOf = isPartOf)       #else stop("You must provide a isPartOf")
  if (year!="")   attributes <- c(attributes, year = year) 
  if (properties!="")  attributes <- c(attributes, properties = properties)   
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("SENSORS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
