#-------------------------------------------------------------------------------
# Program: getSensors.R
# Objective: functions to get the sensor service from WS2
#            * getSensors
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 06/09/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getSensors
##'
##' @description Retrieves the sensors based on search criterion
##' @param uri character, search by the uri of a sensor (optional)
##' @param rdfType character, search by the rdf type of a sensor (optional)
##' @param label character, search by the label of the measure (optional)
##' @param brand character, search by the brand of a sensor (optional)
##' @param serialNumber character, search by serialNumber of a sensor (optional)
##' @param inServiceDate character, search by the inServiceDate of a sensor (optional)
##' @param dateOfPurchase character, search by the date of purchase of a sensor (optional)
##' @param dateOfLastCalibration character, search by the date of last calibration of a sensor (optional)
##' @param personInCharge character, search by the person in charge of a sensor (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
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
##' sensors <- getSensors(uri = "http://www.opensilex.org/demo/2018/s18001")
##' sensors <- getSensors(brand = "Cimel")
##' sensors$data
##' }
##' @export
getSensors <- function(uri = "",
                       rdfType = "",
                       label = "",
                       brand = "",
                       serialNumber = "",
                       inServiceDate = "",
                       dateOfPurchase = "",
                       dateOfLastCalibration = "",
                       personInCharge = "",
                       page = NULL,
                       pageSize = NULL){

  attributes <- list(pageSize = pageSize,page = page)
  
  if (uri!="")                   attributes <- c(attributes, uri = uri)
  if (rdfType!="")               attributes <- c(attributes, rdfType = rdfType)
  if (label!="")                 attributes <- c(attributes, label = label)
  if (brand!="")                 attributes <- c(attributes, brand = brand)
  if (serialNumber!="" )         attributes <- c(attributes, serialNumber = serialNumber)
  if (inServiceDate!="")         attributes <- c(attributes, inServiceDate = inServiceDate)
  if (dateOfPurchase!="")        attributes <- c(attributes, dateOfPurchase = dateOfPurchase)
  if (dateOfLastCalibration!="") attributes <- c(attributes, dateOfLastCalibration = dateOfLastCalibration)
  if (personInCharge!="")        attributes <- c(attributes, personInCharge = personInCharge)
  
  Response <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("SENSORS", configWS)),
                                                    attributes = attributes, wsVersion = 2)
  return(Response)
}
