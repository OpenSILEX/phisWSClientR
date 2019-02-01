#-------------------------------------------------------------------------------
# Program: getSensors.R
# Objective: functions to get the sensor service from WS2
#            * getSensors
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getSensors
##'
##' @description Retrieves the sensors based on search criterion
##' @param token character, a token from getToken function
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
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##' aToken = getToken("guest@opensilex.org","guest")
##' sensors <- getSensors(aToken$data,
##'  uri = "http://www.opensilex.org/demo/2018/s18001")
##' sensors <- getSensors(aToken$data, brand = "Cimel")
##' sensors$data
##' }
##' @export
getSensors <- function(token,
                       uri = "",
                       rdfType = "",
                       label = "",
                       brand = "",
                       serialNumber = "",
                       inServiceDate = "",
                       dateOfPurchase = "",
                       dateOfLastCalibration = "",
                       personInCharge = "",
                       page = NULL,
                       pageSize = NULL,
                       verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize = pageSize,page = page,Authorization=token)
  
  if (uri!="")                   attributes <- c(attributes, uri = uri)
  if (rdfType!="")               attributes <- c(attributes, rdfType = rdfType)
  if (label!="")                 attributes <- c(attributes, label = label)
  if (brand!="")                 attributes <- c(attributes, brand = brand)
  if (serialNumber!="" )         attributes <- c(attributes, serialNumber = serialNumber)
  if (inServiceDate!="")         attributes <- c(attributes, inServiceDate = inServiceDate)
  if (dateOfPurchase!="")        attributes <- c(attributes, dateOfPurchase = dateOfPurchase)
  if (dateOfLastCalibration!="") attributes <- c(attributes, dateOfLastCalibration = dateOfLastCalibration)
  if (personInCharge!="")        attributes <- c(attributes, personInCharge = personInCharge)
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("SENSORS", configWS)),
                                         attributes = attributes,
                                         verbose = verbose)
  return(variableResponse)
}
