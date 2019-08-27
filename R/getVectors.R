#-------------------------------------------------------------------------------
# Program: getVectors.R
# Objective: functions to get the vector service from WS2
#            * getVectors
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getVectors
##'
##' @description retrieves the vectors based on search criterion
##' @param uri character, search by the uri of an annotation (optional)
##' @param rdfType character, search by the rdf type of a sensor (optional)
##' @param label character, search by the label of the measure (optional)
##' @param brand character, search by the brand of a vector (optional)
##' @param serialNumber character, search by serialNumber of a vector (optional)
##' @param inServiceDate character, search by the inServiceDate of a vector (optional)
##' @param dateOfPurchase character, search by the date of purchase of a vector (optional)
##' @param personInCharge character, search by the person in charge of a vector (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the connectToOpenSILEXWS() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToOpenSILEXWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##' vectors <- getVectors(aToken$data, uri = "http://www.opensilex.org/demo/2018/v1801")
##' vectors <- getVectors(aToken$data, serialNumber = "01BD1DD71500001")
##' vectors$data
##' }
##' @export
getVectors <- function(
                       uri = "",
                       rdfType = "",
                       label = "",
                       brand = "",
                       serialNumber = "",
                       inServiceDate = "",
                       dateOfPurchase = "",
                       personInCharge = "",
                       page = NULL,
                       pageSize = NULL,
                       verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize = pageSize,
                     page = page)
  if (uri!="")            attributes <- c(attributes, uri = uri)
  if (rdfType!="")        attributes <- c(attributes, rdfType = rdfType)
  if (label!="")          attributes <- c(attributes, label = label)
  if (brand!="")          attributes <- c(attributes, brand = brand)
  if (serialNumber!="" )  attributes <- c(attributes, serialNumber = serialNumber)
  if (inServiceDate!="")  attributes <- c(attributes, inServiceDate = inServiceDate)
  if (dateOfPurchase!="") attributes <- c(attributes, dateOfPurchase = dateOfPurchase)
  if (personInCharge!="") attributes <- c(attributes, personInCharge = personInCharge)
  
  variableResponse <- getResponseFromWS(resource = paste0(get("VECTORS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}
