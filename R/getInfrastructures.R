#-------------------------------------------------------------------------------
# Program: getInfrastructures.R
# Objective: functions to get the infrastructure service from WS2
#            * getInfrastructures
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getInfrastructures
##'
##' @description retrieves the infrastructures based on search criterion
##' @param uri character, search by the uri of an infrastructure (optional)
##' @param rdfType character, search by the rdf type of an infrastructure (optional)
##' @param label character, search by the label of an infrastructure (optional)
##' @param language character, the language of the response, "en", "fr", etc (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToOpenSILEXWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' infrastructures <- getInfrastructures( 
##'                        uri = "https://emphasis.plant-phenotyping.eu"
##'                      )
##' infrastructures$data
##' }
##' @export
getInfrastructures <- function(
                               uri = "",
                               rdfType = "",
                               label = "",
                               language = "en",
                               page = NULL,
                               pageSize = NULL,
                               verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize = pageSize,
                     page = page)
  if (uri!="")      attributes <- c(attributes, uri = uri)
  if (rdfType!="")  attributes <- c(attributes, rdfType = rdfType)
  if (label!="")    attributes <- c(attributes, label = label)
  if (language!="") attributes <- c(attributes, language = language)
  
  variableResponse <- getResponseFromWS(resource = paste0(get("INFRASTRUCTURES", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}
