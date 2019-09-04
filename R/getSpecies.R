#-------------------------------------------------------------------------------
# Program: getSpecies.R
# Objective: functions to get the specie service from WS2
#            * getSpecies
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getSpecies
##'
##' @description retrieves the specie behind the specie uri
##' @param uri character, search by the uri of a specie (optional)
##' @param label character, search by the label of a specie (optional)
##' @param language character, language of the answer, "en", "fr", etc (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToPHISWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' species <- getSpecies()
##' species$data
##' }
##' @export
getSpecies <- function(
                       uri = "",
                       label = "",
                       language = "",
                       page = NULL,
                       pageSize = NULL){
  
  
  
  attributes <- list(pageSize = pageSize,
                     page = page)
  if (uri!="")      attributes <- c(attributes, uri = uri)
  if (label!="")    attributes <- c(attributes, label = label)
  if (language!="") attributes <- c(attributes, language = language)
  
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("SPECIES", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}
