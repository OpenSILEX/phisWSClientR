#-------------------------------------------------------------------------------
# Program: getUnits.R
# Objective: functions to get the unit service from WS2
#            * getUnits
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getUnits
##'
##' @description Retrieves the unit based on search criterion
##' @param uri character, search by the uri of a unit (optional)
##' @param label character, search by the label of a unit (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToPHISWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' units <- getUnits(uri="http://www.opensilex.org/demo/id/units/u007")
##' units$data
##' }
##' @export
getUnits <- function(
                      uri = "",
                      label = "",
                      page = NULL,
                      pageSize = NULL){
  
  attributes <- list(pageSize = pageSize,
                     page = page)
  if (uri!="")   attributes <- c(attributes, uri = uri)
  if (label!="") attributes <- c(attributes, label = label)
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("UNITS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}
