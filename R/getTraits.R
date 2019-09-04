#-------------------------------------------------------------------------------
# Program: getTraits.R
# Objective: functions to get the trait service from WS2
#            * getTraits
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getTraits
##'
##' @description retrieves the trait based on search criterion
##' @param uri character, search by the uri of a trait (optional)
##' @param label character, search by the label of a trait (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToPHISWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' traits <- getTraits(uri = "http://www.opensilex.org/demo/id/traits/t010")
##' traits$data
##' }
##' @export
getTraits <- function(
                      uri = "",
                      label = "",
                      page = NULL,
                      pageSize = NULL){
  
  
  
  attributes <- list(pageSize = pageSize,
                     page = page)
  if (uri!="")   attributes <- c(attributes, uri = uri)
  if (label!="") attributes <- c(attributes, label = label)
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("TRAITS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}
