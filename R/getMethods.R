#-------------------------------------------------------------------------------
# Program: getMethods.R
# Objective: functions to get the method service from WS2
#            * getMethods2
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 06/09/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getMethods2
##'
##' @description retrieves the method based on search criterion
##' @param uri character, search by the uri of a method (optional)
##' @param label character, search by the label of a method (optional)
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
##' methods<-getMethods2(uri = "http://www.opensilex.org/demo/id/methods/m010")
##' methods$data
##' }
##' @export
getMethods2 <- function(uri = "",
                        label = "",
                        page = NULL,
                        pageSize = NULL){

  attributes <- list(pageSize = pageSize,
                     page = page)
  if (uri!="")   attributes <- c(attributes, uri = uri)
  if (label!="") attributes <- c(attributes, label = label)
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("METHODS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}
