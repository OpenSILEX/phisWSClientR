#-------------------------------------------------------------------------------
# Program: getScientificObjects.R
# Objective: functions to get the scientific objects service from WS2
#            * getScientificObjects
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 06/09/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getScientificObjects
##'
##' @description retrieves the scientific objects based on search criterion
##' @param uri character, search by the uri of an annotation (optional)
##' @param experiment character, search by the uri of an experiment (optional)
##' @param alias character, search by an alias (optional)
##' @param rdfType character, search by the rdfType (optional)
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
##'  test<-getScientificObjects(uri="http://www.opensilex.org/demo/2018/o18000076")
##'  test$data
##' }
##' @export
getScientificObjects <- function(uri = "",
                                 experiment = "",
                                 alias = "",
                                 rdfType = "",
                                 page = NULL,
                                 pageSize = NULL){

  attributes <- list(pageSize=pageSize,page = page)
  if (uri!="")        attributes <- c(attributes, uri = uri)
  if (experiment!="") attributes <- c(attributes, experiment = experiment)
  if (alias!="")      attributes <- c(attributes, alias = alias)
  if (rdfType!="")    attributes <- c(attributes, rdfType = rdfType)
  
  Response <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("SCIENTIFIC_OBJECTS", configWS)),
                                                    attributes = attributes, wsVersion = 2)
  return(Response)
}