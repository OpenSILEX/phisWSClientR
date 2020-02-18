#-------------------------------------------------------------------------------
# Program: getVocabulary.R
# Objective: functions to get vocabulary from the OpenSILEx instance
#            * getVocabulary
# Authors: Hollebecq Jean-Eudes
# Creation: 15/02/2020
# Update: 
#-------------------------------------------------------------------------------

##' @title getVocabulary
##'
##' @description retrieves the vocabularies from the instance
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
##' vocabulary<-getVocabulary()
##' vocabulary$data
##' }
##' @export
getVocabulary <- function(page = NULL, pageSize = NULL){
  
  attributes <- list(pageSize = pageSize, page = page)
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("VOCABULARY", configWS)),
                                                            attributes = attributes, wsVersion = 2)
  return(variableResponse)
}
