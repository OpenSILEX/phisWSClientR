#-------------------------------------------------------------------------------
# Program: getAnnotations.R
# Objective: functions to get the annotations service from WS2
#            * getAnnotations
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 06/09/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getAnnotations
##'
##' @description retrieves the annotation based on search criterion
##' @param uri character, search by the uri of an annotation (optional)
##' @param creator character, search by the uri of th creator of the annotations (optional)
##' @param motivatedBy character, search by the motivation to create the annotation ??? (optional)
##' @param comment character, search by comment on the annotation (optional)
##' @param target character, search by the agronomical object uri that is targeted by an annotation (optional)
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
##' annotations<-getAnnotations(comment = "Ustilago maydis infection" , 
##'                            pageSize=10)
##' str(annotations$data)
##' }
##' @export
getAnnotations <- function(
                           uri = "",
                           creator = "",
                           motivatedBy = "",
                           comment = "",
                           target = "",
                           page = NULL,
                           pageSize = NULL){
  
  attributes <- list(pageSize=pageSize, page = page)
  
  if (uri!="")         attributes <- c(attributes, uri = uri)
  if (creator!="")     attributes <- c(attributes, creator = creator)
  if (motivatedBy!="") attributes <- c(attributes, motivatedBy = motivatedBy)
  if (comment!="")     attributes <- c(attributes, comment = comment)
  if (target!="")      attributes <- c(attributes, target = target)
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("ANNOTATIONS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}