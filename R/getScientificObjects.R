#-------------------------------------------------------------------------------
# Program: getScientificObjects.R
# Objective: functions to get the scientific objects service from WS2
#            * getScientificObjects
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
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
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##'  scientificObjects <- getScientificObjects(
##'                               uri = "http://www.opensilex.org/demo/2018/o18000076")
##'  scientificObjects$data
##' }
##' @export
getScientificObjects <- function(
                                 uri = "",
                                 experiment = "",
                                 alias = "",
                                 rdfType = "",
                                 page = NULL,
                                 pageSize = NULL,
                                 verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize=pageSize,
                     page = page)
  if (uri!="")        attributes <- c(attributes, uri = uri)
  if (experiment!="") attributes <- c(attributes, experiment = experiment)
  if (alias!="")      attributes <- c(attributes, alias = alias)
  if (rdfType!="")    attributes <- c(attributes, rdfType = rdfType)
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("SCIENTIFIC_OBJECTS", configWS)),
                                         attributes = attributes)
  return(variableResponse)
}