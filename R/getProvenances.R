#-------------------------------------------------------------------------------
# Program: getProvenances.R
# Objective: functions to get the provenance service from WS2
#            * getProvenance
# Authors: Hollebecq Jean-Eudes
# Creation: 21/06/2019
# Update: 
#-------------------------------------------------------------------------------

##' @title getProvenance
##'
##' @description retrieves the provenance based on search criterion
##' @param uri character, search by the uri of an provenance (optional)
##' @param label character, search by the label of an provenance (optional)
##' @param comment character, search by the comment in the provenance
##' @param jsonValueFilter json object (as character), search by the json value
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' provenances <- getProvenances(
##'  uri = "http://www.opensilex.org/demo/2018/pv181515071552",
##'   pageSize=10)
##' provenances <- getProvenances(
##'  label ="PROV2019-LEAF",
##'   pageSize=10)
##' provenances$data
##' }
##' @export
getProvenances <- function(
                      uri = "",
                      label = "",
                      comment = "",
                      jsonValueFilter = "",
                      page = NULL,
                      pageSize = NULL,
                      verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize = pageSize,
                     page = page)
  if (uri!="")             attributes <- c(attributes, uri = uri)
  if (label!="")           attributes <- c(attributes, label = label)
  if (comment!="")         attributes <- c(attributes, comment = comment)
  if (jsonValueFilter!="") attributes <- c(attributes, jsonValueFilter = jsonValueFilter)
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("PROVENANCES", configWS)),
                                         attributes = attributes)
  return(variableResponse)
}
