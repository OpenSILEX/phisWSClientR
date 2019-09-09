#-------------------------------------------------------------------------------
# Program: getProvenances.R
# Objective: functions to get the provenance service from WS2
#            * getProvenance
# Authors: Hollebecq Jean-Eudes
# Creation: 21/06/2019
# Update: 06/09/2019 (I.Sanchez) - 09/09/2019 (A.Charleroy)
#-------------------------------------------------------------------------------

##' @title getProvenance
##'
##' @description retrieves the provenance based on search criterion
##' @param uri character, search by the uri of an provenance (optional)
##' @param label character, search by the label of an provenance (optional)
##' @param comment character, search by the comment in the provenance
##' @param jsonValueFilter list, json object (as R list), search by the json value
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
##' provenances <- getProvenances(
##'    uri = "http://www.opensilex.org/demo/2018/pv181515071552",
##'    pageSize = 10)
##' provenances <- getProvenances(
##'                   label = "MAU17-PG_NDVI_PUBLI",
##'                   comment = "NDVI",
##'                   pageSize = 10)
##' provenances <- getProvenances(
##'                   label = "PROV2019-LEAF",
##'                   jsonValueFilter =
##'                   list(SensingDevice = "http://www.opensilex.org/demo/s001",
##'                   Vector = "http://www.opensilex.org/demo/v001")
##'                   pageSize = 10)
##' provenances$data
##' }
##' @export
getProvenances <- function(uri = "",
                      label = "",
                      comment = "",
                      jsonValueFilter = NULL,
                      page = NULL,
                      pageSize = NULL){

  attributes <- list(pageSize = pageSize,page = page)
  if (uri!="")             attributes <- c(attributes, uri = uri)
  if (label!="")           attributes <- c(attributes, label = label)
  if (comment!="")         attributes <- c(attributes, comment = comment)
  # Manage json filter
  if (!is.null(jsonValueFilter) ){
    if(is.list(jsonValueFilter)){
      attributes <- c(attributes, jsonValueFilter = jsonlite::toJSON(list(sensor="test"),auto_unbox = TRUE))
    }else{
      logging::logwarn("jsonValueFilter is not a list. This parameter will not be taken in account
                       for this call.")
    }
  }
  
  Response <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("PROVENANCES", configWS)),
                                                    attributes = attributes, wsVersion = 2)
  return(Response)
}
