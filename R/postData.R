#-------------------------------------------------------------------------------
# Program: postData.R
# Objective: functions to post data to the WS2
#            * postData
# Authors: Hollebecq Jean-Eudes
# Creation: 26/11/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postData
##'
##' @description send a data to the web service
##' @param provenanceUri character, the URI of the provenance of the data. Accessed by from \code{\link{getProvenances}}
##' @param objectUri character, give the uri of the scientific object concerned
##' @param variableUri character, give the uri of the variable concerned
##' @param date character, date of the measurement. Follow ISO 8601 format, see example
##' @param value character, the value of the measurement

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
##'   postData(
##' provenanceUri = "http://www.opensilex.org/demo/2018/pv181515071552",
##' objectUri = "http://www.opensilex.org/demo/2018/o18000076",
##' variableUri = "http://www.opensilex.org/demo/id/variable/v0000001",
##' date = "2017-06-15T10:51:00+0200",
##' value = 1.3)
##'    }
##' @export
postData <- function(provenanceUri, objectUri, variableUri, date, value){
  attributes <- list()
  if (provenanceUri!="")    attributes <- c(attributes, provenanceUri = provenanceUri) else stop("You must provide a provenance for the data")
  if (objectUri!="")      attributes <- c(attributes, objectUri = objectUri)           else stop("You must provide an object to the data")
  if (variableUri!="")   attributes <- c(attributes, variableUri = variableUri)        else stop("You must provide a variable")
  if (date!="") attributes <- c(attributes, date = date)        else stop("You must provide a date. In correct format (ISO 8601)")
  if (value!="")   attributes <- c(attributes, value = value)   else stop("You must provide a value")
  
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("DATA", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
