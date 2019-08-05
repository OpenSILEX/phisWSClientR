#-------------------------------------------------------------------------------
# Program: getEvents.R
# Objective: functions to get the event service from WS2
#            * getEvents
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getEvents
##'
##' @description retrieves the events based on search criterion
##' @param uri character, search by the uri of an event (optional)
##' @param type character, search by the type of an event (optional)
##' @param concernsUri character, search by the URI concerned by the event (optional)
##' @param concernsLabel character, search by the label concerned by the event (optional)
##' @param startDate character, search from the start of range (optional)
##' @param endDate character, search to the end of range (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' events <- getEvents(
##'  type = "http://www.opensilex.org/vocabulary/oeev#Fertilization",
##'   pageSize=10)
##' events$data
##' }
##' @export
getEvents <- function(
                      uri = "",
                      type = "",
                      concernsUri = "",
                      concernsLabel = "",
                      startDate = "",
                      endDate = "",
                      page = NULL,
                      pageSize = NULL){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize = pageSize,
                     page = page)
  if (uri!="")            attributes <- c(attributes, uri = uri)
  if (type!="")           attributes <- c(attributes, type = type)
  if (concernsUri!="")    attributes <- c(attributes, concernsUri = concernsUri)
  if (concernsLabel!="")  attributes <- c(attributes, concernsLabel = concernsLabel)
  if (startDate!="")      attributes <- c(attributes, startDate = startDate)
  if (endDate!="")        attributes <- c(attributes, endDate = endDate)
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("EVENTS", configWS)),
                                         attributes = attributes)
  return(variableResponse)
}
