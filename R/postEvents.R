#-------------------------------------------------------------------------------
# Program: postEvents.R
# Objective: functions to post a new event to the WS2
#            * postEvents
# Authors: Hollebecq Jean-Eudes
# Creation: 11/12/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postEvents
##'
##' @description send an event to the web service
##' @param rdfType character, give the rdfType of the event
##' @param concernedItemsUris list, give the scientific object targeted by the event
##' @param date date, the date of the event. Format ISO8601, see example
##' @param properties list, The event properties
##' @param description character, description of this event
##' @param creator character, the creator of this event
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
##'   postEvents(    
##'     rdfType = "http://www.opensilex.org/vocabulary/oeev#MoveFrom",
##'     concernedItemsUris = list(),
##'     date = "2017-09-08T12:00:00+01:00",
##'     properties = list(
##'       rdfType = "http://xmlns.com/foaf/0.1/Agent",
##'       relation = "http://www.opensilex.org/vocabulary/2018#hasContact",
##'       value= "http://www.opensilex.org/demo/id/agent/marie_dupond"
##'     ),
##'     description = "The pest attack lasted 20 minutes",
##'     creator = "http://www.opensilex.org/demo/id/agent/marie_dupond"
##'   )
##'      }
##' @export
postEvents <- function(rdfType, concernedItemsUris, date,properties, description, creator){
  attributes <- list()
  if (length(concernedItemsUris)!=0) attributes <- c(attributes, concernedItemsUris = list(concernedItemsUris)) else stop("You must provide a list of concerned objects")
  if (length(properties)!=0)  attributes <- c(attributes, properties = list(list(properties)))   else stop("You must provide a body message")
  if (rdfType!="")     attributes <- c(attributes, rdfType = rdfType)         else stop("You must provide a rdfType")
  if (date!="")        attributes <- c(attributes, date = date) else stop("You must provide a date")
  if (description!="") attributes <- c(attributes, description = description)         else stop("You must provide the description of this event")
  if (creator!="")     attributes <- c(attributes, creator = creator)         else stop("You must the creator of this event")
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("EVENTS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}