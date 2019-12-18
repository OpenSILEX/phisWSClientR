#-------------------------------------------------------------------------------
# Program: postScientificObjects.R
# Objective: functions to post a new scientific object to the WS2
#            * postSensors
# Authors: Hollebecq Jean-Eudes
# Creation: 24/09/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postScientificObjects
##'
##' @description send a scientific object to the web service
##' @param rdfType character, the rdfType of the scientific object ex: http://www.opensilex.org/vocabulary/oeso#Plot
##' @param geometry character, give the geometry of this scientific object. For example a plot can be : "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))"
##' @param experiment character, uri of the experiment of the scientific object
##' @param isPartOf character, a scientific object the scientific object is part of ???
##' @param properties list, a list for the properties. 
##' Note: The object can also be a data.frame with the 3 named column rdfType, relation and value. See example.
##' @section Important:
##' The properties list must contain the rdfs:label block ; also known as alias.
##' See example.
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
##' postScientificObjects(
##'   rdfType = "http://www.opensilex.org/vocabulary/oeso#Plot",
##'   experiment="http://www.opensilex.org/demo/DIA2017-1",
##'   properties=list(
##'     list(
##'       rdfType = "http://www.opensilex.org/vocabulary/oeso#Species",
##'       relation = "http://www.opensilex.org/vocabulary/oeso#hasSpecies",
##'       value = "http://www.phenome-fppn.fr/id/species/triticumaestivum"),
##'     list(
##'       rdfType = NA,
##'       relation ="http://www.w3.org/2000/01/rdf-schema#label",
##'       value ="objectAlias")
##' )
##' )
##'    }
##' @export
postScientificObjects <- function(rdfType, experiment, geometry = "", isPartOf = "", properties){
  attributes <- list()
  if (rdfType!="")    attributes <- c(attributes, rdfType = rdfType)       else stop("You must provide a type of scientific object")
  if (experiment!="") attributes <- c(attributes, experiment = experiment) else stop("You must provide a experiment")
  if (geometry!="")   attributes <- c(attributes, geometry = geometry)    
  if (isPartOf!="")   attributes <- c(attributes, isPartOf = isPartOf)
  if (length(properties)!=0)  attributes <- c(attributes, properties = list(properties))
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("SCIENTIFIC_OBJECTS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
