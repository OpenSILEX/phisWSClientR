#-------------------------------------------------------------------------------
# Program: postMethods.R
# Objective: functions to post a new method to the WS2
#            * postMethods
# Authors: Hollebecq Jean-Eudes
# Creation: 26/09/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postMethods
##'
##' @description send a method to the web service
##' @param uri character, give a comment for this method
##' @param comment character, give a comment for this method
##' @param label character, give the label of this method
##' @param ontologiesReferences list, some metadata concerning the ontology of the method. The format of the metadata is a list with every item being a different metadata.  Exemple: list(property = "http://www.w3.org/2004/02/skos/core#closeMatch", object = "http://www.cropontology.org/rdf/CO_715:0000139", seeAlso = "http://www.cropontology.org/ontology/CO_715/")
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
##'   postMethods(
##'   label = "insertionmethod_label",
##'   comment = "comment my method",
##'   ontologiesReferences = list(
##'      property = "http://www.w3.org/2004/02/skos/core#closeMatch",
##'      object = "http://www.cropontology.org/rdf/CO_715:0000139",
##'      seeAlso = "http://www.cropontology.org/ontology/CO_715/")
##'   )
##'      }
##' @export
postMethods <- function(uri = "", label, comment, ontologiesReferences ){
  attributes <- list()
  if (uri!="")                  attributes <- c(attributes, uri = uri)           
  if (label!="")                attributes <- c(attributes, label = label)       else stop("You must provide a label")
  if (comment!="")              attributes <- c(attributes, comment = comment) 
  if (length(ontologiesReferences)!=0) attributes <- c(attributes, ontologiesReferences = ontologiesReferences)  else stop("You must provide a correct property, written is the correct format.  st be one of the following : http://www.w3.org/2008/05/skos#exactMatch, http://www.w3.org/2008/05/skos#closeMatch, http://www.w3.org/2008/05/skos#narrower, http://www.w3.org/2008/05/skos#broader. Given : http://www.w3.org/2008/05/skos/core#closeMatch")
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("METHODS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
