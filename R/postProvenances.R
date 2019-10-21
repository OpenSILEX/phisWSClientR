#-------------------------------------------------------------------------------
# Program: postProvenances.R
# Objective: functions to post a new provenance to the WS2
#            * postProvenance
# Authors: Hollebecq Jean-Eudes
# Creation: 24/09/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postProvenance
##'
##' @description send a provenance to the web service
##' @param comment character, give a comment for this provenance
##' @param label character, give the label of this provenance
##' @param metadata some metadata concerning the provenance. The format of the metadata is a list with every item being a different metadata. Exemple: list(meta1 = "15", meta2 = "owner1")
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
##'   postProvenances(
##'    label = "insertionProvenance_label",
##'    comment = "comment my provenance",
##'    metadata = list(meta1 = "15", meta2 = "owner1")
##'    }
##' @export
postProvenances <- function(label, comment, metadata ){
  attributes <- list()
  if (metadata!="")        attributes <- c(attributes, metadata = metadata)       
  if (label!="")           attributes <- c(attributes, label = label)       else stop("You must provide a label")
  if (comment!="")         attributes <- c(attributes, comment = comment) 
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("PROVENANCES", configWS)),
                                                    attributes = attributes, wsVersion = 2)
  return(Response)
}
