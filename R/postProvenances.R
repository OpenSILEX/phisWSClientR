#-------------------------------------------------------------------------------
# Program: postProvenances.R
# Objective: functions to post a new provenance to the WS2
#            * postProvenance
# Authors: Hollebecq Jean-Eudes, Arnaud Charleroy
# Creation: 24/09/2019
# Update:   04/06/2020
#-------------------------------------------------------------------------------

##' @title postProvenance
##'
##' @description send a provenance to the web service
##' @param provs list of ProvenanceDTO object comment character, 
##'              give a comment for this provenance label character, 
##'              give the label of this provenance 
##'              some metadata concerning the provenance. 
##'              The format of the metadata is a list with every item being a different metadata. 
##'              Exemple: ObjectDTO$new(list(meta1 = "15", meta2 = "owner1")
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'     connectToPHISWS(apiID="ws_private",
##'                url = "http://www.opensilex.org/openSilexAPI/rest/",
##'                username="guest@opensilex.org",
##'                password="guest")
##'     prov = ProvenanceDTO$new(
##'       label = "insertionProvenance_label",
##'       comment = "comment my provenance",
##'       experiments = list("http://opensilex.org/experiment/maug")),
##'       metadata = ObjectDTO$new(list(meta1 = "15", meta2 = "owner1"))
##'     )
##'     response <- postProvenances(list(prov))
##'     response$success
##'     response$metadata
##'    }
##' @export
postProvenances <- function(provs){
  provService <- ProvenancesApi$new() 
  Response <- provService$post4(
    provs
  )
  return(Response)
}
