#-------------------------------------------------------------------------------
# Program: postAnnotations.R
# Objective: functions to post a new annotation to the WS2
#            * postAnnotaions
# Authors: Hollebecq Jean-Eudes
# Creation: 11/12/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postAnnotations
##'
##' @description send an annotation to the web service
##' @param creator character, give the creator of the annotation
##' @param motivatedBy character, give the motivation of the annotation, from w3c ontology oa
##' @param bodyValues list, The annotation message
##' @param targets list, the scientific object(s) targeted by this annotation
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
##'   postAnnotations(    
##'     creator = "http://www.opensilex.org/demo/id/agent/marie_dupond",
##'     motivatedBy = "http://www.w3.org/ns/oa#commenting",
##'     bodyValues = list("the object has been observed"),
##'     targets = list("http://www.phenome-fppn.fr/test/2019/o19000074")
##'   )
##'      }
##' @export
postAnnotations <- function(creator, motivatedBy, bodyValues, targets){
  attributes <- list()
  if (creator!="")     attributes <- c(attributes, creator = creator)         else stop("You must provide a creator")
  if (motivatedBy!="") attributes <- c(attributes, motivatedBy = motivatedBy) else stop("You must provide a motivation")
  if (bodyValues!="")  attributes <- c(attributes, bodyValues = bodyValues)   else stop("You must provide a body message")
  if (targets!="")     attributes <- c(attributes, targets = targets)         else stop("You must provide (at least) one scientific object targeted by this annotation")
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("ANNOTATIONS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}