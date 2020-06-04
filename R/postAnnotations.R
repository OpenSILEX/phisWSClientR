#-------------------------------------------------------------------------------
# Program: postAnnotations.R
# Objective: functions to post a new annotation to the WS2
#            * postAnnotations
# Authors: Hollebecq Jean-Eudes
# Creation: 11/12/2019
# Update: 04/06/2020
#-------------------------------------------------------------------------------

##' @title postAnnotations2
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
##' @noRd
postAnnotations2 <- function(creator, motivatedBy, bodyValues, targets){
  attributes <- list()
  if (creator!="")     attributes <- c(attributes, creator = creator)         else stop("You must provide a creator")
  if (motivatedBy!="") attributes <- c(attributes, motivatedBy = motivatedBy) else stop("You must provide a motivation")
  if (bodyValues!="")  attributes <- c(attributes, bodyValues = list(bodyValues))   else stop("You must provide a body message")
  if (targets!="")     attributes <- c(attributes, targets = list(targets))         else stop("You must provide (at least) one scientific object targeted by this annotation")
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("ANNOTATIONS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}


#-------------------------------------------------------------------------------
# Program: postAnnotations.R
# Objective: functions to post a new annotation to the WS2
#            * postAnnotaions
# Authors: Arnaud Charleroy
# Creation: 04/06/2020
# Update:
#-------------------------------------------------------------------------------

##' @title postAnnotations
##'
##' @description send a list of annotation to the web service
##' @import phisWSClientRTools
##' @param newAnnotations list, AnnotationDTO list 
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
##'                
##'  newAnnotation <- AnnotationDTO$new(
##'    creator = "http://www.opensilex.org/demo/id/agent/marie_dupond",
##'    targets = list("http://www.phenome-fppn.fr/test/2019/o19000074"),
##'    motivatedBy = "http://www.w3.org/ns/oa#describing",
##'    bodyValues =  list("the object has been observed")
##'  )
##' 
##'  response <- postAnnotations(list(newAnnotation))
##'  response$success
##'  response$metadata
##'  }
##' @export
postAnnotations <- function(newAnnotations){
  annoService <- AnnotationsApi$new() 
  Response <- annoService$post1(newAnnotations)
  return(Response)
}