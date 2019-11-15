#-------------------------------------------------------------------------------
# Program: postVariables.R
# Objective: functions to post a new variable to the WS2
#            * postVariables
# Authors: Hollebecq Jean-Eudes
# Creation: 26/09/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postVariables
##'
##' @description send a variable to the web service
##' @param uri character, give an for this variable. If this is empty, the uri will me automatically generated.
##' @param comment character, give a comment for this variable
##' @param label character, give the label of this variable
##' @param ontologiesReferences some metadata concerning the ontology of the variable.  The format of the metadata is a list with every item being a different metadata.  Exemple: list(property = "http://www.w3.org/2004/02/skos/core#closeMatch", object = "http://www.cropontology.org/rdf/CO_715:0000139", seeAlso = "http://www.cropontology.org/ontology/CO_715/")
##' @param trait the uri of the trait composing the variable 
##' @param method the uri of the method composing the 
##' @param unit the uri of the unit composing the 
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
##'   postVariables(
##'    label = "insertionvariable_label",
##'    comment = "comment my variable",
##'    ontologiesReferences = list(property = "http://www.w3.org/2004/02/skos/core#closeMatch",
##'                    object = "http://www.cropontology.org/rdf/CO_715:0000139",
##'                    seeAlso = "http://www.cropontology.org/ontology/CO_715/"),
##'   trait = "http://www.phenome-fppn.fr/test/id/traits/t008",
##'   method = "http://www.phenome-fppn.fr/test/id/methods/m008",
##'   unit = "http://www.phenome-fppn.fr/test/id/units/u008")
##'   }
##' @export
postVariables <- function(uri, label, comment, ontologiesReferences, trait, method, unit){
  attributes <- list()
  if (uri!="")                  attributes <- c(attributes, uri = uri)           
  if (label!="")                attributes <- c(attributes, label = label)       else stop("You must provide a label")
  if (comment!="")              attributes <- c(attributes, comment = comment) 
  if (ontologiesReferences!="") attributes <- c(attributes, ontologiesReferences = ontologiesReferences)  else stop("You must provide a correct property, written is the correct format.  st be one of the following : http://www.w3.org/2008/05/skos#exactMatch, http://www.w3.org/2008/05/skos#closeMatch, http://www.w3.org/2008/05/skos#narrower, http://www.w3.org/2008/05/skos#broader. Given : http://www.w3.org/2008/05/skos/core#closeMatch")
  if (trait!="")                attributes <- c(attributes, trait = trait)   else stop("You must provide a correct trait")
  if (method!="")               attributes <- c(attributes, method = method) else stop("You must provide a correct method")
  if (unit!="")                 attributes <- c(attributes, unit = unit)     else stop("You must provide a correct unit")
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("VARIABLES", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
