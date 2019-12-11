#-------------------------------------------------------------------------------
# Program: postVectors.R
# Objective: functions to post a new vector to the WS2
#            * postVectors
# Authors: Hollebecq Jean-Eudes
# Creation: 24/09/2019
# Update: 15/11/2019 (JE.H)
#-------------------------------------------------------------------------------

##' @title postVectors
##'
##' @description send a vector to the web service
##' @param uri character, give the URI of the variable measured by the vector
##' @param rdfType character, the rdfType of the vector ex: http://www.opensilex.org/vocabulary/oeso#UAV
##' @param label character, give the label of this vector
##' @param brand character, the brand of the vector
##' @param serialNumber character, the serial number of the vector
##' @param inServiceDate character, date which the vector has been in service. Format "AAAA-MM-DD"
##' @param dateOfPurchase character, date which the vector has been purchased. Format "AAAA-MM-DD"
##' @param personInCharge the mail of the person in charge of this vector
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
##' postVectors(
##'   uri="http://www.opensilex.org/demo/2019/v1909",
##'   rdfType = "http://www.opensilex.org/vocabulary/oeso#UAV",
##'   label = "Vector_label",
##'   brand = "Vector_brand",
##'   serialNumber = "1477",
##'   inServiceDate = "2017-06-15",
##'   dateOfPurchase = "2017-06-15",
##'   personInCharge = "admin@opensilex.org"
##' )

##'    }
##' @export
postVectors <- function(uri, rdfType, label, brand, serialNumber, inServiceDate, dateOfPurchase, personInCharge){
  attributes <- list()
  if (uri!="")            attributes <- c(attributes, uri = uri)           else stop("You must provide a variable")
  if (rdfType!="")        attributes <- c(attributes, rdfType = rdfType)   else stop("You must provide a type of vector")
  if (label!="")          attributes <- c(attributes, label = label)       else stop("You must provide a label")
  if (brand!="")          attributes <- c(attributes, brand = brand)       else stop("You must provide a brand")
  if (serialNumber!="")   attributes <- c(attributes, serialNumber = serialNumber) 
  if (inServiceDate!="")  attributes <- c(attributes, inServiceDate = inServiceDate)   else stop("You must provide a date - and correct format")
  if (dateOfPurchase!="") attributes <- c(attributes, dateOfPurchase = dateOfPurchase) else stop("You must provide a date - and correct format")
  if (personInCharge!="") attributes <- c(attributes, personInCharge = personInCharge) else stop("You must provide a person e-mail in charge of this vector")
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("VECTORS", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
