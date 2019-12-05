#-------------------------------------------------------------------------------
# Program: postDataset.R
# Objective: functions to post data to the WS2
#            * postDataset
# Authors: Hollebecq Jean-Eudes
# Creation: 26/11/2019
# Update:
#-------------------------------------------------------------------------------

##' @title postDataset
##'
##' @description send a dataset to the web service. This extend the \code{\link{postData}} function to a whole dataset
##' @param data data.frame, a dataset with columns names after the arguments of  \code{\link{postData}} function

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
##'   postDataset(
##'   
##'   )
##'    }
##' @export
postDataset <- function(data){
  if(sum(c("provenanceUri", "objectUri", "variableUri", "date", "value")%in%names(data))!=5 ) stop(" You should name the columns after the arguments provenanceUri, objectUri, variableUri, date, value")
  
  attributes <- data[, c("provenanceUri", "objectUri", "variableUri", "date", "value")]
  # attributes <- list()
  # if (provenanceUri!="")    attributes <- c(attributes, provenanceUri = provenanceUri) else stop("You must provide a provenance for the data")
  # if (objectUri!="")      attributes <- c(attributes, objectUri = objectUri)           else stop("You must provide an object to the data")
  # if (variableUri!="")   attributes <- c(attributes, variableUri = variableUri)        else stop("You must provide a variable")
  # if (date!="") attributes <- c(attributes, date = date)        else stop("You must provide a date. In correct format (ISO 8601)")
  # if (value!="")   attributes <- c(attributes, value = value)   else stop("You must provide a value")
  # 
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("DATA", configWS)),
                                                     attributes = attributes, wsVersion = 2)
  return(Response)
}
