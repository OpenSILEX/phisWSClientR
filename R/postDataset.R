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
##' data = data.frame(   
##'   provenanceUri = c(
##'   "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
##'   "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
##'   "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
##'   "http://www.phenome-fppn.fr/test/id/provenance/1569422784579"),
##'   objectUri = c(
##'   "http://www.phenome-fppn.fr/test/2019/o19000076",
##'   "http://www.phenome-fppn.fr/test/2019/o19000076",
##'   "http://www.phenome-fppn.fr/test/2019/o19000076",
##'   "http://www.phenome-fppn.fr/test/2019/o19000076"),
##'   variableUri = c(
##'   "http://www.phenome-fppn.fr/test/id/variables/v027",
##'   "http://www.phenome-fppn.fr/test/id/variables/v027", 
##'   "http://www.phenome-fppn.fr/test/id/variables/v027",
##'   "http://www.phenome-fppn.fr/test/id/variables/v027"),
##'   date = c(
##'   "2017-06-15T10:45:00+0200",
##'   "2017-06-15T10:46:00+0200", 
##'   "2017-06-15T11:47:00+0200",
##'   "2017-06-15T11:48:00+0200"),
##'   value = c(1111.3, 1010, 3030, 4040)
##' )
##'   postDataset(
##'     data
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
