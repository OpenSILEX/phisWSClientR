#-------------------------------------------------------------------------------
# Program: postData.R
# Objective: functions to post data to the WS2
#            * postData
# Authors: Hollebecq Jean-Eudes, Arnaud Charleroy
# Creation: 26/11/2019
# Update: 10/02/2020
#-------------------------------------------------------------------------------

##' @title postData
##'
##' @description send a data to the web service
##' @param data a data.frame containing the following columns :
##'        provenanceUri character, the URI of the provenance of the data. Accessed by from \code{\link{getProvenances}}
##'        objectUri character, give the uri of the scientific object concerned
##'        variableUri character, give the uri of the variable concerned
##'        date character, date of the measurement. Follow ISO 8601 format, see example
##'        value character, the value of the measurement

##' @return Response object
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
##' postData(
##'   data
##' )
##' }
##' @export
postData <- function(data){
  if(sum(c("provenanceUri", "objectUri", "variableUri", "date", "value")%in%names(data))!=5 ) stop(" You should name the columns after the arguments provenanceUri, objectUri, variableUri, date, value")
  # transform data into R6 object 
  dataFormatted <- list()
  for(numData in 1:nrow(data)){
    dataObject <- DataPostDTO$new(
      provenanceUri =  data[numData,]$provenanceUri,
      objectUri = data[numData,]$objectUri,
      variableUri = data[numData,]$variableUri,
      date = data[numData,]$date,
      value = ObjectDTO$new(data[numData,]$value))
    dataFormatted <- append(dataFormatted,dataObject)
  }            
  # insert R6 object 
  dataApi <- DataApi$new()
  response <- dataApi$post_data(dataFormatted)
  return(response)
}

##' @title postSensorData
##'
##' @description same as postData for sensor
##' @param data a data.frame containing the following columns :
##'        provenanceUri character, the URI of the provenance of the data. Accessed by from \code{\link{getProvenances}}
##'        variableUri character, give the uri of the variable concerned
##'        date character, date of the measurement. Follow ISO 8601 format, see example
##'        value character, the value of the measurement

##' @return Response object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToPHISWS(apiID="ws_private", url =
##' "http://www.opensilex.org/openSilexAPI/rest/",
##' username="guest@opensilex.org", password="guest") data = data.frame(
##' provenanceUri = c(
##' "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
##' "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
##' "http://www.phenome-fppn.fr/test/id/provenance/1569422784579",
##' "http://www.phenome-fppn.fr/test/id/provenance/1569422784579"), variableUri
##' = c( "http://www.phenome-fppn.fr/test/id/variables/v027",
##' "http://www.phenome-fppn.fr/test/id/variables/v027",
##' "http://www.phenome-fppn.fr/test/id/variables/v027",
##' "http://www.phenome-fppn.fr/test/id/variables/v027"), date = c(
##' "2017-06-15T10:45:00+0200", "2017-06-15T10:46:00+0200",
##' "2017-06-15T11:47:00+0200", "2017-06-15T11:48:00+0200"), value = c(1111.3,
##' 1010, 3030, 4040) ) postSensorData( data )
##'
##' }
##' @export
postSensorData <- function(data ){
  if(sum(c("provenanceUri", "variableUri", "date", "value")%in%names(data))!=4 ) stop(" You should name the columns after the arguments provenanceUri, variableUri, date, value")
   data[,"objectUri"] <- NULL
   print(head(data))
  Response <- opensilexWSClientR::postResponseFromWS(resource = paste0(get("DATA", configWS)),
                                                     attributes = data, wsVersion = 2)
  return(Response)
}

