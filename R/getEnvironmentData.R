#-------------------------------------------------------------------------------
# Program: getEnvironmentData.R
# Objective: functions to get the environment data service from WS1 & WS2
#            * getEnvironment for WS1
#            * getEnvironmentData for WS2
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 06/01/2020 (by I.Sanchez)
#-------------------------------------------------------------------------------


##' @title retrieves the environmental mesures of an experiment from the web service
##'
##' @description Retrieves environmental mesures of an experiment or by dates
##' @param variableCategory character, a category of variables
##' @param startDate data > startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param endDate data < startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param variables list of variables for the request (Ex : "wind speed_weather station_meter per second")
##' @param facility place of the experiment (Ex : "http://www.phenome-fppn.fr/m3p/ec3")
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##'  test<-getEnvironment(experimentURI="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23")
##'  test$data
##'  
##'  getEnvironment( experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23",
##'                  startDate="2017-06-28",
##'                  endDate="2017-06-28")
##'                  
##'  getEnvironment(experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23",
##'     facility="http://www.phenome-fppn.fr/m3p/es2",
##'     variables="net irradiance_calculated variable_watt.m2")
##' }
##' @export
getEnvironment <- function(variableCategory ="",startDate = "",endDate = "" ,variables = "",facility = "",
                           experimentURI ="", page = NULL, pageSize = NULL){
  
  attributes = list(page = page, pageSize=pageSize)
  if (startDate != ""){
    attributes <- c(attributes, startDate = startDate)
  }
  if (endDate != ""){
    attributes <- c(attributes, endDate = endDate)
  }
  if (facility != ""){
    attributes <- c(attributes, facility = facility)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }
  if (variableCategory != ""){
    attributes <- c(attributes, variableCategory = variableCategory)
  }
  if (variables != ""){
    attributes <- c(attributes, variables = utils::URLencode(variables))
  }
  environmentResponse <- opensilexWSClientR::getResponseFromWS(resource = get("ENVIRONMENT",configWS),
                                           attributes = attributes, wsVersion = 1)
  return(environmentResponse)
}


#-------------------------------------------------------------

##' @title getEnvironmentData
##'
##' @description retrieves the environmental data from a variable or a sensor
##' @param variable character, search by the uri of a variable. You can access the list of variables through the \code{\link{getVariables2}} function.
##' @param startDate character, search from start date (optional)
##' @param endDate character, search to end date (optional)
##' @param sensor character, search by the uri of a sensor (optional). You can access the list of sensors through the \code{\link{getSensors}} function.
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param dateSortAsc logical, sort date in ascending order if TRUE
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
##'  # Retrieve the number of available data
##'  environmentData <-getEnvironmentData(
##'                       variable = "http://www.opensilex.org/demo/id/variables/v004"
##'                    )
##'  mycount <-environmentData$totalCount
##'  # Retrieve the environmental data
##'  myenvir <-getEnvironmentData(
##'               pageSize=mycount,
##'               variable = "http://www.opensilex.org/demo/id/variables/v004")
##'  myenvir <-getEnvironmentData(
##'               pageSize=mycount,
##'               variable = "http://www.opensilex.org/demo/id/variables/v004", 
##'               startDate="2017-06-15T10:51:00+0200",
##'               endDate="2017-06-17T10:51:00+0200")
##'  str(myenvir$data)
##'  head(myenvir$data)
##' }
##' @export
getEnvironmentData <- function(
                               variable = "",
                               startDate = "",
                               endDate = "",
                               sensor = "",
                               page = NULL,
                               pageSize = NULL,
                               dateSortAsc = TRUE){
  
  attributes <- list(pageSize=pageSize, page = page)
  if (variable!="")  attributes <- c(attributes, variable = variable)
  if (startDate!="") attributes <- c(attributes, startDate = startDate)
  if (endDate!="")   attributes <- c(attributes, endDate = endDate)
  if (sensor!="")    attributes <- c(attributes, sensor = sensor)
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("ENVIRONMENTS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}