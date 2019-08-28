#-------------------------------------------------------------------------------
# Program: getEnvironmentData.R
# Objective: functions to get the environment data service from WS1 & WS2
#            * getEnvironment for WS1
#            * getEnvironmentData for WS2
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
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
##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @examples
##' \donttest{
##'  connectToOpenSILEXWS(apiID="ws_public", 
##'  username = "guestphis@supagro.inra.fr",
##'  password = "guestphis")
##'  getEnvironment(page=3,
##'                 pageSize=100,
##'                 startDate="2017-06-29",
##'                 endDate = "2017-06-16")
##'  test<-getEnvironment(experimentURI="http://www.phenome-fppn.fr/m3p/ARCH2017-03-30")
##'  test$data
##'  getEnvironment( experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-03-30",
##'                  startDate="2017-06-16",
##'                  endDate="2017-06-29")
##'  getEnvironment(experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-03-30",
##'     facility="http://www.phenome-fppn.fr/m3p/es2",
##'     variables="wind speed_weather station_meter per second")
##' }
##' @export
getEnvironment <- function(variableCategory ="",startDate = "",endDate = "" ,variables = "",facility = "",
                           experimentURI ="", page = NULL, pageSize = NULL){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  
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
  environmentResponse <- getResponseFromWS(resource = get("ENVIRONMENT",configWS),
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
##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToOpenSILEXWS(apiID="ws_private",
##'   url = "http://www.opensilex.org/openSilexAPI/rest/",
##'   "guestphis@opensilex.org","guest")
##'  # Retrieve the number of available data
##'  mycount <- getEnvironmentData(variable = "http://www.opensilex.org/demo/id/variables/v004")$totalCount
##'  # Retrieve the environmental data
##'  myenvir <- getEnvironmentData(
##'               pageSize=mycount,
##'               variable = "http://www.opensilex.org/demo/id/variables/v004")
##'  myenvir <- getEnvironmentData(
##'                pageSize=mycount,
##'                variable = "http://www.opensilex.org/demo/id/variables/v004", 
##'                startDate="2017-06-15T10:51:00+0200",
##'                endDate="2017-06-17T10:51:00+0200")
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
  if (is.null(page)) page <- get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE",configWS)
  
  attributes <- list(pageSize=pageSize,
                     page = page)
  if (variable!="")  attributes <- c(attributes, variable = variable)
  if (startDate!="") attributes <- c(attributes, startDate = startDate)
  if (endDate!="")   attributes <- c(attributes, endDate = endDate)
  if (sensor!="")    attributes <- c(attributes, sensor = sensor)
  
  variableResponse <- getResponseFromWS(resource = paste0(get("ENVIRONMENTS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}