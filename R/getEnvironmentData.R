#-------------------------------------------------------------------------------
# Program: getEnvironmentData.R
# Objective: functions to get the environment data service from WS1 & WS2
#            * getEnvironment for WS1
#            * getEnvironmentData for WS2
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------


##' @title retrieves the environmental mesures of an experiment from the web service
##'
##' @description Retrieves environmental mesures of an experiment or by dates
##' @param token a token
##' @param variableCategory character, a category of variables
##' @param startDate data > startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param endDate data < startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param variables list of variables for the request (Ex : "wind speed_weather station_meter per second")
##' @param facility place of the experiment (Ex : "http://www.phenome-fppn.fr/m3p/ec3")
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  getEnvironment(aToken$data,page=3,pageSize=100,startDate="2012-02-21",endDate = "2012-03-21")
##'  test<-getEnvironment(aToken$data,
##'        experimentURI="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  test$data
##'  getEnvironment(aToken$data,experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01",
##'  startDate="2012-02-21",endDate="2012-02-15 19:20:30")
##'  getEnvironment(aToken$data,experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01",
##'     facility="http://www.phenome-fppn.fr/m3p/ec3",
##'     variables="wind speed_weather station_meter per second")
##' }
##' @export
getEnvironment <- function(token ,variableCategory ="",startDate = "",endDate = "" ,variables = "",facility = "",
                           experimentURI ="", page = NULL, pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  
  attributes = list(sessionId = token, page = page, pageSize=pageSize)
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
                                           attributes = attributes,verbose=verbose)
  return(environmentResponse)
}


#-------------------------------------------------------------

##' @title getEnvironmentData
##'
##' @description retrieves the environmental data from a variable or a sensor
##' @param token character, a token from getToken function
##' @param variable character, search by the uri of a variable
##' @param startDate character, search from start date (optional)
##' @param endDate character, search to end date (optional)
##' @param sensor character, search by the uri of a sensor (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param dateSortAsc logical, sort date in ascending order if TRUE
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "138.102.159.36:8080/uesAPI/rest/")
##'  aToken <- getToken("guest@phis.fr","guest")
##'  # Retrieve the number of available data
##'  mycount<-getEnvironmentData(token=aToken$data, 
##'       variable = "http://www.phenome-fppn.fr/ues/id/variables/v005")$totalCount
##'  # Retrieve the environmental data
##'  myenvir<-getEnvironmentData(token=aToken$data, pageSize=mycount,
##'          variable = "http://www.phenome-fppn.fr/ues/id/variables/v005")
##'  str(myenvir$data)
##' }
##' @export
getEnvironmentData <- function(token,
                             variable = "",
                             startDate = "",
                             endDate = "",
                             sensor = "",
                             page = NULL,
                             pageSize = NULL,
                             dateSortAsc = TRUE,
                             verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE",configWS)

  attributes <- list(pageSize=pageSize,
                    page = page,
                    Authorization=token)
  if (variable!="")  attributes <- c(attributes, variable = variable)
  if (startDate!="") attributes <- c(attributes, startDate = startDate)
  if (endDate!="")   attributes <- c(attributes, endDate = endDate)
  if (sensor!="")    attributes <- c(attributes, sensor = sensor)

  variableResponse <- getResponseFromWS2(resource = paste0(get("ENVIRONMENTS", configWS)),
                                         attributes = attributes,
                                         verbose = verbose)
  return(variableResponse)
}