#-------------------------------------------------------------------------------
# Program: getPhenotypeData.R
# Objective: functions to get the phenotypes service from WS2
#            * getPhenotypeData
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getPhenotypeData
##'
##' @description Retrieves the phenotype measures data from a variable, an experiment or a sensor
##' @param token character, a token from getToken function
##' @param variable character, search by the uri of a variable (optional)
##' @param experiment character, search by the uri of an experiment (optional)
##' @param agroObject character, search by the uri of an agronomical object (optional)
##' @param startDate character, search from start date (optional)
##' @param endDate character, search to end date (optional)
##' @param sensor character, search by the uri of a sensor (optional)
##' @param incertitude character, search by incertitude ???????????????????? (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest")
##'  aToken = getToken("guest@opensilex.org","guest")
##'  phenodata <- getPhenotypeData(aToken$data, variable = "http://www.opensilex.org/demo/id/variables/v010")
##'  phenodata <- getPhenotypeData(aToken$data, variable = "http://www.opensilex.org/demo/id/variables/v010", startDate="2012-02-21", endDate="2012-02-15 19:20:30")
##'  phenodata$data
##' }
##' @export
getPhenotypeData <- function(token,
                         variable = "",
                         experiment = "",
                         agroObject = "",
                         startDate = "",
                         endDate = "",
                         sensor = "",
                         incertitude = "",
                         page = NULL,
                         pageSize = NULL,
                         verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE",configWS)

  attributes <- list(pageSize=pageSize,
                    page = page,
                    Authorization=token)
  if (experiment!="")  attributes <- c(attributes, experiment = experiment)
  if (variable!="")    attributes <- c(attributes, variable = variable)
  if (agroObject!="")  attributes <- c(attributes, agroObject = agroObject)
  if (startDate!="")   attributes <- c(attributes, startDate = startDate)
  if (endDate!="")     attributes <- c(attributes, endDate = endDate)
  if (sensor!="")      attributes <- c(attributes, sensor = sensor)
  if (incertitude!="") attributes <- c(attributes, incertitude = incertitude)

  variableResponse <- getResponseFromWS2(resource = paste0(get("DATASETS", configWS)),
                                         attributes = attributes,
                                         verbose = verbose)
  outputData <- data.frame()
  jsonData <- data.frame(variableResponse$data[[1]])
  AO=jsonData$agronomicalObject
  for(l in 1:length(jsonData$data)){
    AgronomicalObject <- AO[l]
    tmp_dat <- jsonData$data[[l]]
    AOl <- data.frame(AgronomicalObject=AgronomicalObject, tmp_dat)
    outputData=rbind(outputData, AOl)
  }
  variableResponse$data <- outputData
  return(variableResponse)
}