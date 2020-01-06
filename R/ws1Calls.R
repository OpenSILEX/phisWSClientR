#-------------------------------------------------------------------------------
# Program: ws1Calls.R
# Objective: functions called by the user on the web service Phenomeapi
# Author: A. Charleroy
# Creation: 12/08/2016
# Update: 06/09/2019 (by I.Sanchez) - 30/10/2016 (by  A. Charleroy)
#-------------------------------------------------------------------------------

##' @title getProjects retrieves the list of projects from the web service
##'
##' @description Retrieves the list of projects in the WS
##' @param projectName character, Name of the project to search
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##'  getProjects()
##' }
##' @export
getProjects<-function(projectName= "",page=NULL,pageSize=NULL){
  
  attributes = list(page = page, pageSize = pageSize)
  if (projectName != ""){
    attributes <- c(attributes, projectName = projectName)
  }
  projectResponse <- opensilexWSClientR::getResponseFromWS(resource = get("PROJECTS",configWS),
                                                           attributes=attributes, wsVersion = 1)
  return(projectResponse)
}


##' @title retrieves the context of plant linked to an experiment from the web service
##'
##' @description Retrieves context of plant linked to an experiment
##' @param plantAlias character, an alias of plant
##' @param experimentURI character, URI of the experiment
##' @param germplasmURI character, filter by genotype
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##'  plantes<-getPlants(experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23")
##' }
##' @export
getPlants <- function(plantAlias ="", experimentURI = "", germplasmURI = "" ,
                      page = NULL,pageSize = NULL){
  
  attributes = list(page = page, pageSize = pageSize)

  if (plantAlias != ""){
    attributes <- c(attributes, plantAlias = plantAlias)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }
  if (germplasmURI != ""){
    attributes <- c(attributes, germplasmURI = germplasmURI)
  }
  plantsResponse<-opensilexWSClientR::getResponseFromWS(resource = get("PLANTS",configWS), 
                                                        attributes = attributes, wsVersion=1)
  return(plantsResponse)
}

##' @title getPlantsContextByID
##'
##' @description Retrieves the contect of a plant
##' @param plantURI character, a URI identifier of a plant
##' @param experimentURI character, URI of the experiment
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @examples
##' \donttest{
##' # not run (is an internal function!!!)
##' connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##' test<-getPlantsContextByID(
##'            plantURI="http://www.phenome-fppn.fr/m3p/arch/2011/c11005809",
##'           experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23")
##' test$data
##' }
##' @keywords internal
getPlantsContextByID<-function(plantURI ="",experimentURI="",page = NULL,
                               pageSize = NULL){
  
  attributes = list(page = page, pageSize = pageSize)
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }

  if (plantURI  == ""){
    stop("no plantURI selected")
  } else {
    # AC 28/10/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    plantURIEncoded = utils::URLencode(plantURI,reserved = TRUE)
    plantByIDResponse<-opensilexWSClientR::getResponseFromWS(resource = get("PLANTS",configWS),
                                        paramPath=plantURIEncoded,attributes=attributes)
    return(plantByIDResponse)
  }
}


##' @title retrieves the environmental mesures of a plant from the web service
##'
##' @description Retrieves environmental mesures of a plant or by dates
##' @param plantURI character, plant URI
##' @param variableCategory character, a category of variables
##' @param startDate character, data > startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param endDate character, data < startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param variables character, list of variables for the request (Ex: "wind speed_weather station_meter per second")
##' @param facility character, place of the experiment (Ex: "http://www.phenome-fppn.fr/m3p/ec3")
##' @param experimentURI character, URI of the experiment
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @examples
##' \donttest{
##' # not run (is an internal function!!!)
##' connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##' myplant<-getPlantEnvironment(
##'        plantURI="http://www.phenome-fppn.fr/m3p/arch/2011/c11005809",
##'        experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2017-11-23")
##' myplant$data
##' }
##' @keywords internal
getPlantEnvironment <- function(plantURI ="",variableCategory ="",startDate = "",endDate = "",
                                variables = "",facility = "", experimentURI ="",
                                page = NULL,pageSize = NULL){
  
  attributes = list(page = page, pageSize = pageSize)
  if (plantURI  == ""){
    stop("no plantURI given")
  } else {
    attributes = list(page = page, pageSize = pageSize)
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
    }else{
      stop("no experimentURI given")
    }
    if (variableCategory != ""){
      attributes <- c(attributes, variableCategory = variableCategory)
    }
    if (variables != ""){
      attributes <- c(attributes, variables = utils::URLencode(variables))
    }
    # AC 28/10/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    plantURIEncoded =  utils::URLencode(plantURI, reserved = TRUE)
    plantEnvironmentResponse<-opensilexWSClientR::getResponseFromWS(resource = get("PLANTS",configWS),
                                                paramPath = paste0(plantURIEncoded,"/environment"),
                                                attributes =  attributes)
    return(plantEnvironmentResponse)
  }
}

##' @title retrieves the images analysis data from the web service
##'
##' @description Retrieves data from image analysis
##' @param experimentURI URI of the experiment
##' @param variablesName list, variable names of images analysis (ex : "objectSumArea")
##' @param labelView character, label view of an image
##' @param provider character, provider of data
##' @param date character, data for one day (format: YYYY-MM-DD)
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
##' myExp<-"http://www.phenome-fppn.fr/m3p/ARCH2017-03-30"
##' getVariablesByCategory(category ="imagery",experimentURI=myExp)$data$name
##' myImages<-getImagesAnalysis(experimentURI = myExp,
##'            variablesName = list("objectSumArea"),pageSize = 100000)
##' str(myImages$data)
##' }
##' @export
getImagesAnalysis <- function(experimentURI ="", variablesName = list(),
                              labelView ="", provider = "", date = "",
                              page = NULL,pageSize = NULL){
  
  attributes = list(page = page, pageSize = pageSize)
  if (date != ""){
    attributes <- c(attributes, date = date)
  }
  if (is.list(variablesName)){
    if (length(variablesName) != 0){
      attributes <- c(attributes, variablesName = paste(variablesName, collapse = ","))
    }
  } else {
    stop("variablesName is not a list")
  }
  if (labelView != ""){
    attributes <- c(attributes, labelView = labelView)
  }
  if (provider != ""){
    attributes <- c(attributes, provider = provider)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }
  imagesAnalysisResponse <- opensilexWSClientR::getResponseFromWS(resource = get("IMAGESANALYSIS",configWS),
                                              attributes = attributes, wsVersion =1)
  return(imagesAnalysisResponse)
}


##' @title retrieves the irrigation data from the web service
##'
##' @description Retrieves irrigation data
##' @param experimentURI character, URI of the experiment
##' @param variablesName list, variable names of images analysis
##' @param provider character, provider of data
##' @param date character, data for one day (format: YYYY-MM-DD)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @return WSResponse object
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##'  mywater<-getWatering(
##'          experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2017-11-23",
##'          variablesName = list("weightBefore"),pageSize=100000)
##'  head(mywater$data)
##' }
##' @export
getWatering <- function(experimentURI ="", variablesName = list(), provider = "", 
                        date = "",page = NULL,pageSize = NULL){

  attributes = list(page = page, pageSize = pageSize)
  if (date != ""){
    attributes <- c(attributes, date = date)
  }
  if (is.list(variablesName)){
    if (length(variablesName) != 0){
      attributes <- c(attributes, variablesName = paste(variablesName, collapse = ","))
    }
  } else {
    stop("variablesName is not a list")
  }

  if (provider != ""){
    attributes <- c(attributes, provider = provider)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }

  wateringResponse <- opensilexWSClientR::getResponseFromWS(resource = get("WATERING",configWS),
                                        attributes = attributes,
                                        wsVersion=1)
  return(wateringResponse)
}

# ##' @title postPhenotypes
# ##'
# ##' @description Post phenotypes data
# ##' @details !!!in development!!!
# ##' @param experimentURI URI of the experiment
# ##' @param data list of dataframe
# ##'  $ data :'data.frame':  1 obs. of  5 variables:
# ##' .. ..$ plantURI      : chr "http://www.phenome-fppn.fr/m3p/arch/2016/c16001681"
# ##' .. ..$ codeVariableId: chr "leafArea_unspecified_square meter"
# ##' .. ..$ date          : chr "2016-05-18 04:30:10"
# ##' .. ..$ confidence    : logi NA
# ##' .. ..$ value         : int 0'
# ##' @seealso http://147.99.7.5:8080/phenomeapi/api-docs/#!/environment/postPhenotypes
# ##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
# ##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
# ##' service
# ##' @examples
# ##' # Not run (is an internal function)
# ##' @keywords internal
# postPhenotypes <- function( experimentURI = "", data = NULL, reportId = ""){
#
#   attributes = list(sessionId = token)
#   if ( is.null(data)){
#     stop("data attribute must be filled")
#   }
#   if (experimentURI == ""){
#     stop("experimentURI must be given")
#   }
#   if (reportId == ""){
#     stop("reportId must be given")
#   }
#
#   configuration<-list(reportId = reportId)
#   phenotypesList<-list(experimentURI = experimentURI,
#                         configuration = configuration,
#                         data = data)
#
#   # str(phenotypesList)
#   postPhenotypeResponse <- postResponseFromWS(resource = get("PHENOTYPES",configWS),
#                                               attributes = attributes, requestBody = list(phenotypesList),
#                                               verbose=verbose)
#   return(postPhenotypeResponse)
# }
#
#
##' @title getLabelViewByExperimentById
##'
##' @description Retrieves LabelViews used in a specific experiment
##' @param viewType type of view, top or side
##' @param cameraAngle numeric, angle of the camera
##' @param provider origin of the data
##' @param experimentURI experiment unique identifier
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)

##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' # Not run (is an internal function)
##'  connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##' publicLabelView <- getLabelViewByExperimentById(
##'           experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2017-11-23")
##' publicLabelView$data
##' }
##' @keywords internal
getLabelViewByExperimentById <- function(experimentURI="" ,viewType="" ,cameraAngle="",
                                         provider="",page = NULL,pageSize = NULL){

  attributes = list(page = page, pageSize = pageSize)

  if(experimentURI  == ""){
    stop("no experimentURI selected")
  } else {
    if (viewType != ""){
      attributes <- c(attributes, viewType = viewType)
    }
    if (cameraAngle != ""){
      attributes <- c(attributes, cameraAngle = cameraAngle)
    }
    if (provider != ""){
      attributes <- c(attributes, provider = provider)
    }
    # AC 28/10/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    expUrlEncoded<-paste0(utils::URLencode(experimentURI, reserved = TRUE),"/labelViews")
    experimentLabelViewsResponse <- opensilexWSClientR::getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                                      paramPath = expUrlEncoded, 
                                                      attributes =  attributes,
                                                      wsVersion=1
                                                      )

    return(experimentLabelViewsResponse)
  }
}
