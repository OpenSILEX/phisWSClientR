#-------------------------------------------------------------------------------
# Program: wsFunctions.R
# Objective: functions called by the user on the web service Phenomeapi
# Author: A. Charleroy
# Creation: 12/08/2016
# Update: 29/10/2018 (by I.Sanchez) - 30/10/2016 (by  A. Charleroy)
#-------------------------------------------------------------------------------

##' @title retrieves a user identifier for connexion to the web service
##'
##' @description Retrieves a user identifier for connexion to the WebService (WS)
##' @param login login of the user to create the token
##' @param password password of the user to create the token
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return a session token user identifier in the WS
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##' aToken <- getToken("guestphis@supagro.inra.fr","guestphis")
##' aToken$data
##' }
##' @export
getToken<-function(login,password,verbose=FALSE){
  attributes<-list(username = login, password = password)

  # Try 1 on first web service
  tokenResp1<-getTokenResponseWS(resource = get("TOKEN",configWS), attributes = attributes,verbose=verbose)

  # Try 2 on second web service
  tokenResp2<-getTokenResponseWS2(resource = get("BRAPITOKEN",configWS), attributes = attributes,verbose=verbose)

  # Test which WS is OK
  if (tokenResp1$status_code >= 200 && tokenResp1$status_code < 300 && tokenResp2$status_code >=400){
    json = jsonlite::fromJSON(httr::content(tokenResp1, as = "text", encoding = "UTF-8"))
    response <- list(
      currentPage = json$metadata$pagination$currentPage,
      totalCount = json$metadata$pagination$totalCount,
      totalPages = json$metadata$pagination$totalPages,
      codeHttp = tokenResp1$status_code,
      codeHttpMessage = "Query executed and data recovered - WS1",
      codeStatusMessage = json$metadata$status,
      data = json$session_token,
      ws="WS1")
    print("Query executed and data recovered - WS1")
  } else if (tokenResp2$status_code >= 200 && tokenResp2$status_code < 300 && tokenResp1$status_code >=400){
    json = jsonlite::fromJSON(httr::content(tokenResp2, as = "text", encoding = "UTF-8"))
    response <- list(
      #currentPage = json$metadata$pagination$currentPage,
      #totalCount = json$metadata$pagination$totalCount,
      #totalPages = json$metadata$pagination$totalPages,
      codeHttp = tokenResp2$status_code,
      codeHttpMessage = "Query executed and data recovered - WS2",
      codeStatusMessage = json$metadata$status,
      data = json$access_token,
      ws="WS2")

    print("Query executed and data recovered - WS2")
  } else if(tokenResp1$status_code == 500 || tokenResp2$status_code == 500){
       print("WebService internal error")
  } else if(tokenResp1$status_code == 401 || tokenResp2$status_code == 401){
    print("User not authorized")
  } else if(tokenResp1$status_code == 404 || tokenResp2$status_code == 404){
    print("Not found")
  } else if((tokenResp1$status_code >= 400 && tokenResp1$status_code != 401 &&
             tokenResp1$status_code != 404 && tokenResp1$status_code < 500) &&
            (tokenResp2$status_code >= 400 && tokenResp2$status_code != 401 &&
             tokenResp2$status_code != 404 && tokenResp2$status_code < 500)){
    print("Bad user request")
  }

  if (tokenResp1$status_code > 250 && tokenResp2$status_code > 250){
      print("No web service available! Check your login/password and/or your url...")
  }

  # define class S3 and return the list if exists
  if (exists("response")){
    class(response) <- append(class(response),"WSResponse")
    return(response)
  }
}


##' @title getVariablesByCategory
##'
##' @description Retrieves the variable by categories (environment or setpoint...)
##' @param token a token
##' @param category Name of the category to search
##' @param imageryProvider character, provider of the images
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  vars <- getVariablesByCategory(aToken$data,category="imagery",
##'           experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  vars$data
##' }
##' @export
getVariablesByCategory<-function(token,category ="",experimentURI ="",imageryProvider="",
                                 page=NULL,pageSize=NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId=token, page=page, pageSize = pageSize)
  if (category  == ""){
    stop("no category selected")
  } else {
    if (experimentURI != ""){
      attributes <- c(attributes, experimentURI = experimentURI)
    }
    if (imageryProvider != ""){
      attributes <- c(attributes, imageryProvider = imageryProvider)
    }
    variableResponse <- getResponseFromWS(resource=paste0(get("VARIABLES",configWS),"/category/",category),
                                          attributes = attributes,verbose=verbose)
    return(variableResponse)
  }
}


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

##' @title getExperimentById
##'
##' @description retrieves the informations for one experiment
##' @param token a token
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  publicExp<-getExperimentById(aToken$data,
##'         experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  publicExp$data
##' }
##' @keywords internal
getExperimentById <- function(token, experimentURI ="", page = NULL,pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (experimentURI  == ""){
    stop("no experimentURI selected")
  } else {
    # IS 03/11/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    expUrlEncoded<-paste0(utils::URLencode(experimentURI,  reserved = TRUE),"/details")
    experimentResponse<-getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                          paramPath=expUrlEncoded,attributes=attributes,verbose=verbose)
    return(experimentResponse)
  }
}

##' @title retrieves the experiments from the web service
##'
##' @description Retrieves the available experiments and/or linked to a project
##' @param token a token
##' @param projectName  project name
##' @param season character, a year when the experiment was conducted
##' @param sortOrder ordering "ASC" or "DESC"
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken = getToken("guestphis@supagro.inra.fr","guestphis")$data
##'  getExperiments(aToken,page=3,pageSize=100,startDate="2012-02-21",endDate="2012-03-21")
##'  getExperiments(aToken,projectName = "PHIS_Publi")
##'  getExperiments(aToken,sortOrder = "ASC")
##'  getExperiments(aToken,season = 2012 )
##' }
##' @export
getExperiments <- function(token, projectName ="", season = "", sortOrder = "DESC" ,
                           page = NULL,pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)

  if (projectName != ""){
    attributes <- c(attributes, projectName = projectName)
  }
  if (season != ""){
    attributes <- c(attributes, season = season)
  }
  if (sortOrder != ""){
    attributes <- c(attributes, sortOrder = sortOrder)
  }
  experimentResponse<-getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                        attributes = attributes,verbose=verbose)
  return(experimentResponse)
}

##' @title retrieves the context of plant linked to an experiment from the web service
##'
##' @description Retrieves context of plant linked to an experiment
##' @param token a token
##' @param plantAlias an alias of plant
##' @param experimentURI URI of the experiment
##' @param germplasmURI filter by genotype
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_public")
##'  aToken<-getToken("guestphis@supagro.inra.fr","guestphis")$data
##'  plantes<-getPlants(aToken,experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##' }
##' @export
getPlants <- function(token, plantAlias ="", experimentURI = "", germplasmURI = "" ,
                      page = NULL,pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)

  if (plantAlias != ""){
    attributes <- c(attributes, plantAlias = plantAlias)
  }
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }
  if (germplasmURI != ""){
    attributes <- c(attributes, germplasmURI = germplasmURI)
  }
  plantsResponse<-getResponseFromWS(resource = get("PLANTS",configWS),attributes = attributes,verbose=verbose)
  return(plantsResponse)
}

##' @title getPlantsContextByID
##'
##' @description Retrieves the contect of a plant
##' @param token a token
##' @param plantURI character, a URI identifier of a plant
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' # not run (is an internal function!!!)
##' # aToken<-getToken("guestphis@supagro.inra.fr","guestphis")$data
##' # test<-getPlantsContextByID(aToken,plantURI="http://www.phenome-fppn.fr/m3p/arch/2011/c11005809",
##' #       ,experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##' # test$data
##' @keywords internal
getPlantsContextByID<-function(token, plantURI ="",experimentURI="",page = NULL,
                                 pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (experimentURI != ""){
    attributes <- c(attributes, experimentURI = experimentURI)
  }

  if (plantURI  == ""){
    stop("no plantURI selected")
  } else {
    # AC 28/10/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    plantURIEncoded = utils::URLencode(plantURI,reserved = TRUE)
    plantByIDResponse<-getResponseFromWS(resource = get("PLANTS",configWS),
                                        paramPath=plantURIEncoded,attributes=attributes,verbose=verbose)
    return(plantByIDResponse)
  }
}


##' @title retrieves the environmental mesures of a plant from the web service
##'
##' @description Retrieves environmental mesures of a plant or by dates
##' @param token a token
##' @param plantURI plant URI
##' @param variableCategory character, a category of variables
##' @param startDate data > startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param endDate data < startDate (Format: YYYY-MM-DD or YYYY-MM-DD HH:MM:SS )
##' @param variables list of variables for the request (Ex: "wind speed_weather station_meter per second")
##' @param facility place of the experiment (Ex: "http://www.phenome-fppn.fr/m3p/ec3")
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @examples
##' # not run (is an internal function!!!)
##' # aToken<-getToken("guestphis@supagro.inra.fr","guestphis")$data
##' # myplant<-getPlantEnvironment(aToken,
##' #       plantURI="http://www.phenome-fppn.fr/m3p/arch/2011/c11005809",
##' #       experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##' # myplant$data
##' @keywords internal
getPlantEnvironment <- function(token,plantURI ="",variableCategory ="",startDate = "",endDate = "",
                                variables = "",facility = "", experimentURI ="",
                                page = NULL,pageSize = NULL,verbose=FALSE){
  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (plantURI  == ""){
    stop("no plantURI given")
  } else {
    attributes = list(sessionId = token, page = page, pageSize = pageSize)
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
    plantEnvironmentResponse<-getResponseFromWS(resource = get("PLANTS",configWS),
                                                paramPath = paste0(plantURIEncoded,"/environment"),
                                                attributes =  attributes,verbose=verbose)
    return(plantEnvironmentResponse)
  }
}

##' @title retrieves the images analysis data from the web service
##'
##' @description Retrieves data from image analysis
##' @param token a token
##' @param experimentURI URI of the experiment
##' @param variablesName list, variable names of images analysis (ex : "objAreaSum")
##' @param labelView character, label view of an image
##' @param provider character, provider of data
##' @param date character, data for one day (format: YYYY-MM-DD)
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
##'  myImages<-getImagesAnalysis(token = aToken$data,
##'            experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01",
##'            variablesName = list("objAreaSum"),pageSize = 100000,verbose=FALSE)
##'  head(myImages$data)
##' }
##' @export
getImagesAnalysis <- function(token, experimentURI ="", variablesName = list(),
                              labelView ="", provider = "", date = "",
                              page = NULL,pageSize = NULL,verbose=FALSE){

  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
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
  imagesAnalysisResponse <- getResponseFromWS(resource = get("IMAGESANALYSIS",configWS),
                                              attributes = attributes,verbose=verbose)
  return(imagesAnalysisResponse)
}


##' @title retrieves the irrigation data from the web service
##'
##' @description Retrieves irrigation data
##' @param token a token
##' @param experimentURI URI of the experiment
##' @param variablesName list, variable names of images analysis
##' @param provider character, provider of data
##' @param date character, data for one day (format: YYYY-MM-DD)
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
##'  accesToken = getToken("guestphis@supagro.inra.fr","guestphis")
##'  mywater<-getWatering(token=accesToken$data,
##'          experimentURI = "http://www.phenome-fppn.fr/m3p/ARCH2012-01-01",
##'          variablesName = list("weightBefore"),pageSize=100000,verbose=FALSE)
##'  head(mywater$data)
##' }
##' @export
getWatering <- function(token, experimentURI ="", variablesName = list(), provider = "", date = "",
                        page = NULL,pageSize = NULL,verbose=FALSE){

  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
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

  wateringResponse <- getResponseFromWS(resource = get("WATERING",configWS),
                                        attributes = attributes,verbose=verbose)
  return(wateringResponse)
}

# ##' @title postPhenotypes
# ##'
# ##' @description Post phenotypes data
# ##' @details !!!in development!!!
# ##' @param token a token
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
# ##' @details You have to execute the getToken() function first to have access to the web
# ##' service
# ##' @examples
# ##' # Not run (is an internal function)
# ##' @keywords internal
# postPhenotypes <- function(token, experimentURI = "", data = NULL, reportId = "", verbose=FALSE){
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
##' @param token a token
##' @param viewType type of view, top or side
##' @param cameraAngle numeric, angle of the camera
##' @param provider origin of the data
##' @param experimentURI experiment unique identifier
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)
##' @param verbose logical FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' # Not run (is an internal function)
##' # aToken = getToken("guestphis@supagro.inra.fr","guestphis")$data
##' # publicLabelView <- getLabelViewByExperimentById(aToken,
##' #      experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##' # publicLabelView$data
##' @keywords internal
getLabelViewByExperimentById <- function(token ,experimentURI="" ,viewType="" ,cameraAngle="",provider="",
                                         page = NULL,pageSize = NULL,verbose=FALSE){

  if (is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  attributes = list(sessionId = token, page = page, pageSize = pageSize)

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
    experimentLabelViewsResponse <- getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                                      paramPath = expUrlEncoded, attributes =  attributes)

    return(experimentLabelViewsResponse)
  }
}
