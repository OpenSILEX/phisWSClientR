#-------------------------------------------------------------------------------
# Program: getExperiments.R
# Objective: functions to get the experiments service from WS1 or WS2
#             * getExperimentById: for WS1, experiment by id
#             * getExperiments: for WS1, experiment
#             * getExperiments2: for WS2
# Do we have to join getExperiments and getExperiments2????
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
# Update: 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

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
##'  aToken <- getToken("guestphis@supagro.inra.fr","guestphis")
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
##'  aToken <- getToken("guestphis@supagro.inra.fr","guestphis")$data
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

#-------------------------------------------------------------------------------
##' @title getExperiments2
##'
##' @description retrieves the experiments based on search criterion
##' @param token character, a token from getToken function
##' @param uri character, search by the uri of an experiment (optional)
##' @param startDate character, search from the start date of experiment (optional)
##' @param endDate character, search to the end date of experiment (optional)
##' @param field character, search by the field of an experiment (optional)
##' @param campaign character, search by the campaign of an experiment (optional)
##' @param place character, search by the place of an experiment (optional)
##' @param alias character, search by the alias of an experiment (optional)
##' @param keywords character, search by keywords of an experiment (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##' aToken = getToken("guest@opensilex.org","guest")
##' myexp <- getExperiments2(aToken$data,
##'                    uri = "http://www.opensilex.org/demo/DIA2017-1")
##' myexp$data
##' }
##' @export
getExperiments2 <- function(token,
                            uri = "",
                            startDate = "",
                            endDate = "",
                            field = "",
                            campaign = "",
                            place = "",
                            alias = "",
                            keywords = "",
                            page = NULL,
                            pageSize = NULL,
                            verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize = pageSize,
                     page = page,
                     Authorization=token)
  if (uri!="")      attributes <- c(attributes, uri = uri)
  if (startDate!="")attributes <- c(attributes, startDate = startDate)
  if (endDate!="" ) attributes <- c(attributes, endDate = endDate)
  if (field!="")    attributes <- c(attributes, field = field)
  if (campaign!="") attributes <- c(attributes, campaign = campaign)
  if (place!="")    attributes <- c(attributes, place = place)
  if (alias!="")    attributes <- c(attributes, alias = alias)
  if (keywords!="") attributes <- c(attributes, keywords = keywords)
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("EXPERIMENTS", configWS)),
                                         attributes = attributes,
                                         verbose = verbose)
  return(variableResponse)
}
