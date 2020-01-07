#-------------------------------------------------------------------------------
# Program: getExperiments.R
# Objective: functions to get the experiments service from WS1 or WS2
#             * getExperimentById: for WS1, experiment by id
#             * getExperiments: for WS1, experiment
#             * getExperiments2: for WS2
# Do we have to join getExperiments and getExperiments2????
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
# Update: 06/01/2020 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getExperimentById
##'
##' @description retrieves the informations for one experiment
##' @param experimentURI URI of the experiment
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)

##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##'  Exp1<-getExperimentById(
##'         experimentURI ="http://www.phenome-fppn.fr/m3p/ARCH2012-01-01")
##'  Exp1$data
##' }
##' @keywords internal
getExperimentById <- function( experimentURI ="", page = NULL,pageSize = NULL){
  attributes = list(page = page, pageSize = pageSize)
  if (experimentURI  == ""){
    stop("no experimentURI selected")
  } else {
    # IS 03/11/2016 Suppress double URL encoding. Update tomcat allowed encoded slash security parameter
    expUrlEncoded<-paste0(utils::URLencode(experimentURI,  reserved = TRUE),"/details")
    experimentResponse<-opensilexWSClientR::getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                          paramPath=expUrlEncoded, attributes=attributes, wsVersion = 1)
    return(experimentResponse)
  }
}

##' @title retrieves the experiments from the web service
##'
##' @description Retrieves the available experiments and/or linked to a project
##' @param projectName  project name
##' @param season character, a year when the experiment was conducted
##' @param sortOrder ordering "ASC" or "DESC"
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)

##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToPHISWS}} function first to have access to the web
##' service
##' @seealso You have to install the opensilexWSClientR before running any 
##'          request on PHIS web service.
##' @examples
##' \donttest{
##'  connectToPHISWS(apiID="ws_1_public", 
##'                  username = "guestphis@supagro.inra.fr",
##'                  password = "guestphis")
##'  getExperiments()
##' }
##' @export
getExperiments <- function( projectName ="", season = "", sortOrder = "DESC" ,
                           page = NULL,pageSize = NULL){
  
  attributes = list(page = page, pageSize = pageSize)
  
  if (projectName != ""){
    attributes <- c(attributes, projectName = projectName)
  }
  if (season != ""){
    attributes <- c(attributes, season = season)
  }
  if (sortOrder != ""){
    attributes <- c(attributes, sortOrder = sortOrder)
  }
  experimentResponse<-opensilexWSClientR::getResponseFromWS(resource = get("EXPERIMENT",configWS),
                                        attributes = attributes, wsVersion = 1)
  return(experimentResponse)
}

#-------------------------------------------------------------------------------
##' @title getExperiments2
##'
##' @description retrieves the experiments based on search criterion
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
##' myexp <- getExperiments2(
##'               uri = "http://www.opensilex.org/demo/DIA2017-1")
##' myexp$data
##' }
##' @export
getExperiments2 <- function(
                            uri = "",
                            startDate = "",
                            endDate = "",
                            field = "",
                            campaign = "",
                            place = "",
                            alias = "",
                            keywords = "",
                            page = NULL,
                            pageSize = NULL){
  
  
  
  attributes <- list(pageSize = pageSize,  page = page)
  if (uri!="")      attributes <- c(attributes, uri = uri)
  if (startDate!="")attributes <- c(attributes, startDate = startDate)
  if (endDate!="" ) attributes <- c(attributes, endDate = endDate)
  if (field!="")    attributes <- c(attributes, field = field)
  if (campaign!="") attributes <- c(attributes, campaign = campaign)
  if (place!="")    attributes <- c(attributes, place = place)
  if (alias!="")    attributes <- c(attributes, alias = alias)
  if (keywords!="") attributes <- c(attributes, keywords = keywords)
  
  variableResponse <- opensilexWSClientR::getResponseFromWS(resource = paste0(get("EXPERIMENTS", configWS)),
                                         attributes = attributes, wsVersion = 2)
  return(variableResponse)
}
