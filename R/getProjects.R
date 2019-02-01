#-------------------------------------------------------------------------------
# Program: getProjects.R
# Objective: functions to get the project service from WS1 or WS2
#             * getProjects: for WS1
#             * getProjects2: for WS2
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getProjects retrieves the list of projects from the web service
##'
##' @description Retrieves the list of projects in the WS
##' @param token a token
##' @param projectName Name of the project to search
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
##'  getProjects(aToken$data)
##'  getProjects(aToken$data, page = 1)
##'  getProjects(aToken$data, page = 3, pageSize = 100)
##'  getProjects(aToken$data, projectName = "PHIS_Publi")
##' }
##' @export
getProjects<-function(token, projectName = "",page=NULL,pageSize=NULL,verbose=FALSE){
  if(is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  
  attributes = list(sessionId = token, page = page, pageSize = pageSize)
  if (projectName != ""){
    attributes <- c(attributes, projectName = projectName)
  }
  projectResponse<-getResponseFromWS(resource = get("PROJECTS",configWS),attributes=attributes,verbose=verbose)
  return(projectResponse)
}

#---------------------------------------------------------------
##' @title getProjects2
##'
##' @description Retrieves the list of projects in the WS2 with the selected filters
##' @param token character, a token from getToken function
##' @param uri character, search by the URI of a project (optional)
##' @param name character, search by the name of a project (optional)
##' @param acronyme character, search by the acronyme of a project (optional)
##' @param subprojectType character, search by the subproject type of a project (optional)
##' @param financialSupport character, search by the financial support of a project (optional)
##' @param financialName character, search by the financial name of a project (optional)
##' @param dateStart character, the date of the beginning of a project (optional)
##' @param dateEnd character, the date of the end of a project (optional)
##' @param keywords character, search by keywords of a project (optional)
##' @param parentProject character, search by parent project (optional)
##' @param website character, search by website of a project (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API) (optional)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API) (optional)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private",
##'   url = "www.opensilex.org/openSilexAPI/rest/")
##'  aToken = getToken("guest@opensilex.org","guest")
##'  projects <- getProjects2(aToken$data,
##'   uri="http://www.opensilex.org/demo/PHENOME-FPPN")
##'  projects$data
##' }
##' @export

getProjects2 <- function(token,
                         page = NULL,
                         pageSize = NULL,
                         uri = "",
                         name = "",
                         acronyme = "",
                         subprojectType = "",
                         financialSupport = "",
                         financialName = "",
                         dateStart = "",
                         dateEnd = "",
                         keywords = "",
                         parentProject = "",
                         website = "",
                         verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(page = page,
                     pageSize = pageSize,
                     Authorization = token)
  
  if (uri != "")              attributes <- c(attributes, uri = uri)
  if (name != "")             attributes <- c(attributes, name = name)
  if (acronyme != "")         attributes <- c(attributes, acronyme = acronyme)
  if (subprojectType != "")   attributes <- c(attributes, subprojectType = subprojectType)
  if (financialSupport != "") attributes <- c(attributes, financialSupport = financialSupport)
  if (financialName != "")    attributes <- c(attributes, financialName = financialName)
  if (dateStart != "")        attributes <- c(attributes, dateStart = dateStart)
  if (dateEnd != "")          attributes <- c(attributes, dateEnd = dateEnd)
  if (keywords != "")         attributes <- c(attributes, keywords = keywords)
  if (parentProject != "")    attributes <- c(attributes, parentProject = parentProject)
  if (website != "")          attributes <- c(attributes, website = website)
  
  projectResponse <- getResponseFromWS2(resource = get("PROJECTS", configWS),
                                        attributes = attributes,
                                        verbose = verbose)
  return(projectResponse)
}