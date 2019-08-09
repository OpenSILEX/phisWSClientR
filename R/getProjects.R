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
##' @param projectName Name of the project to search
##' @param page displayed page (pagination Plant Breeding API)
##' @param pageSize number of elements by page (pagination Plant Breeding API)

##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToOpenSILEXWS(apiID="ws_public")
##'  getProjects()
##'  getProjects(page = 1)
##'  getProjects(page = 3, pageSize = 100)
##'  getProjects(projectName = "PHIS_Publi")
##' }
##' @export
getProjects<-function( projectName = "",page=NULL,pageSize=NULL){
  if(is.null(page)) page<-get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize<-get("DEFAULT_PAGESIZE",configWS)
  
  attributes = list(sessionId = get("TOKEN_VALUE",configWS), page = page, pageSize = pageSize)
  if (projectName != ""){
    attributes <- c(attributes, projectName = projectName)
  }
  projectResponse<-getResponseFromWS(resource = get("PROJECTS",configWS),attributes=attributes)
  return(projectResponse)
}

#---------------------------------------------------------------
##' @title getProjects2
##'
##' @description Retrieves the list of projects in the WS2 with the selected filters
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
##' @details You have to execute the \code{\link{connectToOpenSILEXWS}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  connectToOpenSILEXWS(apiID="ws_private",
##'   url = "http://www.opensilex.org/openSilexAPI/rest/")
##'  projects <- getProjects2(
##'   uri="http://www.opensilex.org/demo/PHENOME-FPPN")
##'  projects$data
##' }
##' @export

getProjects2 <- function(
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
                     pageSize = pageSize)
  
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
                                        attributes = attributes)
  return(projectResponse)
}