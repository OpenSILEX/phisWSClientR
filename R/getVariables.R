#-------------------------------------------------------------------------------
# Program: getVariables.R
# Objective: functions to get the variables service from WS1 or WS2
#             * getVariablesByCategory: for WS1
#             * getVariables2: for WS2
# Authors: A. Charleroy, I.Sanchez, J.E.Hollebecq, E.Chourrout
# Creation: 24/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

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

#----------------------------------------------------------------------------
##' @title getVariables2
##'
##' @description Retrieves the variable descriptions, trait, method and unit covered by the variable
##' @param token character, a token from getToken function
##' @param uri character, search by the uri of an experiment (optional)
##' @param label character, search by label (optional)
##' @param trait character, search by trait uri (optional)
##' @param method character, search by method uri (optional)
##' @param unit character, search variables by unit uri (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize character, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest")
##'  aToken <- getToken("guest@phis.fr","guest")
##'  vars <- getVariables2(aToken$data, uri = "http://www.phenome-fppn.fr/ues/id/variables/v001")
##'  vars <- getVariables2(aToken$data,label = "Leaf-Area_LAI-Computation_LAI")
##'  vars$data
##' }
##' @export
getVariables2 <- function(token,
                          uri = "",
                          label = "",
                          trait = "",
                          method = "",
                          unit = "",
                          pageSize = NULL,
                          page = NULL,
                          verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE",configWS)
  
  attributes <- list(pageSize=pageSize,
                     page = page,
                     Authorization=token)
  if (uri!="")    attributes <- c(attributes, uri = uri)
  if (label!="")  attributes <- c(attributes, lavel = label)
  if (trait!="")  attributes <- c(attributes, trait = trait)
  if (method!="") attributes <- c(attributes, method = method)
  if (unit!="")   attributes <- c(attributes, unit = unit)
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("VARIABLES", configWS)),
                                         attributes = attributes,
                                         verbose = verbose)
  return(variableResponse)
}
