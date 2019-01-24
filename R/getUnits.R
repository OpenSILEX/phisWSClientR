#-------------------------------------------------------------------------------
# Program: getUnits.R
# Objective: functions to get the unit service from WS2
#            * getUnits
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getUnits
##'
##' @description Retrieves the unit based on search criterion
##' @param token character, a token from getToken function
##' @param uri character, search by the uri of a unit (optional)
##' @param label character, search by the label of a unit (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the getToken() function first to have access to the web
##' service
##' @examples
##' \donttest{
##' initializeClientConnection(apiID="ws_private", url = "138.102.159.36:8080/uesAPI/rest/")
##' aToken = getToken("guest@phis.fr","guest")
##' vars <- getUnits(aToken$data,
##'                    uri = "http://www.phenome-fppn.fr/ues/id/units/u001")
##' vars$data
##' }
##' @export
getUnits <- function(token,
                      uri = "",
                      label = "",
                      page = NULL,
                      pageSize = NULL,
                      verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize = pageSize,
                     page = page,
                     Authorization=token)
  if (uri!="")   attributes <- c(attributes, uri = uri)
  if (label!="") attributes <- c(attributes, label = label)
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("UNITS", configWS)),
                                         attributes = attributes,
                                         verbose = verbose)
  return(variableResponse)
}
