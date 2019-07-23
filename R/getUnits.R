#-------------------------------------------------------------------------------
# Program: getUnits.R
# Objective: functions to get the unit service from WS2
#            * getUnits
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getUnits
##'
##' @description Retrieves the unit based on search criterion
##' @param token character, a token from \code{\link{getToken}} function
##' @param uri character, search by the uri of a unit (optional)
##' @param label character, search by the label of a unit (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{getToken}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connect(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##' aToken = getToken("guest@opensilex.org","guest")
##' units <- getUnits(aToken$data,
##'                    uri="http://www.opensilex.org/demo/id/units/u007")
##' units$data
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
