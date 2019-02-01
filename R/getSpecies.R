#-------------------------------------------------------------------------------
# Program: getSpecies.R
# Objective: functions to get the specie service from WS2
#            * getSpecies
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getSpecies
##'
##' @description retrieves the specie behind the specie uri
##' @param token character, a token from getToken function
##' @param uri character, search by the uri of a specie (optional)
##' @param label character, search by the label of a specie (optional)
##' @param language character, language of the answer, "en", "fr", etc (optional)
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
##' species <- getSpecies(aToken$data)
##' species$data
##' }
##' @export
getSpecies <- function(token,
                       uri = "",
                       label = "",
                       language = "",
                       page = NULL,
                       pageSize = NULL,
                       verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE", configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE", configWS)
  
  attributes <- list(pageSize = pageSize,
                     page = page,
                     Authorization=token)
  if (uri!="")      attributes <- c(attributes, uri = uri)
  if (label!="")    attributes <- c(attributes, label = label)
  if (language!="") attributes <- c(attributes, language = language)
  
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("SPECIES", configWS)),
                                         attributes = attributes,
                                         verbose = verbose)
  return(variableResponse)
}
