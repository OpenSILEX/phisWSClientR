#-------------------------------------------------------------------------------
# Program: getTraits.R
# Objective: functions to get the trait service from WS2
#            * getTraits
# Authors: Hollebecq Jean-Eudes
# Creation: 21/01/2019
# Update: 01/02/2019 (by J-E.Hollebecq) ; 24/01/2019 (by I.Sanchez)
#-------------------------------------------------------------------------------

##' @title getTraits
##'
##' @description retrieves the trait based on search criterion
##' @param token character, a token from \code{\link{getToken}} function
##' @param uri character, search by the uri of a trait (optional)
##' @param label character, search by the label of a trait (optional)
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{getToken}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##' connectToWS(apiID="ws_private", url = "http://www.opensilex.org/openSilexAPI/rest/")
##' aToken = getToken("guestphis@supagro.inra.fr","guestphis")
##' traits <- getTraits(aToken$data,
##'                    uri = "http://www.opensilex.org/demo/id/traits/t010")
##' traits$data
##' }
##' @export
getTraits <- function(
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
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("TRAITS", configWS)),
                                         attributes = attributes)
  return(variableResponse)
}
