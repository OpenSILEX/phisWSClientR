#-------------------------------------------------------------------------------
# Program: getData.R
# Objective: functions to get the Data service from WS2
#            * getData
# Authors: Hollebecq Jean-Eudes
# Creation: 12/03/2019
# Update: 20/06/2019

#-------------------------------------------------------------------------------

##' @title getData
##'
##' @description Retrieves the data from the web service
##' @param token character, a token from \code{\link{getToken}} function
##' @param variableUri character, search by the uri of a variable (NOT optional). You can access the list of variables through \code{\link{getVariables2}} function.
##' @param startDate character, search from start date (optional)
##' @param endDate character, search to end date (optional)
##' @param objectUri character, search by object uri
##' @param objectLabel character, search by object label
##' @param provenanceUri character, search by provenance uri
##' @param provenanceLabel character, search by provenance label
##' @param page numeric, displayed page (pagination Plant Breeding API)
##' @param pageSize numeric, number of elements by page (pagination Plant Breeding API)
##' @param verbose logical, FALSE by default, if TRUE display information about the progress
##' @return WSResponse object
##' @seealso http://docs.brapi.apiary.io/#introduction/url-structure
##' @details You have to execute the \code{\link{getToken}} function first to have access to the web
##' service
##' @examples
##' \donttest{
##'  initializeClientConnection(apiID="ws_private", url = "www.opensilex.org/openSilexAPI/rest/")
##'  token = getToken("guest@opensilex.org","guest")$data
##'  vars = getVariables2(token = token)$data$uri
##'  totalCount <- getData(token, variable = vars[4])$totalCount
##'  data <- getData(token,
##'     variable = vars[4], pageSize = totalCount)
##'  data <- getData(token,
##'     variable = vars[5],
##'    startDate = "2017-05-01", endDate = "2017-06-01", pageSize = totalCount)
##'  data$data
##' }
##' @export
getData <- function(token,
                    variableUri = "",
                    objectUri = "",
                   objectLabel = "",
                   provenanceUri = "",
                   provenanceLabel  = "",
                   startDate = "",
                   endDate = "",
                   page = NULL,
                   pageSize = NULL,
                   verbose = FALSE){
  if (is.null(page)) page <- get("DEFAULT_PAGE",configWS)
  if (is.null(pageSize)) pageSize <- get("DEFAULT_PAGESIZE",configWS)
  
  attributes <- list(pageSize=pageSize,
                     page = page,
                     Authorization=token)
  if (variableUri!="")    attributes <- c(attributes, variableUri = variableUri)
  if (objectUri!="")      attributes <- c(attributes, objectUri = objectUri)
  if (objectLabel!="") attributes <- c(attributes, objectLabel = objectLabel)
  if (startDate!="")   attributes <- c(attributes, startDate = startDate)
  if (endDate!="")     attributes <- c(attributes, endDate = endDate)
  if (provenanceUri!="")  attributes <- c(attributes, provenanceUri = provenanceUri)
  if (provenanceLabel!="") attributes <- c(attributes, provenanceLabel = provenanceLabel)
  
  variableResponse <- getResponseFromWS2(resource = paste0(get("DATASEARCH", configWS)),
                                         attributes = attributes,
                                         verbose = verbose)
  
  # convert value column from character to numeric
  variableResponse$data[,"value"]<-as.numeric(variableResponse$data[,"value"])
  
  return(variableResponse)
}